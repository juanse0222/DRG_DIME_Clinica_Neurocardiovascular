"""
build_project.py — Genera el proyecto QGIS de pacientes DIME geocodificados.

Ejecutar con el propio binario de QGIS (para heredar su entorno Python
correctamente configurado), NO con un intérprete Python externo:

  /Applications/QGIS-final-4_2_1.app/Contents/MacOS/QGIS-final-4_2_1 \
      --code qgis/build_project.py --nologo

Capas:
  - Límite Cali, Comunas, Barrios (contexto administrativo)
  - Pacientes DIME (EAPB vs SLE)  — todos los pacientes geocodificados
  - Pacientes CACI                — solo cohorte CACI, por tipo (ICC/ACV/...)

Fuente de datos: output/geocoded_map_data.csv (sin PII: sin cédula, nombre
ni texto de dirección; coordenadas con jitter de privacidad).
"""
import os
import sys
import traceback

BASE = "/Users/juansebastianhurtadozapata/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME/Documentos EDI/Personal JSH/analisis_caci_2023"
LOG_PATH = os.path.join(BASE, "qgis", "build_log.txt")

log_lines = []
def log(msg):
    print(msg)
    log_lines.append(str(msg))

try:
    from qgis.core import (
        QgsProject, QgsVectorLayer, QgsCoordinateReferenceSystem,
        QgsCategorizedSymbolRenderer, QgsRendererCategory, QgsMarkerSymbol,
        QgsFillSymbol, QgsRuleBasedRenderer, QgsApplication
    )

    layers_dir = os.path.join(BASE, "qgis", "layers")
    csv_path = os.path.join(BASE, "output", "geocoded_map_data.csv")
    out_path = os.path.join(BASE, "qgis", "dime_pacientes_geocodificados.qgz")

    if not os.path.exists(csv_path):
        raise FileNotFoundError(csv_path)

    project = QgsProject.instance()
    project.clear()
    project.setCrs(QgsCoordinateReferenceSystem("EPSG:4326"))
    project.setTitle("DIME - Pacientes geocodificados (EAPB / SLE / CACI)")

    # ── Capas de contexto administrativo (Cali) ──────────────────────────────
    def load_ctx(shp_name, layer_name):
        path = os.path.join(layers_dir, shp_name)
        lyr = QgsVectorLayer(path, layer_name, "ogr")
        if not lyr.isValid():
            log(f"[AVISO] Capa de contexto invalida u omitida: {shp_name}")
            return None
        return lyr

    cali = load_ctx("Cali.shp", "Limite Cali")
    comunas = load_ctx("Comunas_Cali.shp", "Comunas")
    barrios = load_ctx("mc_barrios.shp", "Barrios")

    if cali is not None:
        sym = QgsFillSymbol.createSimple({
            'color': '0,0,0,0', 'outline_color': '20,20,20,255', 'outline_width': '0.6'
        })
        cali.renderer().setSymbol(sym)
    if comunas is not None:
        sym = QgsFillSymbol.createSimple({
            'color': '0,0,0,0', 'outline_color': '90,90,90,200', 'outline_width': '0.4'
        })
        comunas.renderer().setSymbol(sym)
    if barrios is not None:
        sym = QgsFillSymbol.createSimple({
            'color': '0,0,0,0', 'outline_color': '160,160,160,140', 'outline_width': '0.15'
        })
        barrios.renderer().setSymbol(sym)

    for lyr in (barrios, comunas, cali):
        if lyr is not None:
            project.addMapLayer(lyr)
            log(f"Capa de contexto agregada: {lyr.name()} ({lyr.featureCount()} entidades)")

    # ── Capa de pacientes: todos, categorizados por tipo de pagador ─────────
    uri = (
        f"file:///{csv_path}?type=csv&xField=lon&yField=lat"
        f"&crs=EPSG:4326&spatialIndex=no&subsetIndex=no&watchFile=no&delimiter=,"
    )
    patients = QgsVectorLayer(uri, "Pacientes DIME (EAPB vs SLE)", "delimitedtext")
    if not patients.isValid():
        raise RuntimeError("La capa de pacientes (CSV) no es valida: " + patients.error().summary())
    log(f"Capa de pacientes cargada: {patients.featureCount()} registros")

    payer_colors = {
        "EAPB": "#4E79A7",
        "Particular / Libre Eleccion (SLE)": "#F28E2B",
        "Sin dato": "#95A5A6",
    }
    # nombres reales en el CSV (con tildes) — se mapean por separado para
    # evitar problemas de codificacion en literales de este script
    payer_values = {
        "EAPB": "EAPB",
        "Particular / Libre Eleccion (SLE)": "Particular / Libre Elección (SLE)",
        "Sin dato": "Sin dato",
    }
    cats = []
    for label, hexcolor in payer_colors.items():
        real_value = payer_values[label]
        sym = QgsMarkerSymbol.createSimple({
            'name': 'circle', 'color': hexcolor, 'size': '2.0',
            'outline_color': '255,255,255,200', 'outline_width': '0.2'
        })
        cats.append(QgsRendererCategory(real_value, sym, label))
    renderer = QgsCategorizedSymbolRenderer('tipo_pagador', cats)
    patients.setRenderer(renderer)
    project.addMapLayer(patients)

    # ── Capa de pacientes CACI (solo cohorte CACI, coloreada por tipo) ──────
    patients_caci = QgsVectorLayer(uri, "Pacientes CACI", "delimitedtext")
    if not patients_caci.isValid():
        raise RuntimeError("La segunda instancia de la capa CSV no es valida")

    caci_colors = {
        "ICC": "#E15759", "ACV": "#4E79A7", "SCA": "#F28E2B",
        "TEP": "#76B7B2", "TxC": "#59A14F", "Otros CV": "#B07AA1",
    }
    root_rule = QgsRuleBasedRenderer.Rule(None)
    for caci_val, hexcolor in caci_colors.items():
        sym = QgsMarkerSymbol.createSimple({
            'name': 'star', 'color': hexcolor, 'size': '3.4',
            'outline_color': '0,0,0,255', 'outline_width': '0.25'
        })
        rule = QgsRuleBasedRenderer.Rule(sym, filterExp='"caci" = \'%s\'' % caci_val, label=caci_val)
        root_rule.appendChild(rule)
    patients_caci.setRenderer(QgsRuleBasedRenderer(root_rule))
    project.addMapLayer(patients_caci)
    log(f"Capa CACI agregada ({patients_caci.featureCount()} registros totales en la fuente)")

    # ── Vista inicial centrada en Cali ───────────────────────────────────────
    try:
        from qgis.utils import iface
        if iface is not None:
            extent = patients.extent()
            iface.mapCanvas().setExtent(extent)
            iface.mapCanvas().zoomByFactor(1.15)
            iface.mapCanvas().refresh()
            log("Vista de mapa centrada en extent de pacientes")
    except Exception as e:
        log(f"[AVISO] No se pudo ajustar la vista inicial: {e}")

    ok = project.write(out_path)
    log(f"Proyecto guardado: {out_path} (exito={ok})")

except Exception:
    log("ERROR:")
    log(traceback.format_exc())

finally:
    with open(LOG_PATH, "w", encoding="utf-8") as f:
        f.write("\n".join(log_lines))
    try:
        from qgis.core import QgsApplication
        QgsApplication.exit(0)
    except Exception:
        pass
