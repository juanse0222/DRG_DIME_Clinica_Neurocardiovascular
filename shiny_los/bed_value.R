################################################################################
# bed_value.R — Economía del DÍA-CAMA en DIME
#
# Responde tres preguntas que ninguna otra pestaña cubre:
#   1. ¿Cuánto CUESTA la cama a DIME, cuánto se FACTURA y qué dice el TARIFARIO?
#   2. ¿Cuánto varía la tarifa entre pagadores (EAPB)?
#   3. ¿Qué patología (CACI) consume la cama más cara?
#
# ── TRES BASES QUE NO DEBEN MEZCLARSE ────────────────────────────────────────
#   COSTO DIME   tabla CUPS de planeación (costo_general_<año>.xlsx). Lo que a
#                la institución le cuesta sostener esa cama un día.
#   TARIFA       valor_cargo_tarifario efectivamente facturado. Depende del
#                contrato con cada EAPB; es lo que DIME realmente recibe.
#   SOAT 2026    referente normativo externo (UVB $12.110). No es lo que DIME
#                cobra ni lo que le cuesta: es el techo de referencia del sector.
#
# Los costos CUPS se fijan aquí como constantes porque la tabla CUPS
# (data/data_costs/costo_general_2026.xlsx) NO se despliega con la app. Si
# planeación publica tarifas nuevas, actualizar estos valores.
#
# OJO — la columna `costo` del dataset de ventas NO sirve para la cama:
# devuelve $0 en Hospitalización y valores distintos a CUPS en UCI/UCIN
# (773.000 vs 946.000). Por eso se usa la tabla CUPS y no ese campo.
################################################################################

BED_COSTO_CUPS_2026 <- tibble::tribble(
  ~servicio_cama,        ~codigo_cups,   ~costo_dime,
  "UCI",                 "110A01",        946000,
  "UCIN (intermedio)",   "107M01",        599000,
  "Hospitalización",     "129A02/10A002", 300000     # bipersonal/múltiple
)

# SOAT 2026, institución de TERCER nivel (ver inactive_stay_cost.R)
BED_SOAT_2026 <- tibble::tribble(
  ~servicio_cama,        ~codigo_soat, ~soat_dia,
  "UCI",                 "38525",       2269200,
  "UCIN (intermedio)",   "38825",       1220400,
  "Hospitalización",     "38132",        503300
)

bed_value <- local({
  f <- file.path(data_dir, "bed_value.rds")
  if (!file.exists(f)) {
    message("[LOS-App] bed_value.rds no encontrado — pestaña Valor de Cama desactivada. ",
            "Genera con: Rscript shiny_los/prep_data.R")
    return(NULL)
  }
  bv <- readRDS(f)

  # Reconciliación de las tres bases
  bv$comparativo <- bv$por_servicio %>%
    select(servicio_cama, n, tarifa_med, tarifa_p25, tarifa_p75) %>%
    left_join(BED_COSTO_CUPS_2026, by = "servicio_cama") %>%
    left_join(BED_SOAT_2026,       by = "servicio_cama") %>%
    mutate(
      margen_abs  = tarifa_med - costo_dime,
      margen_pct  = round(100 * (tarifa_med - costo_dime) / costo_dime, 1),
      tarifa_vs_soat = round(100 * tarifa_med / soat_dia, 1)
    ) %>%
    arrange(desc(costo_dime))

  # Brecha por pagador: dónde se factura por debajo del costo
  bv$brecha_eapb <- bv$por_eapb %>%
    left_join(BED_COSTO_CUPS_2026 %>% select(servicio_cama, costo_dime),
              by = "servicio_cama") %>%
    mutate(margen_pct = round(100 * (tarifa_med - costo_dime) / costo_dime, 1)) %>%
    arrange(servicio_cama, margen_pct)

  bv$caci_comp <- bv$por_caci %>%
    left_join(BED_COSTO_CUPS_2026 %>% select(servicio_cama, costo_dime),
              by = "servicio_cama") %>%
    mutate(margen_pct = round(100 * (tarifa_med - costo_dime) / costo_dime, 1))

  bv
})

if (!is.null(bed_value)) {
  cs <- bed_value$comparativo
  message("[LOS-App] Valor de cama | ",
          paste(sprintf("%s: tarifa %s vs costo %s (%s%%)",
                        cs$servicio_cama,
                        formatC(cs$tarifa_med, format = "d", big.mark = "."),
                        formatC(cs$costo_dime, format = "d", big.mark = "."),
                        cs$margen_pct), collapse = " | "))
}
