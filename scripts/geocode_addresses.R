################################################################################
# geocode_addresses.R — Geocodifica direcciones de pacientes DIME contra un
# servidor Nominatim local (self-hosted, sin envío de datos a terceros).
#
# Fuente: data/admission_data/data_a_2024_2025_2026.rds — incluye AMBOS tipos
# de atención (AMBULATORIO y HOSPITALARIO), a diferencia del censo de camas
# (data_cense_2017_2026.rds, solo hospitalización). Cobertura 2024-2026,
# consistente con el resto del dashboard (CACI/pte_base también es 2024+).
#
# Salidas:
#   data/geocode_cache.rds        — caché reanudable dirección → lat/lon
#   data/geocoded_patients.rds    — uso interno (incluye id de paciente)
#   data/visitas_mensuales.rds    — visitas por año×mes×pagador (gráfico barras)
#   output/geocoded_map_data.csv  — para QGIS/mapa: SOLO coordenadas (con
#                                    jitter de privacidad) + clasificación,
#                                    sin cédula, nombre ni texto de dirección.
#   shiny_grd/data/comunas_cali.rds — polígonos de comuna (sf, WGS84) para el
#                                    mapa de concentración por comuna.
#
# Requiere: contenedor `nominatim` corriendo en http://localhost:8088
################################################################################

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(curl); library(jsonlite); library(purrr)
  library(lubridate); library(sf)
})

NOMINATIM_URL <- "http://localhost:8088/search"
CACHE_FILE    <- "data/geocode_cache.rds"
# El backend Nominatim corre con 4 workers uvicorn; con más de ~8 conexiones
# concurrentes empiezan a acumularse timeouts/errores HTTP bajo carga sostenida.
CONCURRENCY   <- 8
BATCH_SIZE    <- 200
REQ_TIMEOUT_S <- 25

# DIME Clínica Neurocardiovascular — Avenida 5N #20N-75, Versalles, Cali.
# Geocodificado contra el mismo Nominatim local (nivel calle: "Avenida 5
# Norte, Versalles, Cali" — el # de puerta exacto no está en OSM, pero a
# escala de bandas de distancia en km el error (<300 m) es despreciable.
DIME_LAT <- 3.4613735
DIME_LON <- -76.5289358

# Distancia haversine en km entre (lat,lon) y el punto DIME.
haversine_km <- function(lat, lon, lat0 = DIME_LAT, lon0 = DIME_LON) {
  r <- 6371
  to_rad <- function(x) x * pi / 180
  dlat <- to_rad(lat - lat0); dlon <- to_rad(lon - lon0)
  a <- sin(dlat / 2)^2 + cos(to_rad(lat0)) * cos(to_rad(lat)) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

stopifnot(file.exists("data/admission_data/data_a_2024_2025_2026.rds"))
stopifnot(file.exists("shiny_grd/data/pte_base.rds"))

# ── 1. Cargar y limpiar datos de admisión (ambulatorio + hospitalario) ──────
adm <- readRDS("data/admission_data/data_a_2024_2025_2026.rds")

adm <- adm %>%
  filter(!is.na(documento), !is.na(tipo_de_atencion)) %>%
  mutate(
    id_num          = str_trim(as.character(documento)),
    direccion_clean = str_squish(str_to_upper(coalesce(direccion, ""))),
    fecha_ingreso_d = as.Date(fecha_ingreso),
    año             = year(fecha_ingreso_d),
    mes             = month(fecha_ingreso_d),
    servicio        = str_to_title(str_replace_all(coalesce(departamento_actual, "Sin dato"), "_", " ")),
    tipo_atencion   = str_to_title(tipo_de_atencion),   # "Ambulatorio" / "Hospitalario"
    cuenta          = coalesce(as.character(numero_de_cuenta), as.character(numero_de_ingreso))
  )

# ── 2. Heurística: ¿la dirección tiene detalle de calle o es solo un
#      municipio/ciudad? Se excluyen del geocoding las de solo municipio,
#      según lo solicitado. ────────────────────────────────────────────────
street_kw <- paste(
  "CALLE", "CARRERA", "\\bCRA\\b", "\\bCLL\\b", "\\bCL\\b", "\\bAV\\b",
  "AVENIDA", "DIAGONAL", "\\bDIAG\\b", "TRANSVERSAL", "TRANSV", "MANZANA",
  "\\bMZ\\b", "KILOMETRO", "\\bKM\\b", "VEREDA", "\\bVIA\\b", "CIRCULAR",
  sep = "|"
)
is_street_address <- function(x) {
  x != "" & str_detect(x, "[0-9]") & str_detect(x, street_kw)
}
adm <- adm %>% mutate(geocodable = is_street_address(direccion_clean))

# ── 2b. Normalización de direcciones para el geocodificador ─────────────────
# Nominatim/OSM no reconoce abreviaturas colombianas ("CRA", "NO.", barrios
# con "B/", sufijos pegados como "83BIS"). Se expande a forma estándar antes
# de consultar. Verificado empíricamente contra el servidor local.
other_cities_co <- c("PALMIRA","BUGA","TULUA","YUMBO","JAMUNDI","CANDELARIA","FLORIDA",
                      "PRADERA","GINEBRA","EL CERRITO","BUENAVENTURA","POPAYAN","PASTO",
                      "BOGOTA","MEDELLIN","BARRANQUILLA","CARTAGENA","IPIALES","ARMENIA",
                      "PEREIRA","MANIZALES","NEIVA","IBAGUE","VILLAVICENCIO","MONTERIA",
                      "TUMACO","QUIBDO","CUCUTA","BUCARAMANGA","SANTANDER DE QUILICHAO",
                      "PUERTO TEJADA","CALOTO")

normalize_address <- function(x) {
  x <- str_to_upper(x); x <- str_squish(x)
  # abreviaturas de tipo de vía (en cualquier posición: a veces el municipio
  # va antepuesto, p.ej. "BUGA CLL 13 4-46")
  x <- str_replace_all(x, "\\bCRA\\.?\\b", "CARRERA")
  x <- str_replace_all(x, "\\bCR\\.?\\b",  "CARRERA")
  x <- str_replace_all(x, "\\bKR\\.?\\b",  "CARRERA")
  x <- str_replace_all(x, "\\bCLL\\.?\\b", "CALLE")
  x <- str_replace_all(x, "\\bCALL\\.?\\b","CALLE")
  x <- str_replace_all(x, "\\bCL\\.?\\b",  "CALLE")
  x <- str_replace_all(x, "\\bAV\\.?\\b",  "AVENIDA")
  x <- str_replace_all(x, "\\bAVDA\\.?\\b","AVENIDA")
  x <- str_replace_all(x, "\\bDG\\.?\\b",  "DIAGONAL")
  x <- str_replace_all(x, "\\bDIAG\\.?\\b","DIAGONAL")
  x <- str_replace_all(x, "\\bTV\\.?\\b",  "TRANSVERSAL")
  x <- str_replace_all(x, "\\bTRANSV\\.?\\b", "TRANSVERSAL")
  x <- str_replace_all(x, "\\bMZ\\.?\\b",  "MANZANA")
  # marcadores de número de casa: "NO.", "N0.", "N.", "NRO", "N " suelto
  x <- str_replace_all(x, "N[O0]\\.?\\s*(?=[0-9])", " ")
  x <- str_replace_all(x, "\\bNRO\\.?\\s*(?=[0-9])", " ")
  x <- str_replace_all(x, "\\bN\\.?\\s*(?=[0-9])", " ")
  # sufijos direccionales/BIS pegados al número: "83BIS"->"83 BIS", "34NORTE"->"34 NORTE"
  x <- str_replace_all(x, "(?<=[0-9])(BIS)\\b", " \\1")
  x <- str_replace_all(x, "(?<=[0-9])(NORTE|NTE|SUR|ESTE|OESTE)\\b", " \\1")
  # separador "*" entre números de dirección -> "-"
  x <- str_replace_all(x, "(?<=[0-9])\\*(?=[0-9])", "-")
  x <- str_replace_all(x, "(?<=[0-9])#", " #")
  x <- str_replace_all(x, "\\b(APTO|APT|APARTAMENTO)\\.?\\s*[0-9]+[A-Z]?\\b", " ")
  x <- str_replace_all(x, "\\bPISO\\s*[0-9]+\\b", " ")
  x <- str_replace_all(x, "B/+\\s*", " ")
  x <- str_replace_all(x, "\\bBARRIO\\b", " ")
  x <- str_replace_all(x, "[/\\\\]", " ")
  x <- str_squish(x)
  has_city <- str_detect(x, paste(other_cities_co, collapse = "|"))
  city_suffix <- if_else(has_city, "", " CALI VALLE DEL CAUCA")
  paste0(x, city_suffix, ", COLOMBIA")
}

# Fallback: si la dirección completa no matchea, se reintenta solo con
# "calle/carrera + número", eliminando nombre de barrio/edificio/complejo
# (causa frecuente de fallos en Nominatim para direcciones colombianas).
truncate_after_number <- function(x) {
  base <- str_remove(x, ",\\s*COLOMBIA$")
  m <- str_locate_all(base, "[0-9]+[A-Z]?(\\s*-\\s*[0-9]+[A-Z]?)?")[[1]]
  if (nrow(m) == 0) return(NA_character_)
  last_end <- m[nrow(m), "end"]
  head_part <- str_trim(str_sub(base, 1, last_end))
  if (head_part == base) return(NA_character_)  # nothing to truncate

  # BUG CORREGIDO: truncar "hasta el último número" cortaba también el sufijo
  # de ciudad (propio o el " CALI VALLE DEL CAUCA" añadido por
  # normalize_address), porque el nombre de la ciudad casi siempre queda
  # DESPUÉS del número de casa en la cadena. Sin ciudad, Nominatim buscaba en
  # TODA Colombia y emparejaba calles homónimas en municipios al azar
  # (ej. "Carrera 7" -> Puerto Carreño en vez de Cali/Jamundí), a >90% de los
  # casos con distancias de cientos de km. Se reintroduce explícitamente la
  # ciudad detectada (o CALI por defecto) tras truncar.
  detected_city <- other_cities_co[str_detect(base, other_cities_co)]
  city_suffix <- if (length(detected_city) > 0) paste0(" ", detected_city[1]) else " CALI VALLE DEL CAUCA"
  if (str_detect(head_part, fixed(str_trim(city_suffix), ignore_case = TRUE))) city_suffix <- ""
  paste0(head_part, city_suffix, ", COLOMBIA")
}

n_pac       <- n_distinct(adm$id_num)
n_addr      <- n_distinct(adm$direccion_clean)
n_geocod    <- adm %>% filter(geocodable) %>% distinct(direccion_clean) %>% nrow()
n_muni_only <- n_addr - n_geocod

cat(sprintf(
  "Pacientes únicos: %d | Direcciones únicas: %d\n  → Con detalle de calle (a geocodificar): %d (%.1f%%)\n  → Solo municipio/sin detalle (excluidas): %d (%.1f%%)\n",
  n_pac, n_addr, n_geocod, 100 * n_geocod / n_addr, n_muni_only, 100 * n_muni_only / n_addr
))
cat(sprintf("Tipo de atención — Ambulatorio: %d | Hospitalario: %d\n",
            sum(adm$tipo_atencion == "Ambulatorio"), sum(adm$tipo_atencion == "Hospitalario")))

# ── 3. Clasificación de pagador: EAPB vs. Segmento Libre Elección (SLE) ─────
# SLE = Medicina Prepagada + Pólizas + Particulares (definición del proyecto
# Prexo SLE / AV DIME). Lista de entidades tomada de la metodología histórica
# ya usada en Personal JSH/analisis_comercial_2024/script_analisis_comercial.R
# (columna `eps_3`), aplicada aquí sobre el campo `eps` de admisiones.
#
# SLE se divide en dos subcategorías a pedido explícito:
#   - "SLE - Particulares": pago directo/particular — incluye "PARTICULAR"/
#     "PART." genérico, PREVISER, convenios con médicos específicos
#     ("PART. DR. ..."), afiliados/colaboradores DIME. En los datos, TODOS
#     estos casos llevan el prefijo "PART."/"PARTICULAR" (verificado contra
#     data_a_2024_2025_2026.rds), así que el mismo patrón que ya existía
#     cubre PREVISER y nombres de médicos sin necesidad de listarlos aparte.
#   - "SLE - MP/Pólizas": medicina prepagada y aseguradoras con nombre propio
#     (Colsanitas, Colmédica, Coomeva MP, Medisanitas, Medplus, Allianz, AXA,
#     Seguros Bolívar, Liberty Seguros, Sura Póliza, Panamerican Life) más
#     convenios institucionales (Univalle, Unisalud, Ecopetrol, exámenes de
#     ingreso) que no son ni EAPB ni pago particular directo.
classify_payer_sle <- function(eps) {
  t_up <- str_to_upper(coalesce(eps, ""))
  case_when(
    t_up == ""                                                       ~ "Sin dato",
    # Particulares: PARTICULAR/PART. ya cubre PREVISER y médicos con convenio
    # (ej. "PART. DR. JORGE HOLGUIN", "PARTICULAR PREVISER", "PART. AFILIADOS
    # DIME") porque en el dato crudo todos llevan ese prefijo.
    str_detect(t_up, "PARTICULAR|PART\\.")                            ~ "SLE - Particulares",
    # Medicina prepagada y pólizas con nombre propio
    str_detect(t_up, "ALLIANZ")                                       ~ "SLE - MP/Pólizas",
    str_detect(t_up, "COOMEVA.*(MEDICINA PREPAGADA|\\bMP\\b)")         ~ "SLE - MP/Pólizas",
    str_detect(t_up, "\\bAXA\\b")                                     ~ "SLE - MP/Pólizas",
    str_detect(t_up, "COLSANITAS")                                    ~ "SLE - MP/Pólizas",
    str_detect(t_up, "COLMEDICA")                                     ~ "SLE - MP/Pólizas",
    str_detect(t_up, "MEDISANITAS")                                   ~ "SLE - MP/Pólizas",
    str_detect(t_up, "\\bMEDPLUS\\b")                                 ~ "SLE - MP/Pólizas",
    str_detect(t_up, "PAN ?AMERICAN")                                 ~ "SLE - MP/Pólizas",
    str_detect(t_up, "SEGUROS BOLIVAR")                               ~ "SLE - MP/Pólizas",
    str_detect(t_up, "LIBERTY SEGUROS")                               ~ "SLE - MP/Pólizas",
    str_detect(t_up, "SEGUROS DE VIDA SURAMERICANA|SURA.*POLIZA|POLIZA.*SURA") ~ "SLE - MP/Pólizas",
    # NOTA: planes PAC (Comfenalco/Nueva EPS/SOS/Sura PAC) excluidos del SLE
    # a pedido explícito — son planes complementarios sobre una EAPB, no
    # medicina prepagada/póliza/particular pura.
    str_detect(t_up, "UNIVALLE|UNIVERSIDAD DEL VALLE")                ~ "SLE - MP/Pólizas",
    str_detect(t_up, "UNISALUD")                                      ~ "SLE - MP/Pólizas",
    str_detect(t_up, "EXAMENES DE INGRESO|INTERSALUD")                ~ "SLE - MP/Pólizas",
    str_detect(t_up, "\\bECOPETROL\\b")                               ~ "SLE - MP/Pólizas",
    TRUE                                                              ~ "EAPB"
  )
}
adm <- adm %>% mutate(tipo_pagador = classify_payer_sle(eps))

# ── 4. Unión con cohorte CACI (pte_base) ─────────────────────────────────────
caci_levels <- c("ICC", "ACV", "SCA", "TEP", "TxC", "Otros CV")
pte_base <- readRDS("shiny_grd/data/pte_base.rds") %>%
  mutate(identificacion = as.character(identificacion))

patient_caci <- pte_base %>%
  mutate(caci = factor(as.character(caci), levels = caci_levels)) %>%
  filter(!is.na(caci)) %>%
  group_by(identificacion) %>%
  summarise(caci_principal = caci[which.min(as.integer(caci))], .groups = "drop") %>%
  rename(id_num = identificacion)

adm <- adm %>%
  left_join(patient_caci, by = "id_num") %>%
  mutate(es_caci = !is.na(caci_principal))

cat(sprintf("Pacientes marcados como CACI: %d de %d (%.1f%%)\n",
            n_distinct(adm$id_num[adm$es_caci]), n_pac,
            100 * n_distinct(adm$id_num[adm$es_caci]) / n_pac))

# ── 5. Geocodificación contra Nominatim local (con caché reanudable) ────────
addr_tbl <- adm %>%
  filter(geocodable) %>%
  distinct(direccion_clean) %>%
  arrange(direccion_clean)

cache <- if (file.exists(CACHE_FILE)) {
  readRDS(CACHE_FILE)
} else {
  tibble(direccion_clean = character(), lat = double(), lon = double(),
         display_name = character(), addresstype = character(),
         query_status = character())
}

# Purgar entradas con error de conexión/HTTP para reintentarlas (no son un
# resultado definitivo como "no_match" — probablemente el servidor estaba
# sobrecargado durante una corrida larga anterior).
n_retry <- sum(cache$query_status == "http_error" | str_starts(coalesce(cache$query_status, ""), "fail"))
if (n_retry > 0) {
  cat(sprintf("Reintentando %d direcciones con error de conexión/HTTP previo...\n", n_retry))
  cache <- cache %>% filter(query_status != "http_error", !str_starts(coalesce(query_status, ""), "fail"))
}

to_query <- setdiff(addr_tbl$direccion_clean, cache$direccion_clean)
cat(sprintf("En caché: %d | Pendientes de geocodificar: %d\n", nrow(cache), length(to_query)))

# Ejecuta un lote de consultas ya construidas (addr_orig -> query_string) contra
# Nominatim en paralelo. Devuelve una fila por addr_orig con el resultado.
run_queries <- function(query_by_addr, status_if_match = "matched") {
  pool <- curl::new_pool(total_con = CONCURRENCY, host_con = CONCURRENCY)
  url_for <- function(q) {
    paste0(NOMINATIM_URL, "?q=", curl::curl_escape(q),
           "&format=json&limit=1&countrycodes=co&addressdetails=0")
  }
  urls <- vapply(query_by_addr, url_for, character(1))
  addr_by_url <- setNames(names(query_by_addr), urls)
  out <- new.env(parent = emptyenv())
  make_handle <- function(u) {
    h <- curl::new_handle(url = u)
    curl::handle_setopt(h, timeout = REQ_TIMEOUT_S, connecttimeout = 10)
    h
  }

  make_done <- function(addr) {
    force(addr)
    function(res) {
      row <- list(direccion_clean = addr, lat = NA_real_, lon = NA_real_,
                  display_name = NA_character_, addresstype = NA_character_,
                  query_status = "http_error")
      if (!is.null(res$status_code) && res$status_code == 200) {
        parsed <- tryCatch(jsonlite::fromJSON(rawToChar(res$content)), error = function(e) NULL)
        if (!is.null(parsed) && is.data.frame(parsed) && nrow(parsed) > 0) {
          row$lat          <- as.numeric(parsed$lat[1])
          row$lon          <- as.numeric(parsed$lon[1])
          row$display_name <- parsed$display_name[1]
          row$addresstype  <- if ("addresstype" %in% names(parsed)) parsed$addresstype[1] else NA_character_
          row$query_status <- status_if_match
        } else {
          row$query_status <- "no_match"
        }
      }
      out[[addr]] <- row
    }
  }
  make_fail <- function(addr) {
    force(addr)
    function(msg) {
      out[[addr]] <- list(direccion_clean = addr, lat = NA_real_, lon = NA_real_,
                           display_name = NA_character_, addresstype = NA_character_,
                           query_status = paste0("fail: ", msg))
    }
  }

  for (u in urls) {
    addr <- addr_by_url[[u]]
    curl::curl_fetch_multi(u, done = make_done(addr), fail = make_fail(addr),
                            handle = make_handle(u), pool = pool)
  }
  curl::multi_run(pool = pool)
  bind_rows(as.list(out))
}

geocode_chunk <- function(addresses) {
  # Tier 1: dirección normalizada completa
  q1 <- setNames(vapply(addresses, normalize_address, character(1)), addresses)
  res1 <- run_queries(q1, status_if_match = "matched")

  # Tier 2: para los sin match, reintentar sin barrio/edificio (solo calle+número)
  failed <- res1$direccion_clean[res1$query_status %in% c("no_match", "http_error")]
  if (length(failed) > 0) {
    q2_raw <- vapply(q1[failed], truncate_after_number, character(1))
    q2 <- q2_raw[!is.na(q2_raw)]
    if (length(q2) > 0) {
      res2 <- run_queries(q2, status_if_match = "matched_truncated")
      matched2 <- res2 %>% filter(query_status == "matched_truncated")
      if (nrow(matched2) > 0) {
        res1 <- res1 %>% filter(!direccion_clean %in% matched2$direccion_clean) %>%
          bind_rows(matched2)
      }
    }
  }
  res1
}

if (length(to_query) > 0) {
  chunks <- split(to_query, ceiling(seq_along(to_query) / BATCH_SIZE))
  t0 <- Sys.time()
  for (i in seq_along(chunks)) {
    res <- geocode_chunk(chunks[[i]])
    cache <- bind_rows(cache, res)
    saveRDS(cache, CACHE_FILE, compress = "xz")
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    done_n  <- sum(lengths(chunks[seq_len(i)]))
    rate    <- done_n / elapsed
    eta_s   <- (length(to_query) - done_n) / rate
    cat(sprintf("[%s] Chunk %d/%d — %d/%d dirs (%.1f/s) — ETA %.0f s\n",
                format(Sys.time(), "%H:%M:%S"), i, length(chunks),
                done_n, length(to_query), rate, eta_s))
  }
} else {
  cat("Nada pendiente — caché ya cubre todas las direcciones geocodificables.\n")
}

# ── 6. Resumen de resultados ──────────────────────────────────────────────
# El caché es compartido entre corridas con distintas fuentes de datos (censo,
# admisiones, etc.) y acumula direcciones de todas ellas — se filtra aquí a
# solo las direcciones relevantes para ESTA corrida (addr_tbl) para que los
# porcentajes no superen el 100%.
cache_run     <- cache %>% filter(direccion_clean %in% addr_tbl$direccion_clean)
matched_full  <- sum(cache_run$query_status == "matched", na.rm = TRUE)
matched_trunc <- sum(cache_run$query_status == "matched_truncated", na.rm = TRUE)
matched       <- matched_full + matched_trunc
no_match      <- sum(cache_run$query_status == "no_match", na.rm = TRUE)
other_fail    <- nrow(cache_run) - matched - no_match
n_cache       <- nrow(cache_run)

cat("\n══════════ RESUMEN DE GEOCODIFICACIÓN ══════════\n")
cat(sprintf("Direcciones únicas totales:              %d (100%%)\n", n_addr))
cat(sprintf("  Excluidas (solo municipio/sin calle):  %d (%.1f%%)\n", n_muni_only, 100*n_muni_only/n_addr))
cat(sprintf("  Con detalle de calle (procesadas):     %d (%.1f%%)\n", n_geocod, 100*n_geocod/n_addr))
cat(sprintf("    → Geocodificadas con éxito:          %d (%.1f%% del total, %.1f%% de las procesadas)\n",
            matched, 100*matched/n_addr, 100*matched/n_geocod))
cat(sprintf("       · dirección completa:             %d\n", matched_full))
cat(sprintf("       · solo calle+número (sin barrio):  %d\n", matched_trunc))
cat(sprintf("    → Sin coincidencia en Nominatim:     %d (%.1f%% del total, %.1f%% de las procesadas)\n",
            no_match, 100*no_match/n_addr, 100*no_match/n_geocod))
if (other_fail > 0)
  cat(sprintf("    → Errores de conexión/HTTP:          %d\n", other_fail))

# ── 7. Dataset interno (con id, para trazabilidad y futuros joins) ──────────
geocoded_patients <- adm %>%
  left_join(cache, by = "direccion_clean") %>%
  select(id_num, cuenta, año, mes, edad, tipo_pagador, servicio, tipo_atencion,
         es_caci, caci_principal, direccion_clean, geocodable, lat, lon,
         query_status, addresstype)

saveRDS(geocoded_patients, "data/geocoded_patients.rds", compress = "xz")
cat(sprintf("\nGuardado data/geocoded_patients.rds (%d filas)\n", nrow(geocoded_patients)))

# ── 7b. Nivel visita: una fila por (paciente × cuenta única) ─────────────────
# `cuenta` identifica una atención (ambulatoria o de hospitalización) — varias
# filas comparten la misma cuenta (cargos/movimientos). Se toma la moda de
# `servicio`/`tipo_pagador` por cuenta como representativos de esa visita.
moda <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

visitas <- geocoded_patients %>%
  filter(!is.na(cuenta), !is.na(año)) %>%
  group_by(id_num, cuenta, año, mes, tipo_atencion) %>%
  summarise(servicio = moda(servicio), tipo_pagador = moda(tipo_pagador), .groups = "drop")

# ── 7c. Nivel paciente × año × tipo de atención ─────────────────────────────
# Se separa Ambulatorio/Hospitalario porque son patrones de uso distintos: un
# mismo paciente puede tener, en el mismo año, varias consultas ambulatorias
# Y una hospitalización — se cuentan y clasifican por separado.
visitas_año <- visitas %>%
  group_by(id_num, año, tipo_atencion) %>%
  summarise(
    n_visitas_anio  = n(),
    visitas_max_mes = max(table(mes)),
    servicio_principal = moda(servicio),
    tipo_pagador_anio   = moda(tipo_pagador),
    .groups = "drop"
  ) %>%
  mutate(
    frecuencia = case_when(
      visitas_max_mes > 1 ~ "Múltiples visitas/mes",
      n_visitas_anio > 1  ~ "Múltiples visitas/año",
      TRUE                ~ "Única visita/año"
    )
  )

cat(sprintf("Filas paciente x año x tipo de atención (visitas): %d\n", nrow(visitas_año)))

# ── 7d. Tabla compacta para el gráfico de barras (visitas por mes/pagador) ──
# Usa TODA la población (no solo geocodificados) para reflejar el volumen
# real de atenciones, no sesgado por éxito de geocodificación. Incluye
# `servicio` para que el gráfico de barras se pueda filtrar igual que el mapa.
visitas_mensuales <- visitas %>%
  filter(!is.na(mes)) %>%
  count(año, mes, tipo_pagador, tipo_atencion, servicio, name = "n_visitas")

saveRDS(visitas_mensuales, "data/visitas_mensuales.rds", compress = "xz")
if (dir.exists("shiny_grd/data"))
  saveRDS(visitas_mensuales, "shiny_grd/data/visitas_mensuales.rds", compress = "xz")
cat(sprintf("Guardado visitas_mensuales.rds (%d filas)\n", nrow(visitas_mensuales)))

# ── 7e. Polígonos de comuna (Cali) para mapa de concentración ───────────────
comunas_sf <- st_read("qgis/layers/Comunas_Cali.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  select(comuna = Comuna, nombre) %>%
  st_simplify(dTolerance = 0.0001, preserveTopology = TRUE)

if (dir.exists("shiny_grd/data"))
  saveRDS(comunas_sf, "shiny_grd/data/comunas_cali.rds", compress = "xz")
cat(sprintf("Guardado comunas_cali.rds (%d comunas)\n", nrow(comunas_sf)))

# ── 8. Export seguro para QGIS/mapa — SIN id, SIN dirección, con jitter ─────
# Jitter de ±~60m para reducir el riesgo de reidentificación por dirección
# exacta al graficar puntos individuales. Una coordenada por paciente
# (última dirección válida), repetida en cada fila paciente×año×tipo.
set.seed(20260830)
coords_pac <- geocoded_patients %>%
  filter(query_status %in% c("matched", "matched_truncated"), !is.na(lat), !is.na(lon)) %>%
  distinct(id_num, .keep_all = TRUE) %>%
  transmute(
    id_num,
    lat = lat + rnorm(n(), sd = 0.0005),   # ~55 m
    lon = lon + rnorm(n(), sd = 0.0005),
    precision = if_else(query_status == "matched", "completa", "calle_numero"),
    es_caci, caci_principal, edad,
    distancia_km = round(haversine_km(lat, lon), 2)
  ) %>%
  mutate(
    distancia_banda = cut(distancia_km, breaks = c(-Inf, 1, 2, 5, 10, Inf),
                          labels = c("< 1 km", "1-2 km", "2-5 km", "5-10 km", "> 10 km")),
    edad_grupo = cut(edad, breaks = c(0,10,20,30,40,50,60,70,80,Inf),
                     labels = c("0-9","10-19","20-29","30-39","40-49",
                                "50-59","60-69","70-79","80+"), right = FALSE)
  )

# Unión espacial punto -> comuna (point-in-polygon), en la misma proyección WGS84.
coords_sf <- st_as_sf(coords_pac, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
coords_comuna <- st_join(coords_sf, comunas_sf["nombre"], join = st_within) %>%
  st_drop_geometry() %>%
  select(id_num, comuna_nombre = nombre)
coords_pac <- coords_pac %>% left_join(coords_comuna, by = "id_num")
cat(sprintf("Pacientes geocodificados dentro del perímetro de alguna comuna: %d de %d\n",
            sum(!is.na(coords_pac$comuna_nombre)), nrow(coords_pac)))

map_export <- visitas_año %>%
  inner_join(coords_pac, by = "id_num") %>%
  transmute(
    lat, lon, precision, distancia_km, distancia_banda, edad_grupo,
    comuna = coalesce(comuna_nombre, "Fuera de Cali / sin comuna"),
    tipo_pagador = tipo_pagador_anio,
    tipo_atencion,
    es_caci,
    caci = if_else(es_caci, as.character(caci_principal), "No CACI"),
    servicio = servicio_principal,
    frecuencia,
    n_visitas_anio,
    año
  )

dir.create("output", showWarnings = FALSE)
readr::write_csv(map_export, "output/geocoded_map_data.csv")
cat(sprintf("Guardado output/geocoded_map_data.csv (%d filas paciente x año x tipo, sin PII)\n",
            nrow(map_export)))

# Copia para la app shiny_grd (pestaña "Mapa") — mismo contenido, sin PII.
if (dir.exists("shiny_grd/data")) {
  saveRDS(map_export, "shiny_grd/data/geocoded_map_data.rds", compress = "xz")
  cat(sprintf("Guardado shiny_grd/data/geocoded_map_data.rds (%d filas)\n", nrow(map_export)))

  # ── 8b. Export de CONTACTO — RESTRINGIDO, con nombre/teléfono ─────────────
  # A petición de comunicaciones: lista de contacto para el mismo segmento de
  # pacientes que el mapa (misma grilla paciente×año, mismos filtros), pero
  # CON nombre y teléfono. Sólo se sirve tras contraseña compartida desde la
  # pestaña Mapa (ver comms_secret.R) — nunca se sube a output/ ni a QGIS,
  # sólo a shiny_grd/data/ (gitignored, igual que el resto de data/*.rds).
  contacts_export <- visitas_año %>%
    inner_join(coords_pac, by = "id_num") %>%
    transmute(
      id_num,
      tipo_pagador = tipo_pagador_anio,
      tipo_atencion,
      caci = if_else(es_caci, as.character(caci_principal), "No CACI"),
      servicio = servicio_principal,
      frecuencia,
      comuna = coalesce(comuna_nombre, "Fuera de Cali / sin comuna"),
      año
    ) %>%
    left_join(
      adm %>%
        arrange(id_num, desc(fecha_ingreso_d)) %>%
        distinct(id_num, .keep_all = TRUE) %>%
        transmute(id_num, nombre = str_to_title(paciente),
                  telefono_1, telefono_2, municipio),
      by = "id_num"
    )

  saveRDS(contacts_export, "shiny_grd/data/patient_contacts.rds", compress = "xz")
  cat(sprintf(
    "Guardado shiny_grd/data/patient_contacts.rds (%d filas, CON nombre/teléfono — export restringido)\n",
    nrow(contacts_export)))
}
