################################################################################
# global.R — Estancia Hospitalaria (LOS) · DIME Clínica Neurocardiovascular
################################################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(scales)
library(here)
library(rio)
library(reactable)
library(plotly)
library(DT)
library(zoo)
library(janitor)
library(readxl)
library(MASS)
library(strucchange)
select <- dplyr::select   # MASS masks dplyr::select; restore it

tryCatch(
  Sys.setlocale("LC_TIME", "es_ES.UTF-8"),
  warning = function(w) NULL,
  error   = function(e) NULL
)

# ── Paletas ────────────────────────────────────────────────────────────────────
serv_colors <- c(
  "UCI"            = "#E15759",
  "UCIN"           = "#F28E2B",   # merged UCIN 2° + UCIN 5°
  "UCIN ANGIO"     = "#76B7B2",
  "URGENCIAS OBS"  = "#4E79A7",
  "PISO HOSP"      = "#59A14F",
  "UCIN RESPIRATORIOS" = "#B07AA1",
  "Extensión Hospitalización" = "#EDC948"
)

caci_colors <- c(
  ACV          = "#4E79A7",
  SCA          = "#F28E2B",
  ICC          = "#E15759",
  TEP          = "#76B7B2",
  TXC          = "#59A14F",
  CARDIO_OTHER = "#B07AA1"
)

cop <- function(x) {
  ifelse(is.na(x), "—",
         scales::dollar(x, prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 1))
}

# ── ICD-10 category-3 grouping ─────────────────────────────────────────────────
# Groups raw diagnosis strings (format "XXXX - DESCRIPTION") into clinically
# meaningful disease categories based on the 3-character ICD-10 block.
icd_grupo <- function(diag_str) {
  code3  <- str_extract(as.character(diag_str), "^[A-Z][0-9]{2}")
  letter <- substr(code3, 1, 1)

  dplyr::case_when(
    is.na(code3) ~ "Sin código / NE",

    # ── Cardiovascular principal ────────────────────────────────────────────
    code3 %in% c("I20","I21","I22","I23","I24","I25") ~ "Cardiopatía isquémica (I20-I25)",
    code3 == "I50"                                     ~ "Insuficiencia cardíaca (I50)",
    code3 %in% c("I60","I61","I62","I63","I64","I65",
                 "I66","I67","I68","I69")              ~ "ACV / Enf. cerebrovascular (I60-I69)",
    code3 %in% c("G45","G46")                         ~ "AIT / Isquemia cerebral trans. (G45-G46)",
    code3 == "I26"                                     ~ "Embolia pulmonar — TEP (I26)",
    code3 == "I48"                                     ~ "Fibrilación / aleteo auricular (I48)",
    code3 %in% c("I10","I11","I12","I13")             ~ "Hipertensión arterial (I10-I13)",
    code3 %in% c("I44","I45","I46","I47","I49")       ~ "Arritmias (I44-I49)",
    code3 %in% c("I34","I35","I36","I37","I38")       ~ "Valvulopatías (I34-I38)",
    code3 %in% c("I42","I43")                         ~ "Cardiomiopatías (I42-I43)",
    code3 == "I27"                                     ~ "Hipertensión pulmonar (I27)",
    code3 == "I71"                                     ~ "Aneurisma aórtico (I71)",
    code3 %in% c("Q20","Q21","Q22","Q23","Q24",
                 "Q25","Q26","Q27","Q28")              ~ "Cardiopatía congénita / Malform. vasc.",
    code3 %in% c("Z95","Z96","Z97","Z98")             ~ "Post-procedimiento / Prótesis (Z95-Z98)",
    letter == "I"                                      ~ "Otras enf. circulatorias (I)",

    # ── Infecciones ─────────────────────────────────────────────────────────
    # Enf. infecciosas y parasitarias (A-B)
    letter %in% c("A","B")                            ~ "Infecciones: enf. infecciosas (A-B)",
    # Infecciones respiratorias (J00-J22, excl. EPOC)
    code3 %in% c("J00","J01","J02","J03","J04","J05","J06",
                 "J10","J11","J12","J13","J14","J15","J16",
                 "J17","J18","J20","J21","J22")        ~ "Infecciones: respiratorias (J00-J22)",
    # Sepsis / infección sistémica (A40-A41)
    code3 %in% c("A40","A41")                         ~ "Infecciones: sepsis (A40-A41)",
    # UTI
    code3 == "N39"                                     ~ "Infecciones: urinarias (N39)",

    # ── EPOC y asma ─────────────────────────────────────────────────────────
    code3 %in% c("J44","J45","J46")                   ~ "Enf. pulmonar obstructiva (J44-J46)",

    # ── Enf. endocrina y metabólica (E) ─────────────────────────────────────
    letter == "E"                                      ~ "Enf. endocrina / metabólica (E)",

    # ── Enf. renal y urológica (N, excl. N39) ───────────────────────────────
    letter == "N"                                      ~ "Enf. renal / urológica (N)",

    # ── Enf. neurológica (G, excl. G45-G46) ─────────────────────────────────
    letter == "G"                                      ~ "Enf. neurológica (G)",

    # ── Enf. digestiva (K) ───────────────────────────────────────────────────
    letter == "K"                                      ~ "Enf. digestiva (K)",

    # ── Neoplasias (C-D) ─────────────────────────────────────────────────────
    letter %in% c("C","D")                            ~ "Neoplasias (C-D)",

    # ── Enf. musculoesquelética (M) ──────────────────────────────────────────
    letter == "M"                                      ~ "Enf. musculoesquelética (M)",

    # ── Trastorno mental / conductual (F) ────────────────────────────────────
    letter == "F"                                      ~ "Trastorno mental / conductual (F)",

    # ── Otras enf. respiratorias (J, excl. ya capturadas) ───────────────────
    letter == "J"                                      ~ "Enf. respiratoria — otra (J)",

    # ── Síntomas y signos inespecíficos (R) ──────────────────────────────────
    letter == "R"                                      ~ "Síntomas / signos inespecíficos (R)",

    # ── Estado de salud / procedimientos (Z, excl. Z95-Z98) ─────────────────
    letter == "Z"                                      ~ "Estado de salud / procedimiento (Z)",

    TRUE ~ "Otras / No clasificadas"
  )
}

# Parse COP numbers: handles Colombian format (period=thousands, comma=decimal)
# and also US format (comma=thousands, period=decimal).
parse_cop <- function(x) {
  x <- as.character(x)
  suppressWarnings(dplyr::case_when(
    # Already numeric-coercible (no separators needed)
    !is.na(as.numeric(x)) ~ as.numeric(x),
    # Colombian format: e.g. "57.611.827" or "57.611.827,50"
    grepl("^[0-9]+([.][0-9]{3})+([,][0-9]+)?$", x) ~
      as.numeric(gsub(",", ".", gsub("\\.", "", x))),
    # US format: e.g. "57,611,827" or "57,611,827.50"
    grepl("^[0-9]+([,][0-9]{3})+([.][0-9]+)?$", x) ~
      as.numeric(gsub(",", "", x)),
    # Simple comma-decimal (no thousands): "180,50" → 180.5
    grepl("^[0-9]+,[0-9]+$", x) ~
      as.numeric(gsub(",", ".", x)),
    TRUE ~ NA_real_
  ))
}

# ── Localizar directorio del proyecto ─────────────────────────────────────────
resolve_proj_dir <- function() {
  candidates <- c(
    getwd(),
    dirname(getwd()),
    tryCatch(here::here(), error = function(e) NULL)
  )
  for (d in Filter(Negate(is.null), candidates)) {
    data_d <- file.path(d, "data")
    if (!dir.exists(data_d)) next
    # Accept if raw census OR pre-built cache is present (supports shinyapps.io deploy)
    has_data <- length(list.files(data_d, pattern = "data_cense")) > 0 ||
                file.exists(file.path(data_d, "data_los_processed.rds"))
    if (has_data) return(d)
  }
  stop("No se encontró el directorio data/ con los archivos de la app LOS")
}

proj_dir <- resolve_proj_dir()
data_dir <- file.path(proj_dir, "data")
los_dir  <- file.path(proj_dir, "stay_length")

# ── Procesamiento del censo: LOS por servicio (con caché) ─────────────────────
# Monthly updates: drop the new month's export into data/cense_updates/, named
# data_cense_update_YYYY_MM.<rds|xlsx|xls> (e.g. data_cense_update_2026_08.xlsx
# — the raw hospital-system export works as-is, no conversion to .rds needed).
# The app detects it automatically (by filename pattern) and merges it with
# the base census before processing. This folder is local-only: it is never
# part of the shinyapps.io deploy manifest (see LOS_FILES in
# scripts/deploy_app.R) — only the already-processed data_los_processed.rds
# that comes out of this block gets deployed.
los_cache        <- file.path(data_dir, "data_los_processed.rds")
cense_src        <- file.path(data_dir, "data_cense_2017_2026.rds")
cense_updates_dir <- file.path(data_dir, "cense_updates")

cense_update_files <- sort(list.files(
  cense_updates_dir,
  pattern    = "^data_cense_update_\\d{4}_\\d{2}\\.(rds|xlsx|xls)$",
  full.names = TRUE
))

all_cense_src <- c(cense_src, cense_update_files)
all_cense_src <- all_cense_src[file.exists(all_cense_src)]

cache_stale <- !file.exists(los_cache) ||
               any(file.mtime(all_cense_src) > file.mtime(los_cache))

if (cache_stale) {
  n_upd <- length(cense_update_files)
  message("[LOS-App] Procesando censo",
          if (n_upd > 0) paste0(" + ", n_upd, " actualización(es) mensual(es)") else "",
          " — puede tardar ~30 s...")

  cense_raw <- rio::import(cense_src) %>%
    clean_names() %>%
    mutate(across(everything(), as.character))

  if (n_upd > 0) {
    upd_list <- lapply(cense_update_files, function(f) {
      message("  · Integrando: ", basename(f))
      rio::import(f) %>% clean_names() %>%
        mutate(across(everything(), as.character))
    })
    cense_raw <- bind_rows(c(list(cense_raw), upd_list))
  }

  data_los_serv <- cense_raw %>%
    distinct(cuenta, id, paciente,
             fecha_ingreso_movimiento_cama,
             fecha_egreso_movimiento_cama, .keep_all = TRUE) %>%
    mutate(
      fecha_ingreso = dmy(fecha_ingreso),
      across(c(fecha_ingreso_movimiento_cama, fecha_egreso_movimiento_cama), dmy_hm),
      month      = month(fecha_egreso_movimiento_cama),
      year       = year(fecha_egreso_movimiento_cama),
      month_year = as.yearmon(fecha_egreso_movimiento_cama),
      # difftime(units="days") explícito. `a - b` devuelve difftime con unidades
      # AUTOMÁTICAS (secs/mins/hours/days según la magnitud), así que
      # as.numeric()/86400 sólo acierta si R eligió "secs". Ver dif_bed_serv.
      dif_days   = round(
        as.numeric(difftime(fecha_egreso_movimiento_cama,
                            fecha_ingreso_movimiento_cama, units = "days")), 2)
    ) %>%
    filter(!is.na(year), !str_detect(coalesce(paciente, ""), "TIC")) %>%
    group_by(cuenta, estacion) %>%
    arrange(id, estacion, fecha_egreso_movimiento_cama) %>%
    mutate(
      num_bed_serv      = dense_rank(fecha_egreso_movimiento_cama),
      max_bed_serv      = max(num_bed_serv),
      max_bed_date_serv = max(fecha_egreso_movimiento_cama),
      min_bed_date_serv = min(fecha_ingreso_movimiento_cama),  # first admission to service
      # BUG CORREGIDO (2026-08-19): aquí la resta ocurre DENTRO del group_by,
      # sobre dos escalares por grupo, así que difftime elegía "days" y el
      # /86400 dejaba TODAS las estancias por servicio en 0.00 — de ahí las
      # gráficas planas de "Media de estancia mensual" y "Distribución LOS".
      dif_bed_serv = round(
        as.numeric(difftime(max_bed_date_serv, min_bed_date_serv,
                            units = "days")), 2),
      estacion_2 = case_when(
        estacion == "UCI"                   ~ "UCI",
        estacion == "UCIN"                  ~ "UCIN 2°",
        estacion == "UCIN 5 PISO"           ~ "UCIN 5°",
        estacion == "UCIN ANGIO"            ~ "UCIN ANGIO",
        estacion == "URGENCIAS_OBSERVACION" ~ "URGENCIAS OBS",
        estacion == "HOSPITALIZACION 3 PISO"~ "PISO HOSP",
        # Desde 2026 la clínica separó lo que antes era "UCIN ANGIO" en dos
        # flujos: recuperación post-procedimiento (RECUPERACION DE
        # ANGIOGRAFIA, excluida más abajo) y hospitalización real de esos
        # pacientes en el piso 4 ("HOSPITALIZACION 4 PISO"), que el negocio
        # llama Extensión Hospitalización. No fusionar con ninguna de las dos.
        estacion == "HOSPITALIZACION 4 PISO"~ "Extensión Hospitalización",
        TRUE ~ estacion
      )
    ) %>%
    filter(num_bed_serv == max_bed_serv) %>%
    ungroup() %>%
    distinct(id, hab, fecha_egreso_movimiento_cama,
             fecha_ingreso, fecha_ingreso_movimiento_cama, .keep_all = TRUE)

  rio::export(data_los_serv, los_cache)
  message("[LOS-App] Caché guardado: ", los_cache,
          " | Rango: ", min(data_los_serv$year, na.rm = TRUE),
          "–", max(data_los_serv$year, na.rm = TRUE),
          " | Filas: ", nrow(data_los_serv))
} else {
  data_los_serv <- rio::import(los_cache)
  message("[LOS-App] Caché LOS cargado | Filas: ", nrow(data_los_serv))
}

# Merge UCIN 2° and UCIN 5° into a single UCIN group; exclude Recuperación Angiografía
EXCL_SERVICES <- c("RECUPERACION DE ANGIOGRAFIA")

data_los_serv <- data_los_serv %>%
  mutate(estacion_2 = case_when(
    estacion_2 %in% c("UCIN 2°", "UCIN 5°") ~ "UCIN",
    TRUE ~ estacion_2
  )) %>%
  filter(!estacion_2 %in% EXCL_SERVICES)

# Cap to last complete month (discharges through end of previous month)
los_cutoff <- as.POSIXct(floor_date(Sys.Date(), "month")) - 1
data_los_serv <- data_los_serv %>%
  filter(is.na(fecha_egreso_movimiento_cama) |
           as.POSIXct(fecha_egreso_movimiento_cama) <= los_cutoff)
message("[LOS-App] Corte aplicado: egresos hasta ", format(los_cutoff, "%Y-%m-%d"),
        " | Filas tras corte: ", nrow(data_los_serv))

# ── GRD: datos por admisión (diagnóstico + CACI + demografía) ─────────────────
grd_files <- list.files(data_dir,
                         pattern = "data_grd_2_.*_II\\.(rds|rda)",
                         full.names = TRUE)

data_grd_base <- bind_rows(lapply(grd_files, function(f) {
  rio::import(f) %>%
    clean_names() %>%
    mutate(across(any_of(c("edad", "dif_days", "valor_factura",
                            "estancia_horas", "total_cuenta")),
                  ~ suppressWarnings(as.numeric(as.character(.)))))
})) %>%
  mutate(
    caci = str_to_upper(coalesce(
      if ("caci_3"    %in% names(.)) .data$caci_3    else NA_character_,
      if ("caci_final"%in% names(.)) .data$caci_final else NA_character_,
      if ("caci"      %in% names(.)) .data$caci       else NA_character_
    )),
    año              = as.integer(coalesce(
                         year(fecha_ingreso), year(fecha_de_egreso))),
    numero_de_cuenta = as.character(numero_de_cuenta),
    dif_days         = as.numeric(dif_days),
    valor_factura    = as.numeric(valor_factura)
  ) %>%
  mutate(
    grupo_diag = icd_grupo(diagnostico_egreso_principal)
  )

# ── GRD deduplicado + valor_factura winsorizado (fuente de ventas confiable) ──
# Rationale: raw data_grd_base has outliers in 2025-2026 where valor_factura
# reaches $6B COP/account (impossible clinically). Cap at P99 of 2024 (cleanest year).
# Also deduplicate: one row per unique numero_de_cuenta.
vf_cap_2024 <- {
  vf_2024 <- data_grd_base$valor_factura[data_grd_base$año == 2024]
  quantile(vf_2024, 0.99, na.rm = TRUE)
}
message("[LOS-App] valor_factura P99-2024 cap: ",
        formatC(round(vf_cap_2024), format = "d", big.mark = "."), " COP")

data_grd_dedup <- data_grd_base %>%
  group_by(numero_de_cuenta) %>%
  slice_max(order_by = !is.na(caci), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    vf_win      = pmin(coalesce(valor_factura, 0), vf_cap_2024),
    dif_days_ok = if_else(is.na(dif_days) | dif_days <= 0, NA_real_, dif_days),
    ingreso_dia = vf_win / dif_days_ok
  )

# Median daily GRD income — robust reference for KPI
ingreso_dia_grd <- median(data_grd_dedup$ingreso_dia, na.rm = TRUE)
message("[LOS-App] Ingreso GRD/día (mediana, winsorizado): ",
        formatC(round(ingreso_dia_grd), format = "d", big.mark = "."), " COP")

# ── Costos: itemizados (ventas + costo + CACI) ────────────────────────────────
# Smart loading: use compact pre-aggregated file when available (shinyapps.io),
# fall back to full raw RDS files for local development.
compact_cost_path <- file.path(data_dir, "los_cost_compact.rds")

if (file.exists(compact_cost_path)) {
  data_costo_base <- readRDS(compact_cost_path)
  # Guardarraíl de ESCALA. El anterior comparaba median(venta) < 1, que nunca
  # se disparaba: con el bug de separador de miles la mediana quedaba en ~574,
  # no por debajo de 1. Lo que delata el error es la RAZÓN venta/costo: si las
  # ventas están divididas por ~1000, la mediana de venta/costo cae a ~0,002.
  # Un hospital puede tener márgenes negativos, pero no vender por 1/500 del
  # costo en la mediana — eso siempre es un problema de parseo.
  vc_ratio <- median(data_costo_base$venta / data_costo_base$costo, na.rm = TRUE)
  if (is.finite(vc_ratio) && vc_ratio < 0.05) {
    warning("[LOS-App] venta/costo mediano = ", signif(vc_ratio, 3),
            " — las ventas parecen mal escaladas (¿separador de miles?). ",
            "Regenera el compacto con prep_data.R corregido.", call. = FALSE)
    message("[LOS-App] AVISO ESCALA: venta/costo mediano = ", signif(vc_ratio, 3),
            " — márgenes NO fiables hasta regenerar los_cost_compact.rds")
  }
  message("[LOS-App] Compact cost data loaded: ", nrow(data_costo_base), " rows (",
          round(object.size(data_costo_base) / 1e6, 1), " MB)")
} else {
  cost_files <- list.files(data_dir,
                            pattern = "data_costo_total_3_.*_II\\.rds",
                            full.names = TRUE)
  raw_cost <- bind_rows(lapply(cost_files, function(f) {
    rio::import(f) %>%
      clean_names() %>%
      mutate(across(everything(), as.character))
  }))

  # Normalise caci / costo column names
  if ("caci_3" %in% names(raw_cost) && "caci" %in% names(raw_cost)) {
    raw_cost <- mutate(raw_cost, caci = coalesce(caci, caci_3)) %>% select(-caci_3)
  } else if ("caci_3" %in% names(raw_cost)) {
    raw_cost <- rename(raw_cost, caci = caci_3)
  }
  if ("costo_2" %in% names(raw_cost) && "costo" %in% names(raw_cost)) {
    raw_cost <- mutate(raw_cost, costo = coalesce(costo, costo_2)) %>% select(-costo_2)
  } else if ("costo_2" %in% names(raw_cost)) {
    raw_cost <- rename(raw_cost, costo = costo_2)
  }

  data_costo_base <- raw_cost %>%
    mutate(
      venta          = parse_cop(valor_cargo_tarifario),
      costo          = parse_cop(costo),
      fecha_cargue_d = as.Date(fecha_cargue),
      año            = as.integer(year(fecha_cargue_d)),
      mes_num        = as.integer(month(fecha_cargue_d)),
      caci           = str_to_upper(caci),
      cuenta         = as.character(cuenta),
      dif_days       = suppressWarnings(as.numeric(dif_days)),
      departamento_cargue = coalesce(departamento_cargue, "Otro")
    ) %>%
    group_by(cuenta, caci, dif_days, departamento_cargue, año, mes_num) %>%
    summarise(costo = sum(costo, na.rm = TRUE),
              venta = sum(venta, na.rm = TRUE),
              .groups = "drop")
  message("[LOS-App] Cost data built from raw files: ", nrow(data_costo_base), " rows")
}

################################################################################
# ── REFERENCIA EMPÍRICA de costo/venta por día ───────────────────────────────
#
# POR QUÉ EXISTE
# La referencia contractual (PGP / hoja PyG) vale 3,5 M COP/día, pero eso cae en
# el PERCENTIL 88 de lo observado: el 88 % de las cuentas queda por debajo, así
# que no discrimina nada. Además la distribución es fuertemente asimétrica a la
# derecha (media/mediana = 1,87 · CV = 1,91 · P99 ≈ 20× la mediana), de modo que
# la MEDIA no es un estimador de posición: sigue a la cola, no al centro.
#
# Se usa MEDIANA como valor central y P25–P75 como banda, con P90 de alerta.
# Ambas son robustas a la cola. Se calculan sobre toda la serie (2024–2026): la
# mediana anual es estable (0,94 / 1,12 / 1,02 M), lo que hace defendible una
# referencia de serie larga frente a una ventana contractual de 8 meses.
#
# La referencia contractual NO se elimina: se muestra aparte y etiquetada, para
# comparar meta vs. realidad observada.
################################################################################
ref_stats <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(tibble(n = 0L, mediana = NA_real_, p25 = NA_real_,
                                p75 = NA_real_, p90 = NA_real_, media = NA_real_))
  tibble(
    n       = length(x),
    mediana = median(x),
    p25     = unname(quantile(x, .25)),
    p75     = unname(quantile(x, .75)),
    p90     = unname(quantile(x, .90)),
    media   = mean(x)              # sólo informativa; no se usa como referencia
  )
}

ref_emp <- local({
  # Nivel CUENTA: suma de todos los departamentos de esa cuenta
  acc <- data_costo_base %>%
    group_by(cuenta, dif_days) %>%
    summarise(costo_total = sum(costo, na.rm = TRUE),
              venta_total = sum(venta, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(dif_days), dif_days > 0) %>%
    mutate(costo_dia = costo_total / dif_days,
           venta_dia = venta_total / dif_days)

  # Nivel SERVICIO: el costo/día varía 3 órdenes de magnitud entre servicios
  # (ANGIOGRAFIA ≈ 1,20 M vs RAYOS X ≈ 0,01 M) y la asimetría también cambia
  # (URGENCIAS media/mediana = 3,11 vs UCIN = 1,21), así que una referencia
  # institucional única no puede representarlos a la vez.
  srv <- data_costo_base %>%
    group_by(cuenta, departamento_cargue, dif_days) %>%
    summarise(costo_total = sum(costo, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(dif_days), dif_days > 0) %>%
    mutate(costo_dia = costo_total / dif_days) %>%
    group_by(departamento_cargue) %>%
    group_modify(~ ref_stats(.x$costo_dia)) %>%
    ungroup() %>%
    arrange(desc(n))

  list(
    costo_inst = ref_stats(acc$costo_dia),
    venta_inst = ref_stats(acc$venta_dia),
    costo_serv = srv,
    periodo    = paste(range(data_costo_base$año, na.rm = TRUE), collapse = "–")
  )
})

message("[LOS-App] Ref. empírica costo/día (", ref_emp$periodo, "): mediana ",
        formatC(round(ref_emp$costo_inst$mediana), format = "d", big.mark = "."),
        " | IQR ", formatC(round(ref_emp$costo_inst$p25), format = "d", big.mark = "."),
        "–", formatC(round(ref_emp$costo_inst$p75), format = "d", big.mark = "."),
        " | P90 ", formatC(round(ref_emp$costo_inst$p90), format = "d", big.mark = "."))

# ── Join LOS censo × GRD (para agregar CACI y demografía al censo) ────────────
grd_keys <- data_grd_base %>%
  select(numero_de_cuenta, caci,
         diag_egreso   = diagnostico_egreso_principal,   # avoids .x/.y conflict
         edad_grd      = edad,
         sexo, eps, estado_al_alta, procedimiento_qx,
         valor_factura, dif_days_total = dif_days) %>%
  group_by(numero_de_cuenta) %>%
  slice_max(order_by = !is.na(caci), n = 1, with_ties = FALSE) %>%
  ungroup()

data_los_full <- data_los_serv %>%
  mutate(cuenta = as.character(cuenta)) %>%
  left_join(grd_keys, by = c("cuenta" = "numero_de_cuenta"))

# ── Selección de insumos en supplies/ ────────────────────────────────────────
# El libro de estancias inactivas llega con prefijo numérico creciente cada mes
# (6_Table_… JUN, 7_Table_… JUL, 8_Table_… AGO …). Antes el patrón era "^6_Table"
# fijo, así que la pestaña se quedó congelada en junio aunque llegara julio.
# Ahora se elige SIEMPRE el prefijo numérico más alto.
#
# Además se prefiere una copia de nombre ASCII fijo si existe: los nombres
# originales llevan acentos ("…Gráf…", "KPI´S…") que no sobreviven el viaje
# macOS -> bundle -> Linux, y en shinyapps.io list.files() no los encontraba.
# El despliegue envía sólo las copias ASCII; en local se usa el original más
# nuevo. Ver scripts/refresh_los_supplies.R, que regenera esas copias.
#
# Se resuelve AQUÍ (y no más abajo, junto a Giro Cama) porque data_ei también
# consume el libro mensual: antes leía una copia congelada en stay_length/ que
# se quedó en abr-2026 mientras el resto de la app ya iba en jul-2026.
pick_supply <- function(sup_dir, fixed_name, pattern, numeric_prefix = FALSE) {
  fixed <- file.path(sup_dir, fixed_name)
  if (file.exists(fixed)) return(fixed)

  cands <- list.files(sup_dir, pattern = pattern, full.names = TRUE)
  cands <- cands[!grepl("^~\\$", basename(cands))]        # ignora locks de Excel
  if (!length(cands)) return(NA_character_)

  if (numeric_prefix) {
    n <- suppressWarnings(as.integer(sub("^([0-9]+)_.*$", "\\1", basename(cands))))
    if (all(is.na(n))) return(cands[which.max(file.mtime(cands))])
    return(cands[which.max(replace(n, is.na(n), -1L))])   # prefijo más alto
  }
  cands[which.max(file.mtime(cands))]                      # el más reciente
}

sup_dir  <- file.path(proj_dir, "supplies")
kpi_file <- pick_supply(sup_dir, "KPI_giro_cama.xlsx", "KPI")
bd_file  <- pick_supply(sup_dir, "estancias_inactivas_actual.xlsx",
                        "^[0-9]+_Table.*\\.xlsx$", numeric_prefix = TRUE)

# ── Estancias inactivas (2024-2026, pestaña separada) ─────────────────────────
# Fuente única: la hoja "BD trabajo (3)" del libro mensual, la misma que
# alimenta data_bd_inac y ei_cost. El archivo antiguo de stay_length/ queda
# sólo como respaldo por si el libro mensual no está disponible; no se
# actualiza cada mes y arrastraba la pestaña varios meses atrás.
ei_fallback <- file.path(los_dir, "estancia_inactiva_costo_2026",
                         "estancia_inactiva.xlsx")

data_ei_raw <- if (!is.na(bd_file) && file.exists(bd_file)) {
  message("[LOS-App] Est.Inact. (data_ei) <- ", basename(bd_file), " / BD trabajo (3)")
  suppressMessages(readxl::read_excel(bd_file, sheet = "BD trabajo (3)"))
} else {
  message("[LOS-App] Est.Inact. (data_ei) <- respaldo stay_length/ (puede estar desactualizado)")
  rio::import(ei_fallback)
}

data_ei <- data_ei_raw %>%
  clean_names() %>%
  filter(!is.na(identificacion)) %>%
  mutate(
    across(where(is.logical), ~ NA_character_),
    fecha     = as.Date(fecha),
    año       = year(fecha),
    mes_n     = month(fecha),
    mes_label = format(fecha, "%b %Y"),
    clasificacion = str_trim(str_replace(
      as.character(coalesce(clasificacion_de_la_estancia_inactiva, "No clasificado")),
      "^\\d+\\.\\s*", "")),
    responsable = case_when(
      str_detect(coalesce(clasificacion_de_la_estancia_inactiva, ""), regex("IPS",      ignore_case = TRUE)) ~ "IPS",
      str_detect(coalesce(clasificacion_de_la_estancia_inactiva, ""), regex("EPS",      ignore_case = TRUE)) ~ "EPS",
      str_detect(coalesce(clasificacion_de_la_estancia_inactiva, ""), regex("paciente", ignore_case = TRUE)) ~ "Paciente",
      TRUE ~ "No clasificado"
    ),
    causa_principal = coalesce(
      if_else(!is.na(causa_1_de_estancia_inactiva_por_ips) &
              causa_1_de_estancia_inactiva_por_ips != "",
              causa_1_de_estancia_inactiva_por_ips, NA_character_),
      if_else(!is.na(causa_1_de_estancia_inactiva_por_eps) &
              causa_1_de_estancia_inactiva_por_eps != "",
              causa_1_de_estancia_inactiva_por_eps, NA_character_),
      if_else(!is.na(causa_1_de_estancia_inactiva_por_paciente) &
              causa_1_de_estancia_inactiva_por_paciente != "",
              causa_1_de_estancia_inactiva_por_paciente, NA_character_)
    ),
    causa_principal = str_trim(str_replace(
      as.character(causa_principal), "^\\d+\\.\\s*", "")),
    identificacion = as.character(identificacion),
    # El libro EI trae el documento con prefijo de tipo ("CC 14870112"); GRD lo
    # guarda sin prefijo. Sin normalizar, el cruce caía a ~4 % en 2024 y a 0 %
    # en 2025-2026 — por eso el panel de CACI salía vacío. Se normaliza a sólo
    # dígitos en ambos lados (mismo criterio que doc_clean en data_bd_inac).
    doc_norm = str_remove_all(identificacion, "[^0-9]"),
    valor_total_estancia_inactiva = suppressWarnings(
      as.numeric(valor_total_estancia_inactiva))
  )

# Enrich EI with GRD admissions data (join on normalised documento × año)
grd_ei_norm <- data_grd_base %>%
  mutate(doc_norm = str_remove_all(as.character(documento), "[^0-9]")) %>%
  filter(doc_norm != "")

grd_for_ei <- grd_ei_norm %>%
  group_by(doc_norm, año) %>%
  summarise(
    n_admisiones_grd = n(),
    caci_ei          = paste(sort(unique(na.omit(caci))), collapse = ", "),
    los_total_grd    = round(sum(dif_days,       na.rm = TRUE), 1),
    fact_total_grd   = round(sum(valor_factura,  na.rm = TRUE), 0),
    diag_ei          = first(na.omit(diagnostico_egreso_principal)),
    .groups = "drop"
  ) %>%
  mutate(caci_ei = if_else(caci_ei == "", NA_character_, caci_ei))

# Respaldo a nivel paciente: una estancia inactiva de enero puede corresponder a
# una admisión de diciembre del año anterior. Los conteos del año (admisiones,
# LOS, facturación) siguen siendo del año; sólo el CACI y el diagnóstico —que
# son atributos del paciente— caen a este respaldo cuando no hay ingreso del
# mismo año. Recupera ~5 pp de cobertura en 2026.
grd_for_ei_pac <- grd_ei_norm %>%
  group_by(doc_norm) %>%
  summarise(
    caci_pac = paste(sort(unique(na.omit(caci))), collapse = ", "),
    diag_pac = first(na.omit(diagnostico_egreso_principal)),
    .groups  = "drop"
  ) %>%
  mutate(caci_pac = if_else(caci_pac == "", NA_character_, caci_pac))

data_ei <- data_ei %>%
  left_join(grd_for_ei,     by = c("doc_norm", "año")) %>%
  left_join(grd_for_ei_pac, by = "doc_norm") %>%
  mutate(caci_ei = coalesce(caci_ei, caci_pac),
         diag_ei = coalesce(diag_ei, diag_pac)) %>%
  select(-caci_pac, -diag_pac)

# ── Opciones para selectores UI ───────────────────────────────────────────────
year_choices_los <- sort(unique(na.omit(data_los_serv$year)), decreasing = TRUE)
serv_choices     <- sort(unique(na.omit(data_los_full$estacion_2)))
caci_choices_los <- sort(unique(na.omit(data_grd_base$caci)))
ei_year_choices  <- sort(unique(na.omit(data_ei$año)), decreasing = TRUE)

meses_full  <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
mes_choices <- c("Todos los meses" = "0",
                 setNames(as.character(1:12), meses_full))

# (shinydashboard skin handled via www/styles.css)

# ── Giro Cama: datos KPI (2024-2026) ─────────────────────────────────────────
tryCatch({
  if (is.na(kpi_file)) stop("No se encontró el libro KPI en supplies/")
  if (is.na(bd_file))  stop("No se encontró ningún libro <n>_Table_… en supplies/")
  message("[LOS-App] Giro Cama  <- ", basename(kpi_file))
  message("[LOS-App] Est.Inact. <- ", basename(bd_file))

  # ── Parse KPI sheet ──────────────────────────────────────────────────────
  kpi_raw <- readxl::read_excel(kpi_file,
                                sheet = "KPI´S PROYECTO GIRO CAMA ") %>%
    janitor::clean_names()

  # Month columns present in the file
  mes_cols <- c("ene","feb","mar","abr","may","jun",
                "jul","ago","sep","oct","nov","dic")
  mes_num_map <- setNames(1:12, mes_cols)

  # Forward-fill NOMBRE IND and META so every row knows its indicator
  kpi_filled <- kpi_raw %>%
    mutate(
      nombre_ind = zoo::na.locf(nombre_ind, na.rm = FALSE),
      meta_val   = zoo::na.locf(meta,       na.rm = FALSE)
    ) %>%
    # Mark sub-row position within each (indicator × year block):
    # row type: tipo_1 = year-row (AÑO not NA), tipo_2 = second sub-row, tipo_3 = third sub-row
    group_by(nombre_ind) %>%
    mutate(
      is_year_row = !is.na(ano),
      # Assign a block index per indicator group
      block_idx   = cumsum(is_year_row),
      # Position within block
      sub_pos     = row_number() - (cumsum(is_year_row) - is_year_row) * 0
    ) %>%
    ungroup()

  # Reconstruct sub-row position within each (indicator, block) group
  kpi_filled <- kpi_filled %>%
    group_by(nombre_ind, block_idx) %>%
    mutate(sub_pos = row_number()) %>%
    ungroup() %>%
    # Forward-fill year within each indicator block
    group_by(nombre_ind) %>%
    mutate(year_val = zoo::na.locf(ano, na.rm = FALSE)) %>%
    ungroup()

  # Filter to our 13 key indicators, keep only sub_pos == 1 (main value rows)
  # and melt months to long
  gc_inds <- c(
    "Giro Cama Institucional",
    "Giro cama de UCI",
    "Giro cama de UCIN",
    "Giro cama de HOSP",
    "Días de estancia Institucional",
    "Días de estancia UCI",
    "Días de estancia UCIN",
    "Días de estancia HOSP",
    "Total de días de estancias inactivas",
    "Total de días de estancias inactivas EPS",
    "Total de días de estancias inactivas IPS",
    "Costo de estancia inactiva por EPS",
    "Costo de estancia inactiva por IPS"
  )

  # For giro cama and LOS indicators: sub_pos 1 = egresos/dias, 2 = camas/egresos, 3 = rate/mean
  # For inactive indicators: sub_pos 1 = monthly values only
  kpi_long <- kpi_filled %>%
    filter(nombre_ind %in% gc_inds, !is.na(year_val)) %>%
    select(nombre_ind, meta_val, year_val, sub_pos,
           all_of(intersect(mes_cols, names(.))), total) %>%
    tidyr::pivot_longer(
      cols      = all_of(intersect(mes_cols, names(.))),
      names_to  = "mes_abr",
      values_to = "valor"
    ) %>%
    mutate(
      mes_num  = mes_num_map[mes_abr],
      year_val = as.integer(year_val),
      valor    = suppressWarnings(as.numeric(valor)),
      meta_num = suppressWarnings(as.numeric(meta_val))
    ) %>%
    filter(!is.na(mes_num))

  # Build the processed KPI frame: one row per (indicador, year, mes)
  # sub_pos=3 holds the rate/mean for GC + LOS indicators; sub_pos=1 for inactive/cost
  data_kpi_gc <- bind_rows(
    # Giro cama rates and LOS means: take sub_pos == 3
    kpi_long %>%
      filter(
        nombre_ind %in% c(
          "Giro Cama Institucional","Giro cama de UCI",
          "Giro cama de UCIN","Giro cama de HOSP",
          "Días de estancia Institucional","Días de estancia UCI",
          "Días de estancia UCIN","Días de estancia HOSP"
        ),
        sub_pos == 3
      ) %>%
      mutate(tipo = "rate_mean"),
    # Giro cama egresos: sub_pos == 1
    kpi_long %>%
      filter(
        nombre_ind %in% c(
          "Giro Cama Institucional","Giro cama de UCI",
          "Giro cama de UCIN","Giro cama de HOSP"
        ),
        sub_pos == 1
      ) %>%
      mutate(tipo = "egresos"),
    # Giro cama camas: sub_pos == 2
    kpi_long %>%
      filter(
        nombre_ind %in% c(
          "Giro Cama Institucional","Giro cama de UCI",
          "Giro cama de UCIN","Giro cama de HOSP"
        ),
        sub_pos == 2
      ) %>%
      mutate(tipo = "camas"),
    # LOS dias totales: sub_pos == 1
    kpi_long %>%
      filter(
        nombre_ind %in% c(
          "Días de estancia Institucional","Días de estancia UCI",
          "Días de estancia UCIN","Días de estancia HOSP"
        ),
        sub_pos == 1
      ) %>%
      mutate(tipo = "dias_totales"),
    # LOS egresos: sub_pos == 2
    kpi_long %>%
      filter(
        nombre_ind %in% c(
          "Días de estancia Institucional","Días de estancia UCI",
          "Días de estancia UCIN","Días de estancia HOSP"
        ),
        sub_pos == 2
      ) %>%
      mutate(tipo = "egresos_los"),
    # Inactive days + costs: sub_pos == 1
    kpi_long %>%
      filter(
        nombre_ind %in% c(
          "Total de días de estancias inactivas",
          "Total de días de estancias inactivas EPS",
          "Total de días de estancias inactivas IPS",
          "Costo de estancia inactiva por EPS",
          "Costo de estancia inactiva por IPS"
        ),
        sub_pos == 1
      ) %>%
      mutate(tipo = "inactivo_costo")
  )

  # Also keep the TOTAL column for KPI cards (full-year acum)
  kpi_totals <- kpi_filled %>%
    filter(nombre_ind %in% gc_inds, !is.na(year_val), sub_pos %in% c(1, 3)) %>%
    select(nombre_ind, year_val, sub_pos, total, meta_val) %>%
    mutate(
      year_val = as.integer(year_val),
      total    = suppressWarnings(as.numeric(total)),
      meta_num = suppressWarnings(as.numeric(meta_val))
    )

  # ── Wide panel data for statistical model ────────────────────────────────
  # One row per (year, mes_num) with all institutional-level KPIs
  gc_rate_inst <- data_kpi_gc %>%
    filter(nombre_ind == "Giro Cama Institucional", tipo == "rate_mean") %>%
    select(year_val, mes_num, tasa_gc = valor)

  los_inst <- data_kpi_gc %>%
    filter(nombre_ind == "Días de estancia Institucional", tipo == "rate_mean") %>%
    select(year_val, mes_num, mean_los = valor)

  egresos_inst <- data_kpi_gc %>%
    filter(nombre_ind == "Giro Cama Institucional", tipo == "egresos") %>%
    select(year_val, mes_num, egresos = valor)

  camas_inst <- data_kpi_gc %>%
    filter(nombre_ind == "Giro Cama Institucional", tipo == "camas") %>%
    select(year_val, mes_num, camas = valor)

  dias_inac_tot <- data_kpi_gc %>%
    filter(nombre_ind == "Total de días de estancias inactivas", tipo == "inactivo_costo") %>%
    select(year_val, mes_num, dias_inac_total = valor)

  dias_inac_eps <- data_kpi_gc %>%
    filter(nombre_ind == "Total de días de estancias inactivas EPS", tipo == "inactivo_costo") %>%
    select(year_val, mes_num, dias_inac_eps = valor)

  dias_inac_ips <- data_kpi_gc %>%
    filter(nombre_ind == "Total de días de estancias inactivas IPS", tipo == "inactivo_costo") %>%
    select(year_val, mes_num, dias_inac_ips = valor)

  costo_eps_kpi <- data_kpi_gc %>%
    filter(nombre_ind == "Costo de estancia inactiva por EPS", tipo == "inactivo_costo") %>%
    select(year_val, mes_num, costo_eps = valor)

  costo_ips_kpi <- data_kpi_gc %>%
    filter(nombre_ind == "Costo de estancia inactiva por IPS", tipo == "inactivo_costo") %>%
    select(year_val, mes_num, costo_ips = valor)

  data_kpi_wide <- gc_rate_inst %>%
    full_join(los_inst,       by = c("year_val","mes_num")) %>%
    full_join(egresos_inst,   by = c("year_val","mes_num")) %>%
    full_join(camas_inst,     by = c("year_val","mes_num")) %>%
    full_join(dias_inac_tot,  by = c("year_val","mes_num")) %>%
    full_join(dias_inac_eps,  by = c("year_val","mes_num")) %>%
    full_join(dias_inac_ips,  by = c("year_val","mes_num")) %>%
    full_join(costo_eps_kpi,  by = c("year_val","mes_num")) %>%
    full_join(costo_ips_kpi,  by = c("year_val","mes_num")) %>%
    filter(!is.na(egresos), egresos > 0) %>%
    mutate(
      fecha    = as.Date(paste0(year_val, "-", sprintf("%02d", mes_num), "-01")),
      año_f    = factor(year_val)
    ) %>%
    arrange(fecha)

  # ── Ocupación cama: dias_totales / (camas × días_del_mes) × 100 ──────────────
  serv_ocu_map  <- c(
    "Institucional" = "Días de estancia Institucional",
    "UCI"           = "Días de estancia UCI",
    "UCIN"          = "Días de estancia UCIN",
    "HOSP"          = "Días de estancia HOSP"
  )
  camas_ocu_map <- c(
    "Institucional" = "Giro Cama Institucional",
    "UCI"           = "Giro cama de UCI",
    "UCIN"          = "Giro cama de UCIN",
    "HOSP"          = "Giro cama de HOSP"
  )

  dias_ocu <- data_kpi_gc %>%
    filter(tipo == "dias_totales") %>%
    mutate(servicio = names(serv_ocu_map)[match(nombre_ind, serv_ocu_map)]) %>%
    filter(!is.na(servicio)) %>%
    select(servicio, year_val, mes_num, dias_tot = valor)

  camas_ocu <- data_kpi_gc %>%
    filter(tipo == "camas") %>%
    mutate(servicio = names(camas_ocu_map)[match(nombre_ind, camas_ocu_map)]) %>%
    filter(!is.na(servicio)) %>%
    select(servicio, year_val, mes_num, camas_n = valor)

  data_ocupacion <- dias_ocu %>%
    left_join(camas_ocu, by = c("servicio", "year_val", "mes_num")) %>%
    mutate(
      fecha         = as.Date(paste0(year_val, "-", sprintf("%02d", mes_num), "-01")),
      dias_mes      = as.integer(lubridate::days_in_month(fecha)),
      cap_total     = camas_n * dias_mes,
      ocupacion_pct = round(dias_tot / cap_total * 100, 1),
      alerta        = dplyr::case_when(
        is.na(ocupacion_pct)       ~ "Sin dato",
        ocupacion_pct > 100        ~ "Crítica (>100%)",
        ocupacion_pct >= 90        ~ "Alerta (90-100%)",
        ocupacion_pct >= 85        ~ "Vigilancia (85-90%)",
        TRUE                       ~ "Normal (<85%)"
      ),
      alerta = factor(alerta, levels = c("Normal (<85%)", "Vigilancia (85-90%)",
                                          "Alerta (90-100%)", "Crítica (>100%)", "Sin dato"))
    ) %>%
    filter(!is.na(dias_tot), !is.na(camas_n))

  message("[LOS-App] Ocupación: ", nrow(data_ocupacion), " filas | ",
          sum(data_ocupacion$ocupacion_pct > 100, na.rm = TRUE), " meses >100%")

  # ── Parse BD trabajo (3) — fuente completa con todos los meses 2024-2026 ────
  # Uses FECHA column (reliable date) instead of guessing year from EPS name.
  # BD trabajo (3) has April + May 2026 which the main BD trabajo sheet was missing.
  data_bd_inac <- readxl::read_excel(bd_file, sheet = "BD trabajo (3)") %>%
    janitor::clean_names() %>%
    filter(!is.na(identificacion)) %>%
    mutate(
      fecha_d    = suppressWarnings(as.Date(fecha)),
      year_bd    = as.integer(year(fecha_d)),
      mes_bd_num = as.integer(month(fecha_d)),
      # Clean document number (remove type prefix "CC ", "TI ", etc.)
      doc_clean  = str_trim(str_remove(identificacion,
                     "^(CC|TI|RC|PA|CD|CE|NIT|MS|AS)[ ]+")),
      responsable_bd = case_when(
        str_detect(coalesce(clasificacion_de_la_estancia_inactiva, ""),
                   regex("IPS",      ignore_case = TRUE)) ~ "IPS",
        str_detect(coalesce(clasificacion_de_la_estancia_inactiva, ""),
                   regex("EPS",      ignore_case = TRUE)) ~ "EPS",
        str_detect(coalesce(clasificacion_de_la_estancia_inactiva, ""),
                   regex("paciente", ignore_case = TRUE)) ~ "Paciente",
        TRUE ~ "No clasificado"
      ),
      # Primary cause (first non-empty IPS, then EPS)
      causa_principal_bd = coalesce(
        if_else(!is.na(causa_1_de_estancia_inactiva_por_ips) &
                  causa_1_de_estancia_inactiva_por_ips != "",
                causa_1_de_estancia_inactiva_por_ips, NA_character_),
        if_else(!is.na(causa_1_de_estancia_inactiva_por_eps) &
                  causa_1_de_estancia_inactiva_por_eps != "",
                causa_1_de_estancia_inactiva_por_eps, NA_character_)
      ),
      causa_principal_bd = str_trim(str_remove(
        as.character(causa_principal_bd), "^\\d+\\.\\s*")),
      dias_inac_ips_bd  = suppressWarnings(as.numeric(total_dias_de_estancia_por_ips)),
      dias_inac_eps_bd  = suppressWarnings(as.numeric(total_dias_de_estancia_por_eps)),
      dias_inac_pac_bd  = suppressWarnings(as.numeric(total_dias_de_estancia_por_paciente)),
      estancia_inactiva = suppressWarnings(as.numeric(
        coalesce(total_dias_de_estancia_por_ips,
                 total_dias_de_estancia_por_eps))),
      valor_inac        = suppressWarnings(as.numeric(valor_total_estancia_inactiva)),
      identificacion    = as.character(identificacion)
    ) %>%
    filter(!is.na(fecha_d))   # keep only rows with a valid date

  # ── Cross-match inactive patients with GRD (CACI + diagnóstico) ───────────
  grd_key_ei <- data_grd_base %>%
    filter(!is.na(documento), documento != "") %>%
    mutate(doc = as.character(documento)) %>%
    group_by(doc) %>%
    summarise(
      caci_ei    = paste(sort(unique(na.omit(str_to_upper(caci)))), collapse = ", "),
      diag_ei    = first(na.omit(diagnostico_egreso_principal)),
      n_adm_ei   = n(),
      los_grd_ei = round(mean(dif_days, na.rm = TRUE), 1),
      fact_ei    = round(sum(valor_factura, na.rm = TRUE), 0),
      edad_media = round(mean(suppressWarnings(as.numeric(as.character(edad))), na.rm = TRUE), 0),
      .groups    = "drop"
    ) %>%
    mutate(caci_ei = if_else(caci_ei == "", NA_character_, caci_ei))

  data_bd_inac <- data_bd_inac %>%
    left_join(grd_key_ei, by = c("doc_clean" = "doc")) %>%
    mutate(
      es_caci    = !is.na(caci_ei),
      caci_label = if_else(es_caci, caci_ei, "No CACI"),
      # Build age groups from GRD-matched age
      rangos_edad = case_when(
        !is.na(edad_media) & edad_media < 40 ~ "<40",
        !is.na(edad_media) & edad_media < 60 ~ "40–59",
        !is.na(edad_media) & edad_media < 70 ~ "60–69",
        !is.na(edad_media) & edad_media < 80 ~ "70–79",
        !is.na(edad_media)                   ~ "≥80",
        TRUE                                  ~ "Sin dato"
      ),
      rangos_edad = factor(rangos_edad,
                           levels = c("<40","40–59","60–69","70–79","≥80","Sin dato")),
      # Fix estancia_inactiva: prefer > 0 values, fallback to any non-NA (keeps 0s as valid)
      estancia_inactiva = suppressWarnings({
        d_ips <- as.numeric(total_dias_de_estancia_por_ips)
        d_eps <- as.numeric(total_dias_de_estancia_por_eps)
        d_pac <- as.numeric(total_dias_de_estancia_por_paciente)
        dplyr::case_when(
          !is.na(d_ips) & d_ips > 0 ~ d_ips,
          !is.na(d_eps) & d_eps > 0 ~ d_eps,
          !is.na(d_pac) & d_pac > 0 ~ d_pac,
          !is.na(d_ips)              ~ d_ips,   # keep 0s as valid
          !is.na(d_eps)              ~ d_eps,
          TRUE                       ~ NA_real_
        )
      })
    )

  gc_year_choices <- c("Todos" = "0",
                       "2024" = "2024", "2025" = "2025", "2026" = "2026")

  message("[LOS-App] Giro Cama KPI loaded: ", nrow(data_kpi_gc),
          " rows | BD inactivas: ", nrow(data_bd_inac), " rows")

}, error = function(e) {
  message("[LOS-App] ERROR cargando datos Giro Cama: ", conditionMessage(e))
  data_kpi_gc     <<- tibble()
  data_bd_inac    <<- tibble()
  data_kpi_wide   <<- tibble()
  kpi_totals      <<- tibble()
  data_ocupacion  <<- tibble()
  gc_year_choices <<- c("Todos" = "0")
})

# ── Referencia financiera institucional (PyG 2026, ejecutado 2025) ────────────
# Cross: monthly COSTO DE VENTAS (PyG) ÷ total días de estancia (KPI) = costo/día cama
tryCatch({
  pyg_file <- file.path(
    path.expand("~"), "Desktop", "Proyecto AV DIME",
    "Análisis financiero DIME",
    "EJECUCION FLUJO  2026 - NEPS - SANITAS.xlsx"
  )

  if (file.exists(pyg_file)) {
    pyg_raw <- readxl::read_excel(pyg_file, sheet = "PyG 2026", col_names = FALSE)

    # Row 5 = INGRESOS OPERACIONALES, Row 8 = COSTO DE VENTAS; cols 3-14 = Jan-Dec
    ingresos_m   <- suppressWarnings(as.numeric(unlist(pyg_raw[5, 3:14])))
    costos_m     <- suppressWarnings(as.numeric(unlist(pyg_raw[8, 3:14])))

    # BUG 1 CORREGIDO (2026-08-19): year_val estaba fijo en 2025L mientras la
    # hoja leída es "PyG 2026" -> el costo/día cruzaba costos de 2026 con días
    # de estancia de 2025. Ahora el año se toma del nombre de la hoja.
    PYG_YEAR <- 2026L

    # BUG 2 CORREGIDO: la hoja trae los 12 meses, pero los posteriores al mes
    # en curso son PRESUPUESTO, no ejecución (la línea de costo es sospechosa-
    # mente plana, 4,7–5,6 B todos los meses). Incluirlos contamina la
    # referencia con proyección. Se recortan a los meses ya ejecutados.
    mes_max_real <- if (PYG_YEAR < year(Sys.Date())) 12L
                    else month(Sys.Date()) - 1L      # el mes en curso no cerró

    data_pyg_mensual <- tibble(
      year_val     = PYG_YEAR,
      mes_num      = 1:12,
      ingresos_m   = ingresos_m,
      costo_ventas = costos_m
    ) %>%
      filter(!is.na(ingresos_m), !is.na(costo_ventas), ingresos_m > 0,
             mes_num <= mes_max_real)

    # Días de estancia institucionales del MISMO año que los costos
    dias_tot_kpi <- data_kpi_gc %>%
      filter(nombre_ind == "Días de estancia Institucional",
             tipo == "dias_totales", year_val == PYG_YEAR) %>%
      select(mes_num, dias_est_total = valor) %>%
      filter(!is.na(dias_est_total), dias_est_total > 0)

    if (!nrow(dias_tot_kpi))
      message("[LOS-App] AVISO: sin días de estancia ", PYG_YEAR,
              " en el KPI — la referencia PyG quedará NA.")

    data_pyg_mensual <- data_pyg_mensual %>%
      left_join(dias_tot_kpi, by = "mes_num") %>%
      mutate(
        costo_dia_cama = if_else(!is.na(dias_est_total) & dias_est_total > 0,
                                  costo_ventas / dias_est_total, NA_real_),
        margen_m       = 1 - costo_ventas / ingresos_m
      )

    ing_total  <- sum(data_pyg_mensual$ingresos_m,   na.rm = TRUE)
    cos_total  <- sum(data_pyg_mensual$costo_ventas, na.rm = TRUE)

    pyg_ref <- list(
      costo_dia_cama_prom = median(data_pyg_mensual$costo_dia_cama, na.rm = TRUE),
      year                = PYG_YEAR,
      meses_reales        = nrow(data_pyg_mensual),
      ingresos_M          = round(ing_total  / 1e6, 0),
      costos_M            = round(cos_total  / 1e6, 0),
      margen_pct          = round((1 - cos_total / ing_total) * 100, 1),
      data_pyg_mensual    = data_pyg_mensual
    )
    # Nombres antiguos (…_2025_…) mantenidos por compatibilidad con app.R;
    # el año real es pyg_ref$year, no 2025.
    pyg_ref$ingresos_2025_M <- pyg_ref$ingresos_M
    pyg_ref$costos_2025_M   <- pyg_ref$costos_M
    pyg_ref$margen_2025_pct <- pyg_ref$margen_pct

    message("[LOS-App] PyG ref ", PYG_YEAR, " (", pyg_ref$meses_reales,
            " meses ejecutados) | costo/día cama: ",
            formatC(round(pyg_ref$costo_dia_cama_prom), format = "d", big.mark = "."),
            " COP | Margen: ", pyg_ref$margen_pct, "%")
  } else {
    message("[LOS-App] Archivo PyG no encontrado — referencia financiera deshabilitada")
    pyg_ref <- list(costo_dia_cama_prom = NA_real_, data_pyg_mensual = tibble(),
                    ingresos_2025_M = NA_real_, costos_2025_M = NA_real_,
                    margen_2025_pct = NA_real_)
  }
}, error = function(e) {
  message("[LOS-App] ERROR cargando PyG: ", conditionMessage(e))
  pyg_ref <<- list(costo_dia_cama_prom = NA_real_, data_pyg_mensual = tibble(),
                   ingresos_2025_M = NA_real_, costos_2025_M = NA_real_,
                   margen_2025_pct = NA_real_)
})

# ── Costo de estancia inactiva por responsable (IPS / EAPB) ──────────────────
# Módulo ADITIVO: valoriza los días inactivos a precio de día-cama SOAT 2026.
# No modifica ningún cálculo de costo, venta o margen existente.
# ── Economía del día-cama (costo DIME vs tarifa facturada vs SOAT) ───────────
# DEBE ir ANTES de inactive_stay_cost.R: ese módulo usa bed_value$por_eapb y
# BED_COSTO_CUPS_2026 para valorizar cada día inactivo con la tarifa de su
# servicio y su pagador.
tryCatch({
  source(file.path(proj_dir, "bed_value.R"), local = FALSE)
}, error = function(e) {
  message("[LOS-App] Módulo de valor de cama no cargado: ", conditionMessage(e))
  bed_value <<- NULL
})

tryCatch({
  source(file.path(proj_dir, "inactive_stay_cost.R"), local = FALSE)
}, error = function(e) {
  message("[LOS-App] Módulo de costo de estancia inactiva no cargado: ",
          conditionMessage(e))
  ei_cost <<- NULL
})

# Sello de versión, visible en el tab Est. Inactivas y en el log de arranque.
# Refrescar el navegador NO vuelve a ejecutar global.R: si el sello o la
# cobertura CACI de abajo no coinciden con lo esperado, el proceso de R es viejo.
EI_BUILD <- "2026-09-21 · filtro año/mes + cruce CACI normalizado"

message("[LOS-App] global.R listo. Años: ",
        paste(sort(year_choices_los), collapse = ", "),
        " | Servicios: ", paste(serv_choices, collapse = ", "))
message("[LOS-App] build: ", EI_BUILD)
local({
  cob <- data_ei %>%
    filter(!is.na(año)) %>%
    group_by(año) %>%
    summarise(pct = 100 * mean(!is.na(caci_ei)), .groups = "drop")
  message("[LOS-App] Cobertura CACI en estancias inactivas: ",
          paste(sprintf("%d %.1f%%", cob$año, cob$pct), collapse = " · "),
          "   (0 % = cruce roto, revisar normalización del documento)")
})
