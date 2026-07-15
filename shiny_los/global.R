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

tryCatch(
  Sys.setlocale("LC_TIME", "es_ES.UTF-8"),
  warning = function(w) NULL,
  error   = function(e) NULL
)

# ── Paletas ────────────────────────────────────────────────────────────────────
serv_colors <- c(
  "UCI"            = "#E15759",
  "UCIN 2°"        = "#F28E2B",
  "UCIN 5°"        = "#EDC948",
  "UCIN ANGIO"     = "#76B7B2",
  "URGENCIAS OBS"  = "#4E79A7",
  "PISO HOSP"      = "#59A14F",
  "UCIN RESPIRATORIOS" = "#B07AA1"
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
# Monthly updates: drop  data_cense_update_YYYY_MM.rds  into data/ each month.
# The app detects them automatically and merges before processing.
los_cache    <- file.path(data_dir, "data_los_processed.rds")
cense_src    <- file.path(data_dir, "data_cense_2017_2026.rds")

cense_update_files <- sort(list.files(
  data_dir,
  pattern    = "^data_cense_update_\\d{4}_\\d{2}\\.rds$",
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
      dif_days   = round(
        as.numeric(fecha_egreso_movimiento_cama - fecha_ingreso_movimiento_cama) / 86400, 2)
    ) %>%
    filter(!is.na(year), !str_detect(coalesce(paciente, ""), "TIC")) %>%
    group_by(cuenta, estacion) %>%
    arrange(id, estacion, fecha_egreso_movimiento_cama) %>%
    mutate(
      num_bed_serv      = dense_rank(fecha_egreso_movimiento_cama),
      max_bed_serv      = max(num_bed_serv),
      max_bed_date_serv = max(fecha_egreso_movimiento_cama),
      min_bed_date_serv = min(fecha_ingreso_movimiento_cama),  # first admission to service
      dif_bed_serv = round(
        as.numeric(max_bed_date_serv - min_bed_date_serv) / 86400, 2),
      estacion_2 = case_when(
        estacion == "UCI"                   ~ "UCI",
        estacion == "UCIN"                  ~ "UCIN 2°",
        estacion == "UCIN 5 PISO"           ~ "UCIN 5°",
        estacion == "UCIN ANGIO"            ~ "UCIN ANGIO",
        estacion == "URGENCIAS_OBSERVACION" ~ "URGENCIAS OBS",
        estacion == "HOSPITALIZACION 3 PISO"~ "PISO HOSP",
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
  )

# ── Costos: itemizados (ventas + costo + CACI) ────────────────────────────────
# Smart loading: use compact pre-aggregated file when available (shinyapps.io),
# fall back to full raw RDS files for local development.
compact_cost_path <- file.path(data_dir, "los_cost_compact.rds")

if (file.exists(compact_cost_path)) {
  data_costo_base <- readRDS(compact_cost_path)
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
      venta          = suppressWarnings(as.numeric(gsub(",", ".", valor_cargo_tarifario))),
      costo          = suppressWarnings(as.numeric(costo)),
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

# ── Estancias inactivas (2024-2026, pestaña separada) ─────────────────────────
ei_path <- file.path(los_dir,
                      "estancia_inactiva_costo_2026",
                      "estancia_inactiva.xlsx")

data_ei <- rio::import(ei_path) %>%
  clean_names() %>%
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
    valor_total_estancia_inactiva = suppressWarnings(
      as.numeric(valor_total_estancia_inactiva))
  )

# Enrich EI with GRD admissions data (join on patient documento × año)
grd_for_ei <- data_grd_base %>%
  filter(!is.na(documento), documento != "") %>%
  mutate(documento = as.character(documento)) %>%
  group_by(documento, año) %>%
  summarise(
    n_admisiones_grd = n(),
    caci_ei          = paste(sort(unique(na.omit(caci))), collapse = ", "),
    los_total_grd    = round(sum(dif_days,       na.rm = TRUE), 1),
    fact_total_grd   = round(sum(valor_factura,  na.rm = TRUE), 0),
    diag_ei          = first(na.omit(diagnostico_egreso_principal)),
    .groups = "drop"
  ) %>%
  mutate(caci_ei = if_else(caci_ei == "", NA_character_, caci_ei))

data_ei <- data_ei %>%
  left_join(grd_for_ei, by = c("identificacion" = "documento", "año" = "año"))

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

message("[LOS-App] global.R listo. Años: ",
        paste(sort(year_choices_los), collapse = ", "),
        " | Servicios: ", paste(serv_choices, collapse = ", "))
