################################################################################
# global.R — GRD Dashboard · DIME Clínica Neurocardiovascular
# Cargado una sola vez al iniciar la app (compartido entre sesiones)
################################################################################

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(lubridate)
library(scales)
library(janitor)
library(here)
library(rio)
library(reactable)
library(plotly)
library(DT)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# ── Paleta y etiquetas CACI ───────────────────────────────────────────────────
# Jerarquía clínica: ICC > ACV > SCA > TEP > TxC > Otros CV
caci_levels <- c("ICC", "ACV", "SCA", "TEP", "TxC", "Otros CV")

# Mapa de normalización: valor crudo (base de datos) → etiqueta limpia
caci_map <- c(
  ICC          = "ICC",
  ACV          = "ACV",
  SCA          = "SCA",
  TEP          = "TEP",
  TXC          = "TxC",
  CARDIO_OTHER = "Otros CV",
  OTROS_CV     = "Otros CV",
  OTRO_CV      = "Otros CV"
)

# Colores institucionales DIME (consistentes con test_2.qmd)
caci_colors <- c(
  ICC        = "#E15759",
  ACV        = "#4E79A7",
  SCA        = "#F28E2B",
  TEP        = "#76B7B2",
  TxC        = "#59A14F",
  `Otros CV` = "#B07AA1"
)

# Paleta de años para comparativos históricos
year_colors <- c(
  "2024" = "#C0392B",
  "2025" = "#E67E22",
  "2026" = "#2980B9"
)

# ── Función de normalización CACI ─────────────────────────────────────────────
# Convierte cualquier variante del código crudo al label limpio como factor
# ordenado por jerarquía clínica. Usada tanto en costos como en GRD.
recode_caci <- function(x) {
  x_clean <- str_to_upper(as.character(x))
  x_clean <- str_replace_all(x_clean, "\\s+", "_")
  factor(
    dplyr::recode(x_clean, !!!caci_map, .default = NA_character_),
    levels = caci_levels
  )
}

# ── Formateadores COP ─────────────────────────────────────────────────────────
cop <- function(x) {
  ifelse(is.na(x), "—",
         scales::dollar(x, prefix = "$", big.mark = ".", decimal.mark = ",",
                        accuracy = 1))
}

cop_m <- function(x) {
  scales::dollar(x / 1e6, prefix = "$", suffix = "M",
                 big.mark = ".", decimal.mark = ",", accuracy = 1)
}

cop_kpi <- function(x) {
  if (is.na(x) || length(x) == 0) return("—")
  ax <- abs(x)
  if (ax >= 1e9)
    scales::dollar(x / 1e9, prefix = "$", suffix = " B",
                   big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  else if (ax >= 1e6)
    scales::dollar(x / 1e6, prefix = "$", suffix = " M",
                   big.mark = ".", decimal.mark = ",", accuracy = 0.1)
  else if (ax >= 1e3)
    scales::dollar(x / 1e3, prefix = "$", suffix = " K",
                   big.mark = ".", decimal.mark = ",", accuracy = 1)
  else
    scales::dollar(x, prefix = "$", big.mark = ".", decimal.mark = ",",
                   accuracy = 1)
}

pct_fmt <- function(x) {
  ifelse(is.na(x), "—",
         paste0(scales::number(x, accuracy = 0.1, decimal.mark = ","), "%"))
}

safe_pct <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, round(num / den * 100, 1), NA_real_)
}

# ── Carga de datos procesados ─────────────────────────────────────────────────
# Busca en data/ del proyecto (desarrollo) o shiny_grd/data/ (deploy)
resolve_data_dir <- function() {
  local_dir <- file.path(getwd(), "data")
  if (dir.exists(local_dir) &&
      length(list.files(local_dir, pattern = "data_costo_total_3")) > 0)
    return(local_dir)

  if (requireNamespace("here", quietly = TRUE)) {
    proj_dir <- here::here("data")
    if (dir.exists(proj_dir) &&
        length(list.files(proj_dir, pattern = "data_costo_total_3")) > 0)
      return(proj_dir)
  }

  stop("No se encontró data/. Copia los archivos RDS a shiny_grd/data/ antes de deployar.")
}

data_dir <- resolve_data_dir()

cost_files <- list.files(data_dir, pattern = "data_costo_total_3_.*_II\\.rds",
                         full.names = TRUE)
grd_files  <- list.files(data_dir, pattern = "data_grd_2_.*_II\\.(rds|rda)",
                         full.names = TRUE)

message(sprintf("[GRD-App] Cargando %d archivo(s) de costos y %d de admisiones...",
                length(cost_files), length(grd_files)))

# Las columnas cambian de tipo entre el RDS 2025 y 2026 (Date vs POSIXct,
# character vs integer, etc.). Se normalizan antes de unir con bind_rows.
safe_import <- function(f) {
  df <- rio::import(f)
  # Fechas: forzar a Date (funciona tanto para Date como POSIXct)
  for (col in c("fecha_cargue", "fecha_registro", "fecha_ingreso", "fecha_de_egreso")) {
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  }
  # Numéricas: algunas vienen como character o factor en ciertos archivos
  for (col in c("edad", "año")) {
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  }
  # Texto: algunas vienen como integer
  for (col in c("transaccion", "mes_cargue", "mes", "ingreso", "cod_cargo")) {
    if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
  }
  # Factores ordenados con niveles distintos entre archivos → character
  ordered_cols <- names(df)[sapply(df, is.ordered)]
  for (col in ordered_cols) df[[col]] <- as.character(df[[col]])
  df
}

safe_import_grd <- function(f) {
  df <- rio::import(f) %>% janitor::clean_names()
  # Normalize CACI to a single 'caci' column before binding (avoids collision
  # when 2025 has 'caci'+'caci_3' and 2026 has 'caci_3'+'caci_final')
  if ("caci_3" %in% names(df) && "caci" %in% names(df)) {
    df$caci <- dplyr::coalesce(df$caci_3, df$caci)
  } else if ("caci_3" %in% names(df)) {
    df$caci <- df$caci_3
  } else if ("caci_final" %in% names(df) && !"caci" %in% names(df)) {
    df$caci <- df$caci_final
  }
  df <- df %>% select(-any_of(c("caci_3", "caci_2", "caci_final", "mes_caci")))
  # Type normalization
  for (col in c("fecha_ingreso", "fecha_de_egreso")) {
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  }
  for (col in c("edad", "estancia_horas", "valor_factura", "total_cuenta")) {
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  }
  # Ordered factors with different levels between files → character
  ordered_cols <- names(df)[sapply(df, is.ordered)]
  for (col in ordered_cols) df[[col]] <- as.character(df[[col]])
  df
}

data_costo_raw <- bind_rows(lapply(cost_files, safe_import))
data_grd_raw   <- bind_rows(lapply(grd_files,  safe_import_grd))

# ── Estandarización de columnas ───────────────────────────────────────────────
prep_costo <- function(df) {
  # clean_names translitea ñ→n: "año" pasa a "ano". Lo guardamos antes de mutar.
  df %>%
    janitor::clean_names() %>%
    rename_with(~ "caci",  any_of(c("caci_3", "caci"))) %>%
    rename_with(~ "costo", any_of(c("costo_2", "costo"))) %>%
    mutate(
      fecha_cargue = as.Date(fecha_cargue),
      venta  = readr::parse_number(
        as.character(valor_cargo_tarifario),
        locale = readr::locale(grouping_mark = ",", decimal_mark = ".")
      ),
      costo  = as.numeric(costo),
      # 'año' en los RDS = año de ingreso del paciente (no de fecha_cargue).
      # Después de clean_names queda como 'ano'; si es NA usamos fecha_cargue.
      año = as.integer(dplyr::coalesce(
        suppressWarnings(as.numeric(ano)),
        year(fecha_cargue)
      )),
      mes_cargue = as.integer(mes_cargue),
      caci   = recode_caci(caci)
    ) %>%
    filter(año >= 2024L, !is.na(año))
}

prep_grd <- function(df) {
  # clean_names() y normalización de caci ya se hicieron en safe_import_grd
  df %>%
    mutate(
      fecha_ingreso    = as.Date(fecha_ingreso),
      fecha_de_egreso  = as.Date(fecha_de_egreso),
      caci = recode_caci(caci),
      año  = as.integer(year(coalesce(fecha_ingreso, fecha_de_egreso)))
    ) %>%
    filter(año >= 2024L, !is.na(año))
}

data_costo_base <- prep_costo(data_costo_raw)
data_grd_base   <- prep_grd(data_grd_raw)

# ── Opciones para selectores UI ───────────────────────────────────────────────
year_choices <- sort(unique(na.omit(data_costo_base$año)), decreasing = TRUE)
caci_choices <- levels(data_costo_base$caci)   # orden jerárquico del factor

mes_labels  <- format(as.Date(paste0("2026-", 1:12, "-01")), "%B")
mes_choices <- c("Todos los meses" = "0", setNames(as.character(1:12), mes_labels))

# ── Clasificación por Unidad de Negocio ───────────────────────────────────────
classify_une <- function(dept, dept2 = NA_character_) {
  case_when(
    str_detect(coalesce(dept, ""), "HOSPITALIZACION|HOSPITALIZACIÓN|UCI|UCIN") ~ "Estancia",
    str_detect(coalesce(dept, ""), "ANGIOGRAFI|HEMODINAM")                     ~ "Hemodinamia",
    str_detect(coalesce(dept, ""), "CIRUGIA|CIRUGÍA")                          ~ "Cirugía",
    str_detect(coalesce(dept, ""), "URGENCIAS")                                ~ "Urgencias",
    str_detect(coalesce(dept, ""), "CONSULTA")                                 ~ "Consulta externa",
    str_detect(coalesce(dept, ""), "RESONANCIA|ECOGRAFIA|ECOGRAFÍA|ESCANOGR|RAYOS") ~ "Imágenes",
    str_detect(coalesce(dept, ""), "LABORATORIO")                              ~ "Laboratorio",
    coalesce(dept2, "") == "FARMACIA"                                          ~ "Medicamentos",
    TRUE                                                                       ~ "Otro"
  )
}

# ── Tema bslib DIME ───────────────────────────────────────────────────────────
dime_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#2C3E50",
  secondary  = "#4E79A7",
  success    = "#59A14F",
  warning    = "#F28E2B",
  danger     = "#E15759",
  info       = "#76B7B2"
)

message("[GRD-App] global.R listo. Años disponibles: ",
        paste(sort(year_choices), collapse = ", "),
        ". CACIs: ", paste(caci_choices, collapse = ", "))
