################################################################################
# global.R — GRD Dashboard · DIME Clínica Neurocardiovascular
# Cargado una sola vez al iniciar la app (compartido entre sesiones)
################################################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(scales)
library(janitor)
library(here)
library(rio)
library(reactable)
library(plotly)
library(DT)
library(leaflet)
library(sf)

tryCatch(
  Sys.setlocale("LC_TIME", "es_ES.UTF-8"),
  warning = function(w) NULL,
  error   = function(e) NULL
)

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

# Abreviaturas de meses en español y helper para convertir mes entero → factor
meses_abr <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
mes_factor <- function(m) factor(meses_abr[as.integer(m)], levels = meses_abr)

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

# ── Localizar directorio de datos ────────────────────────────────────────────
# Acepta shiny_grd/data/ (desarrollo desde raíz del proyecto) o data/ (dentro
# de shiny_grd/ al correr localmente o en shinyapps.io).
data_dir <- {
  candidates <- c(
    file.path(getwd(), "data"),
    if (requireNamespace("here", quietly = TRUE)) here::here("data") else character(0)
  )
  found <- Filter(dir.exists, candidates)
  if (length(found) == 0) stop("[GRD-App] No se encontró el directorio data/")
  found[[1]]
}

# ── Importador GRD (siempre se usa, dataset pequeño) ─────────────────────────
safe_import_grd <- function(f) {
  df <- rio::import(f) %>% janitor::clean_names()
  if ("caci_3" %in% names(df) && "caci" %in% names(df)) {
    df$caci <- dplyr::coalesce(df$caci_3, df$caci)
  } else if ("caci_3" %in% names(df)) {
    df$caci <- df$caci_3
  } else if ("caci_final" %in% names(df) && !"caci" %in% names(df)) {
    df$caci <- df$caci_final
  }
  df <- df %>% select(-any_of(c("caci_3", "caci_2", "caci_final", "mes_caci")))
  for (col in c("fecha_ingreso", "fecha_de_egreso"))
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  for (col in c("edad", "estancia_horas", "valor_factura", "total_cuenta"))
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  ordered_cols <- names(df)[sapply(df, is.ordered)]
  for (col in ordered_cols) df[[col]] <- as.character(df[[col]])
  df
}

prep_grd <- function(df) {
  df %>%
    mutate(
      fecha_ingreso   = as.Date(fecha_ingreso),
      fecha_de_egreso = as.Date(fecha_de_egreso),
      caci = recode_caci(caci),
      año  = as.integer(year(coalesce(fecha_ingreso, fecha_de_egreso)))
    ) %>%
    filter(año >= 2024L, !is.na(año))
}

grd_files     <- list.files(data_dir, pattern = "data_grd_2_.*_II\\.(rds|rda)", full.names = TRUE)
data_grd_raw  <- bind_rows(lapply(grd_files, safe_import_grd))
data_grd_base <- prep_grd(data_grd_raw)
rm(data_grd_raw)

# ── Carga de costos: pre-agregada (deploy) o completa (desarrollo local) ──────
#
# En shinyapps.io solo se despliegan los tres archivos compactos generados por
# prep_data.R (~KB). Cargar el RDS completo (~22 MB comprimido, >200 MB en RAM)
# provoca que el proceso sea eliminado con "signal: killed".
#
#   pte_base: paciente × CACI × mes  → la mayoría de las pestañas
#   une_base: CACI × mes × unidad    → Tab 8 "Por unidad"
#   dt_base : paciente × mes × dpto  → Tab 10 "Datos"

if (file.exists(file.path(data_dir, "pte_base.rds"))) {

  message("[GRD-App] Cargando datos pre-agregados (modo deploy)...")
  pte_base <- readRDS(file.path(data_dir, "pte_base.rds"))
  une_base <- readRDS(file.path(data_dir, "une_base.rds"))
  dt_base  <- readRDS(file.path(data_dir, "dt_base.rds"))

  # Restaurar factor con jerarquía clínica (se serializa como character en xz)
  for (.nm in c("pte_base", "une_base", "dt_base")) {
    .df <- get(.nm)
    if ("caci" %in% names(.df))
      .df$caci <- factor(as.character(.df$caci), levels = caci_levels)
    assign(.nm, .df)
  }
  rm(.nm, .df)

} else {

  message("[GRD-App] Archivos pre-agregados no encontrados — cargando RDS completo...")
  message("          Ejecuta shiny_grd/prep_data.R para generar los archivos compactos.")

  safe_import <- function(f) {
    df <- rio::import(f)
    for (col in c("fecha_cargue", "fecha_registro", "fecha_ingreso", "fecha_de_egreso"))
      if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
    for (col in c("edad", "año"))
      if (col %in% names(df))
        df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
    for (col in c("transaccion", "mes_cargue", "mes", "ingreso", "cod_cargo"))
      if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
    ordered_cols <- names(df)[sapply(df, is.ordered)]
    for (col in ordered_cols) df[[col]] <- as.character(df[[col]])
    df
  }

  prep_costo <- function(df) {
    df <- df %>% janitor::clean_names()
    if ("caci_3" %in% names(df) && !"caci" %in% names(df))
      df <- rename(df, caci = caci_3)
    else if ("caci_3" %in% names(df) && "caci" %in% names(df))
      df <- select(df, -caci_3)
    if ("costo_2" %in% names(df) && !"costo" %in% names(df))
      df <- rename(df, costo = costo_2)
    else if ("costo_2" %in% names(df) && "costo" %in% names(df))
      df <- select(df, -costo_2)
    df %>%
      mutate(
        fecha_cargue = as.Date(fecha_cargue),
        venta  = readr::parse_number(
          as.character(valor_cargo_tarifario),
          locale = readr::locale(grouping_mark = ",", decimal_mark = ".")
        ),
        costo      = as.numeric(costo),
        año        = as.integer(dplyr::coalesce(
                       suppressWarnings(as.numeric(ano)), year(fecha_cargue))),
        mes_cargue = as.integer(month(fecha_cargue)),
        caci       = recode_caci(caci)
      ) %>%
      filter(año >= 2024L, !is.na(año))
  }

  classify_une_local <- function(dept, dept2 = NA_character_) {
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

  cost_files      <- list.files(data_dir, pattern = "data_costo_total_3_.*_II\\.rds", full.names = TRUE)
  data_costo_raw  <- bind_rows(lapply(cost_files, safe_import))
  data_costo_base <- prep_costo(data_costo_raw)
  rm(data_costo_raw); gc()

  pte_base <- data_costo_base %>%
    filter(!is.na(caci), !is.na(identificacion)) %>%
    group_by(identificacion, caci, año, mes_cargue) %>%
    summarise(costo = sum(costo, na.rm = TRUE), venta = sum(venta, na.rm = TRUE),
              .groups = "drop")

  une_base <- data_costo_base %>%
    filter(!is.na(caci)) %>%
    mutate(Unidad = classify_une_local(
      if ("departamento_cargue"   %in% names(.)) departamento_cargue   else NA_character_,
      if ("departamento_cargue_2" %in% names(.)) departamento_cargue_2 else NA_character_
    )) %>%
    group_by(año, mes_cargue, caci, Unidad) %>%
    summarise(costo = sum(costo, na.rm = TRUE), .groups = "drop")

  dt_base <- data_costo_base %>%
    filter(!is.na(caci)) %>%
    mutate(departamento_cargue_2 =
             if ("departamento_cargue_2" %in% names(.)) departamento_cargue_2 else NA_character_) %>%
    group_by(año, mes_cargue, caci, identificacion, departamento_cargue_2) %>%
    summarise(costo = sum(costo, na.rm = TRUE), venta = sum(venta, na.rm = TRUE),
              .groups = "drop")

  rm(data_costo_base); gc()
}

# ── Añadir mes_nombre a pte_base (necesario para reactivos de ticket) ─────────
pte_base <- pte_base %>% mutate(mes_nombre = mes_factor(mes_cargue))

# ── Perfil de pacientes: todos los pacientes DIME (no solo CACI) ─────────────
# pre-agregado por shiny_grd/prep_data.R desde data/data_discharges_hosp.rds
edad_grupo_levels <- c("0-9","10-19","20-29","30-39","40-49",
                       "50-59","60-69","70-79","80+")
payer_colors <- c(
  "EAPB"                = "#4E79A7",
  "SLE - Particulares"   = "#F28E2B",
  "SLE - MP/Pólizas"     = "#B07AA1",
  "Sin dato"             = "#95A5A6"
)
sexo_colors <- c(Femenino = "#F28E2B", Masculino = "#4E79A7")

if (file.exists(file.path(data_dir, "perfil_pacientes.rds"))) {
  perfil_pacientes <- readRDS(file.path(data_dir, "perfil_pacientes.rds"))
  perfil_pacientes$edad_grupo <- factor(as.character(perfil_pacientes$edad_grupo),
                                        levels = edad_grupo_levels)
} else if (file.exists(file.path(data_dir, "data_discharges_hosp.rds"))) {
  message("[GRD-App] perfil_pacientes.rds no encontrado — ejecuta shiny_grd/prep_data.R.")
  message("          Se omite la pestaña 'Perfil de pacientes' hasta generarlo.")
  perfil_pacientes <- NULL
} else {
  perfil_pacientes <- NULL
}

perfil_year_choices <- if (!is.null(perfil_pacientes))
  sort(unique(na.omit(perfil_pacientes$año)), decreasing = TRUE) else integer(0)

# ── Mapa de pacientes geocodificados (EAPB / SLE / CACI) ─────────────────────
# pre-generado por scripts/geocode_addresses.R desde data/data_cense_2017_2026.rds
# vía un servidor Nominatim local (self-hosted). Sin cédula, nombre ni texto de
# dirección — solo coordenadas con jitter de privacidad (~55 m).
caci_map_colors <- c(caci_colors, "No CACI" = "#BDBDBD")

# DIME Clínica Neurocardiovascular — Avenida 5N #20N-75, Versalles, Cali.
DIME_LAT <- 3.4613735
DIME_LON <- -76.5289358

distancia_banda_levels <- c("< 1 km", "1-2 km", "2-5 km", "5-10 km", "> 10 km")
distancia_banda_colors <- setNames(
  c("#2ECC71", "#82E0AA", "#F4D03F", "#E67E22", "#C0392B"),
  distancia_banda_levels
)
frecuencia_levels <- c("Única visita/año", "Múltiples visitas/año", "Múltiples visitas/mes")
tipo_atencion_levels <- c("Ambulatorio", "Hospitalario")
tipo_atencion_colors <- c(Ambulatorio = "#59A14F", Hospitalario = "#E15759")

if (file.exists(file.path(data_dir, "geocoded_map_data.rds"))) {
  geocoded_map_data <- readRDS(file.path(data_dir, "geocoded_map_data.rds"))
  geocoded_map_data$caci <- factor(geocoded_map_data$caci,
                                   levels = c(caci_levels, "No CACI"))
  geocoded_map_data$distancia_banda <- factor(as.character(geocoded_map_data$distancia_banda),
                                              levels = distancia_banda_levels)
  geocoded_map_data$frecuencia <- factor(geocoded_map_data$frecuencia,
                                         levels = frecuencia_levels)
  geocoded_map_data$tipo_atencion <- factor(geocoded_map_data$tipo_atencion,
                                            levels = tipo_atencion_levels)
  map_year_choices <- sort(unique(na.omit(geocoded_map_data$año)), decreasing = TRUE)
  map_servicio_choices <- sort(unique(na.omit(geocoded_map_data$servicio)))
} else {
  message("[GRD-App] geocoded_map_data.rds no encontrado — ejecuta scripts/geocode_addresses.R.")
  message("          Se omite la pestaña 'Mapa'.")
  geocoded_map_data <- NULL
  map_year_choices <- integer(0)
  map_servicio_choices <- character(0)
}

if (file.exists(file.path(data_dir, "comunas_cali.rds"))) {
  comunas_cali <- readRDS(file.path(data_dir, "comunas_cali.rds"))
} else {
  comunas_cali <- NULL
}

if (file.exists(file.path(data_dir, "visitas_mensuales.rds"))) {
  visitas_mensuales <- readRDS(file.path(data_dir, "visitas_mensuales.rds"))
} else {
  visitas_mensuales <- NULL
}

# ── Opciones para selectores UI ───────────────────────────────────────────────
year_choices <- sort(unique(na.omit(pte_base$año)), decreasing = TRUE)

# Paleta de años: asignada dinámicamente en orden cronológico.
# Los 3 primeros colores preservan la identidad visual histórica
# (2024→rojo, 2025→naranja, 2026→azul); años futuros reciben colores adicionales.
.year_palette <- c("#C0392B", "#E67E22", "#2980B9",
                   "#27AE60", "#8E44AD", "#16A085", "#F39C12", "#2C3E50")
year_colors   <- setNames(
  .year_palette[seq_along(year_choices)],
  as.character(sort(year_choices))    # ascendente: 2024, 2025, 2026, ...
)
rm(.year_palette)

caci_choices <- caci_levels  # orden jerárquico fijo; no depende de los datos

meses_full  <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
mes_choices <- c("Todos los meses" = "0", setNames(as.character(1:12), meses_full))

message("[GRD-App] global.R listo. Años disponibles: ",
        paste(sort(year_choices), collapse = ", "),
        ". CACIs: ", paste(caci_choices, collapse = ", "))
