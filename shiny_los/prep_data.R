#!/usr/bin/env Rscript
# Run from project root:  Rscript shiny_los/prep_data.R
# Produces shiny_los/data/los_cost_compact.rds  (~compact pre-aggregation)
# Deploy that file instead of the raw 25 MB cost RDS pair.

library(tidyverse)
library(janitor)
library(rio)
library(lubridate)

data_dir <- "shiny_los/data"

cost_files <- list.files(data_dir,
                          pattern = "data_costo_total_3_.*_II\\.rds",
                          full.names = TRUE)
if (!length(cost_files)) stop("No cost RDS files found in ", data_dir)
cat("Cost files found:\n", paste(" •", basename(cost_files), collapse = "\n"), "\n")

raw <- bind_rows(lapply(cost_files, function(f) {
  cat("Loading", basename(f), "...")
  df <- rio::import(f) %>%
    clean_names() %>%
    mutate(across(everything(), as.character))
  cat(" ", nrow(df), "rows\n")
  df
}))
cat("Combined:", nrow(raw), "rows x", ncol(raw), "cols\n")
cat("RAM:", round(object.size(raw) / 1e6, 0), "MB\n")

# Normalise caci / costo column names (handle caci_3 / costo_2 variants)
if ("caci_3" %in% names(raw) && "caci" %in% names(raw)) {
  raw <- mutate(raw, caci = coalesce(caci, caci_3)) %>% select(-caci_3)
} else if ("caci_3" %in% names(raw)) {
  raw <- rename(raw, caci = caci_3)
}

if ("costo_2" %in% names(raw) && "costo" %in% names(raw)) {
  raw <- mutate(raw, costo = coalesce(costo, costo_2)) %>% select(-costo_2)
} else if ("costo_2" %in% names(raw)) {
  raw <- rename(raw, costo = costo_2)
}

# Derive numeric columns
compact <- raw %>%
  mutate(
    # BUG CORREGIDO (2026-08-19): la coma en valor_cargo_tarifario es separador
    # de MILES, no decimal ("9,028" = 9.028 COP; "746,755" = 746.755 COP).
    # El gsub(",", ".") anterior lo convertía en "9.028" -> 9,028 numérico, es
    # decir DIVIDÍA todas las ventas por ~1000 y hacía que cada margen saliera
    # catastróficamente negativo (venta 9,03 frente a costo 10.113).
    # Se eliminan los separadores de miles en vez de reinterpretarlos.
    venta          = suppressWarnings(
                       as.numeric(gsub("[^0-9.-]", "", valor_cargo_tarifario))),
    costo          = suppressWarnings(as.numeric(costo)),
    fecha_cargue_d = as.Date(fecha_cargue),
    año            = as.integer(year(fecha_cargue_d)),
    mes_num        = as.integer(month(fecha_cargue_d)),
    caci           = str_to_upper(caci),
    cuenta         = as.character(cuenta),
    dif_days       = suppressWarnings(as.numeric(dif_days)),
    departamento_cargue = coalesce(departamento_cargue, "Otro")
  ) %>%
  # Pre-aggregate: one row per admission-service-dept-month
  group_by(cuenta, caci, dif_days, departamento_cargue, año, mes_num) %>%
  summarise(
    costo = sum(costo, na.rm = TRUE),
    venta = sum(venta, na.rm = TRUE),
    .groups = "drop"
  )

cat("Compact rows:", nrow(compact), "\n")
cat("RAM compact:", round(object.size(compact) / 1e6, 1), "MB\n")

out <- file.path(data_dir, "los_cost_compact.rds")
saveRDS(compact, out, compress = TRUE)
cat("Saved:", out, " |", round(file.size(out) / 1024), "KB on disk\n")

################################################################################
# VALOR DE CAMA — precálculo compacto
#
# Extrae los cargos de INTERNACIÓN (día-cama) y los resume por servicio, pagador
# y CACI. Se precalcula aquí porque los RDS crudos de costo (~40 MB) NO se
# despliegan; la app sólo recibe este resumen.
#
# Tres bases distintas conviven y NO deben mezclarse:
#   costo    = costo interno DIME (columna `costo` del dataset de costos)
#   tarifa   = valor_cargo_tarifario, lo efectivamente FACTURADO al pagador
#   SOAT     = referente normativo externo (constante en inactive_stay_cost.R)
################################################################################
bed_pat <- regex("^INTERNACI", ignore_case = TRUE)

bed_raw <- raw %>%
  filter(str_detect(coalesce(cargo, ""), bed_pat)) %>%
  mutate(
    tarifa = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", valor_cargo_tarifario))),
    costo  = suppressWarnings(as.numeric(costo)),
    caci   = str_to_upper(caci),
    fecha_cargue_d = as.Date(fecha_cargue),
    año    = as.integer(year(fecha_cargue_d)),
    # Agrupación clínica de la cama. UCIN aquí = cuidado INTERMEDIO (así se
    # factura), no la unidad neonatal.
    servicio_cama = case_when(
      str_detect(cargo, regex("INTENSIVO",  ignore_case = TRUE)) ~ "UCI",
      str_detect(cargo, regex("INTERMEDIO", ignore_case = TRUE)) ~ "UCIN (intermedio)",
      TRUE                                                       ~ "Hospitalización"
    ),
    eapb = str_squish(coalesce(nombre_cliente, "SIN DATO"))
  ) %>%
  filter(!is.na(tarifa), tarifa > 0)

q <- function(x, p) unname(quantile(x, p, na.rm = TRUE))

bed_value <- list(
  # Por servicio (global)
  por_servicio = bed_raw %>%
    group_by(servicio_cama) %>%
    summarise(n = n(),
              tarifa_med = median(tarifa), tarifa_p25 = q(tarifa, .25),
              tarifa_p75 = q(tarifa, .75),
              costo_med  = median(costo, na.rm = TRUE), .groups = "drop"),

  # Por servicio × pagador — aquí está la variación que importa negociar
  por_eapb = bed_raw %>%
    group_by(servicio_cama, eapb) %>%
    summarise(n = n(), tarifa_med = median(tarifa),
              costo_med = median(costo, na.rm = TRUE), .groups = "drop") %>%
    filter(n >= 20),

  # Por patología: qué CACI consume la cama más cara
  por_caci = bed_raw %>%
    filter(!is.na(caci)) %>%
    group_by(servicio_cama, caci) %>%
    summarise(n = n(), tarifa_med = median(tarifa),
              costo_med = median(costo, na.rm = TRUE), .groups = "drop") %>%
    filter(n >= 20),

  por_anio = bed_raw %>%
    group_by(año, servicio_cama) %>%
    summarise(n = n(), tarifa_med = median(tarifa), .groups = "drop")
)

out_bed <- file.path(data_dir, "bed_value.rds")
saveRDS(bed_value, out_bed, compress = TRUE)
cat("Saved:", out_bed, " |", round(file.size(out_bed) / 1024), "KB |",
    "filas cama:", nrow(bed_raw), "\n")
