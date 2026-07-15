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
