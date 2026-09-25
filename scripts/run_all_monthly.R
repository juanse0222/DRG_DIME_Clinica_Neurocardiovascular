################################################################################
# run_all_monthly.R — Master monthly activation script
# DIME Clínica Neurocardiovascular
#
# Usage (from terminal in project root):
#   Rscript scripts/run_all_monthly.R           # auto-detects previous month
#   Rscript scripts/run_all_monthly.R 2026-08   # specific month
#
# What it does (in order):
#   1. GRD pipeline   — admissions + sales + costs → RDS outputs
#   2. GRD Shiny      — copies files, rebuilds compact tables, deploys
#   3. Mortality      — appends deaths, refits GEE models, deploys
#
# Prerequisites — place these files before running:
#   data/admission_data/admission_<month>_<year>.xls
#   data/sales data/sales_<Mon>_<year>.csv          (e.g. sales_Aug_2026.csv)
#   ~/Desktop/DIME/.../DIME_mortality_2026/<Month>_<year>/DIME_mortality_<Month>_<year>.xlsx
################################################################################

suppressPackageStartupMessages({
  library(here); library(lubridate); library(rio); library(tidyverse); library(janitor)
})

# ── Reporting period ──────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  report_month <- args[1]
} else if (!exists("report_month")) {
  prev <- seq(as.Date(format(Sys.time(), "%Y-%m-01")), length = 2, by = "-1 month")[2]
  report_month <- format(prev, "%Y-%m")
}
report_label <- format(as.Date(paste0(report_month, "-01")), "%B %Y")
cat(sprintf("\n========================================\n"))
cat(sprintf("MONTHLY ACTIVATION — DIME\n"))
cat(sprintf("Period: %s\n", report_label))
cat(sprintf("========================================\n\n"))

project_root <- here::here()

################################################################################
# BLOCK 1 — GRD PIPELINE
################################################################################
cat("[BLOCK 1] Running GRD pipeline...\n")
tryCatch({
  source(here("scripts", "run_monthly_report.R"))
  cat("[BLOCK 1] GRD pipeline complete.\n\n")
}, error = function(e) {
  stop("[BLOCK 1] GRD pipeline FAILED: ", conditionMessage(e))
})

################################################################################
# BLOCK 2 — GRD SHINY: copy files + rebuild compact tables + deploy
################################################################################
cat("[BLOCK 2] Updating GRD Shiny dashboard...\n")

current_year <- year(Sys.Date())
cost_src <- here("data", paste0("data_costo_total_3_", current_year, "_II.rds"))
grd_src  <- here("data", paste0("data_grd_2_", current_year, "_II.rda"))
shiny_data <- here("shiny_grd", "data")

if (!file.exists(cost_src)) stop("[BLOCK 2] Cost RDS not found: ", cost_src)
if (!file.exists(grd_src))  stop("[BLOCK 2] GRD RDA not found: ", grd_src)

file.copy(cost_src, file.path(shiny_data, basename(cost_src)), overwrite = TRUE)
file.copy(grd_src,  file.path(shiny_data, basename(grd_src)),  overwrite = TRUE)
cat("[BLOCK 2] Files copied to shiny_grd/data/\n")

source(here("shiny_grd", "prep_data.R"))
cat("[BLOCK 2] Compact tables rebuilt.\n")

cat("[BLOCK 2] Deploying grd-dime...\n")
rsconnect::deployApp("shiny_grd", appName = "grd-dime", forceUpdate = TRUE)
cat("[BLOCK 2] GRD Shiny deployed.\n\n")

################################################################################
# BLOCK 3 — MORTALITY: append deaths + refit GEE models + deploy
################################################################################
cat("[BLOCK 3] Updating mortality dashboard...\n")

prev_month_date <- as.Date(paste0(report_month, "-01")) - days(1)
month_en  <- format(as.Date(paste0(report_month, "-01")), "%B")   # e.g. "August"
month_yr  <- year(prev_month_date)

mort_xlsx <- file.path(
  "~/Desktop/DIME/Documentos EDI/2. Mortalidad/DIME_mortality_2026",
  paste0(month_en, "_", month_yr),
  paste0("DIME_mortality_", month_en, "_", month_yr, ".xlsx")
)
registry  <- "~/Desktop/DIME/Documentos EDI/2. Mortalidad/DIME_mortality_2025/data/mortality_dime_2017_2026.rds"
mort_analysis_dir <- "~/Desktop/DIME/Documentos EDI/2. Mortalidad/mortality_analysis"

if (!file.exists(path.expand(mort_xlsx))) {
  cat(sprintf("[BLOCK 3] WARNING: Mortality XLSX not found:\n  %s\n", mort_xlsx))
  cat("[BLOCK 3] Skipping mortality update — run manually when file is available.\n\n")
} else {
  # Append new month deaths to historical registry
  data_new <- import(path.expand(mort_xlsx)) %>% clean_names()
  data_mh  <- import(path.expand(registry))

  for (col in intersect(names(data_new), names(data_mh))) {
    if (is.character(data_mh[[col]]) && !is.character(data_new[[col]]))
      data_new[[col]] <- as.character(data_new[[col]])
  }
  data_mh_updated <- bind_rows(data_mh, data_new) %>%
    distinct(numero_certificado, fecha_defuncion, hora_defuncion, .keep_all = TRUE)

  export(data_mh_updated, path.expand(registry))
  cat(sprintf("[BLOCK 3] Registry updated: %d → %d rows (max: %s)\n",
              nrow(data_mh), nrow(data_mh_updated),
              format(max(as.Date(data_mh_updated$fecha_defuncion), na.rm = TRUE))))

  # Reclassify discharges and rebuild results_2.rds
  old_wd <- getwd(); setwd(path.expand(mort_analysis_dir))
  source("script/update_monthly.R")
  setwd(old_wd)
  cat("[BLOCK 3] results_2.rds updated.\n")

  # Refit GEE models
  source(here("shiny_mortalidad", "prep_mortality.R"))
  cat("[BLOCK 3] GEE models refit.\n")

  # Deploy
  cat("[BLOCK 3] Deploying mortalidad-dime...\n")
  rsconnect::deployApp("shiny_mortalidad", appName = "mortalidad-dime", forceUpdate = TRUE)
  cat("[BLOCK 3] Mortality Shiny deployed.\n\n")
}

################################################################################
# DONE
################################################################################
cat("========================================\n")
cat(sprintf("ALL DONE — %s\n", report_label))
cat("GRD:       https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/grd-dime/\n")
cat("Mortality: https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/mortalidad-dime/\n")
cat("LOS:       Run separately when census data arrives (see notes below)\n")
cat("========================================\n\n")
cat("LOS dashboard (run manually when data arrives):\n")
cat("  1. Update data/data_cense_2017_2026.rds\n")
cat("  2. Update stay_length/estancia_inactiva_costo_2026/estancia_inactiva.xlsx\n")
cat("  3. Delete data/data_los_processed.rds and shiny_los/data/data_los_processed.rds\n")
cat("  4. rsconnect::deployApp('shiny_los', appName='los-dime', forceUpdate=TRUE)\n")
