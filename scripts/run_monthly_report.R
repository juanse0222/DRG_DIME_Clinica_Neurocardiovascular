################################################################################
# RUN_MONTHLY_REPORT.R — Monthly GRD Pipeline Entry Point
# DIME Clínica Neurocardiovascular
#
# Usage (from terminal):
#   Rscript scripts/run_monthly_report.R           # default: current month − 1
#   Rscript scripts/run_monthly_report.R 2026-04   # specific YYYY-MM
#
# Usage (from RStudio):
#   source(here::here("scripts", "run_monthly_report.R"))
#   # optionally set: report_month <- "2026-04"
#
# What it does:
#   1. Validates / installs required packages
#   2. Sources the CACI classifier
#   3. Runs the full analysis pipeline (analysis_update_2026.R)
#   4. Renders the Quarto report to docs/index.html
#   5. Writes a run log to output/log_<YYYY-MM>.txt
################################################################################

# ── 0. Logging setup ──────────────────────────────────────────────────────────
library(here)

run_start   <- Sys.time()
current_year <- as.integer(format(run_start, "%Y"))

# [ADDED]: Accept YYYY-MM argument from command line or pre-set variable
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  report_month <- args[1]
} else if (!exists("report_month")) {
  # Default: previous calendar month
  prev <- seq(as.Date(format(run_start, "%Y-%m-01")), length = 2, by = "-1 month")[2]
  report_month <- format(prev, "%Y-%m")
}

report_year  <- as.integer(substr(report_month, 1, 4))
report_mon   <- as.integer(substr(report_month, 6, 7))
report_label <- format(as.Date(paste0(report_month, "-01")), "%B %Y")

dir.create(here("output"), showWarnings = FALSE)
dir.create(here("docs"),   showWarnings = FALSE)

log_path <- here("output", paste0("log_", report_month, ".txt"))
log_con  <- file(log_path, open = "wt")

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  message(msg)
  writeLines(msg, log_con)
}

log_msg("========================================")
log_msg("GRD MONTHLY REPORT — DIME")
log_msg(paste("Reporting period:", report_label))
log_msg(paste("Working directory:", here()))
log_msg("========================================")

# ── 1. Package check ──────────────────────────────────────────────────────────
log_msg("Checking required packages...")

required_pkgs <- c(
  "tidyverse", "janitor", "lubridate", "flextable", "gtsummary", "rio",
  "hms", "epikit", "scales", "gt", "zoo", "readxl", "here", "officer",
  "reactable", "plotly", "DT", "quarto", "pacman"
)

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  log_msg(paste("Installing missing packages:", paste(missing_pkgs, collapse = ", ")))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}
log_msg("All packages available.")

# ── 2. Source CACI classifier ─────────────────────────────────────────────────
log_msg("Sourcing CACI classifier...")
source(here("scripts", "function_diagnosis_algorithm_caci.R"))
log_msg("Classifier loaded.")

# ── 3. Run analysis pipeline ──────────────────────────────────────────────────
log_msg("Running analysis pipeline...")

tryCatch({
  source(here("scripts", "pipeline_grd.R"))
  log_msg(sprintf("Pipeline complete. Rows in cost dataset: %d", nrow(data_costo_total_3)))
  log_msg(sprintf("Rows in GRD dataset: %d", nrow(data_grd_5)))
  log_msg(paste("CACI distribution:", paste(
    names(table(data_grd_5$caci_3)),
    table(data_grd_5$caci_3),
    sep = "=", collapse = " | "
  )))
}, error = function(e) {
  log_msg(paste("PIPELINE ERROR:", conditionMessage(e)))
  close(log_con)
  stop(e)
})

# ── 4. Render Quarto report ───────────────────────────────────────────────────
log_msg("Rendering Quarto report...")

qmd_path  <- here("index.qmd")
html_out  <- here("docs", "index.html")

if (!file.exists(qmd_path)) {
  log_msg(paste("QMD NOT FOUND:", qmd_path))
} else {
  tryCatch({
    quarto::quarto_render(
      input         = qmd_path,
      output_format = "html",
      execute_params = list(
        report_year  = report_year,
        report_month = report_mon
      ),
      quiet = FALSE
    )

    log_msg(paste("Report rendered:", html_out))
  }, error = function(e) {
    log_msg(paste("RENDER ERROR:", conditionMessage(e)))
  })
}

# ── 5. Wrap up ────────────────────────────────────────────────────────────────
run_end <- Sys.time()
log_msg(paste("Total elapsed:", round(difftime(run_end, run_start, units = "mins"), 1), "minutes"))
log_msg("========================================")

close(log_con)
message(paste("[DONE] Log saved to:", log_path))
if (file.exists(html_out)) message(paste("[DONE] Report available at:", html_out))
