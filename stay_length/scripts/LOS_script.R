pacman::p_load(tidyverse, lubridate, janitor, epikit, rio, here, flextable, zoo)


#  data <- rbind(data, data_2, data_3, data_4, data_5, data_6, data_7) %>%
#  filter(cuenta != "" & !cama == "otro" & !is.na(fecha_ingreso_movimiento_cama))
#  export(data, here("data", "data_cense_2017_2026.rds"))

################################################################################
# CENSUS ACCUMULATOR — idempotent monthly append
#
# The master file  data/data_cense_2017_2026.rds  is an accumulator: each month
# one manually-exported CSV is appended to it. The previous version of this
# block used rbind() + unconditional export(), which meant a second run silently
# duplicated a whole month into the master (dedup only happened downstream, into
# data_2, AFTER the file had already been written).
#
# This version is safe to re-run:
#   - month guard   : refuses to re-append a month already established in master
#   - dedup on write: canonical key applied BEFORE export, not after
#   - backup        : timestamped copy of the master kept before every write
#   - type-safe     : new export coerced to the master's column types (the master
#                     mixes character / POSIXct / integer, so bind_rows() alone
#                     would abort on fecha_visto_ok and edad)
#
# Month key = year-month of fecha_egreso_movimiento_cama (only 11 NAs in 116k
# rows, so it is a reliable partition key).
#
# NOTE ON MONTH BOUNDARIES: an export for month M also carries a decaying tail of
# movements that closed in early M+1 (beds still open at snapshot time). A month
# is therefore treated as "established" only once it holds >= MIN_ESTABLISHED_ROWS
# rows; anything below that is a partial tail and stays open for backfill. This
# is what keeps May-2026 (62 tail rows, never exported) backfillable while
# June-2026 (970 rows) is correctly blocked.
################################################################################

CENSE_MASTER          <- here("data", "data_cense_2017_2026.rds")
CENSE_BACKUP_DIR      <- here("data", "_cense_backups")
MIN_ESTABLISHED_ROWS  <- 200L      # below this a month counts as a partial tail

# Canonical identity of a bed movement — matches the distinct() used downstream
# (line ~17 below) and in shiny_los/global.R.
CENSE_KEY <- c("cuenta", "id", "paciente",
               "fecha_ingreso_movimiento_cama", "fecha_egreso_movimiento_cama")

# ── Month key ─────────────────────────────────────────────────────────────────
cense_month <- function(x) {
  # fecha_egreso_movimiento_cama is "dd/mm/yy HH:MM" (2-digit year)
  floor_date(as.Date(dmy_hm(x, quiet = TRUE)), "month")
}

cense_month_counts <- function(df) {
  tibble(m = cense_month(df$fecha_egreso_movimiento_cama)) %>%
    filter(!is.na(m)) %>%
    count(m, name = "rows")
}

# ── Type harmonisation ────────────────────────────────────────────────────────
# Coerce `new` column-by-column to the class the master already uses, so the
# master's schema never drifts because one CSV parsed a column differently.
harmonize_to_master <- function(new, master) {
  shared <- intersect(names(new), names(master))
  for (cc in shared) {
    target <- class(master[[cc]])[1]
    v      <- new[[cc]]
    new[[cc]] <- switch(
      target,
      # exports mix dd/mm/yy HH:MM, ISO "2026-05-09 10:02:" and Excel serials
      "POSIXct"   = if (inherits(v, "POSIXct")) v else {
                      cv <- as.character(v)
                      suppressWarnings(coalesce(
                        dmy_hm(cv, quiet = TRUE),
                        ymd_hms(cv, quiet = TRUE, truncated = 3),
                        as.POSIXct(dmy(cv, quiet = TRUE)),
                        as.POSIXct(ymd(cv, quiet = TRUE))
                      ))
                    },
      "Date"      = if (inherits(v, "Date")) v else suppressWarnings(dmy(as.character(v), quiet = TRUE)),
      "integer"   = suppressWarnings(as.integer(as.numeric(as.character(v)))),
      "numeric"   = suppressWarnings(as.numeric(as.character(v))),
      as.character(v)
    )
  }
  new
}

# ── Readers for the raw monthly export ────────────────────────────────────────
# Two layouts exist in the wild:
#   (A) "flat"   — the old CSV: one header row, dd/mm/yy strings, junk cols v27-v29
#   (B) "censo modificado" — the .xls pulled from the system: 6 preamble rows, a
#       "CENSO HOSPITALARIO" title, header rows REPEATED mid-file per station
#       block, a trailing EPS summary block, Excel serial dates, and a merged
#       header cell that pushes "Fecha Ingreso Movimiento Cama" into column 29.
# read_census_export() detects which one it is and always returns layout (A)'s
# schema, so append_census_month() does not care where the file came from.

excel_serial_to_dt <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  n <- suppressWarnings(as.numeric(as.character(x)))
  as.POSIXct(n * 86400, origin = "1899-12-30", tz = "UTC")
}

is_censo_modificado <- function(path) {
  peek <- try(suppressMessages(rio::import(path, col_names = FALSE)), silent = TRUE)
  if (inherits(peek, "try-error") || !ncol(peek)) return(FALSE)
  head_vals <- str_to_upper(as.character(peek[[1]][seq_len(min(6, nrow(peek)))]))
  any(str_detect(coalesce(head_vals, ""), "CENSO HOSPITALARIO"))
}

read_censo_modificado <- function(path) {
  raw <- suppressMessages(rio::import(path, skip = 6)) %>% clean_names()

  if (!"x29" %in% names(raw))
    warning("Columna x29 ausente: el layout del export cambió, revisa el mapeo ",
            "de 'Fecha Ingreso Movimiento Cama'.", call. = FALSE)

  raw %>%
    # merged header cell: the real ingreso-movimiento value lands one column right
    mutate(fecha_ingreso_movimiento_cama = .data[["x29"]]) %>%
    select(-any_of(c("x4", "x5", "x7", "x9", "x29"))) %>%
    # strip repeated in-file header rows and the trailing EPS summary block:
    # a real record always has a numeric cuenta and a station.
    filter(
      !is.na(cuenta), !is.na(estacion),
      str_detect(as.character(cuenta), "[0-9]"),
      str_to_upper(as.character(cuenta))   != "CUENTA",
      str_to_upper(as.character(estacion)) != "ESTACION"
    ) %>%
    # re-emit dates in the master's own string formats
    mutate(
      fecha_ingreso = format(excel_serial_to_dt(fecha_ingreso), "%d/%m/%Y"),
      fecha_ingreso_movimiento_cama =
        format(excel_serial_to_dt(fecha_ingreso_movimiento_cama), "%d/%m/%y %H:%M"),
      fecha_egreso_movimiento_cama =
        format(excel_serial_to_dt(fecha_egreso_movimiento_cama), "%d/%m/%y %H:%M"),
      across(where(is.character), ~ na_if(.x, "NA"))
    )
}

read_census_export <- function(path) {
  if (!file.exists(path)) {
    stop("Export no encontrado: ", path,
         "\n  -> Descarga el censo del mes desde el sistema y pasa la ruta completa.",
         call. = FALSE)
  }
  if (is_censo_modificado(path)) {
    message("  · Layout detectado: censo modificado (.xls del sistema)")
    read_censo_modificado(path)
  } else {
    message("  · Layout detectado: censo plano (CSV)")
    rio::import(path) %>%
      clean_names() %>%
      # any_of(): the trailing empty columns are not present in every export
      select(-any_of(c("v27", "v28", "v29")))
  }
}

# ── Guarded appender ──────────────────────────────────────────────────────────
# force_months              : "YYYY-MM" values to append even if established
#                             (to backfill a month that is only a partial tail).
# dry_run                   : report what would change, write nothing.
# clean_existing_duplicates : also purge the ~26k legacy duplicate rows the
#                             master accumulated from earlier unguarded runs.
#                             OFF by default — a monthly append must not rewrite
#                             history as a side effect. Run it once, deliberately.
append_census_month <- function(path,
                                force_months              = character(),
                                dry_run                   = FALSE,
                                clean_existing_duplicates = FALSE,
                                rolling                   = FALSE) {

  master <- rio::import(CENSE_MASTER) %>% clean_names()
  new    <- read_census_export(path)

  # rolling = TRUE: the "censo modificado" export is a rolling ~3-month window,
  # so it re-ships months already consolidated. Blocking them would reject the
  # file outright and lose the late-closing movements it carries. Safe to force
  # because the append is an anti-join — only genuinely new rows are written.
  if (isTRUE(rolling)) {
    force_months <- union(
      force_months,
      format(cense_month_counts(new)$m, "%Y-%m")
    )
  }

  missing_cols <- setdiff(names(master), names(new))
  extra_cols   <- setdiff(names(new), names(master))
  if (length(missing_cols))
    warning("Columnas ausentes en el export (se rellenan con NA): ",
            paste(missing_cols, collapse = ", "), call. = FALSE)
  if (length(extra_cols))
    warning("Columnas nuevas ignoradas: ", paste(extra_cols, collapse = ", "),
            call. = FALSE)

  new <- harmonize_to_master(new, master)

  # Movements with no egreso date = bed still occupied at snapshot time. They
  # carry no completed stay, cannot be assigned to a month, and would be re-sent
  # (closed) by a later export. They are intentionally NOT appended; downstream
  # already drops them via filter(!is.na(year)).
  n_open <- sum(is.na(cense_month(new$fecha_egreso_movimiento_cama)))
  if (n_open > 0)
    cat(sprintf("  · %s movimiento(s) con cama aún ocupada — se omiten hasta que cierren\n",
                format(n_open, big.mark = ",")))

  have <- cense_month_counts(master)
  want <- cense_month_counts(new)

  status <- want %>%
    rename(rows_new = rows) %>%
    left_join(have %>% rename(rows_master = rows), by = "m") %>%
    mutate(
      rows_master = coalesce(rows_master, 0L),
      key         = format(m, "%Y-%m"),
      established = rows_master >= MIN_ESTABLISHED_ROWS,
      forced      = key %in% force_months,
      action      = case_when(
        established & !forced ~ "BLOQUEADO (ya consolidado)",
        established &  forced ~ "FORZADO (re-append)",
        rows_master > 0       ~ "BACKFILL (cola parcial)",
        TRUE                  ~ "NUEVO"
      )
    )

  cat("\n── Censo: ", basename(path), " ──────────────────────────────\n", sep = "")
  print(as.data.frame(status[, c("key", "rows_new", "rows_master", "action")]))

  keep_months <- status$m[status$action != "BLOQUEADO (ya consolidado)"]
  if (!length(keep_months)) {
    cat("\n[GUARD] Todos los meses del export ya están consolidados. Sin cambios.\n")
    cat("        Para re-procesar: append_census_month(path, force_months = \"YYYY-MM\")\n")
    return(invisible(master))
  }

  new_keep <- new[cense_month(new$fecha_egreso_movimiento_cama) %in% keep_months, , drop = FALSE]

  before <- nrow(master)

  # Dedup BEFORE writing — this is what makes a re-run harmless. It is applied
  # as an anti-join against the master (incoming rows only), NOT as distinct()
  # over the union: the master carries ~26k legacy duplicate rows from earlier
  # unguarded runs, and a routine monthly append must never silently rewrite
  # that history. Use clean_existing_duplicates = TRUE to purge them on purpose.
  new_keep <- new_keep %>%
    distinct(across(all_of(CENSE_KEY)), .keep_all = TRUE) %>%
    anti_join(master %>% distinct(across(all_of(CENSE_KEY))), by = CENSE_KEY)

  added    <- nrow(new_keep)
  combined <- bind_rows(master, new_keep)

  if (clean_existing_duplicates) {
    n_pre    <- nrow(combined)
    combined <- distinct(combined, across(all_of(CENSE_KEY)), .keep_all = TRUE)
    cat(sprintf("[CLEAN] Duplicados históricos eliminados: %s\n",
                format(n_pre - nrow(combined), big.mark = ",")))
  }

  cat(sprintf("\nMaster: %s -> %s filas | movimientos nuevos: %s\n",
              format(before, big.mark = ","),
              format(nrow(combined), big.mark = ","),
              format(added, big.mark = ",")))

  if (added == 0 && !clean_existing_duplicates) {
    cat("[GUARD] El export no aporta movimientos nuevos. Sin escritura.\n")
    return(invisible(master))
  }
  if (dry_run) {
    cat("[DRY-RUN] Nada escrito.\n")
    return(invisible(combined))
  }

  dir.create(CENSE_BACKUP_DIR, showWarnings = FALSE, recursive = TRUE)
  backup <- file.path(CENSE_BACKUP_DIR,
                      sprintf("data_cense_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.copy(CENSE_MASTER, backup, overwrite = FALSE)
  cat("Backup: ", backup, "\n", sep = "")

  rio::export(combined, CENSE_MASTER)
  cat("[OK] Master actualizado.\n")
  cat("     Recuerda borrar las cachés LOS:\n")
  cat("       data/data_los_processed.rds  y  shiny_los/data/data_los_processed.rds\n")

  invisible(combined)
}

# ── Monthly run ───────────────────────────────────────────────────────────────
# Safe to execute repeatedly: the guard blocks consolidated months and the dedup
# absorbs any overlap. Point it at the export you just downloaded.
#
#   append_census_month("~/Downloads/censo_julio_2026.csv")
#   append_census_month("~/Downloads/censo_mayo_2026.csv")            # backfill
#   append_census_month("~/Downloads/censo_junio_2026.csv",
#                       force_months = "2026-06")                     # re-append
#   append_census_month("~/Downloads/censo_julio_2026.csv", dry_run = TRUE)

data <- rio::import(CENSE_MASTER) %>% clean_names()

data_2 <- data %>% 
  distinct(cuenta, id, paciente, fecha_ingreso_movimiento_cama, 
           fecha_egreso_movimiento_cama, .keep_all = T)

data_3 <- data_2 %>% 
  mutate(fecha_ingreso = dmy(fecha_ingreso),
         fecha_visto_ok = dmy(fecha_visto_ok),
         across(c(fecha_ingreso_movimiento_cama, fecha_egreso_movimiento_cama), dmy_hm),
         month = month(fecha_egreso_movimiento_cama),
         year = year(fecha_egreso_movimiento_cama),
         month_year = as.yearmon(fecha_egreso_movimiento_cama), 
         dif_days = round(as.numeric(fecha_egreso_movimiento_cama - fecha_ingreso_movimiento_cama)/86400, digits = 2)) %>%
  filter(!is.na(year) & !str_detect(paciente, "TIC"))

data_3 %>% 
  group_by(estacion) %>% 
  summarise(median_time = median(dif_days, na.rm = T),
            iql = quantile(dif_days, 0.25, na.rm = T),
            iqs = quantile(dif_days, 0.75, na.rm = T),
            riq = paste0(max(dif_days, na.rm = T),"-", min(dif_days, na.rm = T))) %>% 
  ungroup()


data_3 %>% 
  group_by(estacion, year) %>% 
  summarise(median_time = median(dif_days, na.rm = T), .groups = "drop") %>% 
  pivot_wider(id_cols = estacion, names_from = year, values_from = median_time) %>% 
  rowwise() %>% 
  mutate(media = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

data_4 <- data_3 %>% 
  group_by(cuenta, estacion) %>% 
  arrange(id, estacion, fecha_egreso_movimiento_cama) %>% 
  mutate(num_bed_serv = dense_rank(fecha_egreso_movimiento_cama),
        last_bed_date = lag(fecha_egreso_movimiento_cama),
        max_bed_serv = max(num_bed_serv),
        max_bed_date_serv = max(fecha_egreso_movimiento_cama),
        min_bed_date_serv = min(fecha_egreso_movimiento_cama),
        dif_bed_serv = round(as.numeric(max_bed_date_serv - min_bed_date_serv), 2),
        dif_bed_serv = if_else(dif_bed_serv==0, as.numeric(dif_days), dif_bed_serv),
        estacion_2 = case_when(estacion == "UCI" ~ "UCI",
                             estacion == "UCIN"~ "UCIN 2°", 
                             estacion == "UCIN 5 PISO" ~ "UCIN 5°",
                             estacion == "UCIN ANGIO" ~ "UCIN ANGIO",
                             estacion == "URGENCIAS_OBSERVACION" ~ "URGENCIAS OBS",
                             estacion == "HOSPITALIZACION 3 PISO" ~ "PISO HOSP",
                             TRUE ~ estacion,
                             )) %>% 
  filter(num_bed_serv == max_bed_serv) %>% 
  ungroup() %>% 
  distinct(id, hab, fecha_egreso_movimiento_cama, fecha_ingreso, fecha_ingreso_movimiento_cama, .keep_all = T) 

data_5 <- data_4 %>% 
  group_by(estacion_2, month_year, year) %>% 
  summarise(median_time = mean(dif_bed_serv, na.rm = T), .groups = "drop")

data_5 %>% 
  pivot_wider(id_cols = estacion_2, names_from = year, values_from = median_time) %>% 
  rowwise() %>% 
  mutate(media = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()


ggplot(data = data_5 %>% 
         filter(!str_detect(estacion_2, "UCIN ANGIO|RESPIRA|RECUPERAC")), 
       aes(x = estacion_2, y = median_time, fill = estacion_2)) +
  geom_boxplot() +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "skyblue") +
  facet_wrap(~year) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "none"
  ) +
  scale_y_continuous(n.breaks = 10)


################################################################################
# CACI CLASSIFIER  —  function_diagnosis_algorithm_caci.R
#
# Classifies each patient admission into one primary CACI using a strict
# two-tier priority:
#   1) Field priority:  Egreso Principal (EP) > Egreso Secundario (ES) > Ingreso Principal (IP)
#      Ingreso Secundario is EXCLUDED to prevent admission-bias noise.
#   2) Disease hierarchy: ICC > ACV > SCA > TEP > TXC
#
# Business rules embedded:
#   - "Angina de Pecho" excluded from SCA unless "Inestable" also present
#   - GI haemorrhages (digestiva/gastrointestinal) excluded from ACV
#   - TEP requires EP, or a cross-principal match (EP+IP or ES+IP)
#
# Output adds these columns to the input data frame:
#   caci_final        — primary assignment (lowercase: icc/acv/sca/tep/txc/cardio_other/NA)
#   matched_groups    — all CACI groups detected across evaluated fields
#   reason            — human-readable audit trail per row
#   *_matched_fields  — which fields matched per group (semicolon-separated)
#   icd_any           — first ICD-10 code extracted from any evaluated field
#
# [CHANGED]: Consolidated best logic from both script versions (analysis_update_2026.R
#            inline + original function_diagnosis_algorithm_caci.R). Fixed ACV regex
#            (missing "|" between ICD block and text terms). Fixed TEP ip_caci bug
#            (was mapped to "SCA" instead of "TEP"). Standardised all CACI output to
#            lowercase for consistency with downstream pipeline.
################################################################################

library(dplyr)
library(stringr)
library(janitor)

# ── 1. Safe NA-coalesce operator ─────────────────────────────────────────────
`%||%` <- function(x, y) ifelse(is.na(x), y, x)


# ── 2. Regex pattern constructors ────────────────────────────────────────────

make_caci_patterns <- function() {
  list(
    icc = paste0(
      "(INSUFICIENCIA CARDIACA|FALLA CARDIACA|INSUFICIENCIA CARDIACA CONGESTIVA|",
      "FALLA VENTRICULAR|CARDIOMIOPATIA DILATADA|EDEMA PULMONAR CARDIOGENO|",
      "EDEMA AGUDO DE PULMON|INSUFICIENCIA VALVULAR|",
      "\\b(I50|I110|I130|I132))"
    ),
    
    sca = paste0(
      "(INFARTO DE MIOCARDIO|INFARTO AGUDO|INFARTO TRANSMURAL|INFARTO SUBENDOCARDICO|",
      "ANGINA INESTABLE|ANGINA DE PECHO|CORONARIA|CORONARIOPATIA|CARDIOPATIA ISQUEMICA|",
      "ENFERMEDAD ATEROSCLEROTICA DEL CORAZON|",
      "\\b(I20|I21|I22|I23|I24|I25|I251|I255|I429|I250|I252|I256))"
    ),
    
    # [CHANGED]: Added missing "|" between ICD block and free-text terms —
    #            original paste0() version produced a broken regex that never
    #            matched ACCIDENTE VASCULAR, ACV, etc.
    acv = paste0(
      "(HEMORRAGIA CEREBRAL|HEMORRAGIA INTRACEREBRAL|HEMORRAGIA SUBARACNOIDEA|",
      "HEMORRAGIA INTRACRANEAL|",
      "\\b(I60|I61|I62|I620|I670|I61X|I619)|",    # <- "|" was missing here
      "ACCIDENTE VASCULAR|\\bACV\\b|INFARTO CEREBRAL|",
      "ENFERMEDAD CEREBROVASCULAR|CEREBROVASCULAR|",
      "\\b(I63|I64|I65|I66|G45|G459|G458|I638))"
    ),
    
    tx = paste0(
      "(TRASPLANTE|TRASPLANTADO|TRASPLANTADA|TRASPLANTADOS|",
      "COMPLICACIONES DE TRASPLANTE|RECHAZO DE TRASPLANTE|",
      "\\b(Z941|Z943|T862))"
    ),
    
    tep = paste0(
      "(TROMBOEMBOLISMO PULMONAR|EMBOLIA PULMONAR|EMBOLIA PULMONAR AGUDA|",
      "EMBOLISMO PULMONAR|\\b(I26))"
    )
  )
}

make_gi_hemo_pattern <- function() {
  paste0(
    "(HEMORRAGIA (DIGESTIVA|GASTROINTESTINAL)|",
    "SANGRADO (DIGESTIVO|GASTROINTESTINAL)|",
    "SANGRADO DE TUBO DIGESTIVO|SANGRADO TUBO DIGESTIVO|",
    "MELENA|HEMATEMESIS)"
  )
}


# ── 3. Main classifier ────────────────────────────────────────────────────────

classify_caci <- function(
    data,
    cols = list(
      dg_eg_pr  = "diagnostico"
    ),
    icd_extract_pattern = "\\b[A-TV-Z][0-9]{2}(?:\\.[0-9A-Za-z]+)?\\b"
) {
  
  eg_pr  <- cols$dg_eg_pr
  # ing_sc intentionally not used in classification logic
  
  patterns        <- make_caci_patterns()
  gi_hemo_pattern <- make_gi_hemo_pattern()
  
  # [ADDED]: Inline helpers to keep rowwise() blocks readable and testable
  sca_ok <- function(txt, pat) {
    !is.na(txt) &&
      str_detect(txt, regex(pat, ignore_case = TRUE)) &&
      !(str_detect(txt, regex("ANGINA DE PECHO", ignore_case = TRUE)) &
          !str_detect(txt, regex("INESTABLE", ignore_case = TRUE)))
  }
  
  acv_ok <- function(txt, pat, gi_pat) {
    !is.na(txt) &&
      str_detect(txt, regex(pat, ignore_case = TRUE)) &&
      !str_detect(txt, regex(gi_pat, ignore_case = TRUE))
  }
  
  pat_match <- function(txt, pat) {
    !is.na(txt) && str_detect(txt, regex(pat, ignore_case = TRUE))
  }
  
  df_out <- data %>%
    # Coerce diagnosis columns to character safely
    mutate(across(all_of(c(eg_pr)), ~ as.character(.x))) %>%
    mutate(
      diag_eg_pr  = str_to_upper(!!sym(eg_pr)),
    ) %>%
    
    # ── Rowwise field-matching ─────────────────────────────────────────────
    rowwise() %>%
    mutate(
      icc_fields = list(na.omit(c(
        if (pat_match(diag_eg_pr,  patterns$icc)) "egreso_principal"  else NA_character_
      ))),
      
      sca_fields = list(na.omit(c(
        if (sca_ok(diag_eg_pr,  patterns$sca)) "egreso_principal"  else NA_character_
      ))),
      
      acv_fields = list(na.omit(c(
        if (acv_ok(diag_eg_pr,  patterns$acv, gi_hemo_pattern)) "egreso_principal"  else NA_character_
      ))),
      
      tep_fields = list(na.omit(c(
        if (pat_match(diag_eg_pr,  patterns$tep)) "egreso_principal"  else NA_character_
      ))),
      
      tx_fields = list(na.omit(c(
        if (pat_match(diag_eg_pr,  patterns$tx)) "egreso_principal"  else NA_character_
      )))
    ) %>%
    ungroup() %>%
    
    # ── Collapse field lists and validate TEP ─────────────────────────────
    mutate(
      icc_matched_fields = sapply(icc_fields, paste, collapse = "; "),
      sca_matched_fields = sapply(sca_fields, paste, collapse = "; "),
      acv_matched_fields = sapply(acv_fields, paste, collapse = "; "),
      tep_matched_fields = sapply(tep_fields, paste, collapse = "; "),
      tx_matched_fields  = sapply(tx_fields,  paste, collapse = "; "),
      
      # TEP valid only when EP present, or cross-principal (EP+IP or ES+IP)
      tep_valid = lengths(tep_fields) > 0 & (
        str_detect(tep_matched_fields, "egreso_principal") 
      )
    ) %>%
    
    # ── Field-level CACI (hierarchy: ICC > ACV > SCA > TEP > TXC) ─────────
    mutate(
      ep_caci = case_when(
        str_detect(icc_matched_fields, "egreso_principal") ~ "icc",
        str_detect(acv_matched_fields, "egreso_principal") ~ "acv",
        str_detect(sca_matched_fields, "egreso_principal") ~ "sca",
        str_detect(tep_matched_fields, "egreso_principal") ~ "tep",
        str_detect(tx_matched_fields,  "egreso_principal") ~ "txc",
        TRUE ~ NA_character_
      ),
      # Field priority: EP wins, then ES, then IP
      caci_final = case_when(
        !is.na(ep_caci)                                    ~ ep_caci,
        TRUE ~ NA_character_
      ),
      
      # [ADDED]: Fallback bucket for general cardiology not meeting strict SCA criteria
      caci_final = if_else(
        is.na(caci_final) &
          str_detect(diag_eg_pr, regex("ANGINA DE PECHO|\\bI20\\b|\\bI25\\b", ignore_case = TRUE)),
        "cardio_other",
        caci_final
      ),
      
      matched_groups = str_squish(str_c(
        ifelse(lengths(icc_fields) > 0, "ICC", NA_character_),
        ifelse(lengths(acv_fields) > 0, "ACV", NA_character_),
        ifelse(lengths(sca_fields) > 0, "SCA", NA_character_),
        ifelse(tep_valid,               "TEP", NA_character_),
        ifelse(lengths(tx_fields)  > 0, "TXC", NA_character_),
        sep = "; "
      )) %>% str_replace_all("NA; |; NA|^NA$|^;|;$", ""),
      
      reason = case_when(
        !is.na(caci_final) ~ str_c(
          "Asignado a ", str_to_upper(caci_final),
          " (jerarquía ICC>ACV>SCA>TEP>TXC | Campo: EP>ES>IP). Grupos detectados: [",
          matched_groups, "]. ",
          "ICC: ", ifelse(icc_matched_fields == "", "ninguno", icc_matched_fields), " | ",
          "ACV: ", ifelse(acv_matched_fields == "", "ninguno", acv_matched_fields), " | ",
          "SCA: ", ifelse(sca_matched_fields == "", "ninguno", sca_matched_fields), " | ",
          "TEP: ", ifelse(tep_matched_fields == "", "ninguno", tep_matched_fields), " | ",
          "TXC: ", ifelse(tx_matched_fields  == "", "ninguno", tx_matched_fields)
        ),
        TRUE ~ NA_character_
      )
    )
  
  return(df_out)
}


# ── 4. QA / debug helper ─────────────────────────────────────────────────────
# Returns rows where patterns matched in at least one field but caci_final is NA.
# [CHANGED]: Uses any_of() for safer column selection; removed dependency on cols arg.
debug_report <- function(result_df, id_col = "documento") {
  result_df %>%
    filter(
      is.na(caci_final) & (
        icc_matched_fields != "" | sca_matched_fields != "" |
          acv_matched_fields != "" | tep_matched_fields != "" |
          tx_matched_fields  != ""
      )
    ) %>%
    select(any_of(c(
      id_col, "icd_any", "caci_final", "matched_groups",
      "icc_matched_fields", "sca_matched_fields", "acv_matched_fields",
      "tep_matched_fields", "tx_matched_fields",
      "diagnostico_egreso_principal", "diagnostico_egreso_secundario",
      "diagnostico_ingreso_princial"
    ))) %>%
    arrange(.data[[id_col]]) %>%
    head(200)
}

result <- classify_caci(data_4)

result_2 <- result %>%
  mutate(caci_3 = caci_final)   # already lowercase from function


data_6 <- result_2 %>% 
  group_by(estacion_2, month_year, year, caci_3) %>% 
  summarise(median_time = mean(dif_bed_serv, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(caci_3) & !caci_3 == "txc")


ggplot(data = data_6, aes(x = estacion_2, y = median_time, fill = caci_3)) +
  geom_boxplot() +
  facet_wrap(~year) +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom"
  )
