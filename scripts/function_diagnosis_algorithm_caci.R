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
      dg_eg_pr  = "diagnostico_egreso_principal",
      dg_eg_sc  = "diagnostico_egreso_secundario",
      dg_ing_pr = "diagnostico_ingreso_princial",    # typo in source data preserved
      dg_ing_sc = "diagnostico_ingreso_secundario"   # kept for compat; EXCLUDED from logic
    ),
    icd_extract_pattern = "\\b[A-TV-Z][0-9]{2}(?:\\.[0-9A-Za-z]+)?\\b"
) {

  eg_pr  <- cols$dg_eg_pr
  eg_sc  <- cols$dg_eg_sc
  ing_pr <- cols$dg_ing_pr
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
    mutate(across(all_of(c(eg_pr, eg_sc, ing_pr)), ~ as.character(.x))) %>%
    mutate(
      diag_eg_pr  = str_to_upper(!!sym(eg_pr)),
      diag_eg_sc  = str_to_upper(!!sym(eg_sc)),
      diag_ing_pr = str_to_upper(!!sym(ing_pr)),
      diag_all    = str_c(diag_eg_pr, diag_eg_sc, diag_ing_pr,
                          sep = " | ", na.rm = TRUE) %>% str_squish()
    ) %>%
    mutate(icd_any = str_extract(diag_all, icd_extract_pattern)) %>%

    # ── Rowwise field-matching ─────────────────────────────────────────────
    rowwise() %>%
    mutate(
      icc_fields = list(na.omit(c(
        if (pat_match(diag_eg_pr,  patterns$icc)) "egreso_principal"  else NA_character_,
        if (pat_match(diag_eg_sc,  patterns$icc)) "egreso_secundario" else NA_character_,
        if (pat_match(diag_ing_pr, patterns$icc)) "ingreso_principal" else NA_character_
      ))),

      sca_fields = list(na.omit(c(
        if (sca_ok(diag_eg_pr,  patterns$sca)) "egreso_principal"  else NA_character_,
        if (sca_ok(diag_eg_sc,  patterns$sca)) "egreso_secundario" else NA_character_,
        if (sca_ok(diag_ing_pr, patterns$sca)) "ingreso_principal" else NA_character_
      ))),

      acv_fields = list(na.omit(c(
        if (acv_ok(diag_eg_pr,  patterns$acv, gi_hemo_pattern)) "egreso_principal"  else NA_character_,
        if (acv_ok(diag_eg_sc,  patterns$acv, gi_hemo_pattern)) "egreso_secundario" else NA_character_,
        if (acv_ok(diag_ing_pr, patterns$acv, gi_hemo_pattern)) "ingreso_principal" else NA_character_
      ))),

      tep_fields = list(na.omit(c(
        if (pat_match(diag_eg_pr,  patterns$tep)) "egreso_principal"  else NA_character_,
        if (pat_match(diag_eg_sc,  patterns$tep)) "egreso_secundario" else NA_character_,
        if (pat_match(diag_ing_pr, patterns$tep)) "ingreso_principal" else NA_character_
      ))),

      tx_fields = list(na.omit(c(
        if (pat_match(diag_eg_pr,  patterns$tx)) "egreso_principal"  else NA_character_,
        if (pat_match(diag_eg_sc,  patterns$tx)) "egreso_secundario" else NA_character_,
        if (pat_match(diag_ing_pr, patterns$tx)) "ingreso_principal" else NA_character_
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
        str_detect(tep_matched_fields, "egreso_principal") |
          (str_detect(tep_matched_fields, "egreso_secundario") &
             str_detect(tep_matched_fields, "ingreso_principal"))
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
      es_caci = case_when(
        str_detect(icc_matched_fields, "egreso_secundario") ~ "icc",
        str_detect(acv_matched_fields, "egreso_secundario") ~ "acv",
        str_detect(sca_matched_fields, "egreso_secundario") ~ "sca",
        str_detect(tep_matched_fields, "egreso_secundario") ~ "tep",
        str_detect(tx_matched_fields,  "egreso_secundario") ~ "txc",
        TRUE ~ NA_character_
      ),
      # [CHANGED]: TEP → "tep" here; previous analysis_update_2026.R inline version
      #            had str_detect(tep_matched_fields, "ingreso_principal") ~ "SCA" — wrong.
      ip_caci = case_when(
        str_detect(icc_matched_fields, "ingreso_principal") ~ "icc",
        str_detect(acv_matched_fields, "ingreso_principal") ~ "acv",
        str_detect(sca_matched_fields, "ingreso_principal") ~ "sca",
        tep_valid                                           ~ "tep",
        str_detect(tx_matched_fields,  "ingreso_principal") ~ "txc",
        TRUE ~ NA_character_
      ),

      # Field priority: EP wins, then ES, then IP
      caci_final = case_when(
        !is.na(ep_caci)                                    ~ ep_caci,
        is.na(ep_caci) & !is.na(es_caci)                  ~ es_caci,
        is.na(ep_caci) & is.na(es_caci) & !is.na(ip_caci) ~ ip_caci,
        TRUE ~ NA_character_
      ),

      # [ADDED]: Fallback bucket for general cardiology not meeting strict SCA criteria
      caci_final = if_else(
        is.na(caci_final) &
          str_detect(diag_all, regex("ANGINA DE PECHO|\\bI20\\b|\\bI25\\b", ignore_case = TRUE)),
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
    ) %>%
    select(-ep_caci, -es_caci, -ip_caci)

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
