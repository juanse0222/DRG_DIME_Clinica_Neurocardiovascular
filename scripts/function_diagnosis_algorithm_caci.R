############################################################
# CACI CLASSIFICATION SCRIPT
# 
# Goal:
#   From multiple diagnosis fields (ingreso / egreso,
#   principal / secundario), classify patients into:
#     - ICC : Insuficiencia Cardíaca
#     - SCA : Síndrome Coronario Agudo / Card. Isquémica
#     - ACV : Enfermedad Cerebrovascular
#     - TEP : Tromboembolismo Pulmonar
#     - TX  : Trasplante
#
#   The algorithm:
#     1) Normalises diagnosis text (upper case).
#     2) Detects patterns by group (regex + ICD-10).
#     3) Records in which field(s) each group was found.
#     4) Builds:
#        - caci_base: based on any field (original hierarchy).
#        - caci_principal: only if the group appears in a
#          principal diagnosis (ingreso/egreso).
#        - caci_final: prefers caci_principal, else caci_base.
#
#   IMPORTANT CHANGE:
#     - GI hemorrhages (e.g. "Hemorragia gastrointestinal")
#       are explicitly EXCLUDED from ACV classification.
############################################################

############################
# 0. Required packages
############################

# install.packages(c("dplyr", "stringr", "janitor")) # if needed

library(dplyr)
library(stringr)
library(janitor)  # optional, useful for cleaning in other steps


############################
# 1. Small helper
############################

# Safe "OR" for strings: x %||% y returns x unless it is NA
`%||%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}


############################
# 2. Pattern constructors
############################

# 2.1. Main CACI group patterns (Spanish free text + ICD-10)
make_caci_patterns <- function() {
  list(
    # Insuficiencia Cardíaca / Falla cardiaca
    icc = paste0(
      "(",
      "INSUFICIENCIA CARDIACA|FALLA CARDIACA|INSUFICIENCIA CARDIACA CONGESTIVA|FALLA VENTRICULAR|",
      "CARDIOMIOPATIA DILATADA|",
      "EDEMA PULMONAR CARDIOGENO|EDEMA AGUDO DE PULMON|",
      "INSUFICIENCIA VALVULAR|",
      "\\bI50\\b",
      ")"
    ),
    
    # Síndrome Coronario Agudo / Enfermedad coronaria
    sca = paste0(
      "(",
      "INFARTO DE MIOCARDIO|INFARTO AGUDO|INFARTO TRANSMURAL|INFARTO SUBENDOCARDICO|",
      "ANGINA INESTABLE|ANGINA DE PECHO|CORONARIA|CORONARIOPATIA|CARDIOPATIA ISQUEMICA|",
      "ENFERMEDAD ATEROSCLEROTICA DEL CORAZON|",
      "\\bI21\\b|\\bI22\\b|\\bI20\\b|\\bI24\\b|\\bI25\\b|\\bI251\\b",
      ")"
    ),
    
    # Enfermedad Cerebrovascular
    # IMPORTANTE: sin "HEMORRAGIA" genérica para evitar GI / uterina, etc.
    acv = paste0(
      "(",
      "ACCIDENTE VASCULAR|\\bACV\\b|INFARTO CEREBRAL|ENFERMEDAD CEREBROVASCULAR|CEREBROVASCULAR|",
      "HEMORRAGIA CEREBRAL|HEMORRAGIA INTRACEREBRAL|HEMORRAGIA SUBARACNOIDEA|HEMORRAGIA INTRACRANEAL|",
      "\\bI60\\b|\\bI61\\b|\\bI62\\b|\\bI63\\b|\\bI64\\b|\\bI65\\b|\\bI66\\b|\\bI67\\b|\\bI68\\b|\\bI69\\b|\\bG45\\b",
      ")"
    ),
    
    # Trasplante
    tx = paste0(
      "(",
      "TRASPLANTE|TRASPLANTADO|TRASPLANTADA|TRASPLANTADOS|",
      "\\bZ94\\b|COMPLICACIONES DE TRASPLANTE|RECHAZO DE TRASPLANTE",
      ")"
    ),
    
    # Tromboembolismo Pulmonar
    tep = paste0(
      "(",
      "TROMBOEMBOLISMO PULMONAR|EMBOLIA PULMONAR|EMBOLIA PULMONAR AGUDA|EMBOLISMO PULMONAR|",
      "\\bI26\\b",
      ")"
    )
  )
}

# 2.2. Pattern to identify GI hemorrhages (to EXCLUDE from ACV)
make_gi_hemo_pattern <- function() {
  paste0(
    "(",
    "HEMORRAGIA (DIGESTIVA|GASTROINTESTINAL)|",
    "SANGRADO (DIGESTIVO|GASTROINTESTINAL)|",
    "SANGRADO DE TUBO DIGESTIVO|SANGRADO TUBO DIGESTIVO|",
    "MELENA|HEMATEMESIS",
    ")"
  )
}


############################
# 3. Main classifier
############################
# Arguments:
#   data_ingresos_2: data.frame / tibble with diagnosis columns
#   cols: list with names of diagnosis columns in the dataset
#   icd_extract_pattern: regex to extract an ICD-10 code if present
#
# Output:
#   Original data + extra variables:
#     - icd_any           : first ICD-10 code found in any diagnosis text
#     - *_flag            : logical per CACI group (ICC, SCA, ACV, TEP, TX)
#     - *_matched_fields  : which fields matched per group
#     - matched_groups    : list of groups that matched in the record
#     - caci_base         : classification using any field (original hierarchy)
#     - caci_principal    : classification using only principal diagnoses
#     - caci_final        : preferred final classification
#     - reason            : textual explanation of why caci_final was assigned

classify_caci <- function(
    data_ingresos_2,
    cols = list(
      dg_eg_pr = "diagnostico_egreso_principal",
      dg_eg_sc = "diagnostico_egreso_secundario",
      dg_ing_pr = "diagnostico_ingreso_princial",
      dg_ing_sc = "diagnostico_ingreso_secundario"
    ),
    icd_extract_pattern = "\\b[A-TV-Z][0-9]{2}(?:\\.[0-9A-Za-z]+)?\\b"
) {
  # Short aliases for column names
  eg_pr  <- cols$dg_eg_pr
  eg_sc  <- cols$dg_eg_sc
  ing_pr <- cols$dg_ing_pr
  ing_sc <- cols$dg_ing_sc
  
  # Load regex patterns
  patterns       <- make_caci_patterns()
  gi_hemo_pattern <- make_gi_hemo_pattern()
  
  df_out <- data_ingresos_2 %>%
    # Ensure diagnosis columns are characters
    mutate(across(all_of(c(eg_pr, eg_sc, ing_pr, ing_sc)), ~ as.character(.x))) %>%
    
    # Normalised text versions (upper case) and combined field
    mutate(
      diag_eg_pr  = str_to_upper(!!sym(eg_pr)),
      diag_eg_sc  = str_to_upper(!!sym(eg_sc)),
      diag_ing_pr = str_to_upper(!!sym(ing_pr)),
      diag_ing_sc = str_to_upper(!!sym(ing_sc)),
      diag_all    = str_c(diag_eg_pr, diag_eg_sc, diag_ing_pr, diag_ing_sc,
                          sep = " | ", na.rm = TRUE) %>%
        str_squish()
    ) %>%
    
    # First ICD-10 code found in any diagnosis text (optional, but useful)
    mutate(
      icd_any = str_extract(diag_all, icd_extract_pattern)
    ) %>%
    
    # Rowwise logic to detect which fields match each CACI group
    rowwise() %>%
    mutate(
      # ICC --------------------------------------------------------------------
      icc_fields = list(na.omit(c(
        if (!is.na(diag_eg_pr)  && str_detect(diag_eg_pr,  regex(patterns$icc, ignore_case = TRUE))) "egreso_principal"   else NA_character_,
        if (!is.na(diag_eg_sc)  && str_detect(diag_eg_sc,  regex(patterns$icc, ignore_case = TRUE))) "egreso_secundario"   else NA_character_,
        if (!is.na(diag_ing_pr) && str_detect(diag_ing_pr, regex(patterns$icc, ignore_case = TRUE))) "ingreso_principal"   else NA_character_,
        if (!is.na(diag_ing_sc) && str_detect(diag_ing_sc, regex(patterns$icc, ignore_case = TRUE))) "ingreso_secundario"  else NA_character_
      ))),
      
      # SCA --------------------------------------------------------------------
      sca_fields = list(na.omit(c(
        if (!is.na(diag_eg_pr)  && str_detect(diag_eg_pr,  regex(patterns$sca, ignore_case = TRUE))) "egreso_principal"   else NA_character_,
        if (!is.na(diag_eg_sc)  && str_detect(diag_eg_sc,  regex(patterns$sca, ignore_case = TRUE))) "egreso_secundario"   else NA_character_,
        if (!is.na(diag_ing_pr) && str_detect(diag_ing_pr, regex(patterns$sca, ignore_case = TRUE))) "ingreso_principal"   else NA_character_,
        if (!is.na(diag_ing_sc) && str_detect(diag_ing_sc, regex(patterns$sca, ignore_case = TRUE))) "ingreso_secundario"  else NA_character_
      ))),
      
      # ACV --------------------------------------------------------------------
      # Includes only cerebrovascular patterns AND explicitly excludes GI hemorrhage text
      acv_fields = list(na.omit(c(
        if (
          !is.na(diag_eg_pr) &&
          str_detect(diag_eg_pr, regex(patterns$acv, ignore_case = TRUE)) &&
          !str_detect(diag_eg_pr, regex(gi_hemo_pattern, ignore_case = TRUE))
        ) "egreso_principal" else NA_character_,
        
        if (
          !is.na(diag_eg_sc) &&
          str_detect(diag_eg_sc, regex(patterns$acv, ignore_case = TRUE)) &&
          !str_detect(diag_eg_sc, regex(gi_hemo_pattern, ignore_case = TRUE))
        ) "egreso_secundario" else NA_character_,
        
        if (
          !is.na(diag_ing_pr) &&
          str_detect(diag_ing_pr, regex(patterns$acv, ignore_case = TRUE)) &&
          !str_detect(diag_ing_pr, regex(gi_hemo_pattern, ignore_case = TRUE))
        ) "ingreso_principal" else NA_character_,
        
        if (
          !is.na(diag_ing_sc) &&
          str_detect(diag_ing_sc, regex(patterns$acv, ignore_case = TRUE)) &&
          !str_detect(diag_ing_sc, regex(gi_hemo_pattern, ignore_case = TRUE))
        ) "ingreso_secundario" else NA_character_
      ))),
      
      # TX ---------------------------------------------------------------------
      tx_fields = list(na.omit(c(
        if (!is.na(diag_eg_pr)  && str_detect(diag_eg_pr,  regex(patterns$tx, ignore_case = TRUE))) "egreso_principal"   else NA_character_,
        if (!is.na(diag_eg_sc)  && str_detect(diag_eg_sc,  regex(patterns$tx, ignore_case = TRUE))) "egreso_secundario"   else NA_character_,
        if (!is.na(diag_ing_pr) && str_detect(diag_ing_pr, regex(patterns$tx, ignore_case = TRUE))) "ingreso_principal"   else NA_character_,
        if (!is.na(diag_ing_sc) && str_detect(diag_ing_sc, regex(patterns$tx, ignore_case = TRUE))) "ingreso_secundario"  else NA_character_
      ))),
      
      # TEP --------------------------------------------------------------------
      tep_fields = list(na.omit(c(
        if (!is.na(diag_eg_pr)  && str_detect(diag_eg_pr,  regex(patterns$tep, ignore_case = TRUE))) "egreso_principal"   else NA_character_,
        #if (!is.na(diag_eg_sc)  && str_detect(diag_eg_sc,  regex(patterns$tep, ignore_case = TRUE))) "egreso_secundario"   else NA_character_,
        if (!is.na(diag_ing_pr) && str_detect(diag_ing_pr, regex(patterns$tep, ignore_case = TRUE))) "ingreso_principal"   else NA_character_,
        if (!is.na(diag_ing_sc) && str_detect(diag_ing_sc, regex(patterns$tep, ignore_case = TRUE))) "ingreso_secundario"  else NA_character_
      )))
    ) %>%
    ungroup() %>%
    
    # Flags and concise lists of matched fields
    mutate(
      icc_flag = lengths(icc_fields) > 0,
      sca_flag = lengths(sca_fields) > 0,
      acv_flag = lengths(acv_fields) > 0,
      tx_flag  = lengths(tx_fields)  > 0,
      tep_flag = lengths(tep_fields) > 0,
      
      icc_matched_fields = ifelse(icc_flag, sapply(icc_fields, paste, collapse = "; "), NA_character_),
      sca_matched_fields = ifelse(sca_flag, sapply(sca_fields, paste, collapse = "; "), NA_character_),
      acv_matched_fields = ifelse(acv_flag, sapply(acv_fields, paste, collapse = "; "), NA_character_),
      tx_matched_fields  = ifelse(tx_flag,  sapply(tx_fields,  paste, collapse = "; "), NA_character_),
      tep_matched_fields = ifelse(tep_flag, sapply(tep_fields, paste, collapse = "; "), NA_character_),
      
      tep_valid = tep_flag & (
          # both principals
          str_detect(tep_matched_fields %||% "", 
                     "egreso_principal") &
            str_detect(tep_matched_fields %||% "", 
                       "ingreso_principal")|
            # swap cases
            (str_detect(tep_matched_fields %||% "", "egreso_principal") &
                str_detect(tep_matched_fields %||% "", "ingreso_secundario"))
          |
            (
              str_detect(tep_matched_fields %||% "", "egreso_secundario") &
                str_detect(tep_matched_fields %||% "", "ingreso_principal"))
        ),

      
      # Human-readable list of all groups with at least one match
      matched_groups = str_squish(str_c(
        ifelse(icc_flag, "ICC", NA_character_),
        ifelse(sca_flag, "SCA", NA_character_),
        ifelse(acv_flag, "ACV", NA_character_),
        ifelse(tep_flag, "TEP", NA_character_),
        ifelse(tx_flag,  "TX",  NA_character_),
        sep = "; "
      )) %>%
        str_replace_all("NA; |; NA|^NA$|^;|;$", "")
    ) %>%
    
    # 3.1. Base CACI classification (any field, with priority)
    mutate(
      caci_base = case_when(
        icc_flag ~ "ICC",
        acv_flag ~ "ACV",
        tep_valid ~ "TEP",
        sca_flag ~ "SCA",
        tx_flag  ~ "TX",
        TRUE     ~ NA_character_
      )
    ) %>%
    
    # 3.2. Principal-based classification (egreso/ingreso principal)
    mutate(
      icc_has_principal = icc_flag & str_detect(icc_matched_fields %||% "", "principal"),
      acv_has_principal = acv_flag & str_detect(acv_matched_fields %||% "", "principal"),
      tep_has_principal = tep_valid,
      sca_has_principal = sca_flag & str_detect(sca_matched_fields %||% "", "principal"),
      tx_has_principal  = tx_flag  & str_detect(tx_matched_fields  %||% "", "principal"),
      
      caci_principal = case_when(
        icc_has_principal ~ "ICC",
        acv_has_principal ~ "ACV",
        tep_has_principal ~ "TEP",
        sca_has_principal ~ "SCA",
        tx_has_principal  ~ "TX",
        TRUE              ~ NA_character_
      ),
      
      # 3.3. Final classification:
      #      prefer principal-based when available, else base
      caci_final = coalesce(caci_principal, caci_base),
      
      # Explanation text
      reason = case_when(
        !is.na(caci_final) ~ str_c(
          "Asignado a ", caci_final, 
          " (jerarquía ICC > ACV > SCA > TEP > TX). Grupos detectados: [",
          matched_groups, "]. ",
          "Campos ICC: ", icc_matched_fields %||% "ninguno", " | ",
          "Campos SCA: ", sca_matched_fields %||% "ninguno", " | ",
          "Campos ACV: ", acv_matched_fields %||% "ninguno", " | ",
          "Campos TEP: ", tep_valid %||% "ninguno", " | ",
          "Campos TX: ",  tx_matched_fields  %||% "ninguno"
        ),
        TRUE ~ NA_character_
      )
    )
  
  return(df_out)
}


############################
# 4. Debug / QA helper
############################
# Shows records where there IS some matched_fields, but caci_final is NA.
# Useful to see borderline or mis-detected cases.

debug_report <- function(result_df, cols, id_col = "documento") {
  result_df %>%
    filter(
      is.na(caci_final) &
        (
          !is.na(icc_matched_fields) |
            !is.na(sca_matched_fields) |
            !is.na(acv_matched_fields) |
            !is.na(tep_valid) |
            !is.na(tx_matched_fields)
        )
    ) %>%
    select(
      all_of(c(
        id_col,
        "icd_any",
        "caci_final", "caci_principal", "caci_base",
        "matched_groups",
        "icc_matched_fields", "sca_matched_fields", "acv_matched_fields",
        "tep_valid", "tx_matched_fields",
        cols$dg_eg_pr, cols$dg_eg_sc, cols$dg_ing_pr, cols$dg_ing_sc
      ))
    ) %>%
    arrange(.data[[id_col]]) %>%
    head(200)
}


############################
# 5. Example usage (commented)
############################
# Example (adjust column names to your dataset):
#
 result <- classify_caci(
   data_ingresos_2,
   cols = list(
     dg_eg_pr  = "diagnostico_egreso_principal",
     dg_eg_sc  = "diagnostico_egreso_secundario",
     dg_ing_pr = "diagnostico_ingreso_princial",
     dg_ing_sc = "diagnostico_ingreso_secundario"
   )
 )

 head(result %>% 
        select(
          documento,
          diagnostico_ingreso_princial,
          diagnostico_egreso_principal,
          icc_flag, sca_flag, acv_flag, tep_flag, tx_flag,
          tep_valid, caci_base, caci_principal, caci_final,
          reason, icd_any
        ))
 
 result_2 <- result %>% 
   filter(tipo_de_atencion == "HOSPITALARIO") %>% 
   filter(str_detect(departamento_actual, "HOSPITA|UCI|UCIN|URGE")) %>% 
   mutate(diag_all = paste(diagnostico_egreso_principal, diagnostico_ingreso_princial, 
                           diagnostico_egreso_secundario, diagnostico_ingreso_secundario, sep = "-"),
          caci_2 = case_when(icc_matched_fields %in% c("egreso_secundario; ingreso_principal", 
                                                       "egreso_principal; ingreso_secundario",
                                                       "egreso_principal; ingreso_principal", 
                                                       "ingreso_principal", "egreso_principal") | 
                               str_detect(icc_matched_fields, "principal") ~ "icc",
                             acv_matched_fields %in% c("egreso_secundario; ingreso_principal", 
                                                       "egreso_principal; ingreso_secundario",
                                                       "egreso_principal; ingreso_principal", 
                                                       "ingreso_principal", "egreso_principal") | 
                               str_detect(acv_matched_fields, "principal") ~ "acv",
                             #tep_matched_fields %in% c("egreso_principal; ingreso_principal", 
                              #                         #"egreso_secundario; ingreso_principal", 
                               #                        "egreso_principal; ingreso_secundario") | 
                               #str_detect(tep_matched_fields, "principal") ~ "tep",
                             tep_valid == TRUE ~ "tep",
                             sca_matched_fields %in% c("egreso_principal; ingreso_principal",
                                                       "egreso_secundario; ingreso_principal", 
                                                       "egreso_principal; ingreso_secundario",
                                                       "ingreso_principal", "egreso_principal") | 
                               str_detect(sca_matched_fields, "principal") ~ "sca",
                             tx_matched_fields %in% c("egreso_secundario; ingreso_principal", 
                                                      "egreso_principal; ingreso_secundario",
                                                      "egreso_principal; ingreso_principal", 
                                                      "ingreso_principal", "egreso_principal",
                                                      "ingreso_secundario", "egreso_secundario") | 
                               str_detect(tx_matched_fields, "principal") ~ "txc",
                             TRUE ~ NA)) 
 
tabyl(result_2$caci_2)
export(result_2, here("data", "results_2.rds"))

# # Filter to hospitalisations and specific services, if desired:
# result_hosp <- result %>%
#   filter(tipo_de_atencion == "HOSPITALARIO") %>%
#   filter(str_detect(departamento_actual, "HOSPITA|UCI|UCIN|URGE"))
#
# # Debug suspected misclassifications:
# debug_report(
#   result,
#   cols = list(
#     dg_eg_pr  = "diagnostico_egreso_principal",
#     dg_eg_sc  = "diagnostico_egreso_secundario",
#     dg_ing_pr = "diagnostico_ingreso_princial",
#     dg_ing_sc = "diagnostico_ingreso_secundario"
#   ),
#   id_col = "documento"
# )