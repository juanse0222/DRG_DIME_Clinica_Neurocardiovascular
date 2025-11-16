# Required packages
library(dplyr)
library(stringr)
library(janitor)   # optional but useful

# Function to classify CACI groups in a data frame
# Arguments:
#   df: input data.frame / tibble
#   cols: named vector or list of the four diagnosis column names (defaults below)
#   icd_extract_pattern: regex to find ICD-10 codes anywhere in text (adjust if needed)

classify_caci <- function(data_ingresos_2,
                          cols = list(
                            dg_eg_pr = "diagnostico_egreso_principal",
                            dg_eg_sc = "diagnostico_egreso_secundario",
                            dg_ing_pr = "diagnostico_ingreso_princial",
                            dg_ing_sc = "diagnostico_ingreso_secundario"
                          ),
                          icd_extract_pattern = "\\b[A-TV-Z][0-9]{2}(?:\\.[0-9A-Za-z]+)?\\b"
) {
  eg_pr <- cols$dg_eg_pr
  eg_sc <- cols$dg_eg_sc
  ing_pr <- cols$dg_ing_pr
  ing_sc <- cols$dg_ing_sc
  
  patterns <- list(
    icc = paste0(
      "(INSUFICIENCIA CARDIACA|FALLA CARDIACA|INSUFICIENCIA CARDIACA CONGESTIVA|FALLA VENTRICULAR|CARDIOMIOPATIA DILATADA",
      "EDEMA PULMONAR|INSUFUCIENCIA VALVULAR|\\bI50)"),
    iam = paste0(
      "(INFARTO DE MIOCARDIO|INFARTO AGUDO|INFARTO TRANSMURAL|INFARTO SUBENDOCARDICO|",
      "ANGINA INESTABLE|ANGINA DE PECHO|CORONARIA|CORONARIOPATIA|CARDIOPATIA ISQUEMICA|ENFERMEDAD ATEROSCLEROTICA DEL CORAZON|\\bI21\\b|\\bI22\\b|\\bI20\\b|\\bI24\\b|\\bI25\\b|\\bI251\\b)"),
    acv = paste0(
      "(ACCIDENTE VASCULAR|\\bACV\\b|INFARTO CEREBRAL|ENFERMEDAD CEREBROVASCULAR|CEREBROVASCULAR|",
      "HEMORRAGIA INTRACEREBRAL|HEMORRAGIA SUBARACNOIDEA|HEMORRAGIA|\\bI60\\b|\\bI61\\b|\\bI62\\b|\\bI63\\b|\\bI64\\b|\\bI65\\b|\\bI66\\b|\\bI67\\b|\\bI68\\b|\\bI69\\b|\\bG45\\b)"),
    tx = paste0(
      "(TRASPLANTE|TRASPLANTADO|TRASPLANTADA|TRASPLANTADOS|\\bZ94\\b|COMPLICACIONES DE TRASPLANTE|RECHAZO DE TRASPLANTE)"),
    tep = paste0(
      "(TROMBOEMBOLISMO PULMONAR|EMBOLIA PULMONAR|EMBOLIA PULMONAR AGUDA|EMBOLISMO PULMONAR|\\bI26\\b)")
  )
  
  df_out <- data_ingresos_2 %>%
    mutate(across(all_of(c(eg_pr, eg_sc, ing_pr, ing_sc)), ~ as.character(.x))) %>%
    mutate(
      diag_all = str_c(!!sym(eg_pr), !!sym(eg_sc), !!sym(ing_pr), !!sym(ing_sc), sep = " | ", na.rm = TRUE) %>%
        str_squish() %>% str_to_upper(),
      diag_eg_pr = (!!sym(eg_pr)) %>% as.character() %>% str_to_upper(),
      diag_eg_sc = (!!sym(eg_sc)) %>% as.character() %>% str_to_upper(),
      diag_ing_pr = (!!sym(ing_pr)) %>% as.character() %>% str_to_upper(),
      diag_ing_sc = (!!sym(ing_sc)) %>% as.character() %>% str_to_upper()
    ) %>%
    mutate(
      icd_any = str_extract(diag_all, icd_extract_pattern)
    ) %>%
    
    # Build per-field matched lists first (safe handling of NA)
    rowwise() %>%
    mutate(
      icc_fields = list(na.omit(c(
        ifelse(is.na(diag_eg_pr), NA_character_,
               ifelse(str_detect(diag_eg_pr, regex(patterns$icc, ignore_case = TRUE)), "egreso_principal", NA_character_)),
        ifelse(is.na(diag_eg_sc), NA_character_,
               ifelse(str_detect(diag_eg_sc, regex(patterns$icc, ignore_case = TRUE)), "egreso_secundario", NA_character_)),
        ifelse(is.na(diag_ing_pr), NA_character_,
               ifelse(str_detect(diag_ing_pr, regex(patterns$icc, ignore_case = TRUE)), "ingreso_principal", NA_character_)),
        ifelse(is.na(diag_ing_sc), NA_character_,
               ifelse(str_detect(diag_ing_sc, regex(patterns$icc, ignore_case = TRUE)), "ingreso_secundario", NA_character_))
      ))),
      
      sca_fields = list(na.omit(c(
        ifelse(is.na(diag_eg_pr), NA_character_,
               ifelse(str_detect(diag_eg_pr, regex(patterns$iam, ignore_case = TRUE)), "egreso_principal", NA_character_)),
        ifelse(is.na(diag_eg_sc), NA_character_,
               ifelse(str_detect(diag_eg_sc, regex(patterns$iam, ignore_case = TRUE)), "egreso_secundario", NA_character_)),
        ifelse(is.na(diag_ing_pr), NA_character_,
               ifelse(str_detect(diag_ing_pr, regex(patterns$iam, ignore_case = TRUE)), "ingreso_principal", NA_character_)),
        ifelse(is.na(diag_ing_sc), NA_character_,
               ifelse(str_detect(diag_ing_sc, regex(patterns$iam, ignore_case = TRUE)), "ingreso_secundario", NA_character_))
      ))),
      
      acv_fields = list(na.omit(c(
        ifelse(is.na(diag_eg_pr), NA_character_,
               ifelse(str_detect(diag_eg_pr, regex(patterns$acv, ignore_case = TRUE)), "egreso_principal", NA_character_)),
        ifelse(is.na(diag_eg_sc), NA_character_,
               ifelse(str_detect(diag_eg_sc, regex(patterns$acv, ignore_case = TRUE)), "egreso_secundario", NA_character_)),
        ifelse(is.na(diag_ing_pr), NA_character_,
               ifelse(str_detect(diag_ing_pr, regex(patterns$acv, ignore_case = TRUE)), "ingreso_principal", NA_character_)),
        ifelse(is.na(diag_ing_sc), NA_character_,
               ifelse(str_detect(diag_ing_sc, regex(patterns$acv, ignore_case = TRUE)), "ingreso_secundario", NA_character_))
      ))),
      
      tx_fields = list(na.omit(c(
        ifelse(is.na(diag_eg_pr), NA_character_,
               ifelse(str_detect(diag_eg_pr, regex(patterns$tx, ignore_case = TRUE)), "egreso_principal", NA_character_)),
        ifelse(is.na(diag_eg_sc), NA_character_,
               ifelse(str_detect(diag_eg_sc, regex(patterns$tx, ignore_case = TRUE)), "egreso_secundario", NA_character_)),
        ifelse(is.na(diag_ing_pr), NA_character_,
               ifelse(str_detect(diag_ing_pr, regex(patterns$tx, ignore_case = TRUE)), "ingreso_principal", NA_character_)),
        ifelse(is.na(diag_ing_sc), NA_character_,
               ifelse(str_detect(diag_ing_sc, regex(patterns$tx, ignore_case = TRUE)), "ingreso_secundario", NA_character_))
      ))),
      
      tep_fields = list(na.omit(c(
        ifelse(is.na(diag_eg_pr), NA_character_,
               ifelse(str_detect(diag_eg_pr, regex(patterns$tep, ignore_case = TRUE)), "egreso_principal", NA_character_)),
        ifelse(is.na(diag_eg_sc), NA_character_,
               ifelse(str_detect(diag_eg_sc, regex(patterns$tep, ignore_case = TRUE)), "egreso_secundario", NA_character_)),
        ifelse(is.na(diag_ing_pr), NA_character_,
               ifelse(str_detect(diag_ing_pr, regex(patterns$tep, ignore_case = TRUE)), "ingreso_principal", NA_character_)),
        ifelse(is.na(diag_ing_sc), NA_character_,
               ifelse(str_detect(diag_ing_sc, regex(patterns$tep, ignore_case = TRUE)), "ingreso_secundario", NA_character_))
      )))
    ) %>%
    ungroup() %>%
    
    # Flags determined from whether matched-field lists are non-empty (more robust)
    mutate(
      icc_flag = lengths(icc_fields) > 0,
      sca_flag = lengths(sca_fields) > 0,
      acv_flag = lengths(acv_fields) > 0,
      tx_flag  = lengths(tx_fields)  > 0,
      tep_flag = lengths(tep_fields) > 0
    ) %>%
    
    # Concise textual descriptions
    mutate(
      icc_matched_fields = ifelse(lengths(icc_fields) > 0, sapply(icc_fields, function(x) paste(x, collapse = "; ")), NA_character_),
      sca_matched_fields = ifelse(lengths(sca_fields) > 0, sapply(sca_fields, function(x) paste(x, collapse = "; ")), NA_character_),
      acv_matched_fields = ifelse(lengths(acv_fields) > 0, sapply(acv_fields, function(x) paste(x, collapse = "; ")), NA_character_),
      tx_matched_fields  = ifelse(lengths(tx_fields)  > 0, sapply(tx_fields,  function(x) paste(x, collapse = "; ")), NA_character_),
      tep_matched_fields = ifelse(lengths(tep_fields) > 0, sapply(tep_fields, function(x) paste(x, collapse = "; ")), NA_character_)
    ) %>%
    
    # Final caci with same hierarchy (change order to change priority)
    mutate(
      caci = case_when(
        icc_flag ~ "ICC",
        acv_flag ~ "ACV",
        tep_flag ~ "TEP",
        sca_flag ~ "SCA",
        tx_flag  ~ "TX",
        TRUE     ~ NA_character_
      ),
      matched_terms = str_c(
        ifelse(icc_flag, "ICC", NA), 
        ifelse(acv_flag, "ACV", NA),
        ifelse(tep_flag, "TEP", NA),
        ifelse(sca_flag, "SCA", NA),
        ifelse(tx_flag,  "TX",  NA),
        sep = "; "
      ) %>% str_replace_all("(^; )|(; $)", "") %>% str_replace_all("NA; |; NA|NA", "") %>% str_squish(),
      reason = case_when(
        !is.na(caci) ~ str_c("Assigned ", caci, " because: ", matched_terms, " | fields: ",
                             coalesce(icc_matched_fields, sca_matched_fields, acv_matched_fields, tep_matched_fields, tx_matched_fields, "none")),
        TRUE ~ NA_character_
      )
    ) %>%
    
    # tidy up columns
    select(everything(), -icc_fields, -sca_fields, -acv_fields, -tx_fields, -tep_fields, -diag_all,
           -diag_eg_pr, -diag_eg_sc, -diag_ing_pr, -diag_ing_sc)
  
  return(df_out)
}

# Debug helper: show rows where some matched_fields exist but caci is NA
debug_report <- function(result_df, id_col = "documento") {
  result_df %>%
    filter(
      is.na(caci) &
        (
          !is.na(icc_matched_fields) |
            !is.na(sca_matched_fields) |
            !is.na(acv_matched_fields) |
            !is.na(tep_matched_fields) |
            !is.na(tx_matched_fields)
        )
    ) %>%
    select(all_of(c(id_col, "caci", "matched_terms",
                    "icc_matched_fields", "sca_matched_fields", "acv_matched_fields",
                    "tep_matched_fields", "tx_matched_fields", "icd_any",
                    # also include the original diagnosis text columns if present
                    cols$dg_eg_pr, cols$dg_eg_sc, cols$dg_ing_pr, cols$dg_ing_sc))) %>%
    arrange(all_of(id_col)) %>%
    head(200)   # show first 200 problematic rows (tune as needed)
}

# Example usage:
 result <- classify_caci(data_ingresos_2)
 head(result %>% select(documento, diagnostico_ingreso_princial, diagnostico_egreso_principal,
                        icc_flag, sca_flag, acv_flag, tep_flag, tx_flag, caci, reason, icd_any))

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
                            tep_matched_fields %in% c("egreso_secundario; ingreso_principal", 
                                                      "egreso_principal; ingreso_secundario",
                                                      "egreso_principal; ingreso_principal", 
                                                      "ingreso_principal", "egreso_principal") | 
                              str_detect(tep_matched_fields, "principal") ~ "tep",
                            sca_matched_fields %in% c("egreso_secundario; ingreso_principal", 
                                                      "egreso_principal; ingreso_secundario",
                                                      "egreso_principal; ingreso_principal", 
                                                      "ingreso_principal", "egreso_principal") | 
                              str_detect(sca_matched_fields, "principal") ~ "sca",
                            tx_matched_fields %in% c("egreso_secundario; ingreso_principal", 
                                                      "egreso_principal; ingreso_secundario",
                                                      "egreso_principal; ingreso_principal", 
                                                      "ingreso_principal", "egreso_principal") | 
                              str_detect(tx_matched_fields, "principal") ~ "txc",
                            TRUE ~ NA))
