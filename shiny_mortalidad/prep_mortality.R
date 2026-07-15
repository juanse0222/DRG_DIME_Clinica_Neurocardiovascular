#!/usr/bin/env Rscript
# Run from project root:  Rscript shiny_mortalidad/prep_mortality.R
# Fits GEE models and saves all pre-computed outputs for the Shiny app.
# Result: shiny_mortalidad/data/mort_precomputed.rds  (~compact, deploy-ready)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rio)
  library(splines)
  library(geepack)
  library(janitor)
  library(broom)
  library(readxl)
  library(forcats)
})

mort_dir <- "~/Desktop/DIME/Documentos EDI/2. Mortalidad/mortality_analysis"
cat("Loading data from:", mort_dir, "\n")

mes_nombres <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

`%||%` <- function(x, y) ifelse(is.na(x), y, x)

# ── 1. Load sources ────────────────────────────────────────────────────────────
data_raw <- import(file.path(mort_dir, "data/results_2.rds"))

# Try both possible locations for mortality_dime
mort_path_1 <- file.path(mort_dir, "data/mortality_dime_2017_2026.rds")
mort_path_2 <- "~/Desktop/DIME/Documentos EDI/2. Mortalidad/DIME_mortality_2025/data/mortality_dime_2017_2026.rds"
data_mort_total <- import(if (file.exists(mort_path_1)) mort_path_1 else mort_path_2)

data_discharges_total <- import(file.path(mort_dir, "data/data_discharges_hosp.rds"))

cat("Results rows:", nrow(data_raw), "\n")
cat("Mortality rows:", nrow(data_mort_total), "\n")
cat("Discharge rows:", nrow(data_discharges_total), "\n")

# ── 2. Core preparation ───────────────────────────────────────────────────────
data_base <- data_raw %>%
  mutate(
    death           = as.integer(!is.na(fecha_defuncion)),
    año             = as.integer(año),
    mes             = as.integer(mes),
    fecha_de_egreso = as.Date(fecha_de_egreso),
    estancia_horas  = as.numeric(estancia_horas),
    edad            = as.numeric(edad),
    grupo_final = case_when(
      str_detect(sca_matched       %||% "", "principal|defuncion") &
        !(str_detect(diagnostico_egreso_principal %||% "", "ANGINA DE PECHO") &
            !str_detect(diagnostico_egreso_principal %||% "", "INESTABLE")) ~ "sca",
      str_detect(acv_isq_matched   %||% "", "principal|defuncion") ~ "acv_isq",
      str_detect(acv_hemo_matched  %||% "", "principal|defuncion") ~ "acv_hemo",
      str_detect(icc_matched       %||% "", "principal|defuncion") ~ "icc",
      str_detect(tep_matched       %||% "", "principal|defuncion") ~ "tep",
      str_detect(valv_matched      %||% "", "principal|defuncion") ~ "valv",
      str_detect(vasc_matched      %||% "", "principal|defuncion") ~ "vasc",
      str_detect(infx_matched      %||% "", "principal|defuncion") ~ "infx",
      str_detect(cardio_oth_matched %||% "", "principal|defuncion") ~ "cardio_other",
      TRUE ~ "OTRO_DIAGNOSTICO"
    )
  ) %>%
  filter(!is.na(estancia_horas), !is.na(edad), !is.na(año)) %>%
  filter(!año == "2016")

# ── 3. Elixhauser comorbidities ───────────────────────────────────────────────
elix_map <- import(file.path(mort_dir, "data/elixhauser_comorbidities_icd10.xlsx"))
comorb_cols <- intersect(
  c("diagnostico_ingreso_secundario", "diagnostico_egreso_secundario",
    "otros_estados_patologicos", "otros_estados_patologicos_2"),
  names(data_base)
)

check_comorb <- function(df, cols, pattern) {
  df %>%
    select(all_of(cols)) %>%
    mutate(across(everything(),
                  ~ str_detect(as.character(.), regex(pattern, ignore_case = TRUE)))) %>%
    rowSums(na.rm = TRUE) > 0
}

result_elix <- data_base
for (i in seq_len(nrow(elix_map))) {
  nm  <- elix_map$Comorbilidad_Elixhauser[i]
  pat <- elix_map$ICD_10_Regex[i]
  result_elix <- result_elix %>%
    mutate(!!paste0("Elix_", nm) := check_comorb(., comorb_cols, pat))
}

elix_or <- function(df, ...) {
  cols <- intersect(c(...), names(df))
  if (length(cols) == 0L) return(rep(FALSE, nrow(df)))
  df %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), ~ coalesce(., FALSE))) %>%
    rowSums() > 0L
}

result_7 <- result_elix %>%
  mutate(
    Elix_Hipertension_Total  = elix_or(., "Elix_Hipertension_No_Complicada", "Elix_Hipertension_Complicada"),
    Elix_Diabetes_Total      = elix_or(., "Elix_Diabetes_No_Complicada", "Elix_Diabetes_Complicada"),
    Elix_Cancer_Total        = elix_or(., "Elix_Tumor_Solido_Sin_Metastasis", "Elix_Cancer_Metastasico", "Elix_Linfoma"),
    Elix_Otros_Trastornos_Neurologicos =
      elix_or(., "Elix_Otros_Trastornos_Neurologicos", "Elix_Paralisis"),
    año = lubridate::year(fecha_de_egreso)
  )

# ── 4. GEE models ─────────────────────────────────────────────────────────────
target_grds  <- c("sca", "acv_isq", "acv_hemo", "icc", "tep", "valv", "vasc", "infx")
comorbs_dime <- c(
  "Elix_Insuficiencia_Cardiaca_Congestiva", "Elix_Arritmias_Cardiacas",
  "Elix_Enfermedad_Valvular",               "Elix_Trastornos_Circulacion_Pulmonar",
  "Elix_Trastornos_Vasculares_Perifericos", "Elix_Hipertension_Total",
  "Elix_Otros_Trastornos_Neurologicos",     "Elix_Enfermedad_Pulmonar_Cronica",
  "Elix_Diabetes_Total",                    "Elix_Insuficiencia_Renal",
  "Elix_Cancer_Total",                      "Elix_VIH_SIDA",
  "Elix_Obesidad",                          "Elix_Hipotiroidismo"
)
elix_cols  <- intersect(comorbs_dime, names(result_7))
ano_actual <- max(result_7$año, na.rm = TRUE)
cat("Current year:", ano_actual, "\n")

res_anual     <- list()
res_bimensual <- list()
res_mensual   <- list()

for (grd in target_grds) {
  cat("Fitting GEE for:", grd, "... ")

  df_full  <- result_7 %>% filter(grupo_final == grd) %>% arrange(año)
  df_train <- df_full   %>% filter(!año %in% c(2020L, 2021L))
  if (nrow(df_train) < 30L) { cat("too few rows, skip\n"); next }

  ex <- elix_cols
  if (grd == "icc")                     ex <- ex[ex != "Elix_Insuficiencia_Cardiaca_Congestiva"]
  if (grd %in% c("acv_isq","acv_hemo")) ex <- ex[ex != "Elix_Otros_Trastornos_Neurologicos"]
  if (grd == "tep")                     ex <- ex[ex != "Elix_Trastornos_Circulacion_Pulmonar"]
  if (grd == "valv")                    ex <- ex[ex != "Elix_Enfermedad_Valvular"]
  if (grd == "vasc")                    ex <- ex[ex != "Elix_Trastornos_Vasculares_Perifericos"]

  elix_ok <- ex[vapply(ex, function(col) {
    tbl <- table(df_train[[col]], df_train$death)
    nrow(tbl) == 2L && ncol(tbl) == 2L && min(tbl) >= 3L
  }, logical(1L))]

  df_train <- df_train %>%
    mutate(
      departamento_de_ingreso = fct_lump_min(as.factor(departamento_de_ingreso), min = 30L, other_level = "OTROS"),
      departamento_actual     = fct_lump_min(as.factor(departamento_actual),     min = 30L, other_level = "OTROS")
    ) %>%
    droplevels()

  tab_s <- table(df_train$sexo,                     df_train$death)
  tab_i <- table(df_train$departamento_de_ingreso,  df_train$death)
  tab_a <- table(df_train$departamento_actual,       df_train$death)

  preds <- c(
    "ns(edad, df = 3)", "ns(estancia_horas, df = 2)",
    if (nrow(tab_s) == 2L && ncol(tab_s) == 2L && min(tab_s) >= 2L) "sexo" else character(0),
    if (nrow(tab_i) >  1L && min(tab_i) >= 1L) "departamento_de_ingreso" else character(0),
    if (nrow(tab_a) >  1L && min(tab_a) >= 1L) "departamento_actual"     else character(0),
    elix_ok
  )

  modelo <- tryCatch(
    geeglm(as.formula(paste("death ~", paste(preds, collapse = " + "))),
           data   = df_train,
           family = binomial("logit"),
           id     = as.factor(df_train$año),
           corstr = "independence"),
    error = function(e) { cat("model error:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(modelo)) next

  tm_ref <- sum(df_train$death) / nrow(df_train) * 100

  df_full <- df_full %>%
    mutate(
      departamento_de_ingreso = fct_na_value_to_level(
        factor(departamento_de_ingreso, levels = levels(df_train$departamento_de_ingreso)), "OTROS"),
      departamento_actual = fct_na_value_to_level(
        factor(departamento_actual, levels = levels(df_train$departamento_actual)), "OTROS")
    )
  df_full$prediccion <- predict(modelo, newdata = df_full, type = "response")

  res_anual[[grd]] <- df_full %>%
    filter(!año == "2016") %>%
    group_by(año) %>%
    summarise(
      Pacientes = n(),
      Observado = sum(death),
      Esperado  = round(sum(prediccion), 1),
      Tasa      = round(Observado / Pacientes * 100, 2),
      SMR       = if_else(Esperado > 0, Observado / Esperado, NA_real_),
      TMAR      = round(SMR * tm_ref, 2),
      LI        = round(if_else(Observado > 0,
                    (Observado / Esperado) *
                      ((1 - 1/(9*Observado) - 1.96/(3*sqrt(Observado)))^3), NA_real_) * tm_ref, 2),
      LS        = round(if_else(Observado > 0,
                    ((Observado + 1) / Esperado) *
                      ((1 - 1/(9*(Observado+1)) + 1.96/(3*sqrt(Observado+1)))^3), NA_real_) * tm_ref, 2),
      Razon_OE  = round(SMR, 2),
      .groups   = "drop"
    ) %>%
    mutate(GRD = toupper(grd), tasa_ref = round(tm_ref, 2))

  res_bimensual[[grd]] <- df_full %>%
    filter(!año == "2016") %>%
    mutate(mes_2 = floor_date(fecha_de_egreso, "3 months")) %>%
    group_by(mes_2) %>%
    summarise(
      obs     = sum(death),
      exp     = sum(prediccion),
      n       = n(),
      RSMR    = if_else(exp  > 0, obs / exp * tm_ref, NA_real_),
      li_RSMR = if_else(obs  > 0,
        (obs / exp) * ((1 - 1/(9*obs) - 1.96/(3*sqrt(obs)))^3) * tm_ref, NA_real_),
      ls_RSMR = if_else(obs  > 0,
        ((obs+1)/exp) * ((1 - 1/(9*(obs+1)) + 1.96/(3*sqrt(obs+1)))^3) * tm_ref, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(GRD = toupper(grd), tasa_ref = tm_ref)

  res_mensual[[grd]] <- df_full %>%
    filter(año == ano_actual) %>%
    group_by(mes) %>%
    summarise(
      Pacientes = n(),
      Observado = sum(death),
      Esperado  = round(sum(prediccion), 2),
      Tasa      = round(Observado / Pacientes * 100, 2),
      TMAR      = round(if_else(Esperado > 0, Observado / Esperado * tm_ref, NA_real_), 2),
      .groups   = "drop"
    ) %>%
    mutate(GRD      = toupper(grd),
           tasa_ref  = round(tm_ref, 2),
           mes_label = factor(mes_nombres[mes], levels = mes_nombres))

  cat("OK\n")
}

tbl_anual  <- bind_rows(res_anual)
df_bim     <- bind_rows(res_bimensual)
df_mes_act <- bind_rows(res_mensual)

# ── 5. Scalars ────────────────────────────────────────────────────────────────
total_mort_dime       <- nrow(data_mort_total)
total_discharges_dime <- nrow(data_discharges_total)
tasa_bruta_dime       <- round(total_mort_dime / total_discharges_dime * 100, 2)

target_data    <- result_7 %>% filter(grupo_final %in% target_grds)
tot_egresos    <- nrow(target_data)
tot_muertes    <- sum(target_data$death, na.rm = TRUE)
tasa_bruta_grd <- round(tot_muertes / tot_egresos * 100, 2)

act_data         <- target_data %>% filter(año == ano_actual)
act_muertes      <- sum(act_data$death, na.rm = TRUE)
act_egresos      <- nrow(act_data)
act_egresos_dime <- data_discharges_total %>%
  filter(lubridate::year(fecha_de_egreso) == ano_actual) %>% nrow()
act_mort_dime <- data_mort_total %>%
  filter(lubridate::year(fecha_defuncion) == ano_actual) %>% nrow()

tmar_reciente <- tbl_anual %>%
  filter(año == max(año[!is.na(TMAR)], na.rm = TRUE)) %>%
  summarise(v = round(mean(TMAR, na.rm = TRUE), 1)) %>%
  pull(v)

act_exp  <- tbl_anual %>% filter(año == ano_actual) %>%
  summarise(v = round(sum(Esperado, na.rm = TRUE), 1)) %>% pull(v)
act_tmar <- tbl_anual %>% filter(año == ano_actual, !is.na(TMAR)) %>%
  summarise(v = round(mean(TMAR, na.rm = TRUE), 1)) %>% pull(v)
tmar_color <- if (!is.na(act_tmar) && !is.na(tmar_reciente) && act_tmar > tmar_reciente) "danger" else "success"

# ── 6. Chart datasets (pre-computed to avoid heavy data on server) ─────────────
pal <- c(
  ACV_HEMO = "#F8766D", ACV_ISQ = "#E68613", SCA  = "#7CAE00",
  ICC      = "#00BFC4", TEP     = "#00A9FF", INFX = "#C77CFF",
  VALV     = "#CC79A7", VASC    = "#0072B2"
)

dist_acum <- target_data %>%
  count(GRD = toupper(grupo_final)) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

trend_anual <- target_data %>%
  filter(!año == "2016") %>%
  group_by(año, GRD = toupper(grupo_final)) %>%
  summarise(
    Muertes = sum(death, na.rm = TRUE),
    Egresos = n(),
    Tasa    = round(Muertes / Egresos * 100, 2),
    .groups = "drop"
  )

# Demographics (small — keep full)
demo_year <- target_data %>%
  group_by(Año = factor(año), GRD = toupper(grupo_final)) %>%
  summarise(
    N              = n(),
    Muertes        = sum(death, na.rm = TRUE),
    `Tasa (%)`     = round(mean(death, na.rm = TRUE) * 100, 1),
    `Edad mediana` = round(median(edad, na.rm = TRUE), 1),
    `Edad IQR`     = paste0("[", round(quantile(edad, 0.25, na.rm = TRUE), 0),
                             " – ", round(quantile(edad, 0.75, na.rm = TRUE), 0), "]"),
    `% Mujeres`    = round(mean(sexo %in% c("F", "FEMENINO"), na.rm = TRUE) * 100, 1),
    `% Hombres`    = round(mean(sexo %in% c("M", "MASCULINO"), na.rm = TRUE) * 100, 1),
    `LOS mediana (h)` = round(median(estancia_horas, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(Año), desc(Muertes))

# Age distribution (violin — keep compact)
box_data <- target_data %>%
  mutate(GRD = toupper(grupo_final)) %>%
  filter(!is.na(edad)) %>%
  select(GRD, edad, death)

# Comorbidities
elix_display <- c(
  "Hipertensión"          = "Elix_Hipertension_Total",
  "Diabetes"              = "Elix_Diabetes_Total",
  "Insuf. Cardíaca"       = "Elix_Insuficiencia_Cardiaca_Congestiva",
  "Arritmias"             = "Elix_Arritmias_Cardiacas",
  "Enf. Valvular"         = "Elix_Enfermedad_Valvular",
  "Enf. Vasc. Periférica" = "Elix_Trastornos_Vasculares_Perifericos",
  "Enf. Pulmonar Crónica" = "Elix_Enfermedad_Pulmonar_Cronica",
  "Ins. Renal"            = "Elix_Insuficiencia_Renal",
  "Cáncer"                = "Elix_Cancer_Total",
  "Obesidad"              = "Elix_Obesidad",
  "Hipotiroidismo"        = "Elix_Hipotiroidismo",
  "VIH/SIDA"              = "Elix_VIH_SIDA"
)
elix_available <- intersect(elix_display, names(result_7))
elix_labels    <- names(elix_display)[elix_display %in% elix_available]

comorb_data <- if (length(elix_available) > 0) {
  target_data %>%
    filter(death == 1L) %>%
    mutate(GRD = toupper(grupo_final)) %>%
    select(GRD, all_of(elix_available)) %>%
    group_by(GRD) %>%
    summarise(across(everything(), ~ round(mean(., na.rm = TRUE) * 100, 1)),
              .groups = "drop") %>%
    pivot_longer(-GRD, names_to = "Comorbilidad", values_to = "Prevalencia") %>%
    mutate(Comorbilidad = elix_labels[match(Comorbilidad, elix_available)])
} else {
  tibble(GRD = character(), Comorbilidad = character(), Prevalencia = numeric())
}

dept_mort <- target_data %>%
  mutate(departamento_actual = case_when(
    departamento_actual == "UCI"                          ~ "UCI",
    str_detect(coalesce(departamento_actual,""), "UCIN")          ~ "UCIN",
    str_detect(coalesce(departamento_actual,""), "HOSPITALIZACION") ~ "HOSPITALIZACIÓN",
    TRUE ~ coalesce(departamento_actual, "OTRO")
  )) %>%
  group_by(Departamento = departamento_actual) %>%
  summarise(
    Egresos = n(),
    Muertes = sum(death, na.rm = TRUE),
    Tasa    = round(Muertes / Egresos * 100, 1),
    .groups = "drop"
  ) %>%
  filter(Egresos >= 20) %>%
  arrange(desc(Tasa)) %>%
  slice_head(n = 20)

# ── 7. Save ───────────────────────────────────────────────────────────────────
precomp <- list(
  tbl_anual             = tbl_anual,
  df_bim                = df_bim,
  df_mes_act            = df_mes_act,
  dist_acum             = dist_acum,
  trend_anual           = trend_anual,
  demo_year             = demo_year,
  box_data              = box_data,
  comorb_data           = comorb_data,
  dept_mort             = dept_mort,
  pal                   = pal,
  ano_actual            = ano_actual,
  total_discharges_dime = total_discharges_dime,
  total_mort_dime       = total_mort_dime,
  tasa_bruta_dime       = tasa_bruta_dime,
  tot_egresos           = tot_egresos,
  tot_muertes           = tot_muertes,
  tasa_bruta_grd        = tasa_bruta_grd,
  act_egresos_dime      = act_egresos_dime,
  act_egresos           = act_egresos,
  act_mort_dime         = act_mort_dime,
  act_muertes           = act_muertes,
  act_exp               = act_exp,
  act_tmar              = act_tmar,
  tmar_reciente         = tmar_reciente,
  tmar_color            = tmar_color
)

out <- "shiny_mortalidad/data/mort_precomputed.rds"
saveRDS(precomp, out, compress = TRUE)
cat("Saved:", out, "| Size:", round(file.size(out)/1024), "KB\n")
cat("Done. ano_actual =", ano_actual,
    "| Muertes DIME =", total_mort_dime,
    "| Egresos DIME =", total_discharges_dime, "\n")
