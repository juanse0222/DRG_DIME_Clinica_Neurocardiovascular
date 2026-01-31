############################################################
# GRD / CACI COST PIPELINE - DIME
# Author: (you)
# Purpose:
#   1) Clean & prepare admissions (egresos) 2017–2025
#   2) Add CACI classification (ICC, SCA, ACV, TEP, TXC)
#   3) Link CACI admissions with:
#        - CACI “gold standard” Excel files
#        - Sales (ventas) by services
#        - Cost table (planning team)
#        - Medications (FARMACIA)
#   4) Produce final dataset for GRD/CACI cost analysis:
#        - data_costo_total_3
############################################################

pacman::p_load(
  tidyverse, janitor, lubridate, flextable, gtsummary, rio,
  hms, epikit, scales, gt, zoo, readxl, here
)

options(scipen = 999)

Sys.setlocale("LC_TIME", "es_ES")  # months in Spanish if needed

############################################################
# 0. GLOBAL PARAMETERS & SMALL HELPERS
############################################################

# Analysis window (admissions)
FECHA_INICIO <- as.Date("2024-01-01")
FECHA_FIN_VENTAS <- as.Date("2025-09-30")  # adjust as needed

# Regex for “inpatient-type” services
SERVICIOS_INTERNACION_RE <- "UCI|UCIN|HOSPITA|URG"

# Helper: safe OR for strings (avoid NA issues)
`%||%` <- function(x, y) ifelse(is.na(x), y, x)

# Helper: clean Colombian ID fields to digits only
clean_id <- function(x) {
  x %>%
    as.character() %>%
    str_extract("\\d+")
}

# Helper: add year / month (label)
add_year_month <- function(df, fecha_col, prefijo = "") {
  df %>%
    mutate(
      "{{prefijo}}mes"  := month(.data[[fecha_col]], label = TRUE, abbr = TRUE),
      "{{prefijo}}año"  := year(.data[[fecha_col]])
    )
}

############################################################
# 1. LOAD BASE DATA
############################################################

# 1.1 Admissions / Discharges (2017–2025, pre-screened)
data_ingresos <- import(
  "/Users/juansebastianhurtadozapata/Desktop/DIME /Documentos EDI/16. Egresos DIME Mensual/egresos_dime_mensual/data_2025/data_egresos_2017_2025.rda"
)

# 1.2 Sales (ventas) – histórico + trimestre analizado
data_sales <- import(here("data", "data_costs", "data_sales.rds"))

# 1.3 External CACI “gold standard” files
data_sca  <- import(here("data", "sca_2023.xlsx"))
data_acv  <- import(here("data", "acv_2023.xls"))
data_icc  <- import(here("data", "Consolidado IC -TxC.csv"))
data_txc  <- import(here("data", "Consolidado IC -TxC.xlsx"), which = "TxC")
data_tep  <- import(here("data", "tep_2025.xlsx"))

# 1.4 Cost table from planning team
data_costo_2025 <- import(here("data", "data_costs", "costo_general_2024.xlsx"), which = "2025")


############################################################
# 2. ADMISSIONS PRE-PROCESSING
############################################################

data_ingresos_2 <- data_ingresos %>%
  clean_names() %>%
  mutate(
    documento = as.character(documento),
    across(starts_with("fecha"), as.Date)
  ) %>%
  arrange(documento, desc(fecha_ingreso)) %>%
  mutate(
    mes = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año = year(fecha_ingreso)
  ) %>%
  filter(fecha_ingreso > FECHA_INICIO)

############################################################
# 3. CACI CLASSIFICATION (USING YOUR ALGORITHM)
#    - Assume classify_caci() is defined in another file
#      e.g., "function_diagnosis_algorithm_caci.R"
############################################################

# If classify_caci is in a separate file:
# source(here("R", "function_diagnosis_algorithm_caci.R"))

# Here we assume classify_caci() returns:
#  - caci_final (ICC, ACV, TEP, SCA, TXC/TX)
#  - caci_2 or similar (you used this name before)
#  - matched fields & reason (audit)
result <- classify_caci(data_ingresos_2)

# For compatibility with your later code, keep caci_2 and also a final version
result_2 <- result %>%
  mutate(
    caci_2 = if_else(is.na(caci_principal), caci_base, caci_principal),
    caci_2 = str_to_upper(caci_2)
  )

############################################################
# 4. PREPARE EXTERNAL CACI DATASETS (SCA, ACV, ICC, TXC, TEP)
############################################################

# NOTE:
#  Each external CACI dataset will produce:
#    - *_2 : cleaned version
#    - with:
#       * ID as numeric string
#       * ingreso date
#       * month/year
#       * "cruce" key (ID + month + year)

## 4.1 SCA (cedula + fecha_de_ingreso)
data_sca_2 <- data_sca %>%
  clean_names() %>%
  mutate(
    fecha_de_ingreso = as.Date(fecha_de_ingreso),
    cedula           = clean_id(cedula)
  ) %>%
  filter(fecha_de_ingreso > FECHA_INICIO) %>%
  mutate(
    mes_sca = month(fecha_de_ingreso, label = TRUE),
    year    = year(fecha_de_ingreso),
    cruce   = str_c(cedula, mes_sca, year, sep = "")
  )

## 4.2 ACV (Excel date numeric)
data_acv_2 <- data_acv %>%
  clean_names() %>%
  mutate(
    fecha_ingreso = as.numeric(fecha_ingreso),
    fecha_ingreso = as.Date(fecha_ingreso, origin = "1899-12-30"),
    cedula        = clean_id(cedula)
  ) %>%
  filter(
    fecha_ingreso > FECHA_INICIO,
    fecha_ingreso < as.Date("2025-10-01")
  ) %>%
  mutate(
    mes_acv = month(fecha_ingreso, label = TRUE),
    year    = year(fecha_ingreso),
    cruce   = str_c(cedula, mes_acv, year, sep = "")
  )

## 4.3 ICC (nota: había un bug en el regex del ID)
data_icc_2 <- data_icc %>%
  clean_names() %>%
  mutate(
    fecha_de_ingreso        = as.Date(fecha_de_ingreso),
    numero_de_identificacion = clean_id(numero_de_identificacion)
  ) %>%
  filter(fecha_de_ingreso > FECHA_INICIO) %>%
  mutate(
    mes_icc = month(fecha_de_ingreso, label = TRUE),
    year    = year(fecha_de_ingreso),
    cruce   = str_c(numero_de_identificacion, mes_icc, year, sep = "")
  )

## 4.4 TXC
data_txc_2 <- data_txc %>%
  clean_names() %>%
  mutate(
    fecha_de_ingreso        = as.Date(fecha_de_ingreso),
    numero_de_identificacion = clean_id(numero_de_identificacion)
  ) %>%
  filter(fecha_de_ingreso > FECHA_INICIO) %>%
  mutate(
    mes_txc = month(fecha_de_ingreso, label = TRUE),
    year    = year(fecha_de_ingreso),
    cruce   = str_c(numero_de_identificacion, mes_txc, year, sep = "")
  )

## 4.5 TEP (ingreso a UCI/UCIN)
data_tep_2 <- data_tep %>%
  clean_names() %>%
  mutate(
    fecha_de_ingreso_a_uci_ucin = as.Date(fecha_de_ingreso_a_uci_ucin),
    numero_de_identificacion    = clean_id(numero_de_identificacion)
  ) %>%
  filter(fecha_de_ingreso_a_uci_ucin > FECHA_INICIO) %>%
  mutate(
    mes_tep = month(fecha_de_ingreso_a_uci_ucin, label = TRUE),
    year    = year(fecha_de_ingreso_a_uci_ucin),
    cruce   = str_c(numero_de_identificacion, mes_tep, year, sep = "")
  )

############################################################
# 5. ENRICH ADMISSIONS WITH CACI (USING YOUR CLASSIFIER + EXTERNAL FILES)
############################################################

# 5.1 Base admissions + CACI algorithm
data_ingresos_3 <- result_2 %>%
  mutate(
    caci_2 = str_to_upper(caci_2),
    cruce  = str_c(documento, mes, año, sep = "")
  )

# Helper function:
#   Join admissions (data_ingresos_3) with one external CACI dataset
join_caci_group <- function(adm_df, caci_df, group_code,
                            cruce_col = "cruce", diag_col, mes_caci_col,
                            tipo_diag_name = "diagnostico") {
  # adm_df: admissions with caci_2 and cruce
  # caci_df: external validated CACI file
  # group_code: "SCA", "ACV", "ICC", "TXC", "TEP"
  # diag_col: column in caci_df with diagnosis/EAPB/género/etc.
  # mes_caci_col: column in caci_df with month label
  
  # 1) Merge by cruce
  merged <- left_join(
    adm_df,
    caci_df %>% select({{ cruce_col }}, {{ diag_col }}, {{ mes_caci_col }}),
    by = setNames("cruce", cruce_col)
  )
  
  # 2) Patients found by your own criteria (caci_2 + filters)
  propios <- merged %>%
    filter(
      caci_2 == group_code,
      tipo_de_atencion == "HOSPITALARIO"
    ) %>%
    filter(
      str_detect(departamento_actual, SERVICIOS_INTERNACION_RE) |
        str_detect(departamento_de_ingreso, SERVICIOS_INTERNACION_RE)
    ) %>%
    mutate(
      departamento_filtro = str_c(departamento_de_ingreso, departamento_actual, sep = " "),
      dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)
    ) %>%
    filter(
      !str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS"),
      dif_days > 1
    )
  
  # 3) Patients present in the CACI registry (validated external file)
  validados <- merged %>%
    filter(!is.na({{ mes_caci_col }})) %>%
    distinct(numero_de_cuenta, fecha_ingreso, documento, .keep_all = TRUE) %>%
    filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>%
    mutate(
      departamento_filtro = str_c(departamento_de_ingreso, departamento_actual, sep = " "),
      dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)
    ) %>%
    filter(
      !str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS")
    )
  
  # 4) Union + deduplicate
  out <- bind_rows(propios, validados) %>%
    distinct(
      documento, fecha_ingreso, fecha_de_egreso,
      numero_de_cuenta, numero_de_ingreso,
      .keep_all = TRUE
    ) %>%
    mutate(
      caci_3 = if_else(
        is.na({{ diag_col }}),
        caci_2,
        group_code
      ),
      diagnostico_caci = as.character({{ diag_col }}),
      mes_caci         = {{ mes_caci_col }}
    )
  
  out
}

######## 5.2 SCA ########
data_ingresos_sca_3 <- join_caci_group(
  adm_df       = data_ingresos_3,
  caci_df      = data_sca_2,
  group_code   = "SCA",
  cruce_col    = "cruce",
  diag_col     = diagnostico,
  mes_caci_col = mes_sca
)

######## 5.3 ACV ########
data_ingresos_acv_3 <- join_caci_group(
  adm_df       = data_ingresos_3,
  caci_df      = data_acv_2,
  group_code   = "ACV",
  cruce_col    = "cruce",
  diag_col     = dignostico,   # nombre original en tu archivo
  mes_caci_col = mes_acv
) %>%
  rename(diagnostico_caci = dignostico)

######## 5.4 ICC ########

# Ajuste tipos para join por numero_de_cuenta si lo necesitas
data_icc_2 <- data_icc_2 %>%
  mutate(
    numero_de_identificacion = as.character(numero_de_identificacion),
    numero_de_cuenta         = as.character(numero_de_cuenta)
  )

# Merge baseline with ICC registry
data_ingresos_icc <- left_join(
  data_ingresos_3,
  data_icc_2 %>% select(numero_de_cuenta, mes_icc, eapb),
  by = "numero_de_cuenta"
)

data_ingresos_icc_2 <- data_ingresos_icc %>%
  filter(!is.na(eapb)) %>%
  distinct(fecha_de_egreso, fecha_ingreso, numero_de_ingreso, .keep_all = TRUE) %>%
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>%
  mutate(
    departamento_filtro = str_c(departamento_de_ingreso, departamento_actual, sep = " "),
    dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)
  ) %>%
  filter(
    !str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS")
  )

data_ingreso_icc_2.1 <- data_ingresos_icc %>%
  filter(caci_2 == "ICC", tipo_de_atencion == "HOSPITALARIO") %>%
  filter(
    str_detect(departamento_actual, SERVICIOS_INTERNACION_RE) |
      str_detect(departamento_de_ingreso, SERVICIOS_INTERNACION_RE)
  ) %>%
  mutate(
    departamento_filtro = str_c(departamento_de_ingreso, departamento_actual, sep = " "),
    dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)
  ) %>%
  filter(
    !str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS"),
    dif_days > 1
  )

data_ingresos_icc_3 <- bind_rows(data_ingreso_icc_2.1, data_ingresos_icc_2) %>%
  distinct(documento, fecha_ingreso, fecha_de_egreso, numero_de_cuenta,
           numero_de_ingreso, .keep_all = TRUE) %>%
  mutate(
    caci_3          = if_else(is.na(eapb), caci_2, "ICC"),
    diagnostico_caci = eapb,
    mes_caci         = mes_icc
  )

######## 5.5 TXC ########
data_ingresos_txc <- left_join(
  data_ingresos_3,
  data_txc_2 %>%
    mutate(numero_de_identificacion = as.character(numero_de_identificacion)) %>%
    select(numero_de_identificacion, eapb, mes_txc),
  by = c("documento" = "numero_de_identificacion")
)

data_ingresos_txc_3 <- data_ingresos_txc %>%
  filter(!is.na(eapb)) %>%
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>%
  mutate(
    departamento_filtro = str_c(departamento_de_ingreso, departamento_actual, sep = " "),
    dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)
  ) %>%
  filter(
    !str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS")
  ) %>%
  distinct(fecha_ingreso, fecha_de_egreso, numero_de_cuenta, .keep_all = TRUE) %>%
  mutate(
    caci_3          = if_else(is.na(eapb), caci_2, "TXC"),
    diagnostico_caci = eapb,
    mes_caci         = mes_txc
  )

######## 5.6 TEP ########
data_ingresos_tep <- left_join(
  data_ingresos_3,
  data_tep_2 %>%
    mutate(numero_de_identificacion = as.character(numero_de_identificacion)) %>%
    select(numero_de_identificacion, genero, mes_tep),
  by = c("documento" = "numero_de_identificacion")
)

data_ingresos_tep_3 <- data_ingresos_tep %>%
  filter(!is.na(genero)) %>%
  filter(caci_2 == "TEP", tipo_de_atencion == "HOSPITALARIO") %>%
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>%
  mutate(
    departamento_filtro = str_c(departamento_de_ingreso, departamento_actual, sep = " "),
    dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)
  ) %>%
  filter(
    !str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS")
  ) %>%
  distinct(fecha_ingreso, fecha_de_egreso, numero_de_cuenta, .keep_all = TRUE) %>%
  mutate(
    caci_3          = if_else(is.na(mes_tep), caci_2, "TEP"),
    diagnostico_caci = genero,
    mes_caci         = mes_tep
  )

############################################################
# 6. UNIFY CACI ADMISSIONS (GRD BASE)
############################################################

data_grd <- bind_rows(
  data_ingresos_tep_3,
  data_ingresos_sca_3,
  data_ingresos_acv_3,
  data_ingresos_icc_3,
  data_ingresos_txc_3
) %>%
  distinct(documento, numero_de_ingreso, numero_de_cuenta, total_cuenta,
           .keep_all = TRUE)

# Optional manual corrections (you already had these)
data_grd_2 <- data_grd %>%
  mutate(
    caci_3 = case_when(
      numero_de_cuenta %in% c("687896", "693686") ~ "TXC",
      TRUE                                        ~ caci_3
    ),
    dif_days = if_else(
      is.na(dif_days),
      as.numeric(as.Date("2025-06-30") - fecha_ingreso),
      dif_days
    )
  )

############################################################
# 7. SALES (ORDERS) – MATCH WITH CACI ADMISSIONS
############################################################

# 7.1 Clean sales (CARGOS)
data_ordenes_2 <- data_sales %>%
  clean_names() %>%
  mutate(
    cod_cargo = as.character(cod_cargo),
    across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y")),
    mes_cargue = month(fecha_cargue, label = TRUE),
    ingreso     = as.character(ingreso),
    cuenta      = as.character(cuenta)
  ) %>%
  filter(tipo_registro == "CARGOS")

# 7.2 Add CACI info to orders by numero_de_ingreso
data_grd_2 <- data_grd_2 %>%
  mutate(
    numero_de_ingreso = as.character(numero_de_ingreso),
    mes_egreso        = month(fecha_de_egreso, label = TRUE, abbr = TRUE),
    año_egreso        = year(fecha_de_egreso),
    cruce_2           = str_c(documento, mes_egreso, año_egreso, sep = "")
  )

data_ordenes_3 <- left_join(
  data_ordenes_2,
  data_grd_2 %>% select(numero_de_ingreso, caci_3),
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(
    identificacion, cuenta, fecha_cargue, cargo,
    valor_cargo_tarifario, total_cuenta, costo, transaccion,
    fecha_registro, ingreso, .keep_all = TRUE
  )

# 7.3 Orders that matched CACI
data_ordenes_2.2 <- data_ordenes_3 %>%
  filter(!is.na(caci_3))

# 7.4 Orders that did NOT match – second pass using (identificacion + mes/año egreso)
data_revision_ordenes <- left_join(
  data_grd_2,
  data_ordenes_2 %>% select(ingreso, cargo),
  by = c("numero_de_ingreso" = "ingreso")
) %>%
  filter(is.na(cargo)) %>%
  select(-cargo, -cruce) %>%
  mutate(
    mes_egreso = month(fecha_de_egreso, label = TRUE, abbr = TRUE),
    año_egreso = year(fecha_de_egreso),
    cruce_2    = str_c(documento, mes_egreso, año_egreso, sep = "")
  )

data_ordenes_2.1 <- data_ordenes_2 %>%
  mutate(
    mes_egreso = month(fecha_egreso, label = TRUE, abbr = TRUE),
    año_egreso = year(fecha_egreso),
    cruce_2    = str_c(identificacion, mes_egreso, año_egreso, sep = "")
  )

data_ordenes_3.1 <- left_join(
  data_ordenes_2.1,
  data_revision_ordenes %>% select(cruce_2, caci_3),
  by = "cruce_2"
) %>%
  filter(!is.na(caci_3)) %>%
  distinct(
    identificacion, cuenta, fecha_cargue, cargo,
    valor_cargo_tarifario, total_cuenta, costo, transaccion,
    fecha_registro, ingreso, .keep_all = TRUE
  ) %>%
  select(-mes_egreso, -año_egreso, -cruce_2)

# 7.5 Final orders dataset with CACI
data_ordenes_4 <- bind_rows(data_ordenes_2.2, data_ordenes_3.1) %>%
  mutate(
    cod_cargo  = as.character(cod_cargo),
    cod_cargo_2 = str_extract(cod_cargo, ".*(?=-)"),
    cod_cargo_2 = if_else(is.na(cod_cargo_2), cod_cargo, cod_cargo_2)
  )

############################################################
# 8. COST TABLE – JOIN WITH ORDERS
############################################################

data_costo_2025 <- data_costo_2025 %>%
  clean_names() %>%
  mutate(across(c("codigo", "codigo_2"), as.character))

# NOTE: your original used "data_costo_2024" by mistake; here we keep 2025 object name
data_orders <- left_join(
  data_ordenes_4,
  data_costo_2025 %>% select(codigo, valor),
  by = c("cod_cargo_2" = "codigo")
) %>%
  distinct(
    identificacion, cuenta, fecha_cargue, cargo,
    valor_cargo_tarifario, total_cuenta, costo, transaccion,
    fecha_registro, ingreso, .keep_all = TRUE
  ) %>%
  mutate(
    valor = replace_na(valor, 0)
  )

############################################################
# 9. MEDICATIONS (FARMACIA) – MATCH WITH CACI
############################################################

# 9.1 Filter FARMACIA from sales
data_mmto_2 <- data_sales %>%
  clean_names() %>%
  mutate(
    cod_cargo = as.character(cod_cargo),
    across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y"))
  ) %>%
  filter(tipo_registro == "FARMACIA")

data_mmto_3 <- data_mmto_2 %>%
  mutate(
    fecha_adm = as.Date(fecha_registro),
    cuenta    = as.character(cuenta),
    ingreso   = as.character(ingreso)
  )

data_grd_2 <- data_grd_2 %>%
  mutate(numero_de_ingreso = as.character(numero_de_ingreso))

# 9.2 Join meds with CACI admissions
data_mmto_4 <- left_join(
  data_mmto_3,
  data_grd_2 %>% select(numero_de_ingreso, caci_3),
  by = c("ingreso" = "numero_de_ingreso")
)

data_mmto_4_1 <- data_mmto_4 %>%
  distinct(cargo, fecha_registro, fecha_cargue, factura, fecha_adm, .keep_all = TRUE) %>%
  filter(!is.na(caci_3))

# 9.3 Final meds dataset with numeric cost
data_mmto_5 <- data_mmto_4_1 %>%
  mutate(
    mes    = month(fecha_adm, label = TRUE),
    costo  = as.character(costo),
    costo_2 = costo %>% str_replace_all(",", "") %>% as.numeric()
  )

############################################################
# 10. BUILD FINAL COST DATASETS (ORDERS + MEDS)
############################################################

# 10.1 Medications final view (for binding later)
data_mmto_final <- data_mmto_5 %>%
  select(
    departamento_cargue, fecha_factura, nombres, identificacion,
    cod_cargo, cargo, fecha_adm, cuenta,
    mes, caci_3, costo_2
  ) %>%
  mutate(
    medico              = NA_character_,
    departamento_cargue_2 = "FARMACIA",
    fecha_cargue        = fecha_factura, # align with orders
    valor_cargo_tarifario = NA_character_,
    transaccion         = NA_character_
  )

# 10.2 Orders (non-pharma) final view
data_ordenes_final <- data_orders %>%
  mutate(mes_factura = month(fecha_factura, label = TRUE)) %>%
  select(
    nombre_cliente, tipo_cliente, plan, departamento_cargue, fecha_registro,
    id, nombres, identificacion, cod_cargo, cargo, fecha_cargue, ingreso,
    mes_factura, caci_3, cuenta, transaccion, valor, valor_cargo_tarifario,
    profesional_asignado
  ) %>%
  mutate(
    departamento_cargue_2 = departamento_cargue,
    costo_2               = valor
  )

# 10.3 Unified cost dataset (meds + other services)
data_costo_total <- bind_rows(
  data_mmto_final,
  data_ordenes_final
) %>%
  group_by(caci_3) %>%
  ungroup()

############################################################
# 11. ADD PATIENT CHARACTERISTICS FROM GRD
############################################################

data_costo_total_2 <- left_join(
  data_costo_total,
  data_grd_2 %>%
    select(
      edad, sexo, departamento_actual, numero_de_ingreso,
      estado_al_alta, dif_days,
      fecha_ingreso, fecha_de_egreso,
      mes_caci, diagnostico_ingreso_princial,
      diagnostico_egreso_principal,
      diagnostico_egreso_secundario, procedimiento_qx,
      documento
    ),
  by = c("ingreso" = "numero_de_ingreso")
)

# 11.2 Remove duplicates to reduce noise
data_costo_total_2 <- data_costo_total_2 %>%
  distinct(
    cuenta, departamento_cargue, cod_cargo, departamento_actual, dif_days,
    fecha_registro, cargo, id, identificacion, ingreso, estado_al_alta,
    fecha_de_egreso, fecha_ingreso, costo_2, fecha_cargue,
    valor_cargo_tarifario, diagnostico_ingreso_princial,
    diagnostico_egreso_secundario, .keep_all = TRUE
  )

############################################################
# 12. HANDLE RECORDS WITHOUT ADMISSION INFO (SECOND MATCH BY ID)
############################################################

data_costo_total_2.1 <- data_costo_total_2 %>%
  filter(!is.na(departamento_actual))

data_costo_total_2.2 <- data_costo_total_2 %>%
  filter(is.na(departamento_actual)) %>%
  select(
    -edad, -sexo, -departamento_actual,
    -estado_al_alta, -dif_days,
    -fecha_ingreso, -fecha_de_egreso,
    -mes_caci, -diagnostico_ingreso_princial,
    -diagnostico_egreso_principal,
    -diagnostico_egreso_secundario, -procedimiento_qx
  )

data_costo_total_2.2 <- left_join(
  data_costo_total_2.2,
  data_grd_2 %>%
    select(
      edad, sexo, departamento_actual, documento,
      estado_al_alta, dif_days,
      fecha_ingreso, fecha_de_egreso,
      mes_caci, diagnostico_ingreso_princial,
      diagnostico_egreso_principal,
      diagnostico_egreso_secundario, procedimiento_qx
    ),
  by = c("identificacion" = "documento")
) %>%
  distinct(cuenta, costo_2, transaccion, fecha_cargue,
           valor_cargo_tarifario, .keep_all = TRUE)

# 12.3 Final combined dataset
data_costo_total_2 <- bind_rows(
  data_costo_total_2.1,
  data_costo_total_2.2
) %>%
  mutate(departamento = departamento_cargue)

############################################################
# 13. ADD SALES VALUE, TIME VARIABLES, FILTER BY DATE
############################################################

data_costo_total_2.1 <- data_costo_total_2 %>%
  mutate(
    venta = valor_cargo_tarifario %>%
      str_replace_all(",", "") %>%
      as.numeric()
  )

data_costo_total_3 <- data_costo_total_2.1 %>%
  mutate(
    mes        = month(fecha_ingreso, label = TRUE),
    mes_egreso = month(fecha_de_egreso, label = TRUE),
    mes_cargue = month(fecha_cargue, label = TRUE),
    año        = year(fecha_cargue) %>% as.factor()
  ) %>%
  filter(fecha_cargue < as.Date("2025-10-01"))

############################################################
# 14. SAVE MAIN ANALYSIS DATASETS
############################################################

export(data_costo_total_3, "data_costo_total_3_2025_II.rds")
export(data_costo_total_3, "data_costo_total_3_2025_I.xlsx")
export(data_grd_2,        "data_grd_2_2025_II.rda")

############################################################
# 15. QUICK QA EXAMPLES (OPTIONAL – KEEP OR MOVE TO SEPARATE SCRIPT)
############################################################

# 15.1 Number of unique patients by CACI in final dataset
data_costo_total_3 %>%
  group_by(caci_3) %>%
  summarise(
    pacientes = n_distinct(identificacion),
    .groups = "drop"
  )

# 15.2 Basic cost distribution by CACI and month (example table)
tabla_costo_mensual <- data_costo_total_3 %>%
  group_by(mes_cargue, caci_3) %>%
  summarise(
    costo_total = sum(costo_2, na.rm = TRUE),
    venta_total = sum(venta, na.rm = TRUE),
    pacientes   = n_distinct(identificacion),
    .groups = "drop"
  ) %>%
  flextable()

tabla_costo_mensual