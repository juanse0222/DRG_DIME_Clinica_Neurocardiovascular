################################################################################
# INTEGRATED CLINICAL & FINANCIAL PIPELINE — analysis_update_2026.R
# DIME Clínica Neurocardiovascular
#
# Pipeline steps:
#   1.  Load & update admission data (2024–current)
#   2.  Load & update sales data
#   3.  Classify patients by CACI using the canonical function
#   4.  Filter hospitalisations
#   5.  Split multi-account records
#   6.  Match orders (CARGOS) to CACI patients
#   7.  Recover unmatched orders via identity fallback
#   8.  Load planning costs (CUPS values)
#   9.  Load pharmacy records (FARMACIA)
#   10. Build unified cost + sales dataset
#   11. Enrich with patient demographics
#   12. Compute financial summary tables (cost, sales, margin)
#   13. Export results to output/
#
# [CHANGED]: Removed duplicate pacman::p_load call.
# [CHANGED]: Removed inline copy of classify_caci() — now sourced from function file.
# [CHANGED]: Fixed previous-month filename construction (month(month(today())-1) was wrong).
# [CHANGED]: Fixed caci_2 → caci_3 reference in data_grd_2 mutation (line ~14 of old step 14).
# [CHANGED]: Fixed año == "2026" to numeric comparison.
# [CHANGED]: Fixed ip_caci TEP → "SCA" bug (now in function file).
# [CHANGED]: Standardised CACI labels to lowercase throughout ("txc" not "TX").
# [CHANGED]: venta column explicitly coerced to numeric after gsub.
# [ADDED]:   dir.create() guard for output/ folder.
# [ADDED]:   Informative message() calls at each major step.
# [ADDED]:   pacientes_tep columns included in data_costo_total_5.
#
# ⚠ SUPERSEDED (2026-08-25): el flujo mensual vigente es
#     Rscript scripts/refresh_sales_month.R "data/sales data/sales_<Mon>_<año>.csv"
#     Rscript scripts/run_master_monthly.R <YYYY-MM>
#   run_master_monthly.R corre pipeline_grd.R, NO este script. Se conserva como
#   referencia. [CHANGED]: Step 2 ya no escribe ningún acumulado de ventas — sólo
#   lee data_sales_2024_2025_<year>.rds. Ver la nota en Step 2.
################################################################################

pacman::p_load(
  tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms,
  epikit, scales, gt, zoo, readxl, here, officer
)

# [ADDED]: Source canonical CACI classifier
source(here("scripts", "function_diagnosis_algorithm_caci.R"))

# [ADDED]: Ensure output directory exists
dir.create(here("output"), showWarnings = FALSE)

# ── Shared constants ──────────────────────────────────────────────────────────
# [CHANGED]: Compute previous-month label correctly using lubridate arithmetic
prev_month_date  <- floor_date(today(), "month") - days(1)   # last day of previous month
prev_month_long  <- format(prev_month_date, "%B")            # e.g. "abril"
prev_month_abbr  <- format(prev_month_date, "%b")            # e.g. "abr"  (locale-aware)
prev_month_year  <- year(prev_month_date)
current_year     <- year(today())

message(sprintf("[PIPELINE] Starting. Reporting month: %s %d", prev_month_long, prev_month_year))

################################################################################
# 1. ADMISSION DATA (2024–current)
################################################################################
message("[STEP 1] Loading admission data...")

admission_rds <- here("data", "admission_data",
                      paste0("data_a_2024_2025_", current_year, ".rds"))

admission_rds <- here("data", "admission_data",
                      paste0("data_a_2024_2025", ".rds"))

data_admission <- import(admission_rds)

# [CHANGED]: Fixed filename — prev_month_long gives full Spanish month name
admission_xls <- here("data", "admission_data",
                      paste0("admission_", prev_month_long, "_", prev_month_year, ".xls"))


if (!file.exists(admission_xls)) {
  warning(sprintf("[STEP 1] New admission file not found: %s — skipping update.", admission_xls))
  data_admission_2 <- data_admission
} else {
  data_uploaded <- import(admission_xls, skip = 2) %>%
    clean_names() %>%
    select(-any_of(c("x19", "x28"))) %>% 
    mutate(across(
      c(numero_de_ingreso, edad, estancia_horas, valor_factura, total_cuenta),
      as.character
    ))
  

  data_admission_2 <- bind_rows(data_admission, data_uploaded) %>%
    distinct(documento, paciente, fecha_de_egreso, fecha_ingreso, numero_de_cuenta, .keep_all = TRUE) 
  
  export(data_admission_2, here("data", "admission_data", "data_a_2024_2025_2026.rds"))
  
  
  data_admission_2 <- data_admission_2  %>% 
    filter(tipo_de_atencion == "HOSPITALARIO" & str_detect(departamento_actual, "UCI|UCIN|HOSPIT") | (departamento_actual == "URGENCIAS" & estancia_horas > 47)) 

  export(data_admission_2, here("data", "admission_data", "data_a_hosp_2024_2026.rds"))
  message(sprintf("[STEP 1] Admission file updated. Total rows: %d", nrow(data_admission_2)))
}

################################################################################
# 2. SALES DATA (2024–current)
################################################################################
message("[STEP 2] Loading sales data...")

# [CHANGED 2026-08-25]: este paso leía data_sales_2024_<year>.rds, anexaba el CSV
#   del mes con rbind() SIN deduplicar y exportaba un acumulado propio con el
#   nombre antiguo data_sales_2024_2026.rds. Volver a correrlo sobre un mes ya
#   cargado duplicaba las filas: junio 2026 terminó TRIPLICADO (191.934 filas /
#   63.978 transacciones únicas), lo que infló n_dispensaciones en el tablero
#   PROA, que leía ese mismo archivo.
#
#   Este script quedó superseded por pipeline_grd.R (es el que corre
#   run_master_monthly.R), así que ya no debe escribir ningún acumulado: aquí
#   sólo se LEE el acumulado canónico data_sales_2024_2025_<year>.rds.
#
#   La ingesta del mes se hace ANTES, con el script que sí deduplica por
#   `transaccion` y hace backup del acumulado:
#     Rscript scripts/refresh_sales_month.R "data/sales data/sales_<Mon>_<año>.csv"

sales_rds <- here("data", "sales data",
                  paste0("data_sales_2024_2025_", current_year, ".rds"))

if (!file.exists(sales_rds))
  stop("[STEP 2] Acumulado de ventas no encontrado: ", sales_rds, call. = FALSE)

data_sales_2 <- import(sales_rds) %>% clean_names()

# Guardarraíl: avisar si el acumulado todavía no cubre el mes que se reporta,
# en vez de producir cifras silenciosamente incompletas.
ultima_venta <- max(as.Date(data_sales_2$fecha_cargue), na.rm = TRUE)
if (ultima_venta < prev_month_date) {
  warning(sprintf(
    paste0("[STEP 2] El acumulado llega a %s pero se reporta %s %d.\n",
           "  Corre primero: Rscript scripts/refresh_sales_month.R ",
           "\"data/sales data/sales_%s_%d.csv\""),
    format(ultima_venta, "%Y-%m-%d"), prev_month_long, prev_month_year,
    prev_month_abbr, prev_month_year))
}

message(sprintf("[STEP 2] Sales accumulator loaded. Rows: %d | transacciones únicas: %d | hasta %s",
                nrow(data_sales_2), n_distinct(data_sales_2$transaccion),
                format(ultima_venta, "%Y-%m-%d")))


################################################################################
# 3. CACI CLASSIFICATION
################################################################################
message("[STEP 3] Running CACI classifier...")

# [CHANGED]: classify_caci() now sourced from function file; no inline copy
result <- classify_caci(data_admission_2)

result_2 <- result %>%
  mutate(caci_3 = caci_final)   # already lowercase from function


export(result_2, here("data", "results_2.rds"))
message(sprintf("[STEP 3] Classification done. Rows with CACI: %d / %d",
                sum(!is.na(result_2$caci_final)), nrow(result_2)))

################################################################################
# 4. FILTER HOSPITALISATIONS & CURATE ADMISSION TABLE
################################################################################
message("[STEP 4] Filtering hospitalisations...")

data_grd_2 <- result_2 %>%
#  filter(tipo_de_atencion == "HOSPITALARIO") %>%
#  filter(str_detect(departamento_actual, "HOSPITA|UCI|UCIN|URGE")) %>%
  filter(!is.na(diag_eg_pr)) %>%
  mutate(
    dif_days = as.numeric(as.Date(fecha_de_egreso) - as.Date(fecha_ingreso)),
    mes      = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año      = year(fecha_ingreso),
    # [CHANGED]: was referencing non-existent caci_2; correct column is caci_3
    caci_3   = if_else(
      str_detect(coalesce(diag_eg_pr, ""), "TUMOR|CANCER|METASTAS") & !is.na(caci_3),
      NA_character_,
      caci_3
    )
  )

message(sprintf("[STEP 4] Hospitalisation rows: %d", nrow(data_grd_2)))
data_grd_2 %>% count(caci_3) %>% print()

################################################################################
# 5. SPLIT MULTI-ACCOUNT RECORDS
################################################################################
message("[STEP 5] Splitting multi-account records...")

data_grd_3 <- data_grd_2 %>%
  mutate(numero_de_cuenta = str_trim(numero_de_cuenta)) %>%
  separate(numero_de_cuenta,
           into  = c("numero_de_cuenta_1", "numero_de_cuenta_2"),
           sep   = "\\|\\|",
           extra = "drop",
           fill  = "right") %>%
  mutate(across(starts_with("numero_de_cuenta"), str_trim))

data_grd_4 <- data_grd_3 %>%
  filter(!is.na(numero_de_cuenta_2)) %>%
  mutate(numero_de_cuenta = numero_de_cuenta_2) %>%
  select(-c(numero_de_cuenta_2, numero_de_cuenta_1))

data_grd_3 <- data_grd_3 %>%
  select(-numero_de_cuenta_2) %>%
  rename(numero_de_cuenta = numero_de_cuenta_1)

data_grd_5 <- bind_rows(data_grd_3, data_grd_4) %>%
  mutate(
    numero_de_ingreso = as.character(numero_de_ingreso),
    mes_ingreso  = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año_ingreso  = year(fecha_ingreso),
    cruce_2      = paste0(documento, mes_ingreso, año_ingreso)
  )

################################################################################
# 6. MATCH ORDERS (CARGOS) TO CACI PATIENTS
################################################################################
message("[STEP 6] Matching orders to CACI patients...")

data_ordenes_2 <- data_sales_2 %>%
  clean_names() %>%
  mutate(mes_cargue = month(fecha_cargue, label = TRUE),
         ingreso    = as.character(ingreso),
         cuenta     = as.character(cuenta), 
         año_cargue = year(fecha_cargue)) %>%
  filter(tipo_registro == "CARGOS")

data_ordenes_3 <- left_join(
  data_ordenes_2,
  select(data_grd_5, numero_de_ingreso, caci_3),
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario,
           total_cuenta, costo, transaccion, fecha_registro, ingreso, .keep_all = TRUE)

################################################################################
# 7. RECOVER UNMATCHED ORDERS VIA IDENTITY FALLBACK
################################################################################
message("[STEP 7] Recovering unmatched orders...")

data_ordenes_2.2 <- data_ordenes_3 %>% filter(!is.na(caci_3))

data_revision_ordenes <- left_join(
  data_grd_5,
  select(data_ordenes_2, ingreso, cargo),
  by = c("numero_de_ingreso" = "ingreso")
) %>%
  filter(is.na(cargo)) %>%
  select(-cargo) %>%
  mutate(
    mes_egreso  = month(fecha_de_egreso, label = TRUE, abbr = TRUE),
    año_egreso  = year(fecha_de_egreso),
    cruce_2     = paste0(documento, mes_egreso, año_egreso)
  ) %>%
  # [CHANGED]: original had filter(fecha_ingreso >= "2025-12-31") which filtered everything out;
  #            changed to include recent records for the current reporting year
  filter(year(fecha_ingreso) >= (current_year - 1))

data_ordenes_2.1 <- data_ordenes_2 %>%
  mutate(
    mes_egreso  = month(fecha_cargue, label = TRUE, abbr = TRUE),
    año_egreso  = year(fecha_cargue),
    cruce_2     = paste0(identificacion, mes_egreso, año_egreso)
  )

data_ordenes_3.1 <- left_join(
  data_ordenes_2.1,
  select(data_revision_ordenes, cruce_2, caci_3),
  by = "cruce_2"
) %>%
  filter(!is.na(caci_3)) %>%
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario,
           total_cuenta, costo, transaccion, fecha_registro, ingreso, .keep_all = TRUE) %>%
  select(-c(mes_egreso, año_egreso, cruce_2))

data_ordenes_4 <- bind_rows(data_ordenes_2.2, data_ordenes_3.1) %>%
  mutate(
    cod_cargo   = as.character(cod_cargo),
    cod_cargo_2 = str_extract(cod_cargo, ".*(?=-)"),
    cod_cargo_2 = if_else(is.na(cod_cargo_2), cod_cargo, cod_cargo_2),
    code = paste(cod_cargo_2, año_cargue, sep = "_")
  )

message(sprintf("[STEP 7] Orders matched: %d rows, %d unique CACI patients",
                nrow(data_ordenes_4),
                n_distinct(data_ordenes_4$identificacion)))

################################################################################
# 8. PLANNING COSTS (CUPS)
################################################################################
message("[STEP 8] Loading planning costs...")

data_costo_2026 <- import(
  here("data", "data_costs", "costo_general_2026.xlsx"),
  which = "2026"
) %>%
  clean_names() %>%
  mutate(across(c("codigo_dime", "codigo"), as.character),
         año = "2026",
         code = paste(codigo_dime, año, sep = "_"))

data_costo_2025 <- import(
  here("data", "data_costs", "costo_general_2026.xlsx"),
  which = "2025"
) %>%
  clean_names() %>%
  mutate(across(c("codigo_dime", "codigo"), as.character),
         año = "2025", 
         code = paste(codigo_dime, año, sep = "_"))

data_costo_2024 <- import(
  here("data", "data_costs", "costo_general_2026.xlsx"),
  which = "2024"
) %>%
  clean_names() %>%
  mutate(across(c("codigo_dime", "codigo"), as.character),
         año = "2024",
         code = paste(codigo_dime, año, sep = "_"))

data_costo <- rbind(data_costo_2024, data_costo_2025, data_costo_2026) %>% 
  rename("costo_2" = costo)

data_orders <- left_join(
  data_ordenes_4,
  select(data_costo, codigo, code, costo_2),
  by = "code"
) %>%
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario,
           total_cuenta, costo_2, transaccion, fecha_registro, ingreso, .keep_all = TRUE) %>%
  mutate(costo_2 = replace_na(costo_2, 0))

################################################################################
# 9. PHARMACY (FARMACIA)
################################################################################
message("[STEP 9] Loading pharmacy records...")

data_mmto_2 <- data_sales_2 %>%
  clean_names() %>%
  mutate(
    cod_cargo = as.character(cod_cargo),
    across(c(fecha_cargue, fecha_registro), dmy_hm)
  ) %>%
  filter(tipo_registro == "FARMACIA")

data_mmto_3 <- data_mmto_2 %>%
  mutate(
    fecha_adm = fecha_cargue,
    cuenta    = as.character(cuenta),
    ingreso   = as.character(ingreso)
  )

data_mmto_4 <- left_join(
  data_mmto_3,
  select(data_grd_2, numero_de_ingreso, caci_3),
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(cargo, fecha_registro, fecha_cargue, factura, fecha_adm, .keep_all = TRUE) %>%
  filter(!is.na(caci_3))

data_mmto_5 <- data_mmto_4 %>%
  mutate(
    mes      = month(fecha_adm, label = TRUE),
    # [CHANGED]: coerce costo to numeric in one step; original had intermediate character step
    costo_2  = as.numeric(gsub(",", "", as.character(costo)))
  )

################################################################################
# 10. UNIFIED COST + SALES DATASET
################################################################################
message("[STEP 10] Building unified cost dataset...")

data_mmto_final <- data_mmto_5 %>%
  mutate(mes_cargue = month(fecha_cargue, label = TRUE)) %>%
  select(nombre_cliente, tipo_cliente, plan, departamento_cargue, fecha_registro,
         id, nombres, identificacion, cod_cargo, cargo, fecha_cargue, ingreso,
         mes_cargue, caci_3, cuenta, transaccion, costo_2, valor_cargo_tarifario,
         profesional_asignado) %>%
  mutate(
    profesional_asignado = NA_character_,
    departamento_cargue_2 = "FARMACIA"
  )

data_ordenes_final <- data_orders %>%
  mutate(mes_cargue = month(fecha_cargue, label = TRUE)) %>%
  select(nombre_cliente, tipo_cliente, plan, departamento_cargue, fecha_registro,
         id, nombres, identificacion, cod_cargo, cargo, fecha_cargue, ingreso,
         mes_cargue, caci_3, cuenta, transaccion, costo_2, valor_cargo_tarifario,
         profesional_asignado) %>%
  mutate(departamento_cargue_2 = "ORDENES")

data_costo_total <- bind_rows(data_mmto_final, data_ordenes_final)

################################################################################
# 11. ENRICH WITH PATIENT DEMOGRAPHICS
################################################################################
message("[STEP 11] Enriching with patient demographics...")

demog_cols <- c("edad", "sexo", "departamento_actual", "numero_de_ingreso",
                "estado_al_alta", "dif_days", "fecha_ingreso", "fecha_de_egreso",
                "diagnostico_ingreso_princial", "diagnostico_egreso_principal",
                "diagnostico_egreso_secundario", "procedimiento_qx")

data_costo_total_2 <- left_join(
  data_costo_total,
  select(data_grd_2, all_of(demog_cols)),
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(cuenta, departamento_cargue, cod_cargo, departamento_actual, dif_days,
           fecha_registro, cargo, id, identificacion, ingreso, estado_al_alta,
           fecha_de_egreso, fecha_ingreso, costo_2, fecha_cargue, valor_cargo_tarifario,
           diagnostico_ingreso_princial, transaccion, diagnostico_egreso_secundario,
           .keep_all = TRUE)

# Rows with missing admission data → re-join by patient ID
data_costo_total_2.1 <- data_costo_total_2 %>% filter(!is.na(departamento_actual))

data_costo_total_2.2 <- data_costo_total_2 %>%
  filter(is.na(departamento_actual)) %>%
  select(-all_of(setdiff(demog_cols, "numero_de_ingreso"))) %>%
  left_join(
    select(data_grd_2, documento, all_of(setdiff(demog_cols, "numero_de_ingreso"))),
    by = c("identificacion" = "documento")
  ) %>%
  distinct(cuenta, costo_2, transaccion, fecha_cargue, valor_cargo_tarifario, .keep_all = TRUE)

data_costo_total_2 <- bind_rows(data_costo_total_2.1, data_costo_total_2.2) %>%
  mutate(departamento = departamento_cargue)

################################################################################
# 12. FINANCIAL SUMMARY PREPARATION
################################################################################
message("[STEP 12] Building financial summaries...")

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

data_costo_total_3 <- data_costo_total_2 %>%
  mutate(
    # [CHANGED]: venta now explicitly numeric after gsub (was character in old clean_curated)
    venta      = as.numeric(gsub(",", "", as.character(valor_cargo_tarifario))),
    mes        = month(fecha_ingreso, label = TRUE),
    mes_egreso = month(fecha_de_egreso, label = TRUE),
    mes_cargue = month(fecha_cargue, label = TRUE),
    # [CHANGED]: año stored as integer for numeric comparisons (was factor in some versions)
    año        = year(fecha_cargue)
  )

message(sprintf("[STEP 12] Total rows in cost dataset: %d", nrow(data_costo_total_3)))
data_costo_total_3 %>% group_by(caci_3) %>% summarise(n = n_distinct(identificacion))

### 12.1. Patient-level monthly cost by CACI
# [CHANGED]: filter uses numeric year comparison (was == "2026" string in old script)
data_costo_total_4 <- data_costo_total_3 %>%
  filter(fecha_cargue >= paste0(current_year, "-01-01")) %>%
  group_by(identificacion, mes_cargue, año, caci_3) %>%
  summarise(costo = sum(costo_2, na.rm = TRUE),
            venta = sum(venta,   na.rm = TRUE),
            .groups = "drop")

data_caci_total <- data_costo_total_3 %>%
  filter(fecha_cargue >= paste0(current_year, "-01-01")) %>%
  group_by(identificacion, mes_cargue) %>%
  summarise(Total = sum(costo_2, na.rm = TRUE), .groups = "drop")

data_caci_total_2 <- data_caci_total %>%
  group_by(mes_cargue) %>%
  summarise(Total = median(Total, na.rm = TRUE), .groups = "drop")

### 12.2. Summary table: patients, median cost, total cost by CACI and month
# [CHANGED]: pacientes_tep / mediana_tep / costo_orden_tep now included
data_costo_total_5 <- data_costo_total_4 %>%
  group_by(mes_cargue, caci_3) %>%
  summarise(
    costo_orden = sum(costo, na.rm = TRUE),
    pacientes   = n_distinct(identificacion),
    mediana     = median(costo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols     = mes_cargue,
    names_from  = caci_3,
    values_from = c(pacientes, mediana, costo_orden)
  ) %>%
  select(mes_cargue,
         any_of(c("pacientes_acv", "mediana_acv", "costo_orden_acv",
                  "pacientes_sca", "mediana_sca", "costo_orden_sca",
                  "pacientes_icc", "mediana_icc", "costo_orden_icc",
                  "pacientes_tep", "mediana_tep", "costo_orden_tep",
                  "pacientes_txc", "mediana_txc", "costo_orden_txc")))

data_costo_total_5 <- full_join(data_costo_total_5, data_caci_total_2,
                                by = "mes_cargue")

### 12.3. Profitability table: cost, sales, margin by CACI and month
data_costo_total_6 <- data_costo_total_4 %>%
  group_by(mes_cargue, caci_3) %>%
  summarise(
    costo_orden = sum(costo, na.rm = TRUE),
    venta       = sum(venta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rentabilidad   = venta - costo_orden,
    rentabilidad_2 = round(rentabilidad / venta * 100, 2)
  ) %>%
  pivot_wider(
    id_cols     = mes_cargue,
    names_from  = caci_3,
    values_from = c(costo_orden, venta, rentabilidad, rentabilidad_2)
  ) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

data_costo_total_6 %>% 
  flextable()

################################################################################
# 13. EXPORT RESULTS
################################################################################
message("[STEP 13] Exporting results...")

export(data_costo_total_3,
       here("data", paste0("data_costo_total_3_", current_year, "_II.rds")))

export(data_costo_total_3,
       here("data", "Final dataset to work and outputs",
            paste0("data_costo_total_3_", current_year, "_II.rds")))

export(data_grd_5,
       here("data", paste0("data_grd_2_", current_year, "_II.rda")))

export(data_costo_total_5,
       here("output",
            paste0("tabla_costo_medio_paciente_", prev_month_abbr, "_", prev_month_year, ".xlsx")))

message("[PIPELINE] Done.")
