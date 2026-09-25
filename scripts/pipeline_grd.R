################################################################################
# PIPELINE_GRD.R — Integrated Clinical & Financial Pipeline
# DIME Clínica Neurocardiovascular
#
# Replaces: build_2026_dataset.R  +  analysis_update_2026.R
#
# What it produces:
#   data/data_costo_total_3_{year}_II.rds  — cost dataset (ALL years 2024–current)
#   data/data_grd_2_{year}_II.rda          — GRD patient dataset (ALL years 2024–current)
#   output/tabla_costo_medio_<mes>_<year>.xlsx — monthly summary table
#
# Usage:
#   source(here::here("scripts", "pipeline_grd.R"))
#   Rscript scripts/pipeline_grd.R
################################################################################

pacman::p_load(
  tidyverse, janitor, lubridate, rio, here, scales
)

source(here("scripts", "function_diagnosis_algorithm_caci.R"))
dir.create(here("output"), showWarnings = FALSE)

# ── Constants ─────────────────────────────────────────────────────────────────
prev_month_date <- floor_date(today(), "month") - days(1)
prev_month_long <- format(prev_month_date, "%B")      # e.g. "abril"
prev_month_abbr <- format(prev_month_date, "%b")      # e.g. "abr"
prev_month_year <- year(prev_month_date)
current_year    <- year(today())
START_YEAR      <- 2024L                              # earliest year to include

cat(sprintf("[PIPELINE] Iniciando. Mes de reporte: %s %d\n",
            prev_month_long, prev_month_year))

################################################################################
# ── Date helpers ─────────────────────────────────────────────────────────────
################################################################################

# Handles TWO formats mixed in the same column:
#   dd/mm/yy HH:MM        (histórico, 24h, locale europeo)
#   m/d/yy h:MM AM/PM     (exportaciones recientes, 12h, locale US)
# The coalesce(dmy_hm, mdy_hm) approach silently swaps day/month when the day
# number ≤ 12 in US format — dmy_hm succeeds with wrong result before falling
# through. The correct fix: detect format by AM/PM presence first.
fix_fecha_sales <- function(x) {
  if (!is.character(x)) return(x)          # already POSIXct — nothing to do
  x[trimws(x) == ""] <- NA_character_
  is_ampm <- grepl("\\b(AM|PM)\\b", x, ignore.case = TRUE)
  out <- as.POSIXct(rep(NA_real_, length(x)), tz = "America/Bogota")
  if (any(!is_ampm & !is.na(x)))
    out[!is_ampm] <- suppressWarnings(dmy_hm(x[!is_ampm], tz = "America/Bogota"))
  if (any(is_ampm & !is.na(x)))
    out[is_ampm]  <- suppressWarnings(
      parse_date_time(x[is_ampm], orders = "mdy IMp",
                      locale = "C", tz = "America/Bogota")
    )
  out
}

# Handles fecha_factura: empty strings, ISO with optional fractional seconds,
# and double-date strings joined with "||" (takes only the first date).
fix_fecha_factura <- function(x) {
  if (!is.character(x)) return(x)
  x[trimws(x) == ""] <- NA_character_
  x <- sub(" \\|\\|.*", "", trimws(x))
  suppressWarnings(ymd_hms(x, truncated = 3, tz = "America/Bogota"))
}

# Coerces admission columns to consistent types before bind_rows.
harmonise_adm <- function(df) {
  df %>% mutate(
    edad              = as.numeric(edad),
    estancia_horas    = as.numeric(estancia_horas),
    valor_factura     = as.numeric(gsub(",", "", as.character(valor_factura))),
    total_cuenta      = as.numeric(gsub(",", "", as.character(total_cuenta))),
    numero_de_ingreso = as.character(numero_de_ingreso),
    numero_de_cuenta  = as.character(numero_de_cuenta),
    documento         = as.character(documento),
    fecha_ingreso     = as.POSIXct(fecha_ingreso),
    fecha_de_egreso   = as.POSIXct(fecha_de_egreso)
  )
}

################################################################################
# 1. ADMISIONES — cargar acumulado y agregar mes actual
################################################################################
cat("[1] Cargando admisiones...\n")

admission_rds <- here("data", "admission_data",
                      paste0("data_a_2024_2025_", current_year, ".rds"))

data_adm_base <- import(admission_rds)

admission_xls <- here("data", "admission_data",
                      paste0("admission_", prev_month_long, "_", prev_month_year, ".xls"))

if (!file.exists(admission_xls)) {
  warning(sprintf("[1] Archivo de admisiones no encontrado: %s — se omite la actualización.",
                  admission_xls))
  data_admission_2 <- harmonise_adm(data_adm_base)
} else {
  # Siempre recarga el mes objetivo desde el XLS mensual y reemplaza lo que ya
  # exista en el acumulado para ese mes — en vez de omitir la carga si el mes
  # ya "aparece" con solo un puñado de filas parciales (p.ej. de una sync
  # temprana). Un XLS mensual siempre reemplaza por completo su propio mes.
  data_adm_new <- import(admission_xls, skip = 2) %>%
    clean_names() %>%
    select(-any_of(c("x19", "x28"))) %>%
    # Normalise timezone to match accumulated RDS before deduplication
    mutate(fecha_de_egreso = force_tz(as.POSIXct(fecha_de_egreso), "America/Bogota"),
           fecha_ingreso   = force_tz(as.POSIXct(fecha_ingreso),   "America/Bogota"))

  data_adm_base_h    <- harmonise_adm(data_adm_base)
  data_adm_base_kept <- data_adm_base_h %>%
    filter(!(year(as.Date(fecha_de_egreso)) == prev_month_year &
             month(as.Date(fecha_de_egreso)) == month(prev_month_date)))

  n_replaced <- nrow(data_adm_base_h) - nrow(data_adm_base_kept)
  cat(sprintf("[1] %s %d: %d fila(s) parcial(es) reemplazadas por el XLS completo (%d filas nuevas).\n",
              prev_month_long, prev_month_year, n_replaced, nrow(data_adm_new)))

  data_admission_2 <- bind_rows(
    data_adm_base_kept,
    harmonise_adm(data_adm_new)
  ) %>%
    distinct(numero_de_cuenta, .keep_all = TRUE)

  export(data_admission_2, admission_rds)
  cat(sprintf("[1] Admisiones actualizadas. Total filas: %d\n", nrow(data_admission_2)))
}

cat(sprintf("[1] Total admisiones cargadas: %d\n", nrow(data_admission_2)))

################################################################################
# 2. CLASIFICACIÓN CACI (todas las admisiones, sin filtro de año)
################################################################################
cat("[2] Clasificando CACI...\n")

result_full <- classify_caci(data_admission_2)

result_2 <- result_full %>%
  mutate(caci_3 = caci_final)

export(result_2, here("data", "results_2.rds"))
cat(sprintf("[2] Clasificados: %d con CACI / %d total\n",
            sum(!is.na(result_2$caci_final)), nrow(result_2)))

################################################################################
# 3. FILTRAR HOSPITALIZACIONES (todos los años)
################################################################################
cat("[3] Filtrando hospitalizaciones...\n")

data_grd_2 <- result_2 %>%
  filter(tipo_de_atencion == "HOSPITALARIO") %>%
  filter(str_detect(coalesce(departamento_actual, ""), "HOSPITA|UCI|UCIN|URGE")) %>%
  filter(!is.na(diag_eg_pr)) %>%
  mutate(
    dif_days = as.numeric(as.Date(fecha_de_egreso) - as.Date(fecha_ingreso)),
    mes      = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año      = year(fecha_ingreso),
    caci_3   = if_else(
      str_detect(coalesce(diag_eg_pr, ""),
                 regex("TUMOR|CANCER|METASTAS", ignore_case = TRUE)) & !is.na(caci_3),
      NA_character_, caci_3
    )
  )

cat(sprintf("[3] Hospitalizaciones: %d filas\n", nrow(data_grd_2)))
print(table(data_grd_2$caci_3, useNA = "ifany"))

################################################################################
# 4. SEPARAR CUENTAS MÚLTIPLES
################################################################################
cat("[4] Separando cuentas múltiples...\n")

split_cuentas <- function(df) {
  g3 <- df %>%
    mutate(numero_de_cuenta = str_trim(numero_de_cuenta)) %>%
    separate(numero_de_cuenta,
             into  = c("numero_de_cuenta_1", "numero_de_cuenta_2"),
             sep   = "\\|\\|", extra = "drop", fill = "right") %>%
    mutate(across(starts_with("numero_de_cuenta"), str_trim))

  g4 <- g3 %>%
    filter(!is.na(numero_de_cuenta_2)) %>%
    mutate(numero_de_cuenta = numero_de_cuenta_2) %>%
    select(-c(numero_de_cuenta_1, numero_de_cuenta_2))

  g3 <- g3 %>%
    select(-numero_de_cuenta_2) %>%
    rename(numero_de_cuenta = numero_de_cuenta_1)

  bind_rows(g3, g4)
}

data_grd_5 <- split_cuentas(data_grd_2) %>%
  mutate(
    numero_de_ingreso = as.character(numero_de_ingreso),
    mes_ingreso       = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año_ingreso       = year(fecha_ingreso),
    cruce_2           = paste0(documento, mes_ingreso, año_ingreso)
  )

cat(sprintf("[4] Pacientes GRD: %d registros\n", nrow(data_grd_5)))

# Exportar dataset GRD (todos los años)
export(data_grd_5,
       here("data", paste0("data_grd_2_", current_year, "_II.rda")))
export(data_grd_5,
       here("data", "Final dataset to work and outputs",
            paste0("data_grd_2_", current_year, "_II.rda")))

################################################################################
# 5. VENTAS — cargar RDS acumulado y agregar mes actual si falta
################################################################################
cat("[5] Cargando ventas...\n")

sales_rds <- here("data", "sales data",
                  paste0("data_sales_2024_2025_", current_year, ".rds"))

data_sales_raw <- import(sales_rds)
data_sales_2   <- clean_names(data_sales_raw)

# Si las fechas siguen siendo character (primer uso tras exportación manual),
# aplicar parser AM/PM-aware; si ya son POSIXct, no hacer nada.
if (is.character(data_sales_2$fecha_cargue)) {
  cat("[5] Normalizando fechas de ventas...\n")
  data_sales_2 <- data_sales_2 %>%
    mutate(
      fecha_cargue   = fix_fecha_sales(fecha_cargue),
      fecha_registro = fix_fecha_sales(fecha_registro),
      fecha_egreso   = fix_fecha_sales(fecha_egreso),
      fecha_factura  = fix_fecha_factura(fecha_factura)
    )
  export(data_sales_2, sales_rds)
  cat("[5] Fechas normalizadas y RDS actualizado.\n")
}

# Siempre recarga el mes objetivo desde el CSV mensual y reemplaza lo que ya
# exista en el acumulado para ese mes — en vez de omitir la carga si el mes ya
# "aparece" con solo un puñado de filas parciales (p.ej. de una sync
# temprana). Un CSV mensual siempre reemplaza por completo su propio mes.
sales_csv <- here("data", "sales data",
                  paste0("sales_", prev_month_abbr, "_", prev_month_year, ".csv"))

if (!file.exists(sales_csv)) {
  warning(sprintf("[5] CSV de ventas no encontrado: %s — se usa el acumulado existente.", sales_csv))
} else {
  cat(sprintf("[5] Agregando ventas de %s %d desde CSV...\n", prev_month_long, prev_month_year))
  data_sales_new <- import(sales_csv, fill = Inf, skip = 3, header = TRUE) %>%
    clean_names() %>%
    mutate(
      fecha_cargue   = fix_fecha_sales(fecha_cargue),
      fecha_registro = fix_fecha_sales(fecha_registro),
      fecha_egreso   = fix_fecha_sales(fecha_egreso),
      fecha_factura  = fix_fecha_factura(fecha_factura)
    )
  # Align fecha_factura type in the base before binding
  if (is.character(data_sales_2$fecha_factura))
    data_sales_2 <- data_sales_2 %>%
      mutate(fecha_factura = fix_fecha_factura(fecha_factura))

  data_sales_2_kept <- data_sales_2 %>%
    filter(!(year(as.Date(fecha_cargue)) == prev_month_year &
             month(as.Date(fecha_cargue)) == month(prev_month_date)))

  n_replaced <- nrow(data_sales_2) - nrow(data_sales_2_kept)
  cat(sprintf("[5] %s %d: %d fila(s) parcial(es) reemplazadas por el CSV completo (%d filas nuevas).\n",
              prev_month_long, prev_month_year, n_replaced, nrow(data_sales_new)))

  data_sales_2 <- bind_rows(data_sales_2_kept, data_sales_new)
  export(data_sales_2, sales_rds)
  cat(sprintf("[5] Ventas acumuladas actualizadas: %d filas. Max date: %s\n",
              nrow(data_sales_2),
              format(max(as.Date(data_sales_2$fecha_cargue), na.rm = TRUE))))
}

cat(sprintf("[5] Ventas cargadas: %d filas\n", nrow(data_sales_2)))

################################################################################
# 6. ÓRDENES (CARGOS) — cruzar con CACI
################################################################################
cat("[6] Procesando órdenes CARGOS...\n")

# Usar todas las ventas (todos los años) para cubrir admisiones 2024–current
data_ordenes_2 <- data_sales_2 %>%
  mutate(
    cod_cargo  = as.character(cod_cargo),
    mes_cargue = month(fecha_cargue, label = TRUE, abbr = TRUE),
    año_cargue = year(fecha_cargue),
    ingreso    = as.character(ingreso),
    cuenta     = as.character(cuenta)
  ) %>%
  filter(tipo_registro == "CARGOS")

# Cruce primario: por número de ingreso
data_ordenes_3 <- left_join(
  data_ordenes_2,
  select(data_grd_5, numero_de_ingreso, caci_3),
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario,
           total_cuenta, costo, transaccion, fecha_registro, ingreso, .keep_all = TRUE)

data_ordenes_con_caci <- data_ordenes_3 %>% filter(!is.na(caci_3))

cat(sprintf("[6] Órdenes con CACI (primario): %d / %d\n",
            nrow(data_ordenes_con_caci), nrow(data_ordenes_3)))

# Cruce secundario (fallback): por identificación + mes/año de cargue
ingresos_sin_ordenes <- data_grd_5 %>%
  anti_join(data_ordenes_2, by = c("numero_de_ingreso" = "ingreso")) %>%
  mutate(
    mes_egreso = month(fecha_de_egreso, label = TRUE, abbr = TRUE),
    año_egreso = year(fecha_de_egreso),
    cruce_2    = paste0(documento, mes_egreso, año_egreso)
  )

data_ordenes_2_fallback <- data_ordenes_2 %>%
  mutate(
    mes_egreso = month(fecha_cargue, label = TRUE, abbr = TRUE),
    año_egreso = year(fecha_cargue),
    cruce_2    = paste0(identificacion, mes_egreso, año_egreso)
  )

data_ordenes_fallback <- left_join(
  data_ordenes_2_fallback,
  select(ingresos_sin_ordenes, cruce_2, caci_3),
  by = "cruce_2"
) %>%
  filter(!is.na(caci_3)) %>%
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario,
           total_cuenta, costo, transaccion, fecha_registro, ingreso, .keep_all = TRUE) %>%
  select(-c(mes_egreso, año_egreso, cruce_2))

data_ordenes_4 <- bind_rows(data_ordenes_con_caci, data_ordenes_fallback) %>%
  mutate(
    cod_cargo   = as.character(cod_cargo),
    cod_cargo_2 = coalesce(str_extract(cod_cargo, ".*(?=-)"), cod_cargo)
  )

cat(sprintf("[6] Total órdenes con CACI (primario + fallback): %d\n",
            nrow(data_ordenes_4)))

################################################################################
# 7. COSTOS DE REFERENCIA CUPS
################################################################################
cat("[7] Cargando costos CUPS...\n")

data_costo_cups <- import(
  here("data", "data_costs", "costo_general_2024.xlsx"),
  which = "2025"
) %>%
  clean_names() %>%
  mutate(across(any_of(c("codigo", "codigo_2")), as.character))

data_orders <- left_join(
  data_ordenes_4,
  select(data_costo_cups, codigo, valor),
  by = c("cod_cargo_2" = "codigo")
) %>%
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario,
           total_cuenta, costo, transaccion, fecha_registro, ingreso, .keep_all = TRUE) %>%
  mutate(valor = replace_na(valor, 0))

################################################################################
# 8. FARMACIA
################################################################################
cat("[8] Procesando farmacia...\n")

data_mmto_5 <- data_sales_2 %>%
  mutate(
    cod_cargo = as.character(cod_cargo),
    fecha_adm = fecha_cargue,                    # ya es POSIXct — no re-parsear
    cuenta    = as.character(cuenta),
    ingreso   = as.character(ingreso)
  ) %>%
  filter(tipo_registro == "FARMACIA") %>%
  left_join(
    select(data_grd_2, numero_de_ingreso, caci_3),
    by = c("ingreso" = "numero_de_ingreso")
  ) %>%
  distinct(cargo, fecha_registro, fecha_cargue, factura, fecha_adm, .keep_all = TRUE) %>%
  filter(!is.na(caci_3)) %>%
  mutate(
    mes     = month(fecha_adm, label = TRUE, abbr = TRUE),
    costo_2 = as.numeric(gsub(",", "", as.character(costo)))
  )

cat(sprintf("[8] Farmacia con CACI: %d filas\n", nrow(data_mmto_5)))

################################################################################
# 9. DATASET UNIFICADO DE COSTOS
################################################################################
cat("[9] Construyendo dataset unificado...\n")

cols_comunes <- c(
  "nombre_cliente", "tipo_cliente", "plan", "departamento_cargue",
  "fecha_registro", "id", "nombres", "identificacion", "cod_cargo",
  "cargo", "fecha_cargue", "ingreso", "mes_cargue", "caci_3",
  "cuenta", "transaccion", "costo_2", "valor_cargo_tarifario",
  "profesional_asignado"
)

data_mmto_final <- data_mmto_5 %>%
  mutate(
    mes_cargue = month(fecha_cargue, label = TRUE, abbr = TRUE),
    costo_2    = as.numeric(gsub(",", "", as.character(costo)))
  ) %>%
  select(any_of(cols_comunes)) %>%
  mutate(
    departamento_cargue_2 = "FARMACIA",
    profesional_asignado  = NA_character_
  )

data_ordenes_final <- data_orders %>%
  mutate(mes_cargue = month(fecha_cargue, label = TRUE, abbr = TRUE)) %>%
  select(any_of(setdiff(cols_comunes, "costo_2")), valor) %>%
  rename(costo_2 = valor) %>%
  mutate(departamento_cargue_2 = "ORDENES")

data_costo_total <- bind_rows(data_mmto_final, data_ordenes_final)

################################################################################
# 10. ENRIQUECER CON DATOS CLÍNICOS
################################################################################
cat("[10] Enriqueciendo con datos clínicos...\n")

demog_cols <- c(
  "edad", "sexo", "departamento_actual", "estado_al_alta",
  "dif_days", "fecha_ingreso", "fecha_de_egreso",
  "diagnostico_ingreso_princial", "diagnostico_egreso_principal",
  "diagnostico_egreso_secundario", "procedimiento_qx"
)

demog_ingreso <- data_grd_2 %>%
  select(numero_de_ingreso, all_of(intersect(demog_cols, names(data_grd_2))))

data_costo_total_2 <- left_join(
  data_costo_total,
  demog_ingreso,
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(cuenta, departamento_cargue, cod_cargo, departamento_actual,
           dif_days, fecha_registro, cargo, id, identificacion, ingreso,
           estado_al_alta, fecha_de_egreso, fecha_ingreso, costo_2,
           fecha_cargue, valor_cargo_tarifario, diagnostico_ingreso_princial,
           transaccion, diagnostico_egreso_secundario, .keep_all = TRUE)

# Fallback: unir por documento cuando no hay match por ingreso
demog_doc <- data_grd_2 %>%
  select(documento, all_of(intersect(demog_cols, names(data_grd_2))))

d_con <- data_costo_total_2 %>% filter(!is.na(departamento_actual))
d_sin <- data_costo_total_2 %>%
  filter(is.na(departamento_actual)) %>%
  select(-any_of(demog_cols)) %>%
  left_join(demog_doc, by = c("identificacion" = "documento")) %>%
  distinct(cuenta, costo_2, transaccion, fecha_cargue,
           valor_cargo_tarifario, .keep_all = TRUE)

data_costo_total_2 <- bind_rows(d_con, d_sin) %>%
  mutate(departamento = departamento_cargue)

################################################################################
# 11. VARIABLES TEMPORALES FINALES
################################################################################
cat("[11] Procesando variables temporales...\n")

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

data_costo_total_3 <- data_costo_total_2 %>%
  mutate(
    venta      = as.numeric(gsub(",", "", as.character(valor_cargo_tarifario))),
    mes        = month(fecha_ingreso,   label = TRUE, abbr = TRUE),
    mes_egreso = month(fecha_de_egreso, label = TRUE, abbr = TRUE),
    mes_cargue = month(fecha_cargue,    label = TRUE, abbr = TRUE),
    año        = as.integer(year(fecha_cargue))
  ) %>%
  # Incluir solo años desde START_YEAR hasta el año actual
  filter(año >= START_YEAR, año <= current_year)

cat("[11] Distribución por año:\n")
print(table(data_costo_total_3$año, useNA = "ifany"))

################################################################################
# 12. TABLAS RESUMEN (año en curso)
################################################################################
cat("[12] Construyendo tablas resumen...\n")

data_costo_total_4 <- data_costo_total_3 %>%
  filter(año == current_year) %>%
  group_by(identificacion, mes_cargue, año, caci_3) %>%
  summarise(
    costo = sum(costo_2, na.rm = TRUE),
    venta = sum(venta,   na.rm = TRUE),
    .groups = "drop"
  )

# Tabla de costo por CACI y mes
data_costo_total_5 <- data_costo_total_4 %>%
  group_by(mes_cargue, caci_3) %>%
  summarise(
    pacientes   = n_distinct(identificacion),
    mediana     = median(costo, na.rm = TRUE),
    costo_orden = sum(costo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols     = mes_cargue,
    names_from  = caci_3,
    values_from = c(pacientes, mediana, costo_orden)
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# Tabla de rentabilidad por CACI y mes
data_costo_total_6 <- data_costo_total_4 %>%
  group_by(mes_cargue, caci_3) %>%
  summarise(
    costo_orden = sum(costo, na.rm = TRUE),
    venta       = sum(venta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rentabilidad   = venta - costo_orden,
    rentabilidad_2 = if_else(venta > 0, round(rentabilidad / venta * 100, 2), 0)
  )

################################################################################
# 13. EXPORTAR
################################################################################
cat("[13] Exportando...\n")

# Dataset principal: todos los años (2024 al año actual)
export(data_costo_total_3,
       here("data", paste0("data_costo_total_3_", current_year, "_II.rds")))
export(data_costo_total_3,
       here("data", "Final dataset to work and outputs",
            paste0("data_costo_total_3_", current_year, "_II.rds")))

# Tabla resumen mensual
export(data_costo_total_5,
       here("output",
            paste0("tabla_costo_medio_", prev_month_abbr, "_", prev_month_year, ".xlsx")))

cat("[PIPELINE] Listo.\n")
cat(sprintf("  Filas dataset costos: %d  (años: %d–%d)\n",
            nrow(data_costo_total_3), START_YEAR, current_year))
cat(sprintf("  Filas dataset GRD:    %d\n", nrow(data_grd_5)))
