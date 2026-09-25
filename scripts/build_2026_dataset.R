################################################################################
# BUILD_2026_DATASET.R
# Genera data_costo_total_3_2026_II.rds  y  data_grd_2_2026_II.rda
# que alimentan el reporte Quarto.
#
# Fuentes principales:
#   - Admisiones: data_a_2024_2025_2026.rds + admission_april_2026.xls
#   - Ventas:     data_sales_2024_2025_2026.rds  (ya completo, no se modifica)
#   - Costos:     costo_general_2024.xlsx / hoja "2025"
################################################################################

pacman::p_load(tidyverse, janitor, lubridate, rio, here, scales)
source(here("scripts", "function_diagnosis_algorithm_caci.R"))

YEAR <- 2026L
cat(sprintf("[BUILD] Generando dataset %d...\n", YEAR))

# ── 1. Admisiones: agregar abril 2026 al RDS acumulado ───────────────────────
cat("[1] Cargando admisiones...\n")

data_adm_base <- import(here("data", "admission_data",
                              "data_a_2024_2025_2026.rds"))

data_adm_april <- import(
  here("data", "admission_data", "admission_april_2026.xls"),
  skip = 2
) %>%
  clean_names() %>%
  select(-any_of(c("x19", "x28")))

# Coerce types to match before binding (RDS may store some columns as character)
harmonise_adm <- function(df) {
  df %>%
    mutate(
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

data_admission_2 <- bind_rows(
  harmonise_adm(data_adm_base),
  harmonise_adm(data_adm_april)
) %>%
  distinct(documento, paciente, fecha_de_egreso, fecha_ingreso,
           numero_de_cuenta, .keep_all = TRUE)

# Guardar RDS actualizado
export(data_admission_2,
       here("data", "admission_data", "data_a_2024_2025_2026.rds"))
cat(sprintf("[1] Total admisiones: %d filas\n", nrow(data_admission_2)))

# ── 2. Clasificación CACI ────────────────────────────────────────────────────
cat("[2] Clasificando CACI...\n")

result_full <- classify_caci(data_admission_2)

result_2 <- result_full %>%
  filter(tipo_de_atencion == "HOSPITALARIO") %>%
  filter(str_detect(coalesce(departamento_actual, ""), "HOSPITA|UCI|UCIN|URGE")) %>%
  filter(!is.na(diag_eg_pr)) %>%
  filter(year(fecha_ingreso) == YEAR) %>%
  mutate(
    caci_3   = caci_final,
    dif_days = as.numeric(as.Date(fecha_de_egreso) - as.Date(fecha_ingreso)),
    mes      = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año      = year(fecha_ingreso),
    # Excluir oncología que haya quedado clasificada por diagnóstico secundario
    caci_3   = if_else(
      str_detect(coalesce(diag_eg_pr, ""), regex("TUMOR|CANCER|METASTAS", ignore_case = TRUE)) &
        !is.na(caci_3),
      NA_character_, caci_3
    )
  )

cat("[2] Distribución CACI 2026:\n")
print(table(result_2$caci_3, useNA = "ifany"))
export(result_2, here("data", "results_2.rds"))

# ── 3. Separar cuentas múltiples ─────────────────────────────────────────────
cat("[3] Separando cuentas múltiples...\n")

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

data_grd_5 <- split_cuentas(result_2) %>%
  mutate(
    numero_de_ingreso = as.character(numero_de_ingreso),
    mes_ingreso       = month(fecha_ingreso, label = TRUE, abbr = TRUE),
    año_ingreso       = year(fecha_ingreso),
    cruce_2           = paste0(documento, mes_ingreso, año_ingreso)
  )

cat(sprintf("[3] Pacientes GRD 2026: %d ingresos\n", nrow(data_grd_5)))

# Guardar dataset GRD (admisiones clasificadas)
export(data_grd_5, here("data", paste0("data_grd_2_2026_II.rda")))
export(data_grd_5, here("data", "Final dataset to work and outputs",
                         paste0("data_grd_2_2026_II.rda")))

# ── 4. Ventas: cargar el RDS completo (ya tiene abril 2026) ──────────────────
cat("[4] Cargando ventas...\n")

data_sales_raw <- import(
  here("data", "sales data", "data_sales_2024_2025_2026.rds")
)
data_sales_2 <- data_sales_raw %>% clean_names()

# Normalizar fechas: el RDS acumulado mezcla dos formatos según el mes de origen
#   - Histórico (2024–2025): "dd/mm/yy HH:MM"  (24h, locale europeo)
#   - Exportaciones recientes: "m/d/yy h:MM AM/PM"  (12h, locale US)
# El coalesce(dmy_hm, mdy_hm) falla silenciosamente cuando el día ≤ 12 en formato
# US: dmy_hm lo parsea sin error pero con día y mes invertidos.
# La solución correcta es detectar el formato por presencia de AM/PM.
fix_fecha_sales <- function(x) {
  if (!is.character(x)) return(x)        # ya parseado (POSIXct)
  x[trimws(x) == ""] <- NA_character_
  is_ampm <- grepl("\\b(AM|PM)\\b", x, ignore.case = TRUE)
  out <- as.POSIXct(rep(NA_real_, length(x)), tz = "America/Bogota")
  if (any(!is_ampm, na.rm = TRUE))
    out[!is_ampm] <- suppressWarnings(dmy_hm(x[!is_ampm], tz = "America/Bogota"))
  if (any(is_ampm, na.rm = TRUE))
    out[is_ampm]  <- suppressWarnings(
      parse_date_time(x[is_ampm], orders = "mdy IMp",
                      locale = "C", tz = "America/Bogota")
    )
  out
}

if (is.character(data_sales_2$fecha_cargue)) {
  data_sales_2 <- data_sales_2 %>%
    mutate(
      fecha_cargue   = fix_fecha_sales(fecha_cargue),
      fecha_registro = fix_fecha_sales(fecha_registro),
      fecha_egreso   = fix_fecha_sales(fecha_egreso)
    )
}

cat(sprintf("[4] Ventas totales: %d filas\n", nrow(data_sales_2)))

# Filtrar solo 2026
data_sales_2026 <- data_sales_2 %>%
  filter(year(fecha_cargue) == YEAR)

cat(sprintf("[4] Ventas 2026: %d filas\n", nrow(data_sales_2026)))

# ── 5. Órdenes (CARGOS) → cruzar con CACI ───────────────────────────────────
cat("[5] Procesando órdenes CARGOS...\n")

data_ordenes_2 <- data_sales_2026 %>%
  mutate(
    cod_cargo  = as.character(cod_cargo),
    mes_cargue = month(fecha_cargue, label = TRUE),
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
cat(sprintf("[5] Órdenes con CACI: %d / %d\n",
            nrow(data_ordenes_con_caci), nrow(data_ordenes_3)))

# Cruce secundario (fallback): por identificación + mes/año
ingresos_sin_ordenes <- data_grd_5 %>%
  anti_join(data_ordenes_2, by = c("numero_de_ingreso" = "ingreso")) %>%
  mutate(
    mes_egreso = month(fecha_de_egreso, label = TRUE, abbr = TRUE),
    año_egreso = year(fecha_de_egreso),
    cruce_2    = paste0(documento, mes_egreso, año_egreso)
  )

data_ordenes_2.1 <- data_ordenes_2 %>%
  mutate(
    mes_egreso = month(fecha_cargue, label = TRUE, abbr = TRUE),
    año_egreso = year(fecha_cargue),
    cruce_2    = paste0(identificacion, mes_egreso, año_egreso)
  )

data_ordenes_fallback <- left_join(
  data_ordenes_2.1,
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

cat(sprintf("[5] Total órdenes con CACI (primario + fallback): %d\n",
            nrow(data_ordenes_4)))

# ── 6. Costos de referencia CUPS ─────────────────────────────────────────────
cat("[6] Cargando costos CUPS...\n")

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

# ── 7. Farmacia (FARMACIA) ───────────────────────────────────────────────────
cat("[7] Procesando farmacia...\n")

data_mmto_5 <- data_sales_2026 %>%
  mutate(
    cod_cargo  = as.character(cod_cargo),
    fecha_adm  = fecha_cargue,
    cuenta     = as.character(cuenta),
    ingreso    = as.character(ingreso)
  ) %>%
  filter(tipo_registro == "FARMACIA") %>%
  left_join(
    select(result_2, numero_de_ingreso, caci_3),
    by = c("ingreso" = "numero_de_ingreso")
  ) %>%
  distinct(cargo, fecha_registro, fecha_cargue, factura, fecha_adm, .keep_all = TRUE) %>%
  filter(!is.na(caci_3)) %>%
  mutate(
    mes     = month(fecha_adm, label = TRUE),
    costo_2 = as.numeric(gsub(",", "", as.character(costo)))
  )

cat(sprintf("[7] Farmacia con CACI: %d filas\n", nrow(data_mmto_5)))

# ── 8. Dataset unificado de costos ───────────────────────────────────────────
cat("[8] Construyendo dataset unificado...\n")

cols_comunes <- c("nombre_cliente","tipo_cliente","plan","departamento_cargue",
                  "fecha_registro","id","nombres","identificacion","cod_cargo",
                  "cargo","fecha_cargue","ingreso","mes_cargue","caci_3",
                  "cuenta","transaccion","costo_2","valor_cargo_tarifario",
                  "profesional_asignado")

data_mmto_final <- data_mmto_5 %>%
  mutate(mes_cargue = month(fecha_cargue, label = TRUE),
         costo_2    = as.numeric(gsub(",", "", as.character(costo)))) %>%
  select(any_of(cols_comunes)) %>%
  mutate(departamento_cargue_2 = "FARMACIA",
         profesional_asignado  = NA_character_)

data_ordenes_final <- data_orders %>%
  mutate(mes_cargue = month(fecha_cargue, label = TRUE)) %>%
  select(any_of(setdiff(cols_comunes, "costo_2")), valor) %>%
  rename(costo_2 = valor) %>%
  mutate(departamento_cargue_2 = "ORDENES")

data_costo_total <- bind_rows(data_mmto_final, data_ordenes_final)

# ── 9. Enriquecer con datos clínicos del paciente ────────────────────────────
cat("[9] Enriqueciendo con datos clínicos...\n")

demog_cols <- c("edad","sexo","departamento_actual","estado_al_alta",
                "dif_days","fecha_ingreso","fecha_de_egreso",
                "diagnostico_ingreso_princial","diagnostico_egreso_principal",
                "diagnostico_egreso_secundario","procedimiento_qx")

demog_ingreso <- result_2 %>%
  select(numero_de_ingreso, all_of(intersect(demog_cols, names(result_2))))

data_costo_total_2 <- left_join(
  data_costo_total,
  demog_ingreso,
  by = c("ingreso" = "numero_de_ingreso")
) %>%
  distinct(cuenta, departamento_cargue, cod_cargo, departamento_actual,
           dif_days, fecha_registro, cargo, id, identificacion, ingreso,
           estado_al_alta, fecha_de_egreso, fecha_ingreso, costo_2,
           fecha_cargue, valor_cargo_tarifario, diagnostico_ingreso_princial,
           transaccion, diagnostico_egreso_secundario,
           .keep_all = TRUE)

# Fallback por identificación cuando no matcheó por ingreso
demog_doc <- result_2 %>%
  select(documento, all_of(intersect(demog_cols, names(result_2))))

d_con    <- data_costo_total_2 %>% filter(!is.na(departamento_actual))
d_sin    <- data_costo_total_2 %>%
  filter(is.na(departamento_actual)) %>%
  select(-any_of(demog_cols)) %>%
  left_join(demog_doc, by = c("identificacion" = "documento")) %>%
  distinct(cuenta, costo_2, transaccion, fecha_cargue,
           valor_cargo_tarifario, .keep_all = TRUE)

data_costo_total_2 <- bind_rows(d_con, d_sin) %>%
  mutate(departamento = departamento_cargue)

# ── 10. Variables temporales finales ─────────────────────────────────────────
cat("[10] Procesando variables temporales...\n")

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

data_costo_total_3 <- data_costo_total_2 %>%
  mutate(
    venta      = as.numeric(gsub(",", "", as.character(valor_cargo_tarifario))),
    mes        = month(fecha_ingreso, label = TRUE),
    mes_egreso = month(fecha_de_egreso, label = TRUE),
    mes_cargue = month(fecha_cargue, label = TRUE),
    año        = year(fecha_cargue)
  )

cat("[10] Distribución año en dataset final:\n")
print(table(data_costo_total_3$año, useNA = "ifany"))
cat("[10] CACI 2026:\n")
print(table(
  data_costo_total_3$caci_3[data_costo_total_3$año == YEAR],
  useNA = "ifany"
))

# ── 11. Exportar ──────────────────────────────────────────────────────────────
cat("[11] Exportando...\n")
dir.create(here("output"), showWarnings = FALSE)

export(data_costo_total_3,
       here("data", "data_costo_total_3_2026_II.rds"))
export(data_costo_total_3,
       here("data", "Final dataset to work and outputs",
            "data_costo_total_3_2026_II.rds"))

cat("[BUILD] Listo.\n")
cat(sprintf("  Filas dataset costo: %d\n",  nrow(data_costo_total_3)))
cat(sprintf("  Filas dataset GRD:   %d\n",  nrow(data_grd_5)))
