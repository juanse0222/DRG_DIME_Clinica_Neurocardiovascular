################################################################################
# prep_discharges_trend.R — Distribución de egresos por servicio y reingresos
# a 20 días, histórico 2017-2026.
#
# Fuente: data/data_egresos_2017_2026.rds — copia de
# ".../16. Egresos DIME Mensual/egresos_dime_mensual/data_2025/data_egresos_2017_2026.rds"
# (668k encuentros, AMBULATORIO + HOSPITALARIO, 2016-12 a 2026-08). Para
# refrescar: volver a copiar ese archivo sobre data/data_egresos_2017_2026.rds
# y re-correr este script.
#
# La regla de reingreso a 20 días es la misma que ya usa el equipo para los
# reportes mensuales (ver .../Scripts/script_egresos_dime.R líneas 159-228):
# un ingreso a URGENCIAS/UCI/UCIN cuenta como reingreso si el paciente ya
# había sido dado de alta de uno de esos mismos servicios 0-20 días antes.
# Aquí se aplica sobre todo el histórico en vez de un mes a la vez, para
# poder ver la tendencia año a año.
#
# Salidas (shiny_grd/data/, sin PHI — solo conteos agregados):
#   discharges_by_service.rds — año × mes × servicio × tipo_de_atención → n
#   readmissions_trend.rds    — año × mes → admisiones, reingresos, % reingreso
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(here)
})

src_file <- here("data", "data_egresos_2017_2026.rds")
out_dir  <- here("shiny_grd", "data")
stopifnot(file.exists(src_file))

message("Cargando ", src_file, " ...")
data_egresos <- readRDS(src_file) %>% clean_names()

# ── Limpieza mínima común a ambos análisis ───────────────────────────────────
# `fecha_de_egreso` llega como POSIXct (fecha+hora); solo nos interesa el día
# para agrupar por año/mes.
data_egresos <- data_egresos %>%
  mutate(
    fecha_de_egreso = as_date(fecha_de_egreso),
    fecha_ingreso    = as_date(fecha_ingreso),
    año = year(fecha_de_egreso),
    mes = month(fecha_de_egreso)
  ) %>%
  filter(!is.na(año))

message(sprintf("Encuentros cargados: %s (%d - %d)",
                format(nrow(data_egresos), big.mark = "."),
                min(data_egresos$año), max(data_egresos$año)))

# ══════════════════════════════════════════════════════════════════════════
# 1. Distribución de egresos por servicio
# ══════════════════════════════════════════════════════════════════════════
# n_distinct(numero_de_ingreso) en vez de n(): cada admisión puede aparecer
# varias veces en la base cruda (varios cargos por la misma estancia), así
# que contamos admisiones únicas, no filas.
discharges_by_service <- data_egresos %>%
  filter(!is.na(departamento_actual), !is.na(numero_de_ingreso)) %>%
  group_by(año, mes, departamento_actual, tipo_de_atencion) %>%
  summarise(n_egresos = n_distinct(numero_de_ingreso), .groups = "drop") %>%
  rename(servicio = departamento_actual) %>%
  arrange(año, mes, desc(n_egresos))

message(sprintf("discharges_by_service: %d filas — %.1f KB",
                nrow(discharges_by_service),
                as.numeric(object.size(discharges_by_service)) / 1e3))
saveRDS(discharges_by_service, file.path(out_dir, "discharges_by_service.rds"),
        compress = "xz")

# ══════════════════════════════════════════════════════════════════════════
# 2. Reingresos a 20 días (URGENCIAS/UCI/UCIN) — histórico completo
# ══════════════════════════════════════════════════════════════════════════
readmission_flags <- data_egresos %>%
  filter(
    departamento_actual %in% c("URGENCIAS", "UCI", "UCIN"),
    !is.na(documento), !is.na(fecha_ingreso)
  ) %>%
  arrange(documento, fecha_ingreso) %>%
  group_by(documento) %>%
  mutate(
    # lag() trae la fecha de egreso del ingreso ANTERIOR de este mismo
    # paciente (dentro del grupo ya ordenado por fecha) — así comparamos
    # cada ingreso contra su propio historial, no contra otros pacientes.
    fecha_egreso_previo = lag(fecha_de_egreso),
    dias_desde_egreso_previo = as.numeric(fecha_ingreso - fecha_egreso_previo),
    reingreso_20d = !is.na(dias_desde_egreso_previo) &
      dias_desde_egreso_previo >= 0 & dias_desde_egreso_previo <= 20
  ) %>%
  ungroup() %>%
  select(año, mes, reingreso_20d)

# Se descarta `readmission_flags` (nivel paciente) apenas se agrega: el único
# resultado que sale de este script es el conteo por año/mes — sin cédula.
readmissions_trend <- readmission_flags %>%
  group_by(año, mes) %>%
  summarise(
    n_admisiones     = n(),
    n_reingresos_20d = sum(reingreso_20d, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_reingreso = round(100 * n_reingresos_20d / n_admisiones, 2)) %>%
  arrange(año, mes)

rm(readmission_flags)

message(sprintf("readmissions_trend: %d filas — %.1f KB",
                nrow(readmissions_trend),
                as.numeric(object.size(readmissions_trend)) / 1e3))
message(sprintf("Tasa de reingreso histórica global: %.2f%%",
                100 * sum(readmissions_trend$n_reingresos_20d) /
                      sum(readmissions_trend$n_admisiones)))
saveRDS(readmissions_trend, file.path(out_dir, "readmissions_trend.rds"),
        compress = "xz")

message("\nListo. Archivos guardados en ", normalizePath(out_dir))
