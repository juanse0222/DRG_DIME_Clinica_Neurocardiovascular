################################################################################
# prep_data.R — Pre-aggregate GRD cost data before deploying to shinyapps.io
#
# Run this script ONCE from the project root or from shiny_grd/ whenever the
# source RDS files change. It produces three compact files in shiny_grd/data/:
#   pte_base.rds  — patient × CACI × month  (used by most tabs)
#   une_base.rds  — CACI × month × unit     (Tab 8 "Por unidad")
#   dt_base.rds   — patient × month × dept  (Tab 10 "Datos")
#
# These three files together are usually < 1 MB vs the 22 MB+ raw cost RDS,
# which is what causes shinyapps.io to crash with "signal: killed".
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(rio)
})

# ── Locate data directory ─────────────────────────────────────────────────────
data_dir <- if (dir.exists("shiny_grd/data")) "shiny_grd/data" else "data"
message("Using data_dir: ", normalizePath(data_dir))

# ── Helper functions (mirror of global.R) ────────────────────────────────────
caci_levels <- c("ICC", "ACV", "SCA", "TEP", "TxC", "Otros CV")
caci_map <- c(
  ICC = "ICC", ACV = "ACV", SCA = "SCA", TEP = "TEP",
  TXC = "TxC", CARDIO_OTHER = "Otros CV",
  OTROS_CV = "Otros CV", OTRO_CV = "Otros CV"
)

recode_caci <- function(x) {
  x_clean <- str_to_upper(as.character(x))
  x_clean <- str_replace_all(x_clean, "\\s+", "_")
  factor(
    dplyr::recode(x_clean, !!!caci_map, .default = NA_character_),
    levels = caci_levels
  )
}

classify_une <- function(dept, dept2 = NA_character_) {
  case_when(
    str_detect(coalesce(dept, ""), "HOSPITALIZACION|HOSPITALIZACIÓN|UCI|UCIN") ~ "Estancia",
    str_detect(coalesce(dept, ""), "ANGIOGRAFI|HEMODINAM")                     ~ "Hemodinamia",
    str_detect(coalesce(dept, ""), "CIRUGIA|CIRUGÍA")                          ~ "Cirugía",
    str_detect(coalesce(dept, ""), "URGENCIAS")                                ~ "Urgencias",
    str_detect(coalesce(dept, ""), "CONSULTA")                                 ~ "Consulta externa",
    str_detect(coalesce(dept, ""), "RESONANCIA|ECOGRAFIA|ECOGRAFÍA|ESCANOGR|RAYOS") ~ "Imágenes",
    str_detect(coalesce(dept, ""), "LABORATORIO")                              ~ "Laboratorio",
    coalesce(dept2, "") == "FARMACIA"                                          ~ "Medicamentos",
    TRUE                                                                       ~ "Otro"
  )
}

safe_import <- function(f) {
  df <- rio::import(f)
  for (col in c("fecha_cargue", "fecha_registro", "fecha_ingreso", "fecha_de_egreso"))
    if (col %in% names(df)) df[[col]] <- as.Date(df[[col]])
  for (col in c("edad", "año"))
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  for (col in c("transaccion", "mes_cargue", "mes", "ingreso", "cod_cargo"))
    if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
  ordered_cols <- names(df)[sapply(df, is.ordered)]
  for (col in ordered_cols) df[[col]] <- as.character(df[[col]])
  df
}

prep_costo <- function(df) {
  df <- df %>% janitor::clean_names()
  if ("caci_3" %in% names(df) && !"caci" %in% names(df))
    df <- rename(df, caci = caci_3)
  else if ("caci_3" %in% names(df) && "caci" %in% names(df))
    df <- select(df, -caci_3)
  if ("costo_2" %in% names(df) && !"costo" %in% names(df))
    df <- rename(df, costo = costo_2)
  else if ("costo_2" %in% names(df) && "costo" %in% names(df))
    df <- select(df, -costo_2)
  df %>%
    mutate(
      fecha_cargue = as.Date(fecha_cargue),
      venta = readr::parse_number(
        as.character(valor_cargo_tarifario),
        locale = readr::locale(grouping_mark = ",", decimal_mark = ".")
      ),
      costo      = as.numeric(costo),
      año        = as.integer(dplyr::coalesce(
                     suppressWarnings(as.numeric(ano)), year(fecha_cargue))),
      mes_cargue = as.integer(month(fecha_cargue)),
      caci       = recode_caci(caci)
    ) %>%
    filter(año >= 2024L, !is.na(año))
}

# ── Load full cost data ───────────────────────────────────────────────────────
cost_files <- list.files(data_dir, pattern = "data_costo_total_3_.*_II\\.rds",
                         full.names = TRUE)
if (length(cost_files) == 0) stop("No cost RDS files found in ", data_dir)
message(sprintf("Loading %d cost file(s)...", length(cost_files)))

cutoff_date     <- floor_date(Sys.Date(), "month") - days(1)  # last day of prev complete month
data_costo_raw  <- bind_rows(lapply(cost_files, safe_import))
data_costo_base <- prep_costo(data_costo_raw) %>%
  filter(is.na(fecha_cargue) | fecha_cargue <= cutoff_date)
rm(data_costo_raw); gc()
message(sprintf("Cutoff: %s — partial current-month rows excluded.", format(cutoff_date)))

message(sprintf("Cost data: %d rows × %d cols — %.1f MB in memory",
                nrow(data_costo_base), ncol(data_costo_base),
                as.numeric(object.size(data_costo_base)) / 1e6))

# ── 1. pte_base: patient × CACI × month ─ used by most tabs ─────────────────
pte_base <- data_costo_base %>%
  filter(!is.na(caci), !is.na(identificacion)) %>%
  group_by(identificacion, caci, año, mes_cargue) %>%
  summarise(
    costo = sum(costo, na.rm = TRUE),
    venta = sum(venta, na.rm = TRUE),
    .groups = "drop"
  )
message(sprintf("pte_base: %d rows — %.1f MB",
                nrow(pte_base), as.numeric(object.size(pte_base)) / 1e6))
saveRDS(pte_base, file.path(data_dir, "pte_base.rds"), compress = "xz")

# ── 2. une_base: CACI × month × business unit ─ Tab 8 "Por unidad" ───────────
une_col <- intersect(c("departamento_cargue", "departamento_cargue_2"),
                     names(data_costo_base))
une_base <- data_costo_base %>%
  filter(!is.na(caci)) %>%
  mutate(
    Unidad = classify_une(
      if ("departamento_cargue"   %in% une_col) departamento_cargue   else NA_character_,
      if ("departamento_cargue_2" %in% une_col) departamento_cargue_2 else NA_character_
    )
  ) %>%
  group_by(año, mes_cargue, caci, Unidad) %>%
  summarise(costo = sum(costo, na.rm = TRUE), .groups = "drop")
message(sprintf("une_base: %d rows — %.1f MB",
                nrow(une_base), as.numeric(object.size(une_base)) / 1e6))
saveRDS(une_base, file.path(data_dir, "une_base.rds"), compress = "xz")

# ── 3. dt_base: patient × month × CACI × dept type ─ Tab 10 "Datos" ──────────
dt_base <- data_costo_base %>%
  filter(!is.na(caci)) %>%
  group_by(
    año, mes_cargue, caci, identificacion,
    departamento_cargue_2 = if ("departamento_cargue_2" %in% names(.)) departamento_cargue_2 else NA_character_
  ) %>%
  summarise(
    costo = sum(costo, na.rm = TRUE),
    venta = sum(venta, na.rm = TRUE),
    .groups = "drop"
  )
message(sprintf("dt_base: %d rows — %.1f MB",
                nrow(dt_base), as.numeric(object.size(dt_base)) / 1e6))
saveRDS(dt_base, file.path(data_dir, "dt_base.rds"), compress = "xz")

rm(data_costo_base); gc()

# ── 4. perfil_pacientes: todos los pacientes DIME (no solo CACI) ─────────────
# Fuente: data/data_discharges_hosp.rds — un registro por ingreso hospitalario,
# con sexo, edad, EPS/pagador, diagnóstico, canal de ingreso y desenlace.
# Usado en la pestaña "Perfil de pacientes" (características de TODOS los
# pacientes que buscan atención en DIME, no solo la cohorte CACI).
root_data_dir <- if (dir.exists("data")) "data" else file.path(dirname(data_dir), "data")
discharges_file <- file.path(root_data_dir, "data_discharges_hosp.rds")

if (file.exists(discharges_file)) {

  message("\nCargando data_discharges_hosp.rds para perfil de pacientes...")
  data_discharges <- readRDS(discharges_file)

  # Clasificador preliminar EAPB vs. Particular / Libre Elección.
  # SLE (Segmento Libre Elección) = Medicina Prepagada + Pólizas + Particulares
  # (definición del proyecto Prexo SLE / AV DIME). Lista de entidades tomada
  # de la metodología histórica en Personal JSH/analisis_comercial_2024/
  # script_analisis_comercial.R (columna `eps_3`).
  # SLE se divide en dos subcategorías a pedido explícito:
  #   - "SLE - Particulares": PARTICULAR/PART. genérico, PREVISER, convenios
  #     con médicos específicos ("PART. DR. ..."), afiliados/colaboradores
  #     DIME — todos llevan el prefijo "PART."/"PARTICULAR" en el dato crudo.
  #   - "SLE - MP/Pólizas": medicina prepagada y aseguradoras con nombre
  #     propio, más convenios institucionales (Univalle, Unisalud, Ecopetrol).
  classify_payer <- function(eps) {
    eps_up <- str_to_upper(coalesce(eps, ""))
    case_when(
      eps_up == ""                                                     ~ "Sin dato",
      str_detect(eps_up, "PARTICULAR|PART\\.")                          ~ "SLE - Particulares",
      str_detect(eps_up, "ALLIANZ")                                     ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "COOMEVA.*(MEDICINA PREPAGADA|\\bMP\\b)")       ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "\\bAXA\\b")                                   ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "COLSANITAS")                                  ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "COLMEDICA")                                   ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "MEDISANITAS")                                 ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "\\bMEDPLUS\\b")                               ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "PAN ?AMERICAN")                               ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "SEGUROS BOLIVAR")                             ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "LIBERTY SEGUROS")                             ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "SEGUROS DE VIDA SURAMERICANA|SURA.*POLIZA|POLIZA.*SURA") ~ "SLE - MP/Pólizas",
      # NOTA: planes PAC (Comfenalco/Nueva EPS/SOS/Sura PAC) excluidos del SLE
      # a pedido explícito — son planes complementarios sobre una EAPB, no
      # medicina prepagada/póliza/particular pura.
      str_detect(eps_up, "UNIVALLE|UNIVERSIDAD DEL VALLE")              ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "UNISALUD")                                    ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "EXAMENES DE INGRESO|INTERSALUD")              ~ "SLE - MP/Pólizas",
      str_detect(eps_up, "\\bECOPETROL\\b")                             ~ "SLE - MP/Pólizas",
      TRUE                                                              ~ "EAPB"
    )
  }

  # Extrae el código CIE-10 (antes del " - ") de un texto "I219 - INFARTO...".
  extract_dx_code <- function(x) {
    str_trim(str_extract(coalesce(x, ""), "^[A-Z0-9]+(?=\\s*-)"))
  }
  extract_dx_desc <- function(x) {
    d <- str_trim(str_replace(coalesce(x, ""), "^[A-Z0-9]+\\s*-\\s*", ""))
    if_else(d == "", NA_character_, d)
  }

  perfil_pacientes <- data_discharges %>%
    mutate(
      edad   = suppressWarnings(as.numeric(as.character(edad))),
      sexo   = case_when(
        str_to_upper(sexo) %in% c("F", "FEMENINO", "MUJER")   ~ "Femenino",
        str_to_upper(sexo) %in% c("M", "MASCULINO", "HOMBRE") ~ "Masculino",
        TRUE ~ NA_character_
      ),
      edad_grupo = cut(
        edad,
        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
        labels = c("0-9","10-19","20-29","30-39","40-49",
                   "50-59","60-69","70-79","80+"),
        right = FALSE
      ),
      tipo_pagador   = classify_payer(eps),
      dx_cod         = extract_dx_code(diagnostico_ingreso_princial),
      dx_desc        = extract_dx_desc(diagnostico_ingreso_princial),
      via_ingreso    = str_to_title(coalesce(via_d_eingreso, "Sin dato")),
      mortalidad     = estado_al_alta == "MUERTO",
      cirugia        = str_to_upper(coalesce(remision_a_cirugia, "")) == "SI",
      estancia_dias  = estancia_horas / 24,
      año            = as.integer(año),
      mes            = as.integer(mes)
    ) %>%
    filter(!is.na(año)) %>%
    select(
      documento, año, mes, fecha_ingreso, fecha_de_egreso,
      edad, edad_grupo, sexo, tipo_pagador, eps_raw = eps,
      dx_cod, dx_desc, via_ingreso, mortalidad, cirugia,
      estancia_horas, estancia_dias, departamento_de_ingreso
    )

  message(sprintf("perfil_pacientes: %d filas — %.1f MB",
                  nrow(perfil_pacientes),
                  as.numeric(object.size(perfil_pacientes)) / 1e6))
  saveRDS(perfil_pacientes, file.path(data_dir, "perfil_pacientes.rds"), compress = "xz")
  rm(data_discharges, perfil_pacientes); gc()

} else {
  message("\n[AVISO] No se encontró ", discharges_file,
          " — se omite perfil_pacientes.rds (la pestaña 'Perfil de pacientes' ",
          "usará el RDS crudo en modo desarrollo si está disponible).")
}

# ── data/.rscignore: excluir los RDS crudos de costos del despliegue ────────
# rsconnect's .rscignore hace match LITERAL por nombre de archivo (sin
# comodines) y solo contra el contenido del MISMO directorio — por eso este
# archivo vive en data/, no en la raíz de shiny_grd/. Se regenera aquí para
# que siempre liste el/los archivo(s) de costos crudos actuales (el nombre
# cambia cada corte, p.ej. _2026_II -> _2026_III).
writeLines(basename(cost_files), file.path(data_dir, ".rscignore"))
message(sprintf("\ndata/.rscignore actualizado (%d archivo(s) excluidos del despliegue)",
                length(cost_files)))

message("\nDone! Archivos compactos guardados en ", normalizePath(data_dir))
message("Total file sizes:")
for (f in c("pte_base.rds", "une_base.rds", "dt_base.rds", "perfil_pacientes.rds"))
  if (file.exists(file.path(data_dir, f)))
    message(sprintf("  %s: %.1f KB", f,
                    file.size(file.path(data_dir, f)) / 1e3))
message("\nNow deploy: rsconnect::deployApp('shiny_grd', appName='grd-dime', forceUpdate=TRUE)")
