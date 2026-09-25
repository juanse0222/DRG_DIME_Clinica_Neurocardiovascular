################################################################################
# prep_autoservicio_servicio.R — Autoservicio por servicio (Fase 1)
#
# Fuente: data/data_egresos_2017_2026.rds (668k encuentros, AMBULATORIO +
# HOSPITALARIO, 2016-12 a 2026-08) y shiny_grd/data/data_grd_2_2026_II.rda
# (clasificación clínica CACI de DIME, disponible solo 2024-01 a 2026-08).
#
# Produce, sin cédula/nombre/documento — solo conteos y promedios agregados
# por año × mes × servicio (= departamento_actual):
#   servicio_sociodemo.rds     — sociodemografía + financiero
#   servicio_diagnosticos.rds  — diagnósticos (código, descripción, capítulo
#                                 CIE-10) — histórico completo
#   servicio_caci.rds          — condición clínica CACI (ICC/ACV/SCA/TEP/TxC/
#                                 Otros CV) — solo 2024-2026
#
# Para refrescar: volver a copiar data_egresos_2017_2026.rds sobre
# data/data_egresos_2017_2026.rds (o el archivo GRD del corte más reciente
# sobre shiny_grd/data/) y re-correr este script.
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

# ── Clasificador de pagador — misma lógica que shiny_grd/prep_data.R ────────
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
    str_detect(eps_up, "UNIVALLE|UNIVERSIDAD DEL VALLE")              ~ "SLE - MP/Pólizas",
    str_detect(eps_up, "UNISALUD")                                    ~ "SLE - MP/Pólizas",
    str_detect(eps_up, "EXAMENES DE INGRESO|INTERSALUD")              ~ "SLE - MP/Pólizas",
    str_detect(eps_up, "\\bECOPETROL\\b")                             ~ "SLE - MP/Pólizas",
    TRUE                                                              ~ "EAPB"
  )
}

extract_dx_code <- function(x) {
  str_trim(str_extract(coalesce(x, ""), "^[A-Z0-9]+(?=\\s*-)"))
}
extract_dx_desc <- function(x) {
  d <- str_trim(str_replace(coalesce(x, ""), "^[A-Z0-9]+\\s*-\\s*", ""))
  if_else(d == "", NA_character_, d)
}

# ── Capítulo CIE-10 a partir del código (proxy de "grupo de condición
# relevante" para el histórico 2017-2023, donde no existe clasificación CACI).
cie10_chapter <- function(code) {
  letter <- str_sub(str_to_upper(coalesce(code, "")), 1, 1)
  num    <- suppressWarnings(as.numeric(str_extract(code, "(?<=^[A-Z])[0-9]+")))
  case_when(
    is.na(letter) | letter == ""                        ~ "Sin dato",
    letter %in% c("A", "B")                              ~ "Infecciosas y parasitarias",
    letter %in% c("C") | (letter == "D" & num <= 48)      ~ "Neoplasias",
    letter == "D" & num > 48                              ~ "Sangre y sist. inmunitario",
    letter == "E"                                         ~ "Endocrinas, nutricionales y metabólicas",
    letter == "F"                                         ~ "Trastornos mentales y del comportamiento",
    letter == "G"                                         ~ "Sistema nervioso",
    letter == "H" & num <= 59                             ~ "Ojo y anexos",
    letter == "H" & num > 59                              ~ "Oído y apófisis mastoides",
    letter == "I"                                         ~ "Sistema circulatorio",
    letter == "J"                                         ~ "Sistema respiratorio",
    letter == "K"                                         ~ "Sistema digestivo",
    letter == "L"                                         ~ "Piel y tejido subcutáneo",
    letter == "M"                                         ~ "Sistema osteomuscular",
    letter == "N"                                         ~ "Sistema genitourinario",
    letter == "O"                                         ~ "Embarazo, parto y puerperio",
    letter == "P"                                         ~ "Afecciones del periodo perinatal",
    letter == "Q"                                         ~ "Malformaciones congénitas",
    letter == "R"                                         ~ "Síntomas y hallazgos anormales",
    letter %in% c("S", "T")                               ~ "Traumatismos y envenenamientos",
    letter %in% c("V", "W", "X", "Y")                     ~ "Causas externas",
    letter == "Z"                                         ~ "Factores que influyen en el estado de salud",
    TRUE                                                  ~ "Otro"
  )
}

# ── Limpieza común ────────────────────────────────────────────────────────
data_egresos <- data_egresos %>%
  mutate(
    fecha_de_egreso = as_date(fecha_de_egreso),
    fecha_ingreso    = as_date(fecha_ingreso),
    año = year(fecha_de_egreso),
    mes = month(fecha_de_egreso),
    edad = suppressWarnings(as.numeric(as.character(edad))),
    sexo = case_when(
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
    tipo_pagador  = classify_payer(eps),
    estancia_dias = estancia_horas / 24,
    valor_factura = suppressWarnings(as.numeric(as.character(valor_factura))),
    # Diagnóstico de egreso si existe, si no el de ingreso — el "porqué" del
    # encuentro es más preciso al alta, pero muchos AMBULATORIO solo traen
    # diagnóstico de ingreso.
    dx_texto = coalesce(diagnostico_egreso_principal, diagnostico_ingreso_princial),
    dx_cod   = extract_dx_code(dx_texto),
    dx_desc  = extract_dx_desc(dx_texto),
    dx_capitulo = cie10_chapter(dx_cod)
  ) %>%
  filter(!is.na(año), !is.na(departamento_actual), !is.na(numero_de_ingreso)) %>%
  rename(servicio = departamento_actual)

message(sprintf("Encuentros con servicio y año válidos: %s (%d - %d)",
                format(nrow(data_egresos), big.mark = "."),
                min(data_egresos$año), max(data_egresos$año)))

# admisiones únicas por fila-agregación: cada admisión puede tener varios
# cargos en la base cruda.
data_egresos_adm <- data_egresos %>%
  distinct(numero_de_ingreso, .keep_all = TRUE)

# ══════════════════════════════════════════════════════════════════════════
# 1. Sociodemografía + financiero por servicio
# ══════════════════════════════════════════════════════════════════════════
servicio_sociodemo <- data_egresos_adm %>%
  group_by(año, mes, servicio, tipo_de_atencion, edad_grupo, sexo, tipo_pagador) %>%
  summarise(
    n_egresos          = n(),
    estancia_dias_prom = mean(estancia_dias, na.rm = TRUE),
    valor_factura_total = sum(valor_factura, na.rm = TRUE),
    valor_factura_prom  = mean(valor_factura, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(año, mes, servicio)

message(sprintf("servicio_sociodemo: %d filas — %.1f KB",
                nrow(servicio_sociodemo),
                as.numeric(object.size(servicio_sociodemo)) / 1e3))
saveRDS(servicio_sociodemo, file.path(out_dir, "servicio_sociodemo.rds"), compress = "xz")

# ══════════════════════════════════════════════════════════════════════════
# 2. Diagnósticos por servicio (histórico completo, capítulo CIE-10)
# ══════════════════════════════════════════════════════════════════════════
servicio_diagnosticos <- data_egresos_adm %>%
  filter(!is.na(dx_cod), dx_cod != "") %>%
  group_by(año, mes, servicio, tipo_de_atencion, dx_cod, dx_desc, dx_capitulo) %>%
  summarise(n_egresos = n(), .groups = "drop") %>%
  arrange(año, mes, servicio, desc(n_egresos))

message(sprintf("servicio_diagnosticos: %d filas — %.1f KB",
                nrow(servicio_diagnosticos),
                as.numeric(object.size(servicio_diagnosticos)) / 1e3))
saveRDS(servicio_diagnosticos, file.path(out_dir, "servicio_diagnosticos.rds"), compress = "xz")

# ══════════════════════════════════════════════════════════════════════════
# 3. Condición clínica CACI por servicio (solo 2024-2026)
# ══════════════════════════════════════════════════════════════════════════
grd_files <- list.files(out_dir, pattern = "data_grd_2_.*_II\\.(rds|rda)", full.names = TRUE)

if (length(grd_files) > 0) {

  import_grd <- function(f) {
    if (grepl("\\.rda$", f)) {
      e <- new.env(); load(f, envir = e); df <- get(ls(e)[1], envir = e)
    } else {
      df <- readRDS(f)
    }
    df <- df %>% janitor::clean_names()
    # Misma normalización que shiny_grd/global.R::safe_import_grd(): usar
    # caci_3 si existe (coalescido con caci si también existe), si no
    # caci_final. clean_names() convierte "año" -> "ano" (pierde la tilde),
    # así que "año"/"mes" NO se usan de aquí — se recalculan desde fechas.
    if ("caci_3" %in% names(df) && "caci" %in% names(df)) {
      df$caci_norm <- dplyr::coalesce(df$caci_3, df$caci)
    } else if ("caci_3" %in% names(df)) {
      df$caci_norm <- df$caci_3
    } else if ("caci_final" %in% names(df)) {
      df$caci_norm <- df$caci_final
    } else {
      df$caci_norm <- NA_character_
    }
    df
  }

  data_grd_raw <- bind_rows(lapply(grd_files, import_grd))

  # Mismo mapa de normalización que shiny_grd/global.R (caci_map/recode_caci):
  # valores crudos de la base -> etiqueta limpia, jerarquía clínica fija.
  caci_levels <- c("ICC", "ACV", "SCA", "TEP", "TxC", "Otros CV")
  caci_map <- c(
    ICC = "ICC", ACV = "ACV", SCA = "SCA", TEP = "TEP", TXC = "TxC",
    CARDIO_OTHER = "Otros CV", OTROS_CV = "Otros CV", OTRO_CV = "Otros CV"
  )
  recode_caci <- function(x) {
    x_clean <- str_replace_all(str_to_upper(as.character(x)), "\\s+", "_")
    factor(dplyr::recode(x_clean, !!!caci_map, .default = NA_character_),
           levels = caci_levels)
  }

  servicio_caci <- data_grd_raw %>%
    mutate(
      fecha_ingreso    = as.Date(fecha_ingreso),
      fecha_de_egreso  = as.Date(fecha_de_egreso),
      caci  = recode_caci(caci_norm),
      año   = as.integer(year(coalesce(fecha_ingreso, fecha_de_egreso))),
      mes   = as.integer(month(coalesce(fecha_ingreso, fecha_de_egreso))),
      valor_factura = suppressWarnings(as.numeric(as.character(valor_factura)))
    ) %>%
    filter(año >= 2024L, !is.na(año), !is.na(departamento_actual), !is.na(caci)) %>%
    rename(servicio = departamento_actual) %>%
    group_by(año, mes, servicio, tipo_de_atencion, caci) %>%
    summarise(
      n_casos = n_distinct(numero_de_ingreso),
      valor_factura_total = sum(valor_factura, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(año, mes, servicio)

  message(sprintf("servicio_caci: %d filas — %.1f KB (2024-2026)",
                  nrow(servicio_caci),
                  as.numeric(object.size(servicio_caci)) / 1e3))
  saveRDS(servicio_caci, file.path(out_dir, "servicio_caci.rds"), compress = "xz")

} else {
  message("[AVISO] No se encontró archivo data_grd_2_*_II.(rds|rda) en ", out_dir,
          " — se omite servicio_caci.rds.")
}

message("\nListo. Archivos guardados en ", normalizePath(out_dir))
