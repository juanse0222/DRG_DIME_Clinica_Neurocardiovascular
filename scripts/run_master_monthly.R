################################################################################
# run_master_monthly.R — Activación mensual completa
# DIME Clínica Neurocardiovascular
#
# Supersedes run_all_monthly.R: adds the LOS/censo chain, a preflight check of
# every source file, a presentation-readiness report, and makes deployment
# OPT-IN instead of automatic.
#
# ─────────────────────────────────────────────────────────────────────────────
# USO
#   Rscript scripts/run_master_monthly.R                 # mes anterior, sin desplegar
#   Rscript scripts/run_master_monthly.R 2026-07         # mes específico
#   Rscript scripts/run_master_monthly.R 2026-07 --deploy        # + shinyapps.io
#   Rscript scripts/run_master_monthly.R --check                 # sólo preflight
#   Rscript scripts/run_master_monthly.R 2026-07 --skip=los,mort # omitir bloques
#
# Deployment is opt-in on purpose: it publishes to shinyapps.io, where the CEO
# and the Clinic Manager see it. Run without --deploy first, read the readiness
# report at the end, then re-run with --deploy once the numbers look right.
################################################################################

################################################################################
# FUENTES DE INFORMACIÓN — descargar ANTES de ejecutar
################################################################################
#
# 1) ADMISIONES (GRD)                                        [Bloque 1]
#    Sistema  : módulo de egresos hospitalarios
#    Guardar  : data/admission_data/admission_<mes_en>_<año>.xls
#    Ejemplo  : data/admission_data/admission_july_2026.xls
#    Nota     : el mes va en inglés y minúscula.
#
# 2) VENTAS / FACTURACIÓN DETALLADA (CUPS, CARGOS, FARMACIA) [Bloque 1]
#    Sistema  : ventas detallado
#    Guardar  : data/sales data/sales_<Mon>_<año>.csv
#    Ejemplo  : data/sales data/sales_Jul_2026.csv
#    Nota     : las 3 primeras filas son encabezado (skip = 3).
#    OJO      : extraer lo más tarde posible en el mes siguiente. Un corte a los
#               3 días deja ~20% de los cargos sin fecha_factura y SUBESTIMA los
#               ingresos del mes. Los costos (fecha_cargue) no se ven afectados.
#
# 3) TABLA DE COSTOS CUPS                                    [Bloque 1]
#    Guardar  : data/data_costs/costo_general_<año>.xlsx  (hoja "<año>")
#    Nota     : sólo cambia cuando planeación publica tarifas nuevas.
#
# 4) CENSO HOSPITALARIO (movimientos de cama)                [Bloque 3 / LOS]
#    Sistema  : censo hospitalario → exportar a Excel
#    Guardar  : shiny_los/supplies/censo_modificado/censo_modificado_<DDMMAAAA>.xls
#    Nota     : es una ventana móvil (~3 meses). El script toma el archivo más
#               reciente de esa carpeta automáticamente. No hay que recortarlo:
#               los meses ya consolidados se absorben por anti-join.
#
# 5) MORTALIDAD MENSUAL                                      [Bloque 2]
#    Guardar  : ~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME/Documentos EDI/2. Mortalidad/DIME_mortality_2026/
#                 <Month>_<año>/DIME_mortality_<Month>_<año>.xlsx
#    Ejemplo  : .../July_2026/DIME_mortality_July_2026.xlsx
#    Nota     : el mes va en inglés y capitalizado.
#
################################################################################

suppressPackageStartupMessages({
  library(here); library(lubridate); library(rio)
  library(tidyverse); library(janitor)
})

################################################################################
# ── Argumentos ────────────────────────────────────────────────────────────────
################################################################################
args         <- commandArgs(trailingOnly = TRUE)
do_deploy    <- any(args == "--deploy")
check_only   <- any(args == "--check")
skip_arg     <- grep("^--skip=", args, value = TRUE)
skip_blocks  <- if (length(skip_arg))
  str_split(str_remove(skip_arg[1], "^--skip="), ",")[[1]] else character()
month_arg    <- grep("^\\d{4}-\\d{2}$", args, value = TRUE)

report_month <- if (length(month_arg)) month_arg[1] else
  format(seq(as.Date(format(Sys.Date(), "%Y-%m-01")), length = 2, by = "-1 month")[2], "%Y-%m")

report_date  <- as.Date(paste0(report_month, "-01"))
report_label <- format(report_date, "%B %Y")
current_year <- year(report_date)

skipped  <- function(b) b %in% skip_blocks
hdr      <- function(x) cat(sprintf("\n%s\n%s\n%s\n", strrep("=", 78), x, strrep("=", 78)))
step     <- function(x) cat(sprintf("\n[%s] %s\n", format(Sys.time(), "%H:%M:%S"), x))

status <- list()   # collected for the readiness report
note   <- function(block, ok, msg) status[[block]] <<- list(ok = ok, msg = msg)

hdr(sprintf("ACTIVACIÓN MENSUAL DIME — %s", report_label))
cat("Deploy:", if (do_deploy) "SÍ (shinyapps.io)" else "no (--deploy para publicar)", "\n")
if (length(skip_blocks)) cat("Omitiendo:", paste(skip_blocks, collapse = ", "), "\n")

################################################################################
# BLOQUE 0 — PREFLIGHT: ¿están todas las fuentes?
################################################################################
hdr("BLOQUE 0 — Verificación de fuentes")

month_en_lower <- tolower(format(report_date, "%B"))
month_en_cap   <- format(report_date, "%B")
mon_abbr       <- format(report_date, "%b")

censo_dir <- here("shiny_los", "supplies", "censo_modificado")
censo_files <- if (dir.exists(censo_dir))
  list.files(censo_dir, pattern = "\\.(xls|xlsx|csv)$", full.names = TRUE) else character()
censo_latest <- if (length(censo_files))
  censo_files[which.max(file.mtime(censo_files))] else NA_character_

mort_xlsx <- path.expand(file.path(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME/Documentos EDI/2. Mortalidad/DIME_mortality_2026",
  paste0(month_en_cap, "_", current_year),
  paste0("DIME_mortality_", month_en_cap, "_", current_year, ".xlsx")))

sources <- tibble::tribble(
  ~bloque,      ~fuente,                    ~ruta,
  "1 GRD",      "Admisiones",               here("data", "admission_data",
                                                 sprintf("admission_%s_%d.xls", month_en_lower, current_year)),
  "1 GRD",      "Ventas detallado",         here("data", "sales data",
                                                 sprintf("sales_%s_%d.csv", mon_abbr, current_year)),
  "1 GRD",      "Tabla de costos",          here("data", "data_costs",
                                                 sprintf("costo_general_%d.xlsx", current_year)),
  "3 LOS",      "Censo hospitalario",       ifelse(is.na(censo_latest), "(carpeta vacía)", censo_latest),
  "2 Mortal.",  "Mortalidad mensual",       mort_xlsx
) %>%
  mutate(existe = file.exists(ruta),
         edad_d = ifelse(existe, round(as.numeric(difftime(Sys.time(), file.mtime(ruta), units = "days")), 1), NA))

print(sources %>%
        mutate(estado = ifelse(existe, "OK", "FALTA"),
               archivo = basename(ruta)) %>%
        select(bloque, fuente, archivo, estado, edad_d) %>%
        as.data.frame(), row.names = FALSE)

if (any(!sources$existe)) {
  cat("\n>> Fuentes faltantes:\n")
  for (i in which(!sources$existe))
    cat("   -", sources$fuente[i], "->", sources$ruta[i], "\n")
  cat("\n   Los bloques afectados se omitirán o fallarán. Ver la lista de\n",
      "  FUENTES DE INFORMACIÓN al inicio de este script.\n")
}

if (check_only) {
  cat("\n[--check] Sólo verificación. Nada ejecutado.\n")
  quit(save = "no", status = if (all(sources$existe)) 0 else 1)
}

################################################################################
# BLOQUE 1 — GRD: admisiones + ventas + costos → RDS → Shiny
################################################################################
if (skipped("grd")) { note("GRD", NA, "omitido (--skip)"); cat("\n[BLOQUE 1] omitido.\n") } else {
hdr("BLOQUE 1 — Pipeline GRD")
tryCatch({
  step("Ejecutando pipeline_grd.R (admisiones → CACI → costos → exports)")
  source(here("scripts", "pipeline_grd.R"))

  cost_src <- here("data", sprintf("data_costo_total_3_%d_II.rds", current_year))
  grd_src  <- here("data", sprintf("data_grd_2_%d_II.rda",  current_year))
  if (!file.exists(cost_src)) stop("No se generó ", basename(cost_src))
  if (!file.exists(grd_src))  stop("No se generó ", basename(grd_src))

  step("Copiando a shiny_grd/data/ y reconstruyendo tablas compactas")
  file.copy(cost_src, here("shiny_grd", "data", basename(cost_src)), overwrite = TRUE)
  file.copy(grd_src,  here("shiny_grd", "data", basename(grd_src)),  overwrite = TRUE)
  source(here("shiny_grd", "prep_data.R"))

  note("GRD", TRUE, "pipeline + shiny_grd actualizados")
}, error = function(e) note("GRD", FALSE, conditionMessage(e)))
}

################################################################################
# BLOQUE 2 — MORTALIDAD: registro + reclasificación + GEE
################################################################################
if (skipped("mort")) { note("Mortalidad", NA, "omitido (--skip)"); cat("\n[BLOQUE 2] omitido.\n") } else {
hdr("BLOQUE 2 — Mortalidad")
tryCatch({
  if (!file.exists(mort_xlsx)) {
    note("Mortalidad", NA, paste("XLSX del mes no encontrado:", basename(mort_xlsx)))
    cat("[BLOQUE 2] Sin archivo del mes — se omite.\n")
  } else {
    registry <- path.expand(paste0("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME/Documentos EDI/2. Mortalidad/",
                                   "DIME_mortality_2025/data/mortality_dime_2017_2026.rds"))
    step("Anexando defunciones al registro histórico")
    data_new <- import(mort_xlsx) %>% clean_names()
    data_mh  <- import(registry)
    for (col in intersect(names(data_new), names(data_mh)))
      if (is.character(data_mh[[col]]) && !is.character(data_new[[col]]))
        data_new[[col]] <- as.character(data_new[[col]])

    updated <- bind_rows(data_mh, data_new) %>%
      distinct(numero_certificado, fecha_defuncion, hora_defuncion, .keep_all = TRUE)
    export(updated, registry)
    cat(sprintf("  Registro: %d → %d filas (máx %s)\n", nrow(data_mh), nrow(updated),
                format(max(as.Date(updated$fecha_defuncion), na.rm = TRUE))))

    step("Reclasificando egresos (update_monthly.R)")
    # OJO: update_monthly.R:79 llama quit(save="no") cuando no hay registros
    # nuevos. Con source() eso MATA este orquestador con exit 0 — parece éxito
    # pero los bloques siguientes nunca corren. Se ejecuta como subproceso para
    # que su quit() muera dentro del subproceso y no aquí.
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(path.expand("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME/Documentos EDI/2. Mortalidad/mortality_analysis"))
    out <- system2("Rscript", "script/update_monthly.R", stdout = TRUE, stderr = TRUE)
    rc  <- attr(out, "status"); rc <- if (is.null(rc)) 0L else rc
    cat(paste0("  | ", out, collapse = "\n"), "\n")
    setwd(old_wd)
    if (rc != 0) stop("update_monthly.R falló (exit ", rc, ")")

    step("Reajustando modelos GEE (prep_mortality.R)")
    source(here("shiny_mortalidad", "prep_mortality.R"))
    note("Mortalidad", TRUE, "registro + GEE actualizados")
  }
}, error = function(e) { try(setwd(here()), silent = TRUE)
                         note("Mortalidad", FALSE, conditionMessage(e)) })
}

################################################################################
# BLOQUE 3 — LOS / CENSO: append guardado + caché + tablas compactas
################################################################################
if (skipped("los")) { note("LOS", NA, "omitido (--skip)"); cat("\n[BLOQUE 3] omitido.\n") } else {
hdr("BLOQUE 3 — LOS / Censo")
tryCatch({
  if (is.na(censo_latest)) {
    note("LOS", NA, "sin export de censo en shiny_los/supplies/censo_modificado/")
    cat("[BLOQUE 3] Sin censo — se omite.\n")
  } else {
    step(paste("Cargando censo:", basename(censo_latest)))

    # Load ONLY the accumulator helpers from LOS_script.R (the rest of that file
    # is exploratory analysis and must not run here).
    los_src <- readLines(here("stay_length", "scripts", "LOS_script.R"))
    cut_at  <- grep("data <- rio::import(CENSE_MASTER)", los_src, fixed = TRUE)[1]
    eval(parse(text = paste(los_src[2:(cut_at - 1)], collapse = "\n")), envir = globalenv())

    # rolling = TRUE: the export is a moving ~3-month window, so it re-ships
    # consolidated months. The append is an anti-join, so only new rows land.
    append_census_month(censo_latest, rolling = TRUE)

    step("Invalidando cachés LOS y sincronizando shiny_los/data/")
    for (p in c(here("data", "data_los_processed.rds"),
                here("shiny_los", "data", "data_los_processed.rds")))
      if (file.exists(p)) { file.remove(p); cat("  borrado:", basename(p), "\n") }

    for (f in c(here("data", "data_cense_2017_2026.rds"),
                here("data", sprintf("data_costo_total_3_%d_II.rds", current_year)),
                here("data", sprintf("data_grd_2_%d_II.rda", current_year))))
      if (file.exists(f)) file.copy(f, here("shiny_los", "data", basename(f)), overwrite = TRUE)

    step("Actualizando insumos supplies/ (copias ASCII, prefijo más alto)")
    # El código de salida SÍ importa: si falta el libro original de algún insumo
    # el script deja la copia ASCII del mes anterior y la app sigue publicando
    # datos viejos sin error visible. No se aborta el bloque (los demás insumos
    # sí se refrescaron y la app funciona), pero tiene que verse.
    rc_sup <- system2("Rscript", here("scripts", "refresh_los_supplies.R"))
    if (!identical(rc_sup, 0L)) {
      cat("\n  ", strrep("!", 68), "\n", sep = "")
      cat("   AVISO: refresh_los_supplies.R terminó con código ", rc_sup, ".\n", sep = "")
      cat("   Al menos una copia ASCII quedó SIN refrescar (ver el detalle arriba).\n")
      cat("   La app se desplegará con ese insumo del mes ANTERIOR.\n")
      cat("   ", strrep("!", 68), "\n\n", sep = "")
    }

    step("Reconstruyendo los_cost_compact.rds")
    source(here("shiny_los", "prep_data.R"))

    # Regenerar la caché del censo que se acaba de invalidar. Es obligatorio:
    #  (a) el manifiesto de despliegue exige data/data_los_processed.rds, y
    #  (b) sin él el contenedor rehace ~30 s de censo en cada arranque en frío.
    # Se corre como subproceso CON wd = shiny_los para que resolve_proj_dir()
    # (global.R:151) resuelva a shiny_los y escriba en shiny_los/data/.
    step("Regenerando caché LOS (data_los_processed.rds) — ~30 s")
    cache_f <- here("shiny_los", "data", "data_los_processed.rds")
    owd <- setwd(here("shiny_los"))
    rc  <- tryCatch(
      system2("Rscript", c("-e", shQuote("suppressMessages(source('global.R'))")),
              stdout = FALSE, stderr = FALSE),
      finally = setwd(owd)
    )
    setwd(owd)
    if (!file.exists(cache_f))
      stop("no se pudo regenerar data_los_processed.rds (exit ", rc, ")")
    cat(sprintf("  caché LOS regenerada (%.1f MB)\n", file.size(cache_f) / 1e6))

    note("LOS", TRUE, "censo anexado, cachés reconstruidas")
  }
}, error = function(e) note("LOS", FALSE, conditionMessage(e)))
}

################################################################################
# BLOQUE 4 — REPORTE DE ALISTAMIENTO (¿listo para presentar?)
################################################################################
hdr("BLOQUE 4 — Alistamiento para presentación")

safe_months <- function(expr) tryCatch(expr, error = function(e) NA_integer_)

cat("\n── Cobertura de datos hasta", report_label, "──\n\n")

cov <- tibble::tibble(
  Dominio = character(), Metrica = character(), Valor = character()
)
addrow <- function(d, m, v) cov <<- add_row(cov, Dominio = d, Metrica = m, Valor = as.character(v))

# GRD
try({
  p <- readRDS(here("shiny_grd", "data", "pte_base.rds"))
  n <- p %>% filter(.data[[grep("^a.o$", names(p), value = TRUE)[1]]] == current_year) %>%
    count(mes_cargue)
  addrow("GRD", "Meses cargados", paste(sort(n$mes_cargue), collapse = ","))
  addrow("GRD", sprintf("Pacientes mes %d", month(report_date)),
         coalesce(n$n[n$mes_cargue == month(report_date)][1], 0L))
}, silent = TRUE)

# Facturación incompleta — el riesgo principal al presentar ingresos
try({
  s <- readRDS(here("data", "sales data", sprintf("data_sales_2024_2025_%d.rds", current_year)))
  sm <- s %>% mutate(mc = floor_date(as.Date(fecha_cargue), "month")) %>%
    filter(mc == report_date)
  pend <- sum(is.na(sm$fecha_factura)); tot <- nrow(sm)
  addrow("Ventas", "Cargos del mes", format(tot, big.mark = ","))
  addrow("Ventas", "Sin facturar",
         sprintf("%s (%.1f%%)", format(pend, big.mark = ","), 100 * pend / max(tot, 1)))
}, silent = TRUE)

# LOS / censo
try({
  d <- clean_names(readRDS(here("data", "data_cense_2017_2026.rds")))
  mm <- floor_date(as.Date(dmy_hm(d$fecha_egreso_movimiento_cama, quiet = TRUE)), "month")
  addrow("LOS", "Movimientos totales", format(nrow(d), big.mark = ","))
  addrow("LOS", sprintf("Movimientos %s", format(report_date, "%Y-%m")),
         format(sum(mm == report_date, na.rm = TRUE), big.mark = ","))
  addrow("LOS", "Último mes con datos", format(max(mm, na.rm = TRUE), "%Y-%m"))
}, silent = TRUE)

# Mortalidad
try({
  r <- readRDS(path.expand(paste0("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME/Documentos EDI/2. Mortalidad/",
                                  "mortality_analysis/data/results_2.rds")))
  rm_ <- floor_date(as.Date(r$fecha_ingreso), "month")
  addrow("Mortalidad", sprintf("Ingresos %s", format(report_date, "%Y-%m")),
         format(sum(rm_ == report_date, na.rm = TRUE), big.mark = ","))
  addrow("Mortalidad", "Último mes con datos", format(max(rm_, na.rm = TRUE), "%Y-%m"))
}, silent = TRUE)

print(as.data.frame(cov), row.names = FALSE)

cat("\n── Resultado por bloque ──\n\n")
for (b in names(status)) {
  s <- status[[b]]
  tag <- if (is.na(s$ok)) "OMITIDO" else if (s$ok) "OK" else "ERROR"
  cat(sprintf("  %-12s %-8s %s\n", b, tag, s$msg))
}

failed <- names(status)[vapply(status, function(s) isFALSE(s$ok), logical(1))]

# Recordatorio del sesgo de facturación — se presenta cada mes
pend_row <- cov$Valor[cov$Dominio == "Ventas" & cov$Metrica == "Sin facturar"]
if (length(pend_row) && !is.na(pend_row)) {
  cat("\n  AVISO: los ingresos de", report_label, "están incompletos:", pend_row,
      "de los cargos\n  aún no tienen fecha_factura. Presentar el margen del mes",
      "como PROVISIONAL.\n")
}

################################################################################
# BLOQUE 5 — DESPLIEGUE (opt-in)
################################################################################
if (!do_deploy) {
  hdr("DESPLIEGUE OMITIDO")
  cat("Revisa el reporte de arriba. Si los números cuadran, despliega UNO A UNO\n")
  cat("(shinyapps.io sólo admite una tarea por cuenta; encadenarlos da HTTP 409):\n\n")
  cat("  Rscript scripts/deploy_app.R grd\n")
  cat("  Rscript scripts/deploy_app.R mort\n")
  cat("  Rscript scripts/deploy_app.R los\n\n")
  cat("Espera a que termine cada uno antes del siguiente. Para sólo verificar:\n")
  cat("  Rscript scripts/deploy_app.R los --check\n")
} else if (length(failed)) {
  hdr("DESPLIEGUE BLOQUEADO")
  cat("Bloques con error:", paste(failed, collapse = ", "), "\n")
  cat("Corrige antes de publicar; no se desplegó nada.\n")
} else {
  hdr("BLOQUE 5 — Despliegue a shinyapps.io")
  # OJO con .rscignore: sólo respeta nombres a la RAÍZ de la app (p.ej.
  # "supplies", "prep_data.R"). Ignora en silencio comodines ("data/*.rds") y
  # rutas anidadas ("data/data_cense_2017_2026.rds"). Para excluir archivos
  # dentro de subcarpetas hay que pasar un manifiesto explícito con appFiles.
  # supplies/ NO es sólo material interno: global.R:474/:477 lee de ahí los
  # libros KPI (Giro Cama) y ^6_Table (Est. Inactivas). Excluirlos rompe ambas
  # pestañas. Ver scripts/deploy_app.R, que es la ruta recomendada.
  los_files <- c(
    "app.R", "global.R",
    "inactive_stay_cost.R",        # módulo de costo de estancia inactiva
    "bed_value.R",                 # economía del día-cama (pestaña Valor de Cama)
    "data/data_grd_2_2025_II.rda", "data/data_grd_2_2026_II.rda",
    "data/data_los_processed.rds", "data/los_cost_compact.rds",
    "data/bed_value.rds",          # lo genera prep_data.R
    "stay_length/estancia_inactiva_costo_2026/estancia_inactiva.xlsx",
    # copias ASCII de nombre fijo (las regenera refresh_los_supplies.R)
    "supplies/KPI_giro_cama.xlsx",
    "supplies/estancias_inactivas_actual.xlsx",
    "www/fullscreen.js", "www/logo.png", "www/styles.css"
  )

  apps <- list(
    list(dir = "shiny_grd",        name = "grd-dime",        block = "GRD"),
    list(dir = "shiny_mortalidad", name = "mortalidad-dime", block = "Mortalidad"),
    list(dir = "shiny_los",        name = "los-dime",        block = "LOS",
         files = los_files)
  )
  for (a in apps) {
    st <- status[[a$block]]
    if (!is.null(st) && is.na(st$ok)) {
      cat(sprintf("  %-18s omitido (su bloque no corrió)\n", a$name)); next
    }
    step(paste("Desplegando", a$name))
    tryCatch({
      if (!is.null(a$files)) {
        falta <- a$files[!file.exists(file.path(a$dir, a$files))]
        if (length(falta))
          stop("faltan archivos del manifiesto: ", paste(falta, collapse = ", "))
        rsconnect::deployApp(a$dir, appName = a$name,
                             appFiles = a$files, forceUpdate = TRUE)
      } else {
        rsconnect::deployApp(a$dir, appName = a$name, forceUpdate = TRUE)
      }
      cat("  OK:", a$name, "\n")
    }, error = function(e) cat("  ERROR desplegando", a$name, ":", conditionMessage(e), "\n"))
  }
}

################################################################################
hdr(sprintf("FIN — %s", report_label))
cat("GRD:       https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/grd-dime/\n")
cat("Mortalidad:https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/mortalidad-dime/\n")
cat("LOS:       https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/los-dime/\n\n")
