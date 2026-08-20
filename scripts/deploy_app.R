################################################################################
# deploy_app.R — Desplegar UN dashboard a shinyapps.io
# DIME Clínica Neurocardiovascular
#
# POR QUÉ ESTÁ SEPARADO
# ---------------------
# Desplegar los tres seguidos provoca HTTP 409 ("there are N tasks already in
# progress"): shinyapps.io sólo admite una tarea por cuenta a la vez y el
# siguiente despliegue arranca mientras el anterior aún compila. Además, un
# timeout del cliente al sondear NO significa que el build fallara — suele
# terminar bien en el servidor. Este script:
#   - despliega una sola app,
#   - espera a que no haya tareas en vuelo antes de empezar,
#   - reintenta el 409 con backoff largo,
#   - distingue "timeout de sondeo" de "fallo real" y verifica por HTTP.
#
# USO
#   Rscript scripts/deploy_app.R grd
#   Rscript scripts/deploy_app.R mort
#   Rscript scripts/deploy_app.R los
#   Rscript scripts/deploy_app.R los --check      # sólo verificar, no desplegar
#
# Ejecutar de a uno y esperar a que termine antes del siguiente.
################################################################################

suppressPackageStartupMessages({ library(here); library(rsconnect) })

ACCOUNT <- "8cq8ch-juan0sebastian-hurtado0zapata"
SERVER  <- "shinyapps.io"

# Manifiesto explícito para LOS: .rscignore NO admite comodines ni rutas
# anidadas, así que la única forma de excluir data/data_cense_* y los cost RDS
# crudos (≈45 MB) es enumerar lo que sí va.
#
# OJO — supplies/ NO es sólo material interno. global.R lee DOS libros de ahí:
#   :474  list.files(supplies, pattern = "KPI")      -> pestaña Giro Cama
#   :477  list.files(supplies, pattern = "^6_Table") -> pestaña Est. Inactivas
# Excluir la carpeta entera rompe ambas pestañas ("object 'nombre_ind' not
# found"). Se incluyen esos dos .xlsx y se dejan fuera los PPTX y el censo
# crudo. Si cambia el nombre de esos libros, actualiza estas rutas.
# Se envían COPIAS con nombre ASCII. Los originales llevan caracteres no-ASCII
# ("KPI´S…" U+00B4, "…Gráf…" á) que no sobreviven el viaje macOS -> bundle ->
# Linux: en el servidor list.files(pattern="KPI") devolvía character(0) y la
# pestaña Giro Cama moría con «`path` does not exist: 'NA'».
# Regenerar las copias si cambian los originales:
#   cd shiny_los/supplies
#   cp "KPI´S PROYECTOS DIME jun 2026.xlsx"   KPI_giro_cama.xlsx
#   cp "6_Table_Gráf_estan_inac_JUN_2026.xlsx" 6_Table_estancias_inactivas.xlsx
# Nombres FIJOS: no cambian aunque el libro mensual pase de 7_Table_ a 8_Table_.
# Los regenera scripts/refresh_los_supplies.R, que elige el prefijo numérico más
# alto. Correr ese script ANTES de desplegar.
LOS_SUPPLIES <- c(
  "supplies/KPI_giro_cama.xlsx",
  "supplies/estancias_inactivas_actual.xlsx"
)

LOS_FILES <- c(
  "app.R", "global.R",
  "inactive_stay_cost.R",          # módulo de costo de estancia inactiva
  "bed_value.R",                   # economía del día-cama (pestaña Valor de Cama)
  "data/data_grd_2_2025_II.rda", "data/data_grd_2_2026_II.rda",
  "data/data_los_processed.rds", "data/los_cost_compact.rds",
  "data/bed_value.rds",            # lo genera prep_data.R — sin él la pestaña se apaga
  "stay_length/estancia_inactiva_costo_2026/estancia_inactiva.xlsx",
  LOS_SUPPLIES,
  "www/fullscreen.js", "www/logo.png", "www/styles.css"
)

APPS <- list(
  grd  = list(dir = "shiny_grd",        name = "grd-dime",        files = NULL),
  mort = list(dir = "shiny_mortalidad", name = "mortalidad-dime", files = NULL),
  los  = list(dir = "shiny_los",        name = "los-dime",        files = LOS_FILES)
)

args <- commandArgs(trailingOnly = TRUE)
key  <- args[!startsWith(args, "--")][1]
check_only <- any(args == "--check")

if (is.na(key) || !key %in% names(APPS))
  stop("Uso: Rscript scripts/deploy_app.R <grd|mort|los> [--check]", call. = FALSE)

app <- APPS[[key]]
url <- sprintf("https://%s.shinyapps.io/%s/", ACCOUNT, app$name)

app_status <- function() {
  a <- tryCatch(rsconnect::applications(account = ACCOUNT, server = SERVER),
                error = function(e) NULL)
  if (is.null(a)) return(NULL)
  a[a$name == app$name, , drop = FALSE]
}

http_code <- function(u, timeout = 90) {
  out <- suppressWarnings(system2("curl",
    c("-s", "-o", "/dev/null", "-w", "%{http_code}", "--max-time", timeout, u),
    stdout = TRUE, stderr = FALSE))
  out[length(out)]
}

verify <- function() {
  cat("\n── Verificando ", app$name, " ──\n", sep = "")
  st <- app_status()
  if (!is.null(st) && nrow(st))
    cat("  estado servidor : ", st$status[1], " | actualizado: ", st$updated_time[1], "\n", sep = "")
  # 202 = arrancando (app dormida); reintentar hasta 200
  for (i in 1:6) {
    code <- http_code(url)
    cat(sprintf("  HTTP %s (intento %d)\n", code, i))
    if (code == "200") { cat("  OK — la app responde.\n"); return(TRUE) }
    Sys.sleep(15)
  }
  cat("  AVISO: no devolvió 200. Revisar en el panel de shinyapps.io.\n")
  FALSE
}

if (check_only) { verify(); quit(save = "no") }

# ── Esperar a que no haya tareas en vuelo ────────────────────────────────────
cat("── Desplegando ", app$name, " desde ", app$dir, " ──\n", sep = "")
if (!is.null(app$files)) {
  falta <- app$files[!file.exists(file.path(app$dir, app$files))]
  if (length(falta))
    stop("Faltan archivos del manifiesto: ", paste(falta, collapse = ", "),
         "\n  -> Si falta data/data_los_processed.rds, regenéralo:\n",
         "     cd shiny_los && Rscript -e \"source('global.R')\"", call. = FALSE)
}

deploy_args <- list(appDir = app$dir, appName = app$name, forceUpdate = TRUE)
if (!is.null(app$files)) deploy_args$appFiles <- app$files

MAX <- 4
for (attempt in seq_len(MAX)) {
  cat(sprintf("\n[intento %d/%d] %s\n", attempt, MAX, format(Sys.time(), "%H:%M:%S")))
  res <- tryCatch({ do.call(rsconnect::deployApp, deploy_args); "ok" },
                  error = function(e) conditionMessage(e))

  if (identical(res, "ok")) { cat("\n[OK] Despliegue completado.\n"); verify(); break }

  is_409     <- grepl("409|tasks already in progress", res)
  is_timeout <- grepl("Timeout|Failed to perform HTTP", res)

  if (is_409) {
    wait <- 180 * attempt
    cat("  409: hay otra tarea en curso en la cuenta. Esperando ", wait, " s...\n", sep = "")
    if (attempt < MAX) Sys.sleep(wait) else cat("  Sin más reintentos.\n")
  } else if (is_timeout) {
    # El bundle ya subió; el build casi siempre termina bien en el servidor.
    cat("  Timeout de SONDEO (no del build). El servidor suele terminar igual.\n")
    Sys.sleep(120)
    if (verify()) break
    if (attempt == MAX) cat("  Sin más reintentos.\n")
  } else {
    cat("  ERROR real: ", res, "\n", sep = ""); break
  }
}
