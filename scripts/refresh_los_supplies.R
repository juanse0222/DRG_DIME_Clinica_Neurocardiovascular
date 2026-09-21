################################################################################
# refresh_los_supplies.R — Preparar los insumos de shiny_los/supplies/
# DIME Clínica Neurocardiovascular
#
# QUÉ HACE
#   Toma el libro de estancias inactivas MÁS NUEVO (prefijo numérico más alto:
#   6_Table… JUN, 7_Table… JUL, 8_Table… AGO, …) y el libro KPI más reciente, y
#   genera copias con nombre ASCII fijo:
#
#     supplies/estancias_inactivas_actual.xlsx   <- <n>_Table_… más alto
#     supplies/KPI_giro_cama.xlsx                <- KPI… más reciente
#
# POR QUÉ HACEN FALTA LAS COPIAS
#   Los nombres originales llevan acentos ("…Gráf…", "KPI´S…"). Esos bytes no
#   sobreviven el viaje macOS -> bundle -> Linux: en shinyapps.io
#   list.files(pattern="KPI") devolvía character(0) y la pestaña Giro Cama
#   moría con «`path` does not exist: 'NA'». El despliegue envía SÓLO estas
#   copias ASCII, cuyo nombre no cambia de mes a mes — así el manifiesto de
#   scripts/deploy_app.R queda fijo.
#
# USO
#   Rscript scripts/refresh_los_supplies.R
#   Rscript scripts/refresh_los_supplies.R --dry-run
#
# Correr cada mes DESPUÉS de dejar el nuevo <n>_Table_… en shiny_los/supplies/
# y ANTES de desplegar. run_master_monthly.R lo ejecuta solo.
################################################################################

suppressPackageStartupMessages({ library(here) })

dry <- any(commandArgs(trailingOnly = TRUE) == "--dry-run")
sup <- here("shiny_los", "supplies")
if (!dir.exists(sup)) stop("No existe ", sup, call. = FALSE)

# Elegir el <n>_Table_… con prefijo numérico más alto
tbl <- list.files(sup, pattern = "^[0-9]+_Table.*\\.xlsx$", full.names = TRUE)
tbl <- tbl[!grepl("^~\\$", basename(tbl))]                 # ignorar locks de Excel
if (!length(tbl)) stop("No hay ningún <n>_Table_….xlsx en ", sup, call. = FALSE)
n_pref  <- suppressWarnings(as.integer(sub("^([0-9]+)_.*$", "\\1", basename(tbl))))
tbl_new <- tbl[which.max(replace(n_pref, is.na(n_pref), -1L))]

# Elegir el KPI más reciente por fecha de modificación
kpi <- list.files(sup, pattern = "KPI", full.names = TRUE)
kpi <- kpi[!grepl("^~\\$", basename(kpi))]
kpi <- kpi[basename(kpi) != "KPI_giro_cama.xlsx"]          # no copiarse a sí misma
kpi_new <- if (length(kpi)) kpi[which.max(file.mtime(kpi))] else NA_character_

# Cada copia es INDEPENDIENTE. Antes un `stop()` por KPI faltante abortaba el
# script ANTES de refrescar la copia de estancias inactivas: el libro del mes
# quedaba sin copiar y la app seguía leyendo el mes anterior sin avisar. Ahora
# cada trabajo falla por separado y al final se reporta el estado de los dos.
jobs <- list(
  list(src = tbl_new, dst = file.path(sup, "estancias_inactivas_actual.xlsx"),
       falta = paste0("No hay ningún <n>_Table_….xlsx en ", sup)),
  list(src = kpi_new, dst = file.path(sup, "KPI_giro_cama.xlsx"),
       falta = paste0("No está el libro KPI ORIGINAL en ", sup,
                      " (sólo la copia ASCII). Vuelve a dejarlo ahí al cargar el mes."))
)

cat("\n── Insumos LOS ──\n")
cat("Candidatos <n>_Table: ",
    paste(sprintf("%s(%s)", basename(tbl), n_pref), collapse = ", "), "\n", sep = "")

fallos <- character(0)
for (j in jobs) {
  cat(sprintf("\n%-34s <- %s\n", basename(j$dst),
              if (is.na(j$src)) "(sin origen)" else basename(j$src)))
  if (is.na(j$src)) {
    cat("  [AVISO] ", j$falta, "\n", sep = "")
    # Si la copia existe se sigue usando, pero hay que saber CUÁNTO tiene.
    if (file.exists(j$dst))
      cat(sprintf("  La copia existente se conserva (%s, %.2f MB) — puede estar DESACTUALIZADA.\n",
                  format(file.mtime(j$dst), "%Y-%m-%d"), file.size(j$dst) / 1e6))
    else
      cat("  Y TAMPOCO existe la copia: la pestaña que la usa saldrá vacía.\n")
    fallos <- c(fallos, basename(j$dst))
    next
  }
  if (dry) { cat("  [DRY-RUN] sin copiar\n"); next }
  ok <- file.copy(j$src, j$dst, overwrite = TRUE)
  if (!ok) { cat("  [ERROR] no se pudo copiar\n"); fallos <- c(fallos, basename(j$dst)); next }
  cat(sprintf("  OK (%.2f MB)\n", file.size(j$dst) / 1e6))
}

if (!dry) {
  if (!length(fallos)) {
    cat("\n[OK] Copias ASCII actualizadas.\n")
  } else {
    cat("\n[PARCIAL] Sin refrescar: ", paste(fallos, collapse = ", "), "\n", sep = "")
    cat("          Revisa el aviso de arriba ANTES de desplegar.\n")
  }
  cat("     La app usa estas copias; el manifiesto de despliegue no cambia.\n")
  cat("     Siguiente: Rscript scripts/deploy_app.R los\n")
}

# Código de salida distinto de 0 si algo quedó sin refrescar, para que
# run_master_monthly.R (o cualquier encadenado) no lo dé por bueno en silencio.
if (!dry && length(fallos)) quit(status = 1)

# Nota: el libro viejo (p.ej. 6_Table_…) puede quedarse en supplies/ sin
# problema — siempre gana el prefijo más alto.
