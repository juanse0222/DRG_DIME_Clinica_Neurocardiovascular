################################################################################
# refresh_sales_month.R — Reemplazar un mes de ventas ya cargado
# DIME Clínica Neurocardiovascular
#
# PROBLEMA QUE RESUELVE
# ---------------------
# El CSV de ventas se extrae pocos días después del cierre, así que el mes más
# reciente llega con ~20% de los cargos SIN fecha_factura y los ingresos quedan
# subestimados. Cuando se vuelve a extraer semanas después (ya facturado), NO
# basta con reemplazar el CSV y correr el pipeline:
#
#   1. pipeline_grd.R:243 tiene un guard por fecha máxima. Si el acumulado ya
#      llega a ese mes (o más), imprime "ya están en el acumulado" y NO carga
#      nada. El archivo nuevo se ignora en silencio.
#   2. Aun forzándolo, pipeline_grd.R:266 hace bind_rows() sin deduplicar, así
#      que duplicaría las ~71k filas de julio en lugar de reemplazarlas.
#
# Este script reemplaza el mes completo: borra las filas existentes con
# fecha_cargue >= el mes objetivo, anexa el CSV nuevo y deduplica por
# `transaccion` (el grano natural: 1 fila por transacción).
#
# USO
#   # un solo mes (el más reciente)
#   Rscript scripts/refresh_sales_month.R "data/sales data/sales_Jul_2026.csv"
#
#   # VARIOS meses: hay que pasarlos TODOS juntos.
#   Rscript scripts/refresh_sales_month.R \
#       "data/sales data/sales_May_2026.csv" \
#       "data/sales data/sales_Jun_2026.csv" \
#       "data/sales data/sales_Jul_2026.csv"
#
#   # sólo deduplicar, sin CSV
#   Rscript scripts/refresh_sales_month.R --dedup-only 2026
#
#   Añade --dry-run para ver el efecto sin escribir nada.
#
# IMPORTANTE — por qué hay que pasar todos los CSV juntos:
#   El corte va desde el mes más antiguo HACIA ADELANTE, para no dejar huérfana
#   la cola de cargos del mes siguiente que traía el CSV viejo. Refrescar sólo
#   mayo borraría junio, julio y agosto. Hay un guardarraíl que aborta si algún
#   mes a borrar no viene en los CSV, pero pásalos juntos de entrada.
#
# El mes de corte se deduce del propio CSV; para forzarlo, añade "YYYY-MM".
#
# Después de correrlo, sigue con el pipeline normal:
#   Rscript scripts/run_master_monthly.R 2026-07
################################################################################

suppressPackageStartupMessages({
  library(here); library(lubridate); library(rio); library(tidyverse); library(janitor)
})

# ── Reutilizar los parsers de fechas del pipeline sin ejecutar el pipeline ────
# pipeline_grd.R corre entero al hacer source(); aquí evaluamos SÓLO las
# definiciones de funciones fix_fecha_*, para no duplicar esa lógica.
local({
  exprs <- parse(here("scripts", "pipeline_grd.R"))
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 &&
        as.character(e[[1]])[1] %in% c("<-", "=") &&
        is.name(e[[2]]) && grepl("^fix_fecha", as.character(e[[2]]))) {
      eval(e, envir = globalenv())
    }
  }
})
stopifnot(exists("fix_fecha_sales"), exists("fix_fecha_factura"))

SALES_KEY <- "transaccion"

refresh_sales_month <- function(csv_path,
                                from_month = NULL,   # "YYYY-MM"; por defecto, el mes más
                                                     # antiguo presente en los CSV
                                dry_run = FALSE) {

  # csv_path admite VARIOS archivos. Es obligatorio pasarlos todos juntos cuando
  # se refresca un mes antiguo: el corte va desde from_month HACIA ADELANTE (para
  # no dejar huérfana la cola del mes siguiente que traía el CSV viejo), así que
  # refrescar sólo mayo borraría junio, julio y agosto.
  csv_path <- unlist(csv_path, use.names = FALSE)
  falta <- csv_path[!file.exists(csv_path)]
  if (length(falta)) stop("CSV no encontrado: ", paste(falta, collapse = ", "), call. = FALSE)

  read_one <- function(p) {
    import(p, fill = Inf, skip = 3, header = TRUE) %>%
      clean_names() %>%
      mutate(
        fecha_cargue   = fix_fecha_sales(fecha_cargue),
        fecha_registro = fix_fecha_sales(fecha_registro),
        fecha_egreso   = fix_fecha_sales(fecha_egreso),
        fecha_factura  = fix_fecha_factura(fecha_factura)
      )
  }
  new <- bind_rows(lapply(csv_path, read_one))

  meses_csv <- sort(unique(floor_date(as.Date(new$fecha_cargue), "month")))
  meses_csv <- meses_csv[!is.na(meses_csv)]
  from_date <- if (is.null(from_month)) min(meses_csv) else as.Date(paste0(from_month, "-01"))

  sales_rds <- here("data", "sales data",
                    paste0("data_sales_2024_2025_", year(from_date), ".rds"))
  if (!file.exists(sales_rds)) stop("Acumulado no encontrado: ", sales_rds, call. = FALSE)

  cat("\n── Refrescando ventas desde ", format(from_date, "%Y-%m"), " ──\n", sep = "")
  cat("CSV      : ", paste(basename(csv_path), collapse = ", "), "\n", sep = "")
  cat("Acumulado: ", basename(sales_rds), "\n", sep = "")
  cat("Meses en los CSV: ", paste(format(meses_csv, "%Y-%m"), collapse = ", "), "\n\n", sep = "")

  # Guardarraíl: todo mes que se vaya a borrar debe venir en algún CSV, si no se
  # perdería. Se permite que el último mes del corte no esté (cola incompleta).
  old <- import(sales_rds) %>% clean_names()
  if (is.character(old$fecha_cargue))  old$fecha_cargue  <- fix_fecha_sales(old$fecha_cargue)
  if (is.character(old$fecha_factura)) old$fecha_factura <- fix_fecha_factura(old$fecha_factura)

  meses_borrados <- old %>%
    mutate(m = floor_date(as.Date(fecha_cargue), "month")) %>%
    filter(!is.na(m), m >= from_date) %>%
    count(m) %>% filter(n >= 500) %>% pull(m)          # ignora colas triviales

  huerfanos <- setdiff(format(meses_borrados, "%Y-%m"), format(meses_csv, "%Y-%m"))
  if (length(huerfanos)) {
    stop("Se borrarían meses que NO vienen en los CSV: ",
         paste(huerfanos, collapse = ", "),
         "\n  -> Pasa también esos CSV en la misma llamada, p.ej.:\n",
         "     refresh_sales_month(c('sales_May_2026.csv','sales_Jun_2026.csv',",
         "'sales_Jul_2026.csv'), '2026-05')", call. = FALSE)
  }

  # ── Antes ───────────────────────────────────────────────────────────────────
  billed <- function(df, m) {
    s <- df %>% filter(floor_date(as.Date(fecha_cargue), "month") == m)
    if (!nrow(s)) return("sin filas")
    sprintf("%s cargos, %s sin facturar (%.1f%%)",
            format(nrow(s), big.mark = ","),
            format(sum(is.na(s$fecha_factura)), big.mark = ","),
            100 * sum(is.na(s$fecha_factura)) / nrow(s))
  }
  for (m in meses_csv) {
    m <- as.Date(m, origin = "1970-01-01")
    cat(format(m, "%Y-%m"), " | ANTES: ", billed(old, m),
        "\n        | CSV  : ", billed(new, m), "\n", sep = "")
  }

  # ── Reemplazo ───────────────────────────────────────────────────────────────
  # Se corta desde from_date hacia adelante (no sólo el mes exacto) para que la
  # cola de cargos del mes siguiente que traía el CSV viejo no quede duplicada.
  keep <- old %>% filter(is.na(fecha_cargue) | as.Date(fecha_cargue) < from_date)
  dropped <- nrow(old) - nrow(keep)

  # Duplicados heredados FUERA de la ventana que se reemplaza. Se reportan
  # aparte porque no vienen del CSV nuevo: son de cargas previas sin dedup.
  # A diferencia del censo (donde el dedup ocurre aguas abajo), la ruta CARGOS
  # de pipeline_grd.R NO deduplica, así que cada duplicado DUPLICA plata.
  legacy_dup <- nrow(keep) - n_distinct(keep[[SALES_KEY]])
  if (legacy_dup > 0) {
    cat(sprintf("\n[!] %s filas duplicadas heredadas (fuera de la ventana) — se eliminan.\n",
                format(legacy_dup, big.mark = ",")))
    print(keep %>%
            mutate(m = floor_date(as.Date(fecha_cargue), "month")) %>%
            group_by(m) %>%
            summarise(filas = n(), unicas = n_distinct(.data[[SALES_KEY]]), .groups = "drop") %>%
            mutate(dup = filas - unicas) %>% filter(dup > 0) %>% as.data.frame(),
          row.names = FALSE)
  }

  combined <- bind_rows(keep, new) %>%
    # el CSV nuevo va al final: distinct() conserva la PRIMERA aparición, así
    # que ordenamos para que la fila CON factura gane sobre la que no la tiene.
    arrange(!is.na(fecha_factura)) %>%
    distinct(across(all_of(SALES_KEY)), .keep_all = TRUE)

  cat("\nFilas: ", format(nrow(old), big.mark = ","),
      " -> eliminadas ", format(dropped, big.mark = ","),
      " -> +", format(nrow(new), big.mark = ","), " del CSV",
      " -> ", format(nrow(combined), big.mark = ","), " tras dedup por ", SALES_KEY, "\n", sep = "")
  cat("\n── DESPUÉS ──\n")
  for (m in meses_csv) {
    m <- as.Date(m, origin = "1970-01-01")
    cat(format(m, "%Y-%m"), ": ", billed(combined, m), "\n", sep = "")
  }

  if (dry_run) { cat("\n[DRY-RUN] Nada escrito.\n"); return(invisible(combined)) }

  bdir <- here("data", "sales data", "_backups")
  dir.create(bdir, showWarnings = FALSE, recursive = TRUE)
  bkp <- file.path(bdir, sprintf("%s_%s.rds",
                                 tools::file_path_sans_ext(basename(sales_rds)),
                                 format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.copy(sales_rds, bkp)
  cat("\nBackup: ", bkp, "\n", sep = "")

  export(combined, sales_rds)
  cat("[OK] Acumulado actualizado.\n")
  cat("     Siguiente paso: Rscript scripts/run_master_monthly.R ",
      format(from_date, "%Y-%m"), "\n", sep = "")

  invisible(combined)
}

# ── Sólo deduplicar (sin CSV nuevo) ──────────────────────────────────────────
# Para corregir duplicados heredados, p.ej. junio 2026 quedó cargado dos veces
# y la ruta CARGOS no deduplica → costos e ingresos de ese mes salen al doble.
dedup_sales_accumulator <- function(year = 2026, dry_run = FALSE) {
  sales_rds <- here("data", "sales data", paste0("data_sales_2024_2025_", year, ".rds"))
  d <- import(sales_rds) %>% clean_names()

  rep <- d %>%
    mutate(m = floor_date(as.Date(fecha_cargue), "month")) %>%
    group_by(m) %>%
    summarise(filas = n(), unicas = n_distinct(.data[[SALES_KEY]]), .groups = "drop") %>%
    mutate(dup = filas - unicas) %>% filter(dup > 0)

  cat("\n── Duplicados por mes de cargue ──\n")
  if (!nrow(rep)) { cat("Ninguno. Nada que hacer.\n"); return(invisible(d)) }
  print(as.data.frame(rep), row.names = FALSE)

  out <- distinct(d, across(all_of(SALES_KEY)), .keep_all = TRUE)
  cat(sprintf("\nFilas: %s -> %s (elimina %s)\n",
              format(nrow(d), big.mark = ","), format(nrow(out), big.mark = ","),
              format(nrow(d) - nrow(out), big.mark = ",")))

  if (dry_run) { cat("[DRY-RUN] Nada escrito.\n"); return(invisible(out)) }

  bdir <- here("data", "sales data", "_backups")
  dir.create(bdir, showWarnings = FALSE, recursive = TRUE)
  bkp <- file.path(bdir, sprintf("%s_%s.rds",
                                 tools::file_path_sans_ext(basename(sales_rds)),
                                 format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.copy(sales_rds, bkp); cat("Backup: ", bkp, "\n", sep = "")
  export(out, sales_rds)
  cat("[OK] Acumulado deduplicado. Recorre el pipeline para rehacer los agregados.\n")
  invisible(out)
}

# ── CLI ───────────────────────────────────────────────────────────────────────
if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  a <- commandArgs(trailingOnly = TRUE)
  a <- a[a != "--args"]
  dry <- any(a == "--dry-run")
  if (any(a == "--dedup-only")) {
    yr <- grep("^\\d{4}$", a, value = TRUE)
    dedup_sales_accumulator(year = if (length(yr)) as.integer(yr[1]) else 2026, dry_run = dry)
  } else {
    pos   <- a[!startsWith(a, "--")]
    month <- grep("^\\d{4}-\\d{2}$", pos, value = TRUE)
    csvs  <- setdiff(pos, month)
    if (length(csvs))
      refresh_sales_month(csvs,
                          from_month = if (length(month)) month[1] else NULL,
                          dry_run = dry)
  }
}
