################################################################################
# inactive_stay_cost.R — Costo de la ESTANCIA INACTIVA por responsable
# DIME Clínica Neurocardiovascular
#
# ADITIVO: no toca ningún cálculo existente de costos, ventas ni márgenes.
#
# ── CÓMO SE VALORIZA (v2, 2026-08-20) ────────────────────────────────────────
# Antes se aplicaba una tarifa SOAT PLANA (503.300 COP/día) a todos los días
# inactivos. Eso sobrevaloraba la hospitalización —que es donde ocurre la
# mayoría de los días— porque DIME allí factura 176.000 y le cuesta 300.000.
#
# Ahora cada día inactivo se valoriza con la tarifa que corresponde a SU
# servicio y SU pagador:
#
#   1. Se identifica el SERVICIO donde estuvo el paciente cruzando el censo por
#      documento + mes, tomando el servicio donde acumuló más días. Cobertura
#      94 %. No se usa el día exacto porque la columna `fecha` del libro EI es
#      un marcador de mes (siempre día 1) — y las columnas que deberían traer
#      inicio/fin de la inactiva contienen texto ("RVM", "CIR"), no fechas.
#   2. Se mapea la EAPB del libro EI al nombre_cliente de ventas y se toma la
#      tarifa real de esa EAPB en ese servicio (bed_value$por_eapb).
#   3. Respaldos en cascada: mediana del servicio -> SOAT. El campo `origen`
#      deja trazable qué se usó en cada fila.
#
# ── DOS BASES, NO INTERCAMBIABLES ────────────────────────────────────────────
#   costo_dime  = lo que a DIME le cuesta sostener esa cama (tabla CUPS).
#                 Mide RECURSO CONSUMIDO SIN PRODUCCIÓN. Es el argumento de
#                 gestión interna. -> BASE POR DEFECTO
#   tarifa_fact = lo que el pagador paga por ese día (ventas reales).
#                 Mide INGRESO DEJADO DE PERCIBIR. Útil para negociar con EAPB.
#
# En hospitalización la tarifa (176.000) está POR DEBAJO del costo (300.000),
# así que las dos bases no sólo difieren en magnitud: apuntan a problemas
# distintos. Por eso el tablero deja elegir y nunca las suma.
#
# Requiere bed_value cargado antes (ver orden de source en global.R).
################################################################################

EI_UVB_2026 <- 12110

EI_TARIFAS_SOAT_2026 <- tibble::tribble(
  ~codigo,  ~servicio,                              ~uvb,    ~cop_dia,
  "38131",  "Habitación unipersonal (3er nivel)",   48.61,   588700,
  "38132",  "Habitación bipersonal (3er nivel)",    41.56,   503300,
  "38133",  "Habitación tres camas (3er nivel)",    34.55,   418400,
  "38134",  "Habitación 4+ camas (3er nivel)",      31.12,   376900,
  "38525",  "UCI — sala especial",                 187.38,  2269200,
  "38825",  "Cuidado intermedio — sala especial",  100.78,  1220400
)
EI_SOAT_FALLBACK <- 503300   # sólo para filas sin servicio identificable

# Mapeo EAPB: como aparece en el libro EI -> como aparece en ventas.
# Ampliar aquí si entra una EAPB nueva; sin mapeo cae al respaldo por servicio.
EI_MAP_EAPB <- c(
  "SANITAS EPS" = "ENTIDAD PROMOTORA DE SALUD SANITAS SAS",
  "SANITAS PGP" = "ENTIDAD PROMOTORA DE SALUD SANITAS SAS",
  "NUEVA EPS"   = "NUEVA EPS S A",
  "SURA"        = "EPS Y MEDICINA PREPAGADA SURAMERICANA S A",
  "COLSANITAS"  = "COLSANITAS S A COMPANIA DE MEDICINA PREPAGADA",
  "MEDISANITAS" = "MEDISANITAS S.A.S COMPANIA DE MEDICINA PREPAGADA",
  "COMFENALCO"  = "COMFENALCO",
  "SOS"         = "SERVICIO OCCIDENTAL DE SALUD SA SOS",
  "UNIVALLE"    = "UNIVERSIDAD DEL VALLE",
  "EMSSANAR"    = "EMSSANAR ENTIDAD PROMOTORA DE SALUD S A S"
)

# Estación del censo -> grupo de cama tarifado
EI_MAP_SERVICIO <- c(
  "UCI" = "UCI", "UCIN" = "UCIN (intermedio)",
  "UCIN ANGIO" = "UCIN (intermedio)", "UCIN RESPIRATORIOS" = "UCIN (intermedio)",
  "PISO HOSP" = "Hospitalización", "URGENCIAS OBS" = "Hospitalización"
)

# ── Agregador reutilizable ────────────────────────────────────────────────────
# Se expone a nivel global (y no dentro del local()) porque la app lo vuelve a
# llamar sobre el detalle YA FILTRADO por período: antes los paneles "Impacto
# por EAPB" y "Causas IPS/EAPB" mostraban siempre el acumulado 2024-2026,
# ignorando el selector de año. Un solo agregador garantiza que el total y el
# período usen exactamente la misma aritmética.
ei_build_aggs <- function(d, rate_col) {
  r <- d[[rate_col]]
  x <- d %>% mutate(c_ips = dias_ips * r, c_eps = dias_eps * r,
                    c_pac = dias_pac * r, c_tot = dias_total * r)

  mensual <- x %>% group_by(periodo) %>%
    summarise(casos = n(),
              dias_ips = sum(dias_ips), dias_eps = sum(dias_eps),
              costo_ips = sum(c_ips), costo_eps = sum(c_eps),
              costo_total = sum(c_tot), .groups = "drop") %>% arrange(periodo)

  list(
    mensual  = mensual,
    por_eapb = x %>% group_by(eapb) %>%
      summarise(casos = n(), dias_ips = sum(dias_ips), dias_eps = sum(dias_eps),
                costo_ips = sum(c_ips), costo_eps = sum(c_eps),
                costo_total = sum(c_tot), .groups = "drop") %>% arrange(desc(costo_total)),
    por_causa_ips = x %>% filter(dias_ips > 0, !is.na(causa_1_de_estancia_inactiva_por_ips)) %>%
      group_by(causa = stringr::str_squish(causa_1_de_estancia_inactiva_por_ips)) %>%
      summarise(casos = n(), dias = sum(dias_ips), costo = sum(c_ips), .groups = "drop") %>%
      arrange(desc(costo)),
    por_causa_eps = x %>% filter(dias_eps > 0, !is.na(causa_1_de_estancia_inactiva_por_eps)) %>%
      group_by(causa = stringr::str_squish(causa_1_de_estancia_inactiva_por_eps)) %>%
      summarise(casos = n(), dias = sum(dias_eps), costo = sum(c_eps), .groups = "drop") %>%
      arrange(desc(costo)),
    resumen = list(
      n_meses         = nrow(mensual),
      periodo_rango   = if (nrow(mensual))
                          paste(format(range(mensual$periodo), "%Y-%m"), collapse = " a ")
                        else NA_character_,
      mediana_mes_ips = median(mensual$costo_ips, na.rm = TRUE),
      mediana_mes_eps = median(mensual$costo_eps, na.rm = TRUE),
      mediana_mes_total = median(mensual$costo_total, na.rm = TRUE),
      p25_mes_ips     = unname(quantile(mensual$costo_ips, .25, na.rm = TRUE)),
      p75_mes_ips     = unname(quantile(mensual$costo_ips, .75, na.rm = TRUE)),
      dias_ips_total  = sum(x$dias_ips), dias_eps_total = sum(x$dias_eps),
      costo_ips_total = sum(x$c_ips), costo_eps_total = sum(x$c_eps),
      costo_total     = sum(x$c_tot)
    )
  )
}

ei_cost <- local({

  src <- if (exists("bd_file") && !is.na(bd_file) && file.exists(bd_file)) bd_file else NA_character_
  if (is.na(src)) {
    message("[LOS-App] Estancia inactiva: sin libro fuente — módulo desactivado.")
    return(NULL)
  }

  raw <- suppressMessages(readxl::read_excel(src, sheet = "BD trabajo (3)")) %>%
    janitor::clean_names()
  num  <- function(x) suppressWarnings(as.numeric(x))
  norm <- function(x) stringr::str_remove_all(as.character(x), "[^0-9]")

  d <- raw %>%
    mutate(
      fecha_d  = suppressWarnings(as.Date(fecha)),
      periodo  = lubridate::floor_date(fecha_d, "month"),
      anio     = lubridate::year(fecha_d),
      mes_n    = lubridate::month(fecha_d),
      # Mismo criterio que data_ei/data_bd_inac, para que el filtro de
      # Responsable de la pestaña se aplique igual en los tres bloques.
      responsable = dplyr::case_when(
        stringr::str_detect(dplyr::coalesce(clasificacion_de_la_estancia_inactiva, ""),
                            stringr::regex("IPS",      ignore_case = TRUE)) ~ "IPS",
        stringr::str_detect(dplyr::coalesce(clasificacion_de_la_estancia_inactiva, ""),
                            stringr::regex("EPS",      ignore_case = TRUE)) ~ "EPS",
        stringr::str_detect(dplyr::coalesce(clasificacion_de_la_estancia_inactiva, ""),
                            stringr::regex("paciente", ignore_case = TRUE)) ~ "Paciente",
        TRUE ~ "No clasificado"
      ),
      eapb     = dplyr::coalesce(stringr::str_squish(eps), "SIN DATO"),
      idn      = norm(identificacion),
      dias_ips = dplyr::coalesce(num(total_dias_de_estancia_por_ips), 0),
      dias_eps = dplyr::coalesce(num(total_dias_de_estancia_por_eps), 0),
      dias_pac = dplyr::coalesce(num(total_dias_de_estancia_por_paciente), 0)
    ) %>%
    filter(!is.na(periodo)) %>%
    mutate(dias_total = dias_ips + dias_eps + dias_pac) %>%
    filter(dias_total > 0)

  # ── 1. Servicio dominante por paciente-mes (desde el censo) ────────────────
  serv_pac <- if (exists("data_los_serv")) {
    data_los_serv %>%
      mutate(idn = norm(id),
             ym  = lubridate::floor_date(as.Date(fecha_egreso_movimiento_cama), "month")) %>%
      filter(idn != "", !is.na(ym), !is.na(estacion_2)) %>%
      group_by(idn, ym, est = estacion_2) %>%
      summarise(dd = sum(dif_bed_serv, na.rm = TRUE), .groups = "drop") %>%
      group_by(idn, ym) %>% slice_max(dd, n = 1, with_ties = FALSE) %>% ungroup() %>%
      transmute(idn, ym, servicio_cama = unname(EI_MAP_SERVICIO[est]))
  } else tibble(idn = character(), ym = as.Date(character()), servicio_cama = character())

  # ── 2. Tarifas de referencia ───────────────────────────────────────────────
  tar_eapb <- if (!is.null(bed_value)) bed_value$por_eapb %>%
                 select(servicio_cama, eapb_v = eapb, tarifa_eapb = tarifa_med)
              else tibble(servicio_cama = character(), eapb_v = character(),
                          tarifa_eapb = numeric())
  tar_serv <- if (!is.null(bed_value)) bed_value$por_servicio %>%
                 select(servicio_cama, tarifa_serv = tarifa_med)
              else tibble(servicio_cama = character(), tarifa_serv = numeric())
  costo_serv <- if (exists("BED_COSTO_CUPS_2026"))
                  BED_COSTO_CUPS_2026 %>% select(servicio_cama, costo_serv = costo_dime)
                else tibble(servicio_cama = character(), costo_serv = numeric())

  d <- d %>%
    left_join(serv_pac, by = c("idn" = "idn", "periodo" = "ym")) %>%
    mutate(eapb_v = unname(EI_MAP_EAPB[eapb])) %>%
    left_join(tar_eapb,   by = c("servicio_cama", "eapb_v")) %>%
    left_join(tar_serv,   by = "servicio_cama") %>%
    left_join(costo_serv, by = "servicio_cama") %>%
    mutate(
      origen = case_when(
        !is.na(tarifa_eapb) ~ "EAPB × servicio",
        !is.na(tarifa_serv) ~ "mediana del servicio",
        TRUE                ~ "sin servicio → SOAT"
      ),
      # BASE A — tarifa facturada (ingreso dejado de percibir)
      tarifa_fact = dplyr::coalesce(tarifa_eapb, tarifa_serv, EI_SOAT_FALLBACK),
      # BASE B — costo DIME (recurso consumido). Respaldo: hospitalización,
      # que concentra ~71 % de los días inactivos identificados.
      costo_dime  = dplyr::coalesce(costo_serv, 300000)
    )

  # ── 3. Agregados para AMBAS bases ──────────────────────────────────────────
  # `d` lleva anio/mes_n/responsable para que la app pueda filtrar el detalle
  # por período y volver a llamar ei_build_aggs() con la misma aritmética.
  list(
    detalle   = d,
    costo     = ei_build_aggs(d, "costo_dime"),    # base por defecto
    tarifa    = ei_build_aggs(d, "tarifa_fact"),
    cobertura = d %>% count(origen) %>% mutate(pct = round(100 * n / sum(n), 1)),
    serv_dist = d %>% count(servicio_cama),
    tarifas   = EI_TARIFAS_SOAT_2026
  )
})

if (!is.null(ei_cost)) {
  cb <- ei_cost$cobertura
  message("[LOS-App] Estancia inactiva v2 (EAPB × servicio) | cobertura ",
          paste(sprintf("%s %s%%", cb$origen, cb$pct), collapse = " · "),
          " | mediana mensual IPS: costo ",
          formatC(round(ei_cost$costo$resumen$mediana_mes_ips), format = "d", big.mark = "."),
          " / tarifa ",
          formatC(round(ei_cost$tarifa$resumen$mediana_mes_ips), format = "d", big.mark = "."))
}
