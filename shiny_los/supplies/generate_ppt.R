################################################################################
# generate_ppt.R — Giro Cama & Estancia · Junta Directiva DIME
# Genera: supplies/Giro_Cama_Informe_Ejecutivo.pptx
# Ejecutar desde: shiny_los/
################################################################################
suppressPackageStartupMessages({
  library(officer)
  library(flextable)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readxl)
  library(janitor)
  library(scales)
  library(zoo)
  library(MASS)
  library(strucchange)
})

# ── Paleta corporativa ────────────────────────────────────────────────────────
COL_AZUL   <- "#1B3A6B"   # azul marino DIME
COL_ROJO   <- "#C0392B"
COL_VERDE  <- "#1A7A4A"
COL_AMBER  <- "#E67E22"
COL_GRIS   <- "#95A5A6"
COL_LIGHT  <- "#EBF5FB"
yr_cols    <- c("2024" = "#F28E2B", "2025" = "#59A14F", "2026" = "#4E79A7")

# ── Cargar datos (fuente única: global.R) ─────────────────────────────────────
message("Cargando datos ...")
suppressPackageStartupMessages(source("global.R"))
select <- dplyr::select

# ── Helpers ───────────────────────────────────────────────────────────────────
cop_m <- function(x) {
  ifelse(is.na(x), "—",
         paste0("$", formatC(round(x / 1e6, 1), format = "f", digits = 1,
                              big.mark = ".", decimal.mark = ","), "M"))
}

save_plot <- function(p, w = 8, h = 4.2) {
  f <- tempfile(fileext = ".png")
  ggsave(f, p, width = w, height = h, dpi = 180, bg = "white")
  f
}

theme_dime <- function() {
  theme_classic(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", color = COL_AZUL, size = 12),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.title    = element_blank(),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4)
    )
}

# ── Pre-computar datos clave ──────────────────────────────────────────────────
message("Calculando indicadores ...")

# 1. KPI panel wide (ya disponible como data_kpi_wide)
panel <- data_kpi_wide   # año, mes, tasa_gc, mean_los, egresos, camas, dias_inac_total ...

# 2. Resumen anual
anual <- panel %>%
  group_by(Año = year_val) %>%
  summarise(
    Egresos        = sum(egresos,         na.rm = TRUE),
    `GC promedio`  = round(mean(tasa_gc,  na.rm = TRUE), 2),
    `LOS medio`    = round(mean(mean_los, na.rm = TRUE), 2),
    `Días inac.`   = sum(dias_inac_total, na.rm = TRUE),
    `Días inac IPS`= sum(dias_inac_ips,  na.rm = TRUE),
    `Días inac EPS`= sum(dias_inac_eps,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Var GC (%)`  = round((`GC promedio` / lag(`GC promedio`)   - 1) * 100, 1),
    `Var LOS (%)` = round((`LOS medio`   / lag(`LOS medio`)     - 1) * 100, 1),
    `Var inac.(%)`= round((as.numeric(`Días inac.`) / lag(as.numeric(`Días inac.`)) - 1) * 100, 1)
  )

# 3. Costos acumulados
costo_eps_acum <- sum(data_kpi_gc$valor[
  data_kpi_gc$nombre_ind == "Costo de estancia inactiva por EPS" &
  data_kpi_gc$tipo == "inactivo_costo"], na.rm = TRUE)
costo_ips_acum <- sum(data_kpi_gc$valor[
  data_kpi_gc$nombre_ind == "Costo de estancia inactiva por IPS" &
  data_kpi_gc$tipo == "inactivo_costo"], na.rm = TRUE)
dias_inac_acum <- sum(panel$dias_inac_total, na.rm = TRUE)
costo_dia_prom <- if (dias_inac_acum > 0) (costo_eps_acum + costo_ips_acum) / dias_inac_acum else NA

# 4. Último GC institucional
gc_ultimo <- panel %>% filter(!is.na(tasa_gc)) %>% arrange(desc(fecha)) %>% slice(1)
gc_val    <- round(gc_ultimo$tasa_gc, 2)
gc_mes    <- format(gc_ultimo$fecha, "%B %Y")

# 5. Breakpoint
bp_date <- tryCatch({
  ts_v <- ts(panel$tasa_gc, frequency = 12, start = c(panel$year_val[1], panel$mes_num[1]))
  bp   <- strucchange::breakpoints(ts_v ~ 1)
  if (!is.na(bp$breakpoints[1])) panel$fecha[bp$breakpoints[1]] else NULL
}, error = function(e) NULL)

# 6. Modelo Gamma
df_m <- panel %>% filter(!is.na(tasa_gc), !is.na(mean_los), !is.na(dias_inac_total),
                          !is.na(egresos), tasa_gc > 0, egresos > 0)
m_gc <- tryCatch(
  glm(tasa_gc ~ mean_los + dias_inac_total + log(egresos) + año_f,
      family = Gamma(link = "log"), data = df_m),
  error = function(e) NULL
)
irr_los  <- if (!is.null(m_gc)) round(exp(coef(m_gc)["mean_los"]),         4) else NA
irr_inac <- if (!is.null(m_gc)) round(exp(coef(m_gc)["dias_inac_total"]),  6) else NA
r_gc_los <- round(cor(panel$tasa_gc, panel$mean_los,       use = "complete.obs"), 3)
r_gc_inac<- round(cor(panel$tasa_gc, panel$dias_inac_total, use = "complete.obs"), 3)

# 7. CACI en estancias inactivas
pct_caci   <- round(mean(data_bd_inac$es_caci, na.rm = TRUE) * 100, 1)
caci_tab   <- data_bd_inac %>%
  filter(es_caci) %>%
  tidyr::separate_rows(caci_ei, sep = ", ") %>%
  count(CACI = caci_ei, sort = TRUE) %>%
  mutate(Pct = round(n / sum(n) * 100, 1))

# 8. Top causas IPS
top_causas <- data_bd_inac %>%
  filter(!is.na(causa_principal_bd), causa_principal_bd != "") %>%
  group_by(causa_principal_bd) %>%
  summarise(dias = sum(estancia_inactiva, na.rm = TRUE), casos = n(), .groups = "drop") %>%
  arrange(desc(dias)) %>% slice_head(n = 8)

# ── Gráficos ──────────────────────────────────────────────────────────────────
message("Generando gráficos ...")

## G1: Giro Cama institucional 2024-2026 con meta
g1 <- ggplot(panel %>% filter(!is.na(tasa_gc)),
             aes(x = fecha, y = tasa_gc, color = factor(year_val), group = 1)) +
  geom_line(color = COL_AZUL, linewidth = 1.1) +
  geom_point(aes(color = factor(year_val)), size = 2.5) +
  geom_hline(yintercept = 10, linetype = "dashed", color = COL_ROJO, linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = COL_GRIS,
              fill = COL_GRIS, alpha = 0.15, linewidth = 0.7) +
  annotate("text", x = min(panel$fecha, na.rm=TRUE), y = 10.25,
           label = "Meta = 10", hjust = 0, color = COL_ROJO, size = 3.5) +
  scale_color_manual(values = yr_cols) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(breaks = pretty_breaks(6), limits = c(6, 12)) +
  labs(title = "Giro de Cama Institucional — enero 2024 a junio 2026",
       x = NULL, y = "GC (egresos / cama)") +
  theme_dime() +
  { if (!is.null(bp_date))
      geom_vline(xintercept = as.numeric(bp_date), linetype = "dotted",
                 color = "red", linewidth = 1)
    else list() }
f_g1 <- save_plot(g1, 9, 4)

## G2: LOS por servicio (UCIN + UCI + HOSP + Extensión Hospitalización) 2024-2026
los_serv <- data_los_full %>%
  filter(year >= 2024,
         estacion_2 %in% c("UCI","UCIN","PISO HOSP","Extensión Hospitalización"),
         !is.na(dif_bed_serv), dif_bed_serv >= 0) %>%
  group_by(Servicio = estacion_2, Año = year) %>%
  summarise(Media = round(mean(dif_bed_serv, na.rm=TRUE), 2),
            P90   = round(quantile(dif_bed_serv, .9, na.rm=TRUE), 1),
            N     = n(), .groups = "drop")
g2 <- ggplot(los_serv, aes(x = factor(Año), y = Media, fill = Servicio)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.2) +
  geom_errorbar(aes(ymin = Media, ymax = P90),
                position = position_dodge(0.9), width = 0.25, color = "grey40") +
  scale_fill_manual(values = c("UCI"="#E15759","UCIN"="#F28E2B","PISO HOSP"="#59A14F",
                                "Extensión Hospitalización"="#EDC948")) +
  labs(title = "Estancia Media por Servicio 2024-2026 (barra = media, línea = P90)",
       x = "Año", y = "Días de estancia") +
  theme_dime()
f_g2 <- save_plot(g2, 8, 4)

## G3: Días inactivos mensuales (IPS vs EPS)
inac_long <- data_kpi_gc %>%
  filter(nombre_ind %in% c("Total de días de estancias inactivas EPS",
                            "Total de días de estancias inactivas IPS"),
         tipo == "inactivo_costo", !is.na(valor)) %>%
  mutate(fecha     = as.Date(paste0(year_val,"-",sprintf("%02d",mes_num),"-01")),
         Atribución = if_else(str_detect(nombre_ind,"EPS"), "EPS","IPS"))
g3 <- ggplot(inac_long, aes(x = fecha, y = valor, fill = Atribución)) +
  geom_col(position = "stack", color = "white", linewidth = 0.1) +
  scale_fill_manual(values = c("EPS"=COL_AMBER,"IPS"=COL_ROJO)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(title = "Días de Estancia Inactiva mensuales — EPS vs IPS",
       x = NULL, y = "Días inactivos") +
  theme_dime()
f_g3 <- save_plot(g3, 9, 4)

## G4: CACI en pacientes con estancia inactiva
caci_inac <- data_bd_inac %>%
  filter(es_caci, !is.na(caci_ei)) %>%
  tidyr::separate_rows(caci_ei, sep = ", ") %>%
  group_by(CACI = caci_ei, Responsable = responsable_bd) %>%
  summarise(n = n(), dias = sum(estancia_inactiva, na.rm=TRUE), .groups="drop")
resp_cols <- c("IPS"=COL_ROJO,"EPS"=COL_AMBER,"Paciente"="#4E79A7","No clasificado"="grey60")
g4 <- ggplot(caci_inac, aes(x = reorder(CACI, n), y = n, fill = Responsable)) +
  geom_col(position = "stack", color = "white", linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(values = resp_cols) +
  labs(title = paste0("Pacientes CACI con Estancia Inactiva (", pct_caci, "% del total)"),
       x = NULL, y = "N° pacientes") +
  theme_dime()
f_g4 <- save_plot(g4, 7, 3.5)

## G5: Costos mensuales EPS + IPS
cost_long <- data_kpi_gc %>%
  filter(nombre_ind %in% c("Costo de estancia inactiva por EPS",
                            "Costo de estancia inactiva por IPS"),
         tipo == "inactivo_costo", !is.na(valor)) %>%
  mutate(fecha     = as.Date(paste0(year_val,"-",sprintf("%02d",mes_num),"-01")),
         Atribución = if_else(str_detect(nombre_ind,"EPS"),"EPS","IPS"))
g5 <- ggplot(cost_long, aes(x = fecha, y = valor / 1e6,
                             color = Atribución, group = Atribución)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_manual(values = c("EPS"=COL_AMBER,"IPS"=COL_ROJO)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = function(x) paste0("$",x,"M")) +
  labs(title = "Costo mensual de Estancia Inactiva (Millones COP)",
       x = NULL, y = "Costo (M COP)") +
  theme_dime()
f_g5 <- save_plot(g5, 9, 4)

## G6: Correlación GC ~ LOS scatter
g6 <- ggplot(panel %>% filter(!is.na(tasa_gc), !is.na(mean_los)),
             aes(x = mean_los, y = tasa_gc, color = factor(year_val))) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40",
              fill = "grey80", linewidth = 0.8, alpha = 0.25) +
  scale_color_manual(values = yr_cols, name = "Año") +
  annotate("text", x = min(panel$mean_los,na.rm=TRUE),
           y = max(panel$tasa_gc,na.rm=TRUE),
           label = paste0("r = ", r_gc_los), hjust = 0, size = 4, color = COL_AZUL) +
  labs(title = "Relación: Estancia Media vs. Giro de Cama",
       x = "LOS Medio Institucional (días)", y = "Giro de Cama") +
  theme_dime()
f_g6 <- save_plot(g6, 7, 4)

## G7: Pareto causas IPS
g7_df <- top_causas %>%
  mutate(
    causa_lbl = str_wrap(str_trunc(causa_principal_bd, 42), 30),
    acum_pct  = cumsum(dias) / sum(dias) * 100,
    escala    = max(dias) / 100
  )
g7 <- ggplot(g7_df, aes(x = reorder(causa_lbl, dias))) +
  geom_col(aes(y = dias, fill = dias), color = "white", linewidth = 0.2) +
  geom_line(aes(y = acum_pct * unique(escala), group = 1), color = COL_AZUL, linewidth = 1) +
  geom_point(aes(y = acum_pct * unique(escala)), color = COL_AZUL, size = 2) +
  scale_fill_gradient(low = COL_AMBER, high = COL_ROJO) +
  scale_y_continuous(
    name     = "Días inactivos",
    sec.axis = sec_axis(~ . / unique(g7_df$escala),
                        name = "% Acumulado", labels = function(x) paste0(x, "%"))
  ) +
  coord_flip() +
  labs(title = "Pareto — Causas principales de Estancia Inactiva (IPS)",
       x = NULL) +
  theme_dime() + theme(legend.position = "none", axis.text.y = element_text(size = 8))
f_g7 <- save_plot(g7, 9, 4.5)

## G8: Marginal effect LOS → GC (from model)
if (!is.null(m_gc)) {
  los_seq  <- seq(min(df_m$mean_los,na.rm=TRUE), max(df_m$mean_los,na.rm=TRUE), length.out=60)
  marg_los <- bind_rows(lapply(unique(df_m$year_val), function(yr) {
    tibble(mean_los=los_seq,
           dias_inac_total=mean(df_m$dias_inac_total,na.rm=TRUE),
           egresos=mean(df_m$egresos,na.rm=TRUE),
           año_f=factor(yr,levels=levels(df_m$año_f)), year_val=yr)
  })) %>% mutate(pred = predict(m_gc, newdata=., type="response"))

  g8 <- ggplot(marg_los, aes(x=mean_los, y=pred, color=factor(year_val))) +
    geom_line(linewidth=1.2) +
    geom_hline(yintercept=10, linetype="dashed", color=COL_ROJO, linewidth=0.7) +
    scale_color_manual(values=yr_cols, name="Año") +
    annotate("text", x=min(los_seq), y=10.2, label="Meta GC = 10",
             hjust=0, color=COL_ROJO, size=3.5) +
    labs(title="Efecto marginal: Estancia Media → Giro de Cama (modelo Gamma GLM)",
         x="Estancia Media (días)", y="GC predicho") +
    theme_dime()
  f_g8 <- save_plot(g8, 8, 4)
} else { f_g8 <- NULL }

# ── Construir PPT ─────────────────────────────────────────────────────────────
message("Construyendo presentación ...")

# helper: añade imagen a slide actual
add_img <- function(prs, file, left=0.3, top=1.5, width=9, height=4.5) {
  ph_with(prs, external_img(file, width=width, height=height),
          location=ph_location(left=left, top=top, width=width, height=height))
}

# helper: slide con título + layout en blanco
new_slide <- function(prs, titulo, subtitulo = NULL) {
  prs <- add_slide(prs, layout="Blank", master="Office Theme")
  prs <- ph_with(prs, value=titulo,
                 location=ph_location(left=0.3, top=0.15, width=9.2, height=0.8))
  # título: negrita, azul
  prs <- ph_with(prs,
    value = fpar(ftext(titulo,
      fp_text(color=COL_AZUL, font.size=22, bold=TRUE,
              font.family="Calibri"))),
    location=ph_location(left=0.3, top=0.15, width=9.2, height=0.7))
  if (!is.null(subtitulo)) {
    prs <- ph_with(prs,
      value=fpar(ftext(subtitulo,
        fp_text(color="#555555", font.size=12, font.family="Calibri"))),
      location=ph_location(left=0.3, top=0.78, width=9.2, height=0.45))
  }
  prs
}

# helper: tabla flextable en slide
add_ft <- function(prs, ft, left=0.3, top=1.4, width=9.2) {
  ph_with(prs, ft, location=ph_location(left=left, top=top, width=width))
}

ft_theme <- function(ft) {
  ft %>%
    theme_booktabs() %>%
    bg(part="header", bg=COL_AZUL) %>%
    color(part="header", color="white") %>%
    bold(part="header") %>%
    font(fontname="Calibri", part="all") %>%
    fontsize(size=9, part="all") %>%
    fontsize(size=10, part="header") %>%
    set_table_properties(layout="autofit")
}

prs <- read_pptx()

# ── SLIDE 1: Portada ──────────────────────────────────────────────────────────
prs <- add_slide(prs, layout="Blank", master="Office Theme")
prs <- ph_with(prs,
  value=fpar(
    ftext("DIME Clínica Neurocardiovascular",
          fp_text(color=COL_AZUL, font.size=14, bold=FALSE, font.family="Calibri")),
    fp_p=fp_par(text.align="center")
  ),
  location=ph_location(left=0.5, top=1.2, width=9, height=0.5))
prs <- ph_with(prs,
  value=fpar(
    ftext("Giro de Cama, Estancia y Costos de Inactividad",
          fp_text(color=COL_AZUL, font.size=28, bold=TRUE, font.family="Calibri")),
    fp_p=fp_par(text.align="center")
  ),
  location=ph_location(left=0.5, top=1.9, width=9, height=1.1))
prs <- ph_with(prs,
  value=fpar(
    ftext("Análisis 2024 – 2026  |  Junta Directiva",
          fp_text(color="#444444", font.size=14, bold=FALSE, font.family="Calibri")),
    fp_p=fp_par(text.align="center")
  ),
  location=ph_location(left=0.5, top=3.2, width=9, height=0.5))
prs <- ph_with(prs,
  value=fpar(
    ftext(format(Sys.Date(), "%B %Y"),
          fp_text(color=COL_GRIS, font.size=12, font.family="Calibri")),
    fp_p=fp_par(text.align="center")
  ),
  location=ph_location(left=0.5, top=4.0, width=9, height=0.4))

# ── SLIDE 2: Resumen Ejecutivo (KPIs) ────────────────────────────────────────
prs <- new_slide(prs, "Resumen Ejecutivo",
                 paste0("Período: enero 2024 – junio 2026  |  GC institucional último mes: ",
                        gc_val, " (", gc_mes, ")"))

kpi_tbl <- tibble(
  KPI              = c("Giro de Cama institucional (prom. 2024)",
                       "Giro de Cama institucional (prom. 2025)",
                       "Giro de Cama institucional (prom. 2026*)",
                       "Meta GC institucional",
                       "LOS medio institucional 2024",
                       "LOS medio institucional 2025",
                       "LOS medio institucional 2026*",
                       "Días inactivos totales 2024",
                       "Días inactivos totales 2025",
                       "Días inactivos totales 2026*",
                       "Costo inactividad EPS (acum.)",
                       "Costo inactividad IPS (acum.)",
                       "% Pacientes inactivos con CACI"),
  Valor            = c(
    as.character(round(mean(panel$tasa_gc[panel$year_val==2024],na.rm=TRUE),2)),
    as.character(round(mean(panel$tasa_gc[panel$year_val==2025],na.rm=TRUE),2)),
    as.character(round(mean(panel$tasa_gc[panel$year_val==2026],na.rm=TRUE),2)),
    "10",
    as.character(round(mean(panel$mean_los[panel$year_val==2024],na.rm=TRUE),2)),
    as.character(round(mean(panel$mean_los[panel$year_val==2025],na.rm=TRUE),2)),
    as.character(round(mean(panel$mean_los[panel$year_val==2026],na.rm=TRUE),2)),
    as.character(sum(panel$dias_inac_total[panel$year_val==2024],na.rm=TRUE)),
    as.character(sum(panel$dias_inac_total[panel$year_val==2025],na.rm=TRUE)),
    as.character(sum(panel$dias_inac_total[panel$year_val==2026],na.rm=TRUE)),
    cop_m(costo_eps_acum),
    cop_m(costo_ips_acum),
    paste0(pct_caci, "%")
  ),
  Semáforo         = c(
    ifelse(mean(panel$tasa_gc[panel$year_val==2024],na.rm=TRUE)>=10,"🟢","🔴"),
    ifelse(mean(panel$tasa_gc[panel$year_val==2025],na.rm=TRUE)>=10,"🟢","🔴"),
    ifelse(mean(panel$tasa_gc[panel$year_val==2026],na.rm=TRUE)>=10,"🟢","🔴"),
    "—","—","—","—","—","—","—","🔴","🔴","⚠️"
  )
) %>%
  flextable() %>% ft_theme() %>%
  width(j=1, width=5.5) %>%
  width(j=2, width=2.0) %>%
  width(j=3, width=0.9) %>%
  bg(i=c(1:3,5:7), j=2,
     bg=ifelse(
       suppressWarnings(as.numeric(c(
         round(mean(panel$tasa_gc[panel$year_val==2024],na.rm=TRUE),2),
         round(mean(panel$tasa_gc[panel$year_val==2025],na.rm=TRUE),2),
         round(mean(panel$tasa_gc[panel$year_val==2026],na.rm=TRUE),2),
         round(mean(panel$mean_los[panel$year_val==2024],na.rm=TRUE),2),
         round(mean(panel$mean_los[panel$year_val==2025],na.rm=TRUE),2),
         round(mean(panel$mean_los[panel$year_val==2026],na.rm=TRUE),2)
       ))) >= c(10,10,10,3.5,3.5,3.5), "#d5f5e3","#fadbd8"), part="body")

prs <- add_ft(prs, kpi_tbl, top=1.35)

# ── SLIDE 3: Tendencia Giro de Cama ──────────────────────────────────────────
prs <- new_slide(prs, "Tendencia del Giro de Cama Institucional 2024-2026",
                 "Línea punteada roja = meta institucional 10 | Punto de quiebre detectado por test de Bai-Perron")
prs <- add_img(prs, f_g1, top=1.35, height=4.7)

# ── SLIDE 4: Estancia por Servicio ───────────────────────────────────────────
prs <- new_slide(prs, "Estancia Media por Servicio y Año",
                 "Barra = media | Línea de error = percentil 90")
prs <- add_img(prs, f_g2, top=1.35, height=4.7)

# ── SLIDE 5: Resumen anual comparativo ───────────────────────────────────────
prs <- new_slide(prs, "Comparativo Anual — Indicadores Clave")

anual_ft <- anual %>%
  rename(`GC prom`=`GC promedio`, `LOS (d)`=`LOS medio`,
         `Inac. total`=`Días inac.`, `Inac. IPS`=`Días inac IPS`,
         `Inac. EPS`=`Días inac EPS`,
         `∆ GC`=`Var GC (%)`, `∆ LOS`=`Var LOS (%)`, `∆ Inac`=`Var inac.(%)`) %>%
  flextable() %>% ft_theme() %>%
  set_table_properties(layout="autofit") %>%
  color(j="∆ GC",
        color=ifelse(!is.na(anual$`Var GC (%)`),
                     ifelse(anual$`Var GC (%)` > 0, COL_VERDE, COL_ROJO),
                     "black"), part="body") %>%
  color(j="∆ LOS",
        color=ifelse(!is.na(anual$`Var LOS (%)`),
                     ifelse(anual$`Var LOS (%)` < 0, COL_VERDE, COL_ROJO),
                     "black"), part="body") %>%
  color(j="∆ Inac",
        color=ifelse(!is.na(anual$`Var inac.(%)`),
                     ifelse(anual$`Var inac.(%)` < 0, COL_VERDE, COL_ROJO),
                     "black"), part="body") %>%
  bold(j=c("∆ GC","∆ LOS","∆ Inac"), part="body")

prs <- add_ft(prs, anual_ft, top=1.35)

prs <- ph_with(prs,
  value=fpar(
    ftext("Verde = mejora  |  Rojo = deterioro  |  * 2026 enero–junio",
          fp_text(color=COL_GRIS, font.size=9, italic=TRUE, font.family="Calibri"))
  ),
  location=ph_location(left=0.3, top=6.2, width=9, height=0.3))

# ── SLIDE 6: Correlación GC ~ LOS ────────────────────────────────────────────
prs <- new_slide(prs, "Relación entre Estancia Media y Giro de Cama",
                 paste0("Pearson r = ", r_gc_los,
                        "  |  Razón multiplicativa del modelo (Gamma GLM) = ", irr_los))
prs <- add_img(prs, f_g6, left=0.3, top=1.35, width=5.5, height=4.4)
prs <- ph_with(prs,
  value=fpar(
    ftext(paste0(
      "Cada día adicional de estancia media\n",
      "multiplica el Giro de Cama por ", irr_los, "\n",
      "(", round((irr_los-1)*100,1), "% de cambio).\n\n",
      "Una reducción de 0.5 días en LOS\n",
      "equivale a un incremento estimado\n",
      "de ~0.4–0.6 egresos/cama por mes."
    ),
    fp_text(color=COL_AZUL, font.size=11, font.family="Calibri"))
  ),
  location=ph_location(left=6.1, top=1.8, width=3.6, height=3.5))

# ── SLIDE 7: Días inactivos ───────────────────────────────────────────────────
prs <- new_slide(prs, "Estancias Inactivas — Evolución Mensual",
                 "Acumulado total 2024-2026")
prs <- add_img(prs, f_g3, top=1.35, height=4.7)

# ── SLIDE 8: Pareto causas ────────────────────────────────────────────────────
prs <- new_slide(prs, "Causas de Estancia Inactiva — Análisis de Pareto",
                 "Primeras 8 causas ordenadas por días inactivos acumulados")
prs <- add_img(prs, f_g7, top=1.35, width=9.2, height=4.8)

# ── SLIDE 9: CACI en estancias inactivas ────────────────────────────────────
prs <- new_slide(prs, paste0("Pacientes CACI con Estancia Inactiva: ", pct_caci, "%"),
                 "El 85.7% de los pacientes inactivos fueron cruzados con GRD. 43% tienen diagnóstico CACI.")

prs <- add_img(prs, f_g4, left=0.3, top=1.35, width=5.8, height=4)

caci_ft <- caci_tab %>%
  rename(`Casos`=n, `%`=Pct) %>%
  flextable() %>% ft_theme() %>%
  width(j=1, width=1.2) %>%
  width(j=2, width=1.0) %>%
  width(j=3, width=0.8)

prs <- add_ft(prs, caci_ft, left=6.3, top=1.8, width=3.3)
prs <- ph_with(prs,
  value=fpar(
    ftext(paste0(
      "SCA, ICC y ACV concentran\n>95% de los casos CACI\ncon estancia inactiva.\n\n",
      "Esto vincula directamente\nla gestión de inactivos\ncon la ruta clínica\nde alta complejidad."
    ),
    fp_text(color=COL_AZUL, font.size=10, font.family="Calibri"))
  ),
  location=ph_location(left=6.3, top=3.8, width=3.3, height=2.2))

# ── SLIDE 10: Impacto Económico ───────────────────────────────────────────────
prs <- new_slide(prs, "Impacto Económico de la Estancia Inactiva",
                 paste0("Costo/día inactivo promedio: ",
                        cop_m(costo_dia_prom), "  |  Acumulado 2024-2026"))
prs <- add_img(prs, f_g5, top=1.35, height=4.7)

# ── SLIDE 11: Modelo Estadístico ──────────────────────────────────────────────
prs <- new_slide(prs, "Modelo Estadístico: Determinantes del Giro de Cama",
                 "GLM Gamma (liga log) — Respuesta: Giro de Cama institucional mensual")

if (!is.null(m_gc)) {
  coefs <- coef(summary(m_gc))
  ci    <- tryCatch(confint(m_gc), error=function(e) matrix(NA,nrow(coefs),2))
  mod_tbl <- tibble(
    Variable      = rownames(coefs),
    `Razón (exp)` = round(exp(coefs[,1]), 4),
    `IC 2.5%`     = round(exp(ci[,1]),    4),
    `IC 97.5%`    = round(exp(ci[,2]),    4),
    `p-valor`     = round(coefs[,4],      4),
    Sig           = case_when(coefs[,4]<0.001~"***",coefs[,4]<0.01~"**",
                              coefs[,4]<0.05~"*",coefs[,4]<0.10~".",TRUE~"")
  ) %>%
    mutate(Variable = case_when(
      Variable == "(Intercept)"     ~ "Intercepto",
      Variable == "mean_los"        ~ "Estancia media (días)",
      Variable == "dias_inac_total" ~ "Días inactivos totales",
      Variable == "log(egresos)"    ~ "log(Egresos) — control demanda",
      str_detect(Variable,"año_f")  ~ paste0("Año ",str_extract(Variable,"[0-9]{4}")),
      TRUE ~ Variable
    )) %>%
    flextable() %>% ft_theme() %>%
    width(j=1, width=3.0) %>%
    width(j=2:5, width=1.3) %>%
    color(j="`Razón (exp)`",
          color=ifelse(exp(coefs[,1])<1, COL_ROJO, COL_VERDE), part="body")
  prs <- add_ft(prs, mod_tbl, top=1.4, width=9)
} else {
  prs <- ph_with(prs,
    value=fpar(ftext("Modelo no disponible (datos insuficientes)",
                     fp_text(color=COL_ROJO, font.size=12, font.family="Calibri"))),
    location=ph_location(left=0.5, top=2.5, width=9, height=1))
}

prs <- ph_with(prs,
  value=fpar(
    ftext(paste0("Razón LOS = ", irr_los, " | Razón Días Inactivos = ", irr_inac,
                 " | r(GC~LOS) = ", r_gc_los, " | r(GC~Inac) = ", r_gc_inac),
          fp_text(color=COL_GRIS, font.size=9, italic=TRUE, font.family="Calibri"))
  ),
  location=ph_location(left=0.3, top=6.25, width=9, height=0.3))

# ── SLIDE 12: Efecto marginal ─────────────────────────────────────────────────
if (!is.null(f_g8)) {
  prs <- new_slide(prs, "Efecto Marginal: Estancia Media → Giro de Cama",
                   "Valores predichos por el modelo Gamma GLM — resto de variables fijas en su media")
  prs <- add_img(prs, f_g8, top=1.35, height=4.7)
}

# ── SLIDE 13: Hallazgos y Recomendaciones ────────────────────────────────────
prs <- new_slide(prs, "Hallazgos y Recomendaciones Operacionales")

bullets <- c(
  paste0("El Giro de Cama institucional alcanzó ", gc_val, " en ", gc_mes,
         ". La meta de 10 egresos/cama se logra de forma intermitente."),
  paste0("Correlación negativa LOS-GC (r = ", r_gc_los,
         "): reducir la estancia es la palanca primaria de rotación."),
  paste0("Días de estancia inactiva acumulados 2024-2026: ",
         format(dias_inac_acum, big.mark="."),
         ". Costo IPS acum.: ", cop_m(costo_ips_acum), "."),
  paste0(pct_caci, "% de los pacientes con estancia inactiva son CACI",
         " (SCA, ICC, ACV predominan) — vinculación directa con la ruta de alta complejidad."),
  "RECOM 1: Implementar alerta de inactividad > 48 h por IPS con seguimiento diario en UCI/UCIN.",
  "RECOM 2: Reducir LOS en UCIN y HOSP en 0.5 días/año como meta incremental; impacto estimado: +4–6% GC.",
  "RECOM 3: Negociar con EPS tiempos de respuesta de autorización ≤ 24 h para reducir inactividad atribuible.",
  "RECOM 4: Protocolo de alta anticipada antes de las 14:00 h para maximizar la rotación diaria de camas."
)

for (i in seq_along(bullets)) {
  prs <- ph_with(prs,
    value = fpar(
      ftext(paste0(if (i <= 4) "▶  " else "✓  ", bullets[i]),
            fp_text(
              color     = if (i <= 4) COL_AZUL else COL_VERDE,
              font.size = 11,
              bold      = i > 4,
              font.family = "Calibri"
            )),
      fp_p = fp_par(padding.bottom = 4)
    ),
    location = ph_location(left=0.4, top=0.9 + (i-1)*0.65, width=9.2, height=0.6)
  )
}

# ── GUARDAR ───────────────────────────────────────────────────────────────────
out_path <- "supplies/Giro_Cama_Informe_Ejecutivo.pptx"
print(prs, target = out_path)
message("✅ PPT guardado en: ", out_path)
