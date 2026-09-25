################################################################################
# app.R — Estancia Hospitalaria (LOS) · DIME Clínica Neurocardiovascular
# Tabs: Resumen | Tendencias | Por Servicio | Larga Estancia |
#       Diagnóstico (CACI) | Ventas y Costos | Estancias Inactivas | Datos
################################################################################

source("global.R")

SERV_CORE <- c("UCI", "UCIN", "PISO HOSP", "Extensión Hospitalización")

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "32px",
               style = "margin-right:8px; vertical-align:middle;"),
      "Estancias · LOS"
    ),
    titleWidth = 260
  ),

  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem("Resumen",            tabName = "resumen",       icon = icon("chart-line")),
      menuItem("Tendencias",         tabName = "tendencias",    icon = icon("arrow-up")),
      menuItem("Por Servicio",       tabName = "por_servicio",  icon = icon("hospital")),
      menuItem("Larga Estancia",     tabName = "larga",         icon = icon("exclamation-triangle")),
      menuItem("Diagnóstico (CACI)", tabName = "diagnostico",   icon = icon("stethoscope")),
      menuItem("Ventas y Costos",    tabName = "financiero",    icon = icon("dollar-sign")),
      menuItem("Giro Cama",          tabName = "giro_cama",     icon = icon("bed")),
      menuItem("Est. Inactivas",     tabName = "inactivas",     icon = icon("pause")),
      menuItem("Valor de Cama",      tabName = "valor_cama",    icon = icon("bed")),
      menuItem("Datos",              tabName = "datos",         icon = icon("table"))
    ),
    tags$hr(style = "border-color:rgba(255,255,255,.2); margin:8px 0;"),
    tags$div(
      style = "padding: 0 14px;",
      selectInput("yr", "Año de análisis",
                  choices = year_choices_los, selected = max(year_choices_los)),
      selectInput("yr_desde", "Histórico desde",
                  choices = rev(year_choices_los), selected = min(year_choices_los)),
      selectInput("mon", "Mes", choices = mes_choices, selected = "0"),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      tags$label("Servicios",
                 style = "color:rgba(255,255,255,.8); font-weight:600; font-size:.85rem;"),
      checkboxGroupInput("serv_sel", NULL,
                         choices = serv_choices, selected = serv_choices),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      tags$label("CACI incluidos",
                 style = "color:rgba(255,255,255,.8); font-weight:600; font-size:.85rem;"),
      checkboxGroupInput("caci_sel", NULL,
                         choices = caci_choices_los, selected = caci_choices_los),
      tags$small(tags$em("El filtro CACI aplica solo a Diagnóstico y Ventas/Costos."),
                 style = "color:rgba(255,255,255,.55); display:block; margin-bottom:8px;"),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      tags$label("Umbral larga estancia",
                 style = "color:rgba(255,255,255,.8); font-weight:600; font-size:.85rem;"),
      selectInput("threshold_type", NULL,
                  choices  = c("Percentil 75 por servicio"  = "p75",
                               "Percentil 90 por servicio"  = "p90",
                               "Percentil 95 por servicio"  = "p95",
                               "Días fijos"                 = "fixed"),
                  selected = "p90"),
      conditionalPanel(
        "input.threshold_type === 'fixed'",
        numericInput("threshold_days", "Días de corte:", value = 10, min = 1, max = 90)
      ),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      actionButton("btn_update", "Actualizar",
                   icon = icon("sync"), class = "btn-primary btn-block"),
      br(),
      tags$small(tags$em(
        "Actualizado: ", textOutput("last_update_los", inline = TRUE)
      ), style = "color:rgba(255,255,255,.6);")
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$script(src = "fullscreen.js")
    ),
    tabItems(

      # ════════════════════════════════════════════════════════════════════════
      # Tab 1 · Resumen
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "resumen",
        uiOutput("kpi_los"),
        br(),
        fluidRow(
          box(title = tagList(icon("chart-bar"),
                              " Variación interanual — Egresos · LOS · Giro Cama · Días inactivos"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_resumen_op_yr", height = "240px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("dollar-sign"), " Ventas y costos por año — variación interanual"),
              solidHeader = TRUE, status = "success", width = 7,
              plotlyOutput("plot_resumen_fin_yr", height = "280px")),
          box(title = tagList(icon("table"), " Resumen financiero por año"),
              solidHeader = TRUE, status = "success", width = 5,
              reactableOutput("tabla_resumen_fin_yr"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("exclamation-triangle"),
                              " Finanzas — pacientes de larga estancia (ventas GRD y costo itemizado)"),
              solidHeader = TRUE, status = "warning", width = 12,
              plotlyOutput("plot_resumen_fin_long", height = "240px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("bed"), " Media de estancia mensual — servicios hospitalización"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("plot_trend_serv", height = "340px")),
          box(title = tagList(icon("chart-bar"), " Distribución LOS por año — servicios core"),
              solidHeader = TRUE, status = "primary", width = 5,
              plotlyOutput("plot_dist_year", height = "340px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 2 · Tendencias
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "tendencias",
        tags$div(
          class = "callout callout-info",
          style = "border-left-color:#4E79A7; padding:8px 12px; margin-bottom:12px; background:#EBF5FB;",
          tags$small(icon("info-circle"),
            " Esta pestaña muestra la estancia para ",
            tags$strong("todos los pacientes"),
            ", sin filtro por diagnóstico CACI/DRG.")
        ),
        fluidRow(
          box(title = tagList(icon("chart-line"), " Evolución mensual de la media LOS por servicio"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_tend_trend", height = "340px")),
          box(title = tagList(icon("chart-bar"), " Distribución LOS por servicio y año"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_tend_box_year", height = "340px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("table"), " Media de días por servicio y año"),
              solidHeader = TRUE, status = "primary", width = 6,
              reactableOutput("tabla_tend_pivot")),
          box(title = tagList(icon("chart-bar"), " Comparación anual: media LOS por servicio"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_tend_annual_bar", height = "340px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 3 · Por Servicio
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "por_servicio",
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Distribución días de estancia por servicio y año"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_box_serv", height = "420px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("table"), " Percentiles de estancia por servicio"),
              solidHeader = TRUE, status = "primary", width = 6,
              reactableOutput("tabla_percentiles")),
          box(title = tagList(icon("chart-line"), " Tendencia mensual — seleccione servicio"),
              solidHeader = TRUE, status = "primary", width = 6,
              selectInput("serv_trend", NULL,
                          choices  = serv_choices,
                          selected = serv_choices[1]),
              plotlyOutput("plot_serv_trend_month", height = "300px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 4 · Larga Estancia
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "larga",
        uiOutput("kpi_long"),
        br(),
        fluidRow(
          box(title = tagList(icon("percent"), " % Larga estancia por servicio"),
              solidHeader = TRUE, status = "primary", width = 5,
              plotlyOutput("plot_pct_long", height = "350px")),
          box(title = tagList(icon("users"), " Características — larga estancia"),
              solidHeader = TRUE, status = "primary", width = 7,
              reactableOutput("tabla_long_chars"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Distribución: larga vs. estancia normal"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_long_dist", height = "320px")),
          box(title = tagList(icon("stethoscope"), " Diagnósticos más frecuentes — larga estancia"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_long_diag", height = "320px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 5 · Diagnóstico (CACI)
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "diagnostico",
        fluidRow(
          box(title = tagList(icon("chart-bar"), " LOS total por grupo CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_caci_box", height = "380px")),
          box(title = tagList(icon("chart-line"), " Evolución anual media LOS por CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_caci_trend", height = "380px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Distribución LOS por servicio — seleccione CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              selectInput("caci_serv_sel", NULL,
                          choices  = c("Todos los grupos" = "TODOS"),
                          selected = "TODOS"),
              plotlyOutput("plot_caci_serv_dist", height = "320px")),
          box(title = tagList(icon("cut"), " LOS por procedimiento quirúrgico (top 15)"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_proc_los", height = "380px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 6 · Ventas y Costos
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "financiero",
        uiOutput("kpi_financiero"),
        br(),
        fluidRow(
          box(title = tagList(icon("ruler-combined"),
                              " Referencia empírica por servicio — mediana e IQR observados"),
              solidHeader = TRUE, status = "success", width = 12,
              # collapsed = FALSE a propósito: reactable no se dibuja dentro de
              # una caja colapsada y quedaba en blanco al expandirla.
              collapsible = TRUE, collapsed = FALSE,
              tags$small(uiOutput("ref_emp_nota"), style = "color:#6c757d;"),
              br(),
              reactableOutput("tabla_ref_emp"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("circle"), " LOS vs. Costo total por CACI"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("plot_scatter_los_cost", height = "380px")),
          box(title = tagList(icon("chart-bar"), " Costo diario promedio por CACI"),
              solidHeader = TRUE, status = "primary", width = 5,
              plotlyOutput("plot_cost_day_caci", height = "380px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("building"), " Referencia institucional contractual — PyG ejecutado"),
              solidHeader = TRUE, status = "success", width = 12,
              uiOutput("pyg_ref_kpis"),
              br(),
              fluidRow(
                column(6, plotlyOutput("plot_pyg_mensual", height = "260px")),
                column(6, uiOutput("pyg_costo_inac_box"))
              )
          )
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("table"), " Resumen financiero por CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              tags$div(
                class = "callout callout-warning",
                style = "background:#FEF9E7; border-left:4px solid #F39C12; padding:8px 12px; margin-bottom:10px;",
                tags$small(icon("info-circle"),
                  HTML(" <b>Fuentes:</b>
                  <sup>¹</sup> <i>Ventas GRD</i> = valor_factura del sistema GRD, deduplicado por cuenta y sin outliers (cap P99-2024 ≈ $225 M COP).
                  Los valores 2025-2026 presentaban registros atípicos ($6 B COP/cuenta) que inflarían el total;
                  se excluyen para garantizar comparabilidad. <br>
                  <i>Costo total</i> = suma itemizada (medicamentos, dispositivos, procedimientos, hotelería).
                  Para CACI de alta complejidad el costo puede superar la tarifa GRD (stents, DAI, marcapasos).
                  Valores en COP."))
              ),
              reactableOutput("tabla_fin_resumen"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab GC · Giro Cama & Ocupación
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "giro_cama",
        tabsetPanel(type = "tabs",

          # ── Sub-tab 1: Indicadores KPI ──────────────────────────────────────
          tabPanel("Indicadores KPI",
            br(),
            fluidRow(
              box(width = 3, solidHeader = TRUE, status = "primary",
                title = tagList(icon("filter"), " Filtro año"),
                selectInput("gc_yr", "Año",
                            choices  = c("Todos" = "0",
                                         "2024" = "2024", "2025" = "2025", "2026" = "2026"),
                            selected = "0")
              ),
              box(width = 9, solidHeader = FALSE,
                uiOutput("gc_kpi_cards")
              )
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-line"), " Giro Cama mensual por servicio (2024-2026)"),
                  solidHeader = TRUE, status = "primary", width = 8,
                  plotlyOutput("gc_plot_trend", height = "360px")),
              box(title = tagList(icon("tachometer-alt"), " Meta vs. Resultado acumulado"),
                  solidHeader = TRUE, status = "primary", width = 4,
                  plotlyOutput("gc_plot_meta_bar", height = "360px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("table"), " Tabla resumen: Giro Cama por servicio y mes"),
                  solidHeader = TRUE, status = "primary", width = 12,
                  reactableOutput("gc_tabla_kpi"))
            )
          ),

          # ── Sub-tab 2: Tendencias y Quiebres ───────────────────────────────
          tabPanel("Tendencias y Quiebres",
            br(),
            fluidRow(
              box(title = tagList(icon("chart-line"),
                                  " Giro Cama Institucional — tendencia + punto de quiebre"),
                  solidHeader = TRUE, status = "primary", width = 12,
                  plotlyOutput("gc_plot_breakpoint", height = "380px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-area"),
                                  " LOS Institucional vs. Días Inactivos (eje dual)"),
                  solidHeader = TRUE, status = "primary", width = 7,
                  plotlyOutput("gc_plot_los_inac", height = "340px")),
              box(title = tagList(icon("circle"),
                                  " Correlación: Giro Cama ~ Estancia Media"),
                  solidHeader = TRUE, status = "primary", width = 5,
                  plotlyOutput("gc_plot_corr_los", height = "340px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("circle"),
                                  " Correlación: Giro Cama ~ Días Inactivos"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_corr_inac", height = "320px")),
              box(title = tagList(icon("th"),
                                  " Matriz de correlación"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_corr_matrix", height = "320px"))
            )
          ),

          # ── Sub-tab 3: Modelo Estadístico ───────────────────────────────────
          tabPanel("Modelo Estadístico",
            br(),
            fluidRow(
              box(width = 12, solidHeader = TRUE, status = "info",
                title = tagList(icon("info-circle"), " Metodología"),
                tags$p(HTML(
                  "Se ajusta un <b>GLM Gamma con liga logarítmica</b> sobre la variable respuesta
                  <i>Giro de Cama mensual</i> (tasa continua = egresos/camas). Los predictores son:
                  <b>estancia media</b> (días), <b>días de estancia inactiva total</b> mensual,
                  <b>volumen de egresos</b> (ajuste por demanda) y año como variable de control.
                  Los coeficientes se interpretan como <b>razones multiplicativas</b> sobre el Giro de Cama:
                  un valor &lt; 1 indica reducción de la rotación; un valor &gt; 1, incremento."
                ))
              )
            ),
            fluidRow(
              box(title = tagList(icon("table"), " Coeficientes (Razón multiplicativa, IC 95%, p-valor)"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  reactableOutput("gc_tabla_modelo")),
              box(title = tagList(icon("chart-line"),
                                  " Giro de Cama: observado vs. predicho"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_pred_obs", height = "340px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-area"),
                                  " Efecto marginal: Egresos ~ Estancia Media por año"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_marg_los", height = "320px")),
              box(title = tagList(icon("chart-area"),
                                  " Efecto marginal: Egresos ~ Días Inactivos"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_marg_inac", height = "320px"))
            )
          ),

          # ── Sub-tab 4: Estancias Inactivas BD ───────────────────────────────
          tabPanel("Estancias Inactivas — Paciente",
            br(),
            fluidRow(
              box(width = 3, solidHeader = TRUE, status = "primary",
                title = tagList(icon("filter"), " Filtros"),
                selectInput("gc_bd_yr", "Año",
                            choices  = c("Todos" = "0",
                                         "2022" = "2022", "2023" = "2023",
                                         "2024" = "2024", "2025" = "2025", "2026" = "2026"),
                            selected = "0"),
                selectInput("gc_bd_resp", "Responsable",
                            choices  = c("Todos" = "0",
                                         "IPS" = "IPS", "EPS" = "EPS", "Paciente" = "Paciente"),
                            selected = "0")
              ),
              box(width = 9, solidHeader = FALSE,
                uiOutput("gc_bd_kpi_cards")
              )
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-bar"),
                                  " Pareto de causas (IPS) — días inactivos"),
                  solidHeader = TRUE, status = "primary", width = 7,
                  plotlyOutput("gc_plot_pareto", height = "400px")),
              box(title = tagList(icon("users"),
                                  " Distribución por rangos de edad"),
                  solidHeader = TRUE, status = "primary", width = 5,
                  plotlyOutput("gc_plot_edad", height = "400px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-line"),
                                  " Tendencia mensual por responsable"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_bd_trend", height = "320px")),
              box(title = tagList(icon("stethoscope"),
                                  " Top diagnósticos GRD en estancias inactivas"),
                  solidHeader = TRUE, status = "primary", width = 6,
                  plotlyOutput("gc_plot_diag", height = "320px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("table"), " Detalle pacientes — estancia inactiva"),
                  solidHeader = TRUE, status = "primary", width = 12,
                  DTOutput("gc_tabla_bd_detail"))
            )
          ),

          # ── Sub-tab 5: Impacto Económico ────────────────────────────────────
          tabPanel("Impacto Económico",
            br(),
            fluidRow(
              box(width = 12, solidHeader = FALSE,
                uiOutput("gc_econ_kpi_cards")
              )
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("dollar-sign"),
                                  " Costo mensual estancia inactiva (EPS vs IPS)"),
                  solidHeader = TRUE, status = "primary", width = 7,
                  plotlyOutput("gc_plot_costo_trend", height = "340px")),
              box(title = tagList(icon("chart-bar"),
                                  " Días inactivos mensuales vs. tendencia"),
                  solidHeader = TRUE, status = "primary", width = 5,
                  plotlyOutput("gc_plot_inac_bar", height = "340px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("table"),
                                  " Comparativo anual — KPIs principales"),
                  solidHeader = TRUE, status = "primary", width = 12,
                  reactableOutput("gc_tabla_anual"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("lightbulb"),
                                  " Análisis e interpretación"),
                  solidHeader = TRUE, status = "warning", width = 12,
                  uiOutput("gc_interpretacion"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-area"),
                                  " Análisis de oportunidad — mejora del Giro de Cama"),
                  solidHeader = TRUE, status = "success", width = 12,
                  uiOutput("gc_oportunidad_kpis"),
                  br(),
                  plotlyOutput("gc_plot_oportunidad", height = "300px"))
            )
          ),

          # ── Sub-tab 6: Ocupación ────────────────────────────────────────────
          tabPanel("Ocupación",
            br(),
            fluidRow(
              box(width = 3, solidHeader = TRUE, status = "primary",
                title = tagList(icon("filter"), " Filtro año"),
                selectInput("ocu_yr", "Año",
                            choices  = c("Todos" = "0",
                                         "2024" = "2024", "2025" = "2025", "2026" = "2026"),
                            selected = "0"),
                tags$div(style = "margin-top:10px; font-size:.82rem; line-height:1.6;",
                  tags$strong("Referencias NICE/NHS:"), tags$br(),
                  tags$span(style = "color:#27AE60;", "■"),
                  HTML(" &lt;85%: Normal"), tags$br(),
                  tags$span(style = "color:#F39C12;", "■"),
                  " 85-90%: Vigilancia", tags$br(),
                  tags$span(style = "color:#E74C3C;", "■"),
                  " 90-100%: Alerta", tags$br(),
                  tags$span(style = "color:#8E44AD;", "■"),
                  " &gt;100%: Sobreocupación"
                )
              ),
              box(width = 9, solidHeader = FALSE,
                uiOutput("ocu_kpi_cards")
              )
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("chart-line"), " Ocupación mensual por servicio (2024-2026)"),
                  solidHeader = TRUE, status = "primary", width = 8,
                  plotlyOutput("ocu_plot_trend", height = "360px")),
              box(title = tagList(icon("th"), " Mapa de calor — Ocupación Institucional"),
                  solidHeader = TRUE, status = "warning", width = 4,
                  plotlyOutput("ocu_plot_heatmap", height = "360px"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("exclamation-triangle"), " Meses con sobreocupación (>100%)"),
                  solidHeader = TRUE, status = "danger", width = 12,
                  uiOutput("ocu_alertas"))
            ),
            br(),
            fluidRow(
              box(title = tagList(icon("table"), " Detalle mensual de ocupación por servicio"),
                  solidHeader = TRUE, status = "primary", width = 12,
                  reactableOutput("ocu_tabla"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 7 · Estancias Inactivas
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "inactivas",
        fluidRow(
          box(title = tagList(icon("filter"), " Filtros adicionales"),
              solidHeader = TRUE, status = "primary", width = 3,
              selectInput("ei_yr", "Año",
                          choices  = c("Todos" = "0",
                                       setNames(as.character(ei_year_choices),
                                                ei_year_choices)),
                          selected = "0"),
              selectInput("ei_mes", "Mes", choices = mes_choices, selected = "0"),
              selectInput("ei_resp", "Responsable",
                          choices  = c("Todos" = "0",
                                       "IPS" = "IPS", "EPS" = "EPS", "Paciente" = "Paciente"),
                          selected = "0"),
              tags$small(HTML(paste0(
                "<em>Datos 2024–2026.</em><br>Los filtros aplican a <b>todos</b> ",
                "los indicadores, tablas y gráficos de esta pestaña.<br>",
                # Sello de versión: si esta línea no aparece, el proceso de R
                # está sirviendo código viejo (refrescar el navegador NO recarga
                # global.R — hay que detener y relanzar la app).
                "<span style='color:#adb5bd'>build ", EI_BUILD, "</span>")),
                style = "color:#6c757d;")),
          box(solidHeader = FALSE, width = 9,
              uiOutput("ei_periodo_txt"),
              uiOutput("kpi_ei"))
        ),

        # ── Valorización a día-cama (módulo aditivo) ──────────────────────────
        br(),
        fluidRow(
          box(title = tagList(icon("money-bill-wave"),
                              " Impacto económico de la estancia inactiva — valor de cama"),
              solidHeader = TRUE, status = "warning", width = 12,
              radioButtons("ei_base", "Base de valorización",
                inline = TRUE,
                choiceNames = list(
                  HTML("<b>Costo DIME</b> — recurso consumido sin producción"),
                  HTML("<b>Tarifa facturada</b> — ingreso dejado de percibir")),
                choiceValues = c("costo", "tarifa"), selected = "costo"),
              uiOutput("kpi_ei_costo"),
              tags$small(uiOutput("ei_costo_nota"), style = "color:#6c757d;"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("chart-column"),
                              " Costo mensual por responsable — IPS vs EAPB"),
              solidHeader = TRUE, status = "warning", width = 7,
              plotlyOutput("plot_ei_costo_mes", height = "380px")),
          box(title = tagList(icon("building-columns"), " Impacto por EAPB"),
              solidHeader = TRUE, status = "warning", width = 5,
              reactableOutput("tabla_ei_costo_eapb"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("screwdriver-wrench"),
                              " Causas IPS — intervenibles internamente"),
              solidHeader = TRUE, status = "danger", width = 6,
              plotlyOutput("plot_ei_causa_ips", height = "360px")),
          box(title = tagList(icon("file-signature"),
                              " Causas EAPB — gestión con el pagador"),
              solidHeader = TRUE, status = "info", width = 6,
              plotlyOutput("plot_ei_causa_eps", height = "360px"))
        ),

        br(),
        fluidRow(
          box(title = tagList(icon("list-ol"), " Causas principales — días perdidos"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_causas_ei", height = "380px")),
          box(title = tagList(icon("chart-line"), " Evolución mensual de días inactivos"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_trend_ei", height = "380px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("stethoscope"), " CACI de pacientes con estancia inactiva"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_ei_caci", height = "340px")),
          box(title = tagList(icon("building"), " EPS — estancias inactivas vs. admisiones GRD"),
              solidHeader = TRUE, status = "primary", width = 6,
              reactableOutput("tabla_ei_eps"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("table"), " Detalle por paciente (enriquecido con GRD)"),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_ei_detail"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 8 · Datos
      # ════════════════════════════════════════════════════════════════════════
      # ════════════════════════════════════════════════════════════════════════
      # Tab · Valor de Cama — continuación económica de Est. Inactivas
      # No repite los indicadores de estancia inactiva: aquí se explica CUÁNTO
      # vale el día-cama que allá se pierde, y por qué ese valor cambia.
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "valor_cama",
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = tagList(icon("circle-info"), " Qué muestra esta pestaña"),
              uiOutput("bv_intro"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("scale-balanced"),
                              " Las tres bases del día-cama, por servicio"),
              solidHeader = TRUE, status = "primary", width = 12,
              reactableOutput("bv_tabla_comparativo"),
              br(),
              tags$small(uiOutput("bv_nota_bases"), style = "color:#6c757d;"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("chart-column"),
                              " Costo vs tarifa vs SOAT — UCI · UCIN · Hospitalización"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("bv_plot_bases", height = "380px")),
          box(title = tagList(icon("triangle-exclamation"),
                              " Margen sobre el costo de la cama"),
              solidHeader = TRUE, status = "warning", width = 5,
              plotlyOutput("bv_plot_margen", height = "380px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("handshake"),
                              " Variación por pagador (EAPB) — insumo de negociación"),
              solidHeader = TRUE, status = "info", width = 12,
              selectInput("bv_serv", "Servicio",
                          choices = c("UCI", "UCIN (intermedio)", "Hospitalización"),
                          selected = "UCI", width = "260px"),
              plotlyOutput("bv_plot_eapb", height = "400px"),
              br(),
              reactableOutput("bv_tabla_eapb"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("stethoscope"),
                              " Por patología (CACI) — qué diagnóstico ocupa la cama más cara"),
              solidHeader = TRUE, status = "success", width = 12,
              plotlyOutput("bv_plot_caci", height = "400px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("book"), " Nota metodológica y trazabilidad"),
              solidHeader = TRUE, status = "primary", width = 12,
              collapsible = TRUE, collapsed = FALSE,
              uiOutput("bv_metodologia"))
        )
      ),

      tabItem(tabName = "datos",
        fluidRow(
          box(title = tagList(icon("search"), " Explorador LOS — nivel servicio"),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_los_dt"))
        )
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  trigger <- reactiveVal(0)
  observeEvent(input$btn_update, trigger(trigger() + 1), ignoreNULL = FALSE)

  output$last_update_los <- renderText(format(Sys.Date(), "%d/%m/%Y"))

  # ── Parámetros ───────────────────────────────────────────────────────────
  params <- reactive({
    trigger()
    list(
      yr       = as.integer(isolate(input$yr)),
      yr_desde = as.integer(isolate(input$yr_desde)),
      mon      = as.integer(isolate(input$mon)),
      servs    = isolate(input$serv_sel),
      cacis    = isolate(input$caci_sel)
    )
  })

  # ── LOS censo filtrado (NO usa filtro CACI) ──────────────────────────────
  los_filt <- reactive({
    p  <- params()
    df <- data_los_full %>%
      filter(
        year >= p$yr_desde,
        year <= p$yr,
        dif_bed_serv >= 0,
        !is.na(dif_bed_serv),
        estacion_2 %in% p$servs
      )
    if (p$mon > 0) df <- df %>% filter(month == p$mon)
    df
  })

  # ── GRD filtrado (con filtro CACI — solo para pestañas diagnóstico) ──────
  grd_filt <- reactive({
    p  <- params()
    df <- data_grd_base %>%
      filter(
        año >= p$yr_desde,
        año <= p$yr,
        !is.na(dif_days), dif_days >= 0,
        is.na(caci) | caci %in% p$cacis
      )
    if (p$mon > 0) {
      df <- df %>%
        filter(month(coalesce(fecha_ingreso, fecha_de_egreso)) == p$mon)
    }
    df
  })

  # ── Costos filtrados (con filtro CACI + mes_num correcto) ────────────────
  costo_filt <- reactive({
    p  <- params()
    df <- data_costo_base %>%
      filter(
        año >= p$yr_desde,
        año <= p$yr,
        is.na(caci) | caci %in% p$cacis
      )
    if (p$mon > 0) df <- df %>% filter(mes_num == p$mon)
    df
  })

  # ── Umbrales de larga estancia ───────────────────────────────────────────
  umbral_df <- reactive({
    tt  <- input$threshold_type
    pct <- switch(tt, p75 = 0.75, p90 = 0.90, p95 = 0.95, fixed = NULL)

    if (!is.null(pct)) {
      los_filt() %>%
        group_by(estacion_2) %>%
        summarise(umbral = quantile(dif_bed_serv, pct, na.rm = TRUE),
                  .groups = "drop")
    } else {
      tibble(estacion_2 = unique(los_filt()$estacion_2),
             umbral     = as.numeric(input$threshold_days))
    }
  })

  los_flagged <- reactive({
    los_filt() %>%
      left_join(umbral_df(), by = "estacion_2") %>%
      mutate(larga_estancia = dif_bed_serv > umbral)
  })

  cost_per_cuenta <- reactive({
    costo_filt() %>%
      group_by(cuenta, caci, dif_days) %>%
      summarise(
        costo_total = sum(costo,  na.rm = TRUE),
        venta_total = sum(venta,  na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      mutate(
        costo_dia = if_else(!is.na(dif_days) & dif_days > 0,
                             costo_total / dif_days, NA_real_),
        venta_dia = if_else(!is.na(dif_days) & dif_days > 0,
                             venta_total / dif_days, NA_real_),
        margen    = venta_total - costo_total
      )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 1 · Resumen
  # ══════════════════════════════════════════════════════════════════════════
  output$kpi_los <- renderUI({
    df <- los_flagged()
    req(nrow(df) > 0)

    p      <- params()
    n_adm  <- n_distinct(df$cuenta)
    df_core<- df %>% filter(estacion_2 %in% SERV_CORE)
    med_core <- mean(df_core$dif_bed_serv, na.rm = TRUE)
    pct_long <- round(mean(df$larga_estancia, na.rm = TRUE) * 100, 1)
    ei_days  <- sum(data_ei$total_dias_de_estancia_por_ips, na.rm = TRUE)

    # Latest GC rate from KPI (most recent month available)
    gc_last <- data_kpi_gc %>%
      filter(nombre_ind == "Giro Cama Institucional", tipo == "rate_mean",
             !is.na(valor), year_val >= p$yr_desde, year_val <= p$yr) %>%
      arrange(desc(year_val), desc(mes_num)) %>%
      slice(1)
    gc_val   <- if (nrow(gc_last) > 0) round(gc_last$valor[1], 2) else NA
    gc_col   <- if (!is.na(gc_val) && gc_val >= 10) "green" else if (!is.na(gc_val) && gc_val >= 8) "yellow" else "red"
    gc_label <- if (!is.na(gc_val)) paste0(gc_val, " ← meta 10")  else "Sin datos"

    # Economic cost of inactive stays
    costo_inac_tot <- sum(data_kpi_gc$valor[
      data_kpi_gc$nombre_ind == "Costo de estancia inactiva por IPS" &
      data_kpi_gc$tipo == "inactivo_costo" &
      data_kpi_gc$year_val >= p$yr_desde &
      data_kpi_gc$year_val <= p$yr], na.rm = TRUE)

    fluidRow(
      valueBox(
        value    = format(n_adm, big.mark = "."),
        subtitle = "Registros servicio-estancia",
        icon     = icon("hospital"),
        color    = "blue",
        width    = 2
      ),
      valueBox(
        value    = paste0(round(med_core, 1), " días"),
        subtitle = "Media LOS (UCI/UCIN/HOSP)",
        icon     = icon("calendar"),
        color    = "light-blue",
        width    = 2
      ),
      valueBox(
        value    = gc_label,
        subtitle = "Giro de Cama institucional (último mes)",
        icon     = icon("bed"),
        color    = gc_col,
        width    = 3
      ),
      valueBox(
        value    = paste0(pct_long, "%"),
        subtitle = "% Larga estancia",
        icon     = icon("exclamation-triangle"),
        color    = if (pct_long > 20) "red" else if (pct_long > 10) "yellow" else "green",
        width    = 2
      ),
      valueBox(
        value    = format(ei_days, big.mark = "."),
        subtitle = "Días inactivos IPS",
        icon     = icon("pause"),
        color    = "orange",
        width    = 1
      ),
      valueBox(
        value    = cop(costo_inac_tot),
        subtitle = "Costo inactividad IPS (período)",
        icon     = icon("dollar-sign"),
        color    = "red",
        width    = 2
      )
    )
  })

  output$plot_trend_serv <- renderPlotly({
    df <- los_filt() %>%
      filter(estacion_2 %in% SERV_CORE) %>%
      group_by(estacion_2, month_year) %>%
      summarise(med = mean(dif_bed_serv, na.rm = TRUE),
                n   = n(), .groups = "drop") %>%
      mutate(fecha = as.Date(as.yearmon(month_year)))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha, y = med,
                        color = estacion_2, group = estacion_2,
                        text = paste0("<b>", estacion_2, "</b><br>",
                                      format(fecha, "%b %Y"), "<br>",
                                      "Media: ", round(med, 1), " días  N=", n))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_color_manual(values = serv_colors, na.value = "grey70") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Días (media)", color = "Servicio") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$plot_dist_year <- renderPlotly({
    df  <- los_filt() %>% filter(!is.na(year), estacion_2 %in% SERV_CORE)
    req(nrow(df) > 5)
    cap <- quantile(df$dif_bed_serv, 0.99, na.rm = TRUE)

    p <- ggplot(df, aes(x = factor(year), y = dif_bed_serv, fill = factor(year))) +
      geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3, fatten = 2) +
      scale_y_continuous(limits = c(0, cap)) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "Año", y = "Días de estancia", fill = "Año",
           caption = "Solo servicios UCI / UCIN / HOSP") +
      theme_classic() + theme(legend.position = "none")

    ggplotly(p)
  })

  # ── Resumen tab: egresos + LOS + GC by year ───────────────────────────────
  fin_yr_tbl <- reactive({
    p <- params()

    # Egresos and LOS from GRD
    grd_yr <- data_grd_base %>%
      filter(año >= p$yr_desde, año <= p$yr, !is.na(año)) %>%
      group_by(Año = año) %>%
      summarise(
        Egresos        = n(),
        `LOS medio (d)` = round(mean(dif_days, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>% arrange(Año)

    # GC from KPI — one row per year (mean of monthly rate_mean values)
    gc_yr <- data_kpi_gc %>%
      filter(nombre_ind == "Giro Cama Institucional", tipo == "rate_mean",
             !is.na(year_val), year_val >= p$yr_desde, year_val <= p$yr) %>%
      mutate(Año = as.integer(year_val)) %>%
      group_by(Año) %>%
      summarise(`GC prom.` = round(mean(valor, na.rm = TRUE), 2), .groups = "drop") %>%
      distinct(Año, .keep_all = TRUE)

    # Inactive days from KPI — one row per year (sum of monthly values)
    inac_yr <- data_kpi_gc %>%
      filter(nombre_ind == "Total de días de estancias inactivas",
             tipo == "inactivo_costo",
             !is.na(year_val), year_val >= p$yr_desde, year_val <= p$yr) %>%
      mutate(Año = as.integer(year_val)) %>%
      group_by(Año) %>%
      summarise(`Días inac.` = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      distinct(Año, .keep_all = TRUE)

    # Ventas: GRD deduplicado con valor_factura winsorizado (M COP)
    ventas_yr <- data_grd_dedup %>%
      filter(año >= p$yr_desde, año <= p$yr, !is.na(año)) %>%
      group_by(Año = año) %>%
      summarise(`Ventas GRD (M)` = round(sum(vf_win, na.rm = TRUE) / 1e6, 0), .groups = "drop")

    # Costos: itemizados por año (M COP)
    costos_yr <- data_costo_base %>%
      filter(año >= p$yr_desde, año <= p$yr, !is.na(año)) %>%
      group_by(Año = año) %>%
      summarise(`Costo total (M)` = round(sum(costo, na.rm = TRUE) / 1e6, 0), .groups = "drop")

    df <- grd_yr %>%
      left_join(gc_yr,     by = "Año") %>%
      left_join(inac_yr,   by = "Año") %>%
      left_join(ventas_yr, by = "Año") %>%
      left_join(costos_yr, by = "Año") %>%
      arrange(Año) %>%
      mutate(
        `∆ Egresos (%)`   = round((Egresos / lag(Egresos) - 1) * 100, 1),
        `∆ LOS (%)`       = round((`LOS medio (d)` / lag(`LOS medio (d)`) - 1) * 100, 1),
        `∆ GC (%)`        = round((`GC prom.` / lag(`GC prom.`) - 1) * 100, 1),
        `∆ Días inac.(%)` = round((`Días inac.` / lag(`Días inac.`) - 1) * 100, 1),
        `∆ Ventas (%)`    = round((`Ventas GRD (M)` / lag(`Ventas GRD (M)`) - 1) * 100, 1),
        `∆ Costo (%)`     = round((`Costo total (M)` / lag(`Costo total (M)`) - 1) * 100, 1)
      )
    df
  })

  output$plot_resumen_fin_yr <- renderPlotly({
    df <- fin_yr_tbl()
    req(nrow(df) > 0, "Ventas GRD (M)" %in% names(df))

    df_fin <- df %>%
      filter(!is.na(`Ventas GRD (M)`)) %>%
      select(Año, `Ventas GRD (M)`, `Costo total (M)`) %>%
      tidyr::pivot_longer(-Año, names_to = "Tipo", values_to = "Valor_M") %>%
      filter(!is.na(Valor_M)) %>%
      mutate(
        Tipo    = recode(Tipo,
                         "Ventas GRD (M)" = "Ventas GRD",
                         "Costo total (M)" = "Costo itemizado"),
        delta_pct = NA_real_  # computed below per series
      )

    # Add delta labels
    fin_wide <- df %>%
      select(Año, `Ventas GRD (M)`, `Costo total (M)`, `∆ Ventas (%)`, `∆ Costo (%)`)

    p <- ggplot(df_fin,
                aes(x = factor(Año), y = Valor_M, fill = Tipo,
                    text = paste0("<b>", Tipo, " ", Año, "</b><br>",
                                  "$", scales::comma(Valor_M, big.mark = ".", decimal.mark = ","),
                                  " M COP"))) +
      geom_col(position = position_dodge(width = 0.7), color = "white", linewidth = 0.2, alpha = 0.9, width = 0.6) +
      scale_fill_manual(values = c("Ventas GRD" = "#4E79A7", "Costo itemizado" = "#E15759")) +
      scale_y_continuous(labels = function(x) paste0("$", scales::comma(x, big.mark = ".", decimal.mark = ","), "M")) +
      labs(x = NULL, y = "Millones COP", fill = NULL,
           caption = "¹ Ventas GRD: valor_factura deduplicado, cap P99-2024. Costo: suma itemizada.") +
      theme_classic() +
      theme(legend.position = "bottom",
            plot.caption = element_text(size = 7, color = "grey50"),
            axis.text.x  = element_text(size = 9))

    # Annotate delta % for ventas
    delta_annot <- fin_wide %>%
      filter(!is.na(`∆ Ventas (%)`)) %>%
      mutate(label = paste0(ifelse(`∆ Ventas (%)` > 0, "+", ""), `∆ Ventas (%)`, "%"),
             Valor_M = `Ventas GRD (M)`)
    if (nrow(delta_annot) > 0) {
      p <- p + geom_text(data = delta_annot,
                         aes(x = factor(Año), y = Valor_M, label = label),
                         inherit.aes = FALSE,
                         vjust = -0.4, size = 3, color = "#4E79A7", fontface = "bold")
    }

    ggplotly(p, tooltip = "text") %>%
      layout(legend      = list(orientation = "h", y = -0.2),
             margin      = list(b = 60),
             annotations = list(list(
               x = 0.5, y = -0.18, xref = "paper", yref = "paper",
               text = "<sup>¹</sup> Ventas GRD: cap P99-2024 (~$225M/cuenta). Costo: itemizado.",
               showarrow = FALSE, font = list(size = 9, color = "grey"))))
  })

  output$tabla_resumen_fin_yr <- renderReactable({
    df <- fin_yr_tbl()
    req(nrow(df) > 0)

    mk_delta_style <- function(good_when_positive) {
      function(v) {
        if (is.na(v)) return(list())
        pos <- v > 0
        if ((good_when_positive && pos) || (!good_when_positive && !pos))
          list(color = "#59A14F", fontWeight = "bold")
        else if (v != 0)
          list(color = "#E15759")
        else list()
      }
    }

    reactable(df,
      searchable = FALSE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 70),
      columnGroups = list(
        colGroup(name = "Operacional (GRD / KPI)",
                 columns = c("Egresos","LOS medio (d)","GC prom.","Días inac.",
                             "∆ Egresos (%)","∆ LOS (%)","∆ GC (%)","∆ Días inac.(%)")),
        colGroup(name = "Financiero (M COP) ¹",
                 columns = c("Ventas GRD (M)","Costo total (M)",
                             "∆ Ventas (%)","∆ Costo (%)"))
      ),
      columns = list(
        Año              = colDef(minWidth = 60, sticky = "left",
                                  style = list(fontWeight = "bold")),
        Egresos          = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `LOS medio (d)`  = colDef(format = colFormat(digits = 1)),
        `GC prom.`       = colDef(format = colFormat(digits = 2),
                                   style = function(v) {
                                     if (!is.na(v) && v >= 10) list(color="#59A14F",fontWeight="bold")
                                     else if (!is.na(v) && v >= 8) list(color="#F28E2B")
                                     else if (!is.na(v)) list(color="#E15759")
                                     else list()
                                   }),
        `Días inac.`       = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `∆ Egresos (%)`    = colDef(format = colFormat(suffix = "%", digits = 1),
                                     style = mk_delta_style(TRUE)),
        `∆ LOS (%)`        = colDef(format = colFormat(suffix = "%", digits = 1),
                                     style = mk_delta_style(FALSE)),
        `∆ GC (%)`         = colDef(format = colFormat(suffix = "%", digits = 1),
                                     style = mk_delta_style(TRUE)),
        `∆ Días inac.(%)` = colDef(format = colFormat(suffix = "%", digits = 1),
                                    style = mk_delta_style(FALSE)),
        `Ventas GRD (M)`  = colDef(minWidth = 110, align = "right",
                                    format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        `Costo total (M)` = colDef(minWidth = 110, align = "right",
                                    format = colFormat(prefix = "$", separators = TRUE, digits = 0),
                                    style = function(v) list(color = "#E15759")),
        `∆ Ventas (%)`    = colDef(format = colFormat(suffix = "%", digits = 1),
                                    style = mk_delta_style(TRUE)),
        `∆ Costo (%)`     = colDef(format = colFormat(suffix = "%", digits = 1),
                                    style = mk_delta_style(FALSE))
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  # ── Resumen: operational 4-indicator delta chart ─────────────────────────
  # Native plotly subplot — avoids ggplotly/facet_wrap categorical axis bug
  output$plot_resumen_op_yr <- renderPlotly({
    df <- fin_yr_tbl()
    req(nrow(df) >= 2)

    ind_colors <- c("Egresos"       = "#4E79A7",
                    "LOS medio (d)" = "#E15759",
                    "GC prom."      = "#59A14F",
                    "Días inac."    = "#F28E2B")

    df_yr <- df %>%
      group_by(Año) %>%
      summarise(
        Egresos           = mean(Egresos,           na.rm = TRUE),
        `LOS medio (d)`   = mean(`LOS medio (d)`,   na.rm = TRUE),
        `GC prom.`        = mean(`GC prom.`,         na.rm = TRUE),
        `Días inac.`      = mean(`Días inac.`,       na.rm = TRUE),
        `∆ Egresos (%)`   = mean(`∆ Egresos (%)`,   na.rm = TRUE),
        `∆ LOS (%)`       = mean(`∆ LOS (%)`,       na.rm = TRUE),
        `∆ GC (%)`        = mean(`∆ GC (%)`,        na.rm = TRUE),
        `∆ Días inac.(%)` = mean(`∆ Días inac.(%)`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Año = as.character(as.integer(Año))) %>%
      arrange(Año) %>%
      filter(!is.na(`GC prom.`))   # limit to years with full KPI coverage

    make_panel <- function(val_col, delta_col, color, title) {
      vals  <- df_yr[[val_col]]
      delts <- df_yr[[delta_col]]
      yrs   <- df_yr$Año
      ok    <- !is.na(vals) & is.finite(vals)

      delta_lbl <- ifelse(
        !is.na(delts) & is.finite(delts),
        paste0(ifelse(delts > 0, "+", ""), round(delts, 1), "%"),
        ""
      )

      plot_ly(
        x             = yrs[ok],
        y             = vals[ok],
        type          = "bar",
        marker        = list(color = color, opacity = 0.88,
                             line  = list(color = "white", width = 1)),
        text          = delta_lbl[ok],
        textposition  = "outside",
        textfont      = list(size = 9, color = "#333333"),
        cliponaxis    = FALSE,
        hovertemplate = paste0("<b>", title, " %{x}</b><br>%{y:.2f}<extra></extra>"),
        showlegend    = FALSE
      ) %>%
        layout(
          xaxis = list(title = "", type = "category", tickfont = list(size = 9)),
          yaxis = list(title = "", tickfont = list(size = 8)),
          annotations = list(list(
            x = 0.5, y = 1.06, text = paste0("<b>", title, "</b>"),
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "bottom",
            showarrow = FALSE, font = list(size = 9)
          ))
        )
    }

    p1 <- make_panel("Egresos",       "∆ Egresos (%)",   "#4E79A7", "Egresos")
    p2 <- make_panel("LOS medio (d)", "∆ LOS (%)",       "#E15759", "LOS medio (d)")
    p3 <- make_panel("GC prom.",      "∆ GC (%)",        "#59A14F", "GC prom.")
    p4 <- make_panel("Días inac.",    "∆ Días inac.(%)", "#F28E2B", "Días inac.")

    subplot(p1, p2, p3, p4, nrows = 1, shareY = FALSE, shareX = FALSE,
            titleX = TRUE, titleY = FALSE) %>%
      layout(showlegend = FALSE, margin = list(t = 35, b = 30, l = 40, r = 10))
  })

  # ── Resumen: long-stay financial chart ────────────────────────────────────
  output$plot_resumen_fin_long <- renderPlotly({
    p <- params()

    flagged_cuentas <- los_flagged() %>%
      filter(larga_estancia) %>%
      pull(cuenta) %>%
      unique()

    if (length(flagged_cuentas) == 0) {
      return(plotly_empty() %>%
        layout(title = "Sin pacientes de larga estancia en el período seleccionado"))
    }

    ventas_long <- data_grd_dedup %>%
      filter(numero_de_cuenta %in% flagged_cuentas,
             año >= p$yr_desde, año <= p$yr, !is.na(año)) %>%
      group_by(Año = año) %>%
      summarise(
        n_larga       = n(),
        `Ventas LE (M)` = round(sum(vf_win, na.rm = TRUE) / 1e6, 0),
        `LOS med (d)` = round(mean(dif_days, na.rm = TRUE), 1),
        .groups = "drop"
      )

    costos_long <- data_costo_base %>%
      filter(cuenta %in% flagged_cuentas,
             año >= p$yr_desde, año <= p$yr, !is.na(año)) %>%
      group_by(Año = año) %>%
      summarise(`Costo LE (M)` = round(sum(costo, na.rm = TRUE) / 1e6, 0), .groups = "drop")

    df_long <- ventas_long %>%
      left_join(costos_long, by = "Año") %>%
      filter(!is.na(`Ventas LE (M)`))

    req(nrow(df_long) > 0)

    df_piv <- df_long %>%
      tidyr::pivot_longer(cols = c(`Ventas LE (M)`, `Costo LE (M)`),
                          names_to = "Tipo", values_to = "Valor_M") %>%
      filter(!is.na(Valor_M)) %>%
      mutate(tooltip = paste0(
        "<b>", Tipo, " — ", Año, "</b><br>$",
        scales::comma(Valor_M, big.mark = ".", decimal.mark = ","), " M COP",
        if_else(Tipo == "Ventas LE (M)",
                paste0("<br>", n_larga, " pacientes larga estancia | LOS med: ", `LOS med (d)`, " d"),
                "")
      ))

    p_plot <- ggplot(df_piv,
      aes(x = factor(Año), y = Valor_M, fill = Tipo, text = tooltip)) +
      geom_col(position = position_dodge(width = 0.7), color = "white", linewidth = 0.2, alpha = 0.9, width = 0.6) +
      scale_fill_manual(values = c("Ventas LE (M)" = "#4E79A7",
                                   "Costo LE (M)"  = "#E15759")) +
      scale_y_continuous(
        labels = function(x) paste0("$", scales::comma(x, big.mark = ".", decimal.mark = ","), "M")) +
      labs(x = NULL, y = "Millones COP", fill = NULL,
           caption = "Larga estancia = supera umbral P90 por servicio. Ventas GRD cap P99-2024.") +
      theme_classic() +
      theme(legend.position = "bottom",
            plot.caption    = element_text(size = 7, color = "grey50"))

    ggplotly(p_plot, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 2 · Tendencias
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_tend_trend <- renderPlotly({
    df <- los_filt() %>%
      group_by(estacion_2, month_year) %>%
      summarise(med = mean(dif_bed_serv, na.rm = TRUE),
                n   = n(), .groups = "drop") %>%
      mutate(fecha = as.Date(as.yearmon(month_year)))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha, y = med,
                        color = estacion_2, group = estacion_2,
                        text = paste0("<b>", estacion_2, "</b><br>",
                                      format(fecha, "%b %Y"), "<br>",
                                      "Media: ", round(med, 1), " d  N=", n))) +
      geom_line(linewidth = 1) + geom_point(size = 1.8) +
      scale_color_manual(values = serv_colors, na.value = "grey70") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Días (media)", color = "Servicio") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$plot_tend_box_year <- renderPlotly({
    df  <- los_filt() %>% filter(!is.na(year))
    req(nrow(df) > 5)
    cap <- quantile(df$dif_bed_serv, 0.99, na.rm = TRUE)

    p <- ggplot(df, aes(x = estacion_2, y = dif_bed_serv, fill = estacion_2)) +
      geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
      facet_wrap(~ year, nrow = 2) +
      scale_fill_manual(values = serv_colors, na.value = "grey70") +
      scale_y_continuous(limits = c(0, cap)) +
      labs(x = NULL, y = "Días de estancia") +
      theme_classic() +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 7),
            legend.position = "none",
            strip.text   = element_text(face = "bold", size = 8))

    ggplotly(p) %>% layout(showlegend = FALSE)
  })

  output$tabla_tend_pivot <- renderReactable({
    tbl <- los_filt() %>%
      group_by(Servicio = estacion_2, Año = year) %>%
      summarise(med = round(mean(dif_bed_serv, na.rm = TRUE), 1),
                N   = n(), .groups = "drop") %>%
      tidyr::pivot_wider(id_cols = Servicio,
                         names_from  = Año,
                         values_from = med) %>%
      rowwise() %>%
      mutate(Media = round(mean(c_across(where(is.numeric)), na.rm = TRUE), 1)) %>%
      ungroup()

    reactable(tbl,
      searchable = FALSE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(
        align = "center", minWidth = 70,
        style = function(v) {
          if (is.numeric(v) && !is.na(v)) {
            bg <- if (v > 7) "#fadbd8" else if (v > 4) "#fef9e7" else "#eafaf1"
            list(background = bg)
          }
        }
      ),
      columns = list(
        Servicio = colDef(minWidth = 110, sticky = "left", align = "left"),
        Media    = colDef(minWidth = 80, style = function(v) {
          list(fontWeight = "bold",
               background = if (!is.na(v) && v > 7) "#fadbd8"
                            else if (!is.na(v) && v > 4) "#fef9e7" else "#eafaf1")
        })
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_tend_annual_bar <- renderPlotly({
    df <- los_filt() %>%
      group_by(estacion_2, year) %>%
      summarise(med = mean(dif_bed_serv, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = factor(year), y = med, fill = estacion_2,
                        text = paste0("<b>", estacion_2, "</b><br>",
                                      "Año: ", year, "<br>",
                                      "Media: ", round(med, 1), " días"))) +
      geom_col(position = "dodge", color = "black", linewidth = 0.15) +
      scale_fill_manual(values = serv_colors, na.value = "grey70") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = "Año", y = "Media (días)", fill = "Servicio") +
      theme_classic() + theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 3 · Por Servicio
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_box_serv <- renderPlotly({
    df  <- los_filt() %>% filter(!is.na(year))
    req(nrow(df) > 5)
    cap <- quantile(df$dif_bed_serv, 0.99, na.rm = TRUE)

    p <- ggplot(df, aes(x = estacion_2, y = dif_bed_serv, fill = estacion_2)) +
      geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.3) +
      facet_wrap(~ year, nrow = 2) +
      scale_fill_manual(values = serv_colors, na.value = "grey70") +
      scale_y_continuous(limits = c(0, cap)) +
      labs(x = NULL, y = "Días de estancia") +
      theme_classic() +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 7),
            legend.position = "none",
            strip.text   = element_text(face = "bold", size = 8))

    ggplotly(p) %>% layout(showlegend = FALSE)
  })

  output$tabla_percentiles <- renderReactable({
    tbl <- los_filt() %>%
      group_by(Servicio = estacion_2, Año = year) %>%
      summarise(
        N       = n(),
        P25     = round(quantile(dif_bed_serv, 0.25, na.rm = TRUE), 1),
        Media   = round(mean(dif_bed_serv, na.rm = TRUE), 1),
        P75     = round(quantile(dif_bed_serv, 0.75, na.rm = TRUE), 1),
        P90     = round(quantile(dif_bed_serv, 0.90, na.rm = TRUE), 1),
        P95     = round(quantile(dif_bed_serv, 0.95, na.rm = TRUE), 1),
        Máx     = round(max(dif_bed_serv, na.rm = TRUE), 1),
        .groups = "drop"
      )

    reactable(tbl,
      groupBy = "Servicio", searchable = TRUE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 70,
                              format = colFormat(digits = 1)),
      columns = list(
        Servicio = colDef(minWidth = 110, sticky = "left", align = "left",
                          aggregate = "unique", format = colFormat(),
                          grouped   = JS("function(ci){return ci.value;}")),
        Año  = colDef(minWidth = 60, format = colFormat(digits = 0)),
        N    = colDef(format = colFormat(digits = 0), aggregate = "sum"),
        P25  = colDef(aggregate = "mean"),
        Media= colDef(aggregate = "mean"),
        P75  = colDef(aggregate = "mean"),
        P90  = colDef(aggregate = "mean"),
        P95  = colDef(aggregate = "mean"),
        Máx  = colDef(aggregate = "max")
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_serv_trend_month <- renderPlotly({
    serv <- input$serv_trend
    df <- los_filt() %>%
      filter(estacion_2 == serv) %>%
      group_by(month_year) %>%
      summarise(med = mean(dif_bed_serv, na.rm = TRUE),
                p90 = quantile(dif_bed_serv, 0.90, na.rm = TRUE),
                n   = n(), .groups = "drop") %>%
      mutate(fecha = as.Date(as.yearmon(month_year)))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha)) +
      geom_ribbon(aes(ymin = med, ymax = p90), fill = "#4E79A7", alpha = 0.2) +
      geom_line(aes(y = med, color = "Media"), linewidth = 1.1) +
      geom_line(aes(y = p90, color = "P90"), linewidth = 0.8, linetype = "dashed") +
      scale_color_manual(values = c("Media" = "#2C3E50", "P90" = "#E15759")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Días", color = NULL, title = serv) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5))

    ggplotly(p)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 4 · Larga Estancia
  # ══════════════════════════════════════════════════════════════════════════
  output$kpi_long <- renderUI({
    df     <- los_flagged()
    req(nrow(df) > 0)
    n_long  <- sum(df$larga_estancia, na.rm = TRUE)
    n_total <- nrow(df)
    pct_l   <- if (n_total > 0) round(n_long / n_total * 100, 1) else 0
    med_long  <- mean(df$dif_bed_serv[df$larga_estancia == TRUE],  na.rm = TRUE)
    med_short <- mean(df$dif_bed_serv[df$larga_estancia == FALSE], na.rm = TRUE)
    u_med     <- mean(umbral_df()$umbral, na.rm = TRUE)

    fluidRow(
      valueBox(
        value    = format(n_long, big.mark = "."),
        subtitle = "Casos larga estancia",
        icon     = icon("exclamation-triangle"),
        color    = "red",
        width    = 3
      ),
      valueBox(
        value    = paste0(pct_l, "%"),
        subtitle = "% del total",
        icon     = icon("percent"),
        color    = if (pct_l > 20) "red" else if (pct_l > 10) "yellow" else "green",
        width    = 2
      ),
      valueBox(
        value    = paste0(round(med_long, 1), " días"),
        subtitle = "Media LOS — larga estancia",
        icon     = icon("calendar"),
        color    = "orange",
        width    = 3
      ),
      valueBox(
        value    = paste0(round(med_short, 1), " días"),
        subtitle = "Media LOS — normal",
        icon     = icon("calendar"),
        color    = "green",
        width    = 2
      ),
      valueBox(
        value    = paste0(round(u_med, 1), " días"),
        subtitle = "Umbral mediano aplicado",
        icon     = icon("sliders-h"),
        color    = "light-blue",
        width    = 2
      )
    )
  })

  output$plot_pct_long <- renderPlotly({
    df <- los_flagged() %>%
      group_by(estacion_2) %>%
      summarise(n_long  = sum(larga_estancia, na.rm = TRUE),
                n_total = n(),
                pct     = round(n_long / n_total * 100, 1),
                .groups = "drop") %>%
      arrange(desc(pct))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = reorder(estacion_2, pct), y = pct, fill = estacion_2,
                        text = paste0("<b>", estacion_2, "</b><br>",
                                      n_long, "/", n_total, " (", pct, "%)"))) +
      geom_col(color = "black", linewidth = 0.2) +
      geom_hline(yintercept = 10, linetype = "dashed", color = "grey50") +
      coord_flip() +
      scale_fill_manual(values = serv_colors, na.value = "grey70") +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "% larga estancia") +
      theme_classic() + theme(legend.position = "none")

    ggplotly(p, tooltip = "text")
  })

  output$tabla_long_chars <- renderReactable({
    df <- los_flagged() %>%
      filter(larga_estancia) %>%
      group_by(Servicio = estacion_2) %>%
      summarise(
        Casos        = n(),
        `Media (d)`  = round(mean(dif_bed_serv,           na.rm = TRUE), 1),
        `P90 (d)`    = round(quantile(dif_bed_serv, 0.90, na.rm = TRUE), 1),
        `Edad prom.` = round(mean(as.numeric(edad_grd),   na.rm = TRUE), 0),
        `% Fallec.`  = round(mean(coalesce(estado_al_alta, "") == "FALLECIDO",
                                  na.rm = TRUE) * 100, 1),
        `% Con GRD`  = round(mean(!is.na(caci), na.rm = TRUE) * 100, 1),
        .groups      = "drop"
      ) %>%
      arrange(desc(Casos))
    req(nrow(df) > 0)

    reactable(df,
      searchable = FALSE, striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 80),
      columns = list(
        Servicio     = colDef(minWidth = 130, align = "left", sticky = "left"),
        Casos        = colDef(format = colFormat(separators = TRUE)),
        `Media (d)`  = colDef(format = colFormat(digits = 1)),
        `P90 (d)`    = colDef(format = colFormat(digits = 1)),
        `Edad prom.` = colDef(format = colFormat(digits = 0)),
        `% Fallec.`  = colDef(format = colFormat(suffix = "%", digits = 1),
                               style = function(v) {
                                 if (!is.na(v) && v > 10)
                                   list(color = "#E15759", fontWeight = "bold")
                                 else list()
                               }),
        `% Con GRD`  = colDef(format = colFormat(suffix = "%", digits = 1))
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_long_dist <- renderPlotly({
    df <- los_flagged() %>%
      mutate(Tipo = if_else(larga_estancia, "Larga estancia", "Estancia normal"))
    req(nrow(df) > 5)
    cap <- quantile(df$dif_bed_serv, 0.99, na.rm = TRUE)

    p <- ggplot(df, aes(x = dif_bed_serv, fill = Tipo)) +
      geom_histogram(bins = 40, alpha = 0.7, position = "identity", color = "white") +
      scale_x_continuous(limits = c(0, cap)) +
      scale_fill_manual(values = c("Larga estancia" = "#E15759",
                                   "Estancia normal" = "#4E79A7")) +
      labs(x = "Días de estancia", y = "Frecuencia", fill = NULL) +
      theme_classic() + theme(legend.position = "bottom")

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$plot_long_diag <- renderPlotly({
    flagged_cuentas <- los_flagged() %>%
      filter(larga_estancia) %>%
      pull(cuenta) %>%
      unique()

    # Group diagnoses by ICD-10 category-3 block
    df <- data_grd_base %>%
      filter(numero_de_cuenta %in% flagged_cuentas,
             !is.na(diagnostico_egreso_principal),
             diagnostico_egreso_principal != "") %>%
      mutate(grupo = grupo_diag) %>%
      group_by(grupo) %>%
      summarise(
        n         = n(),
        ejemplos  = paste(
          unique(str_sub(str_squish(diagnostico_egreso_principal), 1, 45))[1:min(3, n())],
          collapse = " | "),
        .groups   = "drop"
      ) %>%
      filter(grupo != "Sin código / NE") %>%
      arrange(desc(n)) %>%
      slice_head(n = 18)

    if (nrow(df) == 0) {
      return(plotly_empty() %>%
        layout(title = "Sin diagnóstico disponible (pacientes no cruzados con GRD)"))
    }

    p <- ggplot(df, aes(x = reorder(grupo, n), y = n,
                        text = paste0("<b>", grupo, "</b><br>",
                                      "Casos: ", n, "<br>",
                                      "<i>Ej: ", str_trunc(ejemplos, 80), "</i>"))) +
      geom_col(fill = "#E15759", color = "white", linewidth = 0.2) +
      coord_flip() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(x = NULL, y = "Casos de larga estancia") +
      theme_classic() + theme(axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text")
  })

  # ── Update CACI selector choices after data loads ────────────────────────
  observe({
    choices <- c("Todos los grupos" = "TODOS",
                 setNames(caci_choices_los, caci_choices_los),
                 "Sin CACI" = "SIN_CACI")
    updateSelectInput(session, "caci_serv_sel", choices = choices)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 5 · Diagnóstico (CACI)
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_caci_box <- renderPlotly({
    df <- grd_filt() %>%
      mutate(caci = coalesce(caci, "Sin CACI")) %>%
      filter(!is.na(dif_days))
    req(nrow(df) > 5)
    cap <- quantile(df$dif_days, 0.99, na.rm = TRUE)

    caci_cols_all <- c(caci_colors, "Sin CACI" = "grey60")
    p <- ggplot(df, aes(x = caci, y = dif_days, fill = caci,
                        text = paste0("<b>", caci, "</b><br>LOS: ", round(dif_days, 1), " d"))) +
      geom_jitter(aes(color = caci), width = 0.2, size = 0.5, alpha = 0.15) +
      geom_boxplot(outlier.shape = NA, alpha = 0.75, width = 0.55) +
      scale_fill_manual(values  = caci_cols_all, na.value = "grey70") +
      scale_color_manual(values = caci_cols_all, na.value = "grey70") +
      scale_y_continuous(limits = c(0, cap)) +
      labs(x = "CACI", y = "Días de estancia total") +
      theme_classic() + theme(legend.position = "none")

    ggplotly(p, tooltip = "text")
  })

  output$plot_caci_trend <- renderPlotly({
    df <- grd_filt() %>%
      mutate(caci = coalesce(caci, "Sin CACI")) %>%
      filter(!is.na(dif_days)) %>%
      group_by(caci, año) %>%
      summarise(med = mean(dif_days, na.rm = TRUE),
                n   = n(), .groups = "drop")
    req(nrow(df) > 0)

    caci_cols_all <- c(caci_colors, "Sin CACI" = "grey60")
    p <- ggplot(df, aes(x = año, y = med, color = caci, group = caci,
                        text = paste0("<b>", caci, "</b><br>",
                                      "Año: ", año, "<br>",
                                      "Media: ", round(med, 1), " d  N=", n))) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      scale_color_manual(values = caci_cols_all, na.value = "grey70") +
      scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), 1)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = "Año", y = "Media LOS (días)", color = "CACI") +
      theme_classic() + theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$plot_caci_serv_dist <- renderPlotly({
    sel <- input$caci_serv_sel
    df  <- los_flagged() %>%
      mutate(CACI = coalesce(str_to_upper(caci), "Sin CACI")) %>%
      filter(!is.na(dif_bed_serv), dif_bed_serv >= 0)

    if (!is.null(sel) && sel != "TODOS") {
      if (sel == "SIN_CACI") df <- df %>% filter(CACI == "Sin CACI")
      else                    df <- df %>% filter(CACI == sel)
    }
    req(nrow(df) > 5)
    cap <- quantile(df$dif_bed_serv, 0.99, na.rm = TRUE)

    p <- ggplot(df %>% filter(dif_bed_serv <= cap),
                aes(x = estacion_2, y = dif_bed_serv,
                    fill = estacion_2,
                    text = paste0("<b>", estacion_2, "</b><br>",
                                  "LOS: ", round(dif_bed_serv, 1), " días<br>",
                                  "CACI: ", CACI))) +
      geom_violin(trim = TRUE, alpha = 0.8) +
      geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
      scale_fill_manual(values = serv_colors, na.value = "grey60") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Días de estancia (LOS)") +
      theme_classic() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

    ggplotly(p, tooltip = "text")
  })

  output$plot_proc_los <- renderPlotly({
    df <- grd_filt() %>%
      filter(!is.na(procedimiento_qx), procedimiento_qx != "", !is.na(dif_days)) %>%
      mutate(proc_corto = str_sub(str_squish(procedimiento_qx), 1, 40)) %>%
      group_by(proc_corto) %>%
      summarise(med = mean(dif_days, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      arrange(desc(med)) %>%
      slice_head(n = 15)

    if (nrow(df) == 0) {
      return(plotly_empty() %>%
               layout(title = "Sin procedimientos quirúrgicos\npara el período seleccionado"))
    }

    p <- ggplot(df, aes(x = reorder(proc_corto, med), y = med,
                        text = paste0(proc_corto, "<br>",
                                      "Media: ", round(med, 1), " d  N=", n))) +
      geom_col(fill = "#4E79A7", color = "black", linewidth = 0.2) +
      coord_flip() +
      labs(x = NULL, y = "Media LOS (días)") +
      theme_classic() + theme(axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 6 · Ventas y Costos
  # ══════════════════════════════════════════════════════════════════════════

  # Referencia EMPÍRICA por servicio (mediana + IQR observados), separada de la
  # referencia CONTRACTUAL (PGP/PyG). La contractual cae en el P88 de lo
  # observado, así que no discrimina; ésta sí. Ver ref_emp en global.R.
  output$ref_emp_nota <- renderUI({
    ci <- ref_emp$costo_inst
    HTML(paste0(
      "Distribución observada ", ref_emp$periodo, " · n = ",
      format(ci$n, big.mark = "."), " cuentas. Institucional: mediana <b>",
      cop(ci$mediana), "</b>, IQR ", cop(ci$p25), "–", cop(ci$p75),
      ", P90 ", cop(ci$p90), ". ",
      "Se usa <b>mediana e IQR</b> y no la media porque la distribución es ",
      "fuertemente asimétrica (media/mediana ≈ ",
      round(ci$media / ci$mediana, 2), " · P99 ≈ 20× la mediana): la media ",
      "sigue la cola, no el centro. La referencia contractual (PGP/PyG) se ",
      "muestra aparte en los indicadores de arriba."
    ))
  })

  output$tabla_ref_emp <- renderReactable({
    d <- ref_emp$costo_serv %>%
      filter(n >= 30) %>%
      transmute(Servicio = departamento_cargue, n,
                Mediana = mediana, P25 = p25, P75 = p75,
                `P90 (alerta)` = p90,
                `Asimetría` = round(media / mediana, 2))
    money <- colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0))
    reactable(
      d, compact = TRUE, striped = TRUE, defaultPageSize = 12,
      defaultSorted = list(n = "desc"),
      columns = list(Mediana = money, P25 = money, P75 = money,
                     `P90 (alerta)` = money,
                     `Asimetría` = colDef(
                       style = function(v) if (!is.na(v) && v > 2)
                         list(color = "#B00020", fontWeight = "bold") else NULL))
    )
  })

  output$kpi_financiero <- renderUI({
    df <- cost_per_cuenta() %>% filter(!is.na(caci))
    req(nrow(df) > 0)

    # MEDIANA, no media: la distribución de costo/día es fuertemente asimétrica
    # (media/mediana ≈ 1,87 · CV ≈ 1,91 · P99 ≈ 20× la mediana), así que la media
    # sigue la cola y no el centro. Ver ref_emp en global.R.
    med_costo_dia      <- median(df$costo_dia, na.rm = TRUE)
    costo_dia_cama_pyg <- pyg_ref$costo_dia_cama_prom
    margen_pyg         <- pyg_ref$margen_2025_pct
    margen_col         <- if (is.na(margen_pyg)) "light-blue" else
                          if (margen_pyg >= 30) "green" else
                          if (margen_pyg >= 20) "yellow" else "red"

    fluidRow(
      valueBox(
        value    = cop(med_costo_dia),
        subtitle = HTML(paste0(
          "Costo diario <b>mediano</b><br><small>(itemizado; IQR ",
          cop(ref_emp$costo_inst$p25), "–", cop(ref_emp$costo_inst$p75), ")</small>")),
        icon     = icon("dollar-sign"),
        color    = "red",
        width    = 3
      ),
      valueBox(
        value    = cop(ingreso_dia_grd),
        subtitle = HTML("Ingreso por día de estancia<br><small>(GRD mediana, sin outliers)</small>"),
        icon     = icon("chart-line"),
        color    = "light-blue",
        width    = 3
      ),
      valueBox(
        value    = cop(costo_dia_cama_pyg),
        subtitle = HTML(paste0(
          "Costo cama/día <b>contractual</b><br><small>(PyG ", pyg_ref$year, ", ",
          pyg_ref$meses_reales, " meses ejecutados · P",
          round(100 * mean(df$costo_dia <= costo_dia_cama_pyg, na.rm = TRUE)),
          " de lo observado)</small>")),
        icon     = icon("bed"),
        color    = "orange",
        width    = 3
      ),
      valueBox(
        value    = if (is.na(margen_pyg)) "—" else paste0(margen_pyg, "%"),
        subtitle = HTML(paste0("Margen bruto institucional<br><small>(PyG ",
                               pyg_ref$year, ", ", pyg_ref$meses_reales,
                               " meses ejecutados)</small>")),
        icon     = icon("percent"),
        color    = margen_col,
        width    = 3
      )
    )
  })

  output$plot_scatter_los_cost <- renderPlotly({
    df <- cost_per_cuenta() %>%
      filter(!is.na(caci), !is.na(dif_days), dif_days >= 0, costo_total > 0)
    req(nrow(df) > 5)

    cap_d <- quantile(df$dif_days,    0.99, na.rm = TRUE)
    cap_c <- quantile(df$costo_total, 0.99, na.rm = TRUE)

    df2 <- df %>%
      mutate(cap_days = pmin(dif_days, cap_d),
             cap_cost = pmin(costo_total, cap_c))

    n_per_caci <- df2 %>% count(caci)
    add_smooth <- any(n_per_caci$n >= 10)

    p <- ggplot(df2, aes(x = cap_days, y = cap_cost / 1e6, color = caci,
                         text = paste0("<b>", caci, "</b><br>",
                                       "LOS: ", round(cap_days, 1), " d<br>",
                                       "Costo: ", cop(costo_total)))) +
      geom_point(alpha = 0.5, size = 1.5)

    if (add_smooth)
      p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8)

    p <- p +
      scale_color_manual(values = caci_colors, na.value = "grey70") +
      scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
      labs(x = "Días de estancia", y = "Costo total (M COP)", color = "CACI") +
      theme_classic() + theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })

  output$plot_cost_day_caci <- renderPlotly({
    df <- cost_per_cuenta() %>%
      filter(!is.na(caci), !is.na(costo_dia), costo_dia > 0)
    req(nrow(df) > 5)
    cap <- quantile(df$costo_dia / 1e3, 0.99, na.rm = TRUE)

    p <- ggplot(df, aes(x = caci, y = costo_dia / 1e3, fill = caci)) +
      geom_boxplot(outlier.size = 0.7, outlier.alpha = 0.4) +
      scale_fill_manual(values = caci_colors, na.value = "grey70") +
      scale_y_continuous(limits = c(0, cap),
                         labels = function(x) paste0("$", round(x), "k")) +
      labs(x = "CACI", y = "Costo por día (miles COP)") +
      theme_classic() + theme(legend.position = "none")

    ggplotly(p)
  })

  output$tabla_fin_resumen <- renderReactable({
    # Ventas: GRD deduplicado con valor_factura winsorizado al P99-2024 (excluye outliers 2025-2026)
    # Costo: itemizado de data_costo_base (columna costo, confiable)
    p <- params()

    ventas_grd <- data_grd_dedup %>%
      filter(año >= p$yr_desde, año <= p$yr,
             is.na(caci) | caci %in% p$cacis) %>%
      mutate(CACI = coalesce(str_to_upper(caci), "Sin CACI")) %>%
      group_by(CACI) %>%
      summarise(
        Egresos        = n(),
        `Ventas GRD`   = sum(vf_win, na.rm = TRUE),
        `LOS prom (d)` = round(mean(dif_days, na.rm = TRUE), 1),
        .groups        = "drop"
      )

    costos_caci <- costo_filt() %>%
      mutate(CACI = coalesce(str_to_upper(caci), "Sin CACI")) %>%
      group_by(CACI) %>%
      summarise(
        Cuentas       = n_distinct(cuenta),
        `Costo total` = sum(costo, na.rm = TRUE),
        .groups       = "drop"
      )

    df <- full_join(ventas_grd, costos_caci, by = "CACI") %>%
      mutate(
        `Ventas x egreso` = if_else(!is.na(Egresos) & Egresos > 0,
                                     round(`Ventas GRD`  / Egresos, 0), NA_real_),
        `Costo x egreso`  = if_else(!is.na(Egresos) & Egresos > 0,
                                     round(`Costo total` / Egresos, 0), NA_real_),
        `Margen est. (%)` = if_else(!is.na(`Ventas GRD`) & `Ventas GRD` > 0,
                                     round((`Ventas GRD` - `Costo total`) /
                                             `Ventas GRD` * 100, 1), NA_real_)
      ) %>%
      arrange(desc(`Ventas GRD`))
    req(nrow(df) > 0)

    reactable(df,
      searchable = TRUE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 90),
      columns = list(
        CACI             = colDef(minWidth = 130, sticky = "left", align = "left",
                                   style = list(fontWeight = "bold")),
        Egresos          = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `Ventas GRD`     = colDef(
          minWidth = 160, align = "right",
          header   = tagList("Ventas GRD",
                             tags$small(" ¹", style = "color:#F39C12; font-size:10px")),
          format   = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        `LOS prom (d)`   = colDef(format = colFormat(digits = 1)),
        Cuentas          = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `Costo total`    = colDef(minWidth = 160, align = "right",
                                   format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        `Ventas x egreso` = colDef(minWidth = 145, align = "right",
                                    format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        `Costo x egreso`  = colDef(
          minWidth = 145, align = "right",
          format   = colFormat(prefix = "$", separators = TRUE, digits = 0),
          style    = function(v) {
            if (is.na(v)) return(list())
            list(color = "#E15759")
          }),
        `Margen est. (%)` = colDef(
          minWidth = 115, align = "right",
          style    = function(v) {
            if (is.na(v)) return(list())
            if (v >= 20) list(color = "#27AE60", fontWeight = "bold")
            else if (v >= 0) list(color = "#F39C12")
            else list(color = "#E15759", fontWeight = "bold")
          })
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  # ── PyG reference KPI row ─────────────────────────────────────────────────
  output$pyg_ref_kpis <- renderUI({
    if (is.null(pyg_ref) || is.na(pyg_ref$costo_dia_cama_prom)) {
      return(tags$p(class = "text-muted", "Referencia PyG no disponible."))
    }
    ing_M  <- pyg_ref$ingresos_2025_M
    cos_M  <- pyg_ref$costos_2025_M
    mar    <- pyg_ref$margen_2025_pct

    fluidRow(
      valueBox(
        value    = paste0("$", formatC(round(ing_M / 1e3, 1), format = "f", digits = 1,
                                        big.mark = ".", decimal.mark = ","), " B"),
        subtitle = "Ingresos operacionales 2025",
        icon     = icon("coins"),
        color    = "blue",
        width    = 3
      ),
      valueBox(
        value    = paste0("$", formatC(round(cos_M / 1e3, 1), format = "f", digits = 1,
                                        big.mark = ".", decimal.mark = ","), " B"),
        subtitle = "Costo de ventas 2025",
        icon     = icon("receipt"),
        color    = "red",
        width    = 3
      ),
      valueBox(
        value    = paste0(mar, "%"),
        subtitle = "Margen bruto 2025",
        icon     = icon("percent"),
        color    = if (!is.na(mar) && mar >= 25) "green" else "yellow",
        width    = 3
      ),
      valueBox(
        value    = cop(pyg_ref$costo_dia_cama_prom),
        subtitle = HTML("Costo por cama/día<br><small>(Costo ventas ÷ días estancia KPI)</small>"),
        icon     = icon("bed"),
        color    = "orange",
        width    = 3
      )
    )
  })

  # ── Monthly PyG trend chart ────────────────────────────────────────────────
  output$plot_pyg_mensual <- renderPlotly({
    df <- pyg_ref$data_pyg_mensual
    req(nrow(df) > 0)

    meses_es <- c("Ene","Feb","Mar","Abr","May","Jun",
                  "Jul","Ago","Sep","Oct","Nov","Dic")
    df <- df %>%
      mutate(mes_label = meses_es[mes_num],
             mes_label = factor(mes_label, levels = meses_es),
             ingresos_M = ingresos_m   / 1e6,
             costos_M   = costo_ventas / 1e6)

    p <- ggplot(df, aes(x = mes_label)) +
      geom_col(aes(y = ingresos_M, fill = "Ingresos", text = paste0("Ingresos: $", round(ingresos_M), "M")),
               alpha = 0.85) +
      geom_col(aes(y = costos_M,   fill = "Costo ventas", text = paste0("Costo: $", round(costos_M), "M")),
               alpha = 0.85) +
      scale_fill_manual(values = c("Ingresos" = "#4E79A7", "Costo ventas" = "#E15759")) +
      labs(x = NULL, y = "Millones COP", fill = NULL,
           title = "Ingresos vs. Costo de ventas 2025 (PyG ejecutado)") +
      theme_classic() +
      theme(legend.position = "bottom", plot.title = element_text(size = 11))

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25),
             margin  = list(t = 35))
  })

  # ── Estimated cost of inactive stays (PyG-derived) ─────────────────────────
  output$pyg_costo_inac_box <- renderUI({
    costo_dia <- pyg_ref$costo_dia_cama_prom
    if (is.na(costo_dia)) return(NULL)

    dias_inac_total <- sum(data_ei$total_dias_de_estancia_por_ips, na.rm = TRUE) +
                       sum(data_ei$total_dias_de_estancia_por_eps, na.rm = TRUE)
    costo_inac_total <- costo_dia * dias_inac_total

    tagList(
      tags$h4("Costo estimado de estancias inactivas",
              style = "margin-top:10px; font-weight:bold; color:#2C3E50"),
      tags$p(style = "color:#555; font-size:13px;",
        HTML(paste0(
          "Base <b>contractual</b>: costo por cama/día del PyG ", pyg_ref$year,
          " × total de días de estancia inactiva. ",
          "En la pestaña <b>Est. Inactivas</b> hay una segunda valorización con base ",
          "<b>tarifaria</b> (día-cama SOAT 2026) y desglosada por responsable ",
          "IPS/EAPB — son dos bases distintas, no se suman."
        ))
      ),
      fluidRow(
        valueBox(
          value    = formatC(round(dias_inac_total), format = "d", big.mark = "."),
          subtitle = "Días inactivos totales (IPS + EPS)",
          icon     = icon("bed"),
          color    = "orange",
          width    = 6
        ),
        valueBox(
          value    = cop(costo_inac_total),
          subtitle = HTML("Costo directo estimado<br><small>(días inactivos × costo/cama/día PyG)</small>"),
          icon     = icon("dollar-sign"),
          color    = "red",
          width    = 6
        )
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab GC · Giro Cama & Ocupación
  # ══════════════════════════════════════════════════════════════════════════

  gc_serv_colors <- c(
    "Giro Cama Institucional" = "#2C3E50",
    "Giro cama de UCI"        = "#E15759",
    "Giro cama de UCIN"       = "#F28E2B",
    "Giro cama de HOSP"       = "#59A14F"
  )
  gc_metas <- c(
    "Giro Cama Institucional" = 10,
    "Giro cama de UCI"        = 8.8,
    "Giro cama de UCIN"       = 15,
    "Giro cama de HOSP"       = 6.5
  )

  # ── Filtered KPI reactive ────────────────────────────────────────────────
  gc_kpi_filt <- reactive({
    df <- data_kpi_gc
    if (!is.null(input$gc_yr) && input$gc_yr != "0")
      df <- df %>% filter(year_val == as.integer(input$gc_yr))
    df
  })

  gc_wide_filt <- reactive({
    df <- data_kpi_wide
    if (!is.null(input$gc_yr) && input$gc_yr != "0")
      df <- df %>% filter(year_val == as.integer(input$gc_yr))
    df
  })

  gc_bd_filt <- reactive({
    df <- data_bd_inac
    if (!is.null(input$gc_bd_yr) && input$gc_bd_yr != "0")
      df <- df %>% filter(year_bd == as.integer(input$gc_bd_yr))
    if (!is.null(input$gc_bd_resp) && input$gc_bd_resp != "0")
      df <- df %>% filter(responsable_bd == input$gc_bd_resp)
    df
  })

  # ── Sub-tab 1: Indicadores KPI ───────────────────────────────────────────
  output$gc_kpi_cards <- renderUI({
    df <- gc_kpi_filt() %>%
      filter(nombre_ind %in% names(gc_metas), tipo == "rate_mean")
    req(nrow(df) > 0)

    make_box <- function(ind, label, meta) {
      val <- mean(df$valor[df$nombre_ind == ind], na.rm = TRUE)
      col <- if (is.na(val)) "light-blue" else if (val >= meta) "green" else if (val >= meta * 0.9) "yellow" else "red"
      valueBox(
        value    = if (is.na(val)) "—" else round(val, 2),
        subtitle = paste0(label, " (meta ", meta, ")"),
        icon     = icon("bed"),
        color    = col,
        width    = 3
      )
    }
    fluidRow(
      make_box("Giro Cama Institucional", "GC Institucional", 10),
      make_box("Giro cama de UCI",        "GC UCI",           8.8),
      make_box("Giro cama de UCIN",       "GC UCIN",          15),
      make_box("Giro cama de HOSP",       "GC HOSP",          6.5)
    )
  })

  output$gc_plot_trend <- renderPlotly({
    df <- data_kpi_gc %>%
      filter(nombre_ind %in% names(gc_metas), tipo == "rate_mean", !is.na(valor)) %>%
      mutate(fecha = as.Date(paste0(year_val, "-", sprintf("%02d", mes_num), "-01")))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha, y = valor, color = nombre_ind, group = nombre_ind,
                        text = paste0("<b>", nombre_ind, "</b><br>",
                                      format(fecha, "%b %Y"), "<br>",
                                      "GC: ", round(valor, 2),
                                      " | Meta: ", gc_metas[nombre_ind]))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      geom_hline(data = tibble(nombre_ind = names(gc_metas), meta = gc_metas),
                 aes(yintercept = meta, color = nombre_ind),
                 linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
      scale_color_manual(values = gc_serv_colors, na.value = "grey60") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(x = NULL, y = "Giro de cama (egresos/cama)", color = "Servicio") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_plot_meta_bar <- renderPlotly({
    df <- data_kpi_gc %>%
      filter(nombre_ind %in% names(gc_metas), tipo == "rate_mean", !is.na(valor)) %>%
      group_by(nombre_ind, year_val) %>%
      summarise(prom = mean(valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        meta     = gc_metas[nombre_ind],
        pct_meta = round(prom / meta * 100, 1),
        color_ok = prom >= meta
      )
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = factor(year_val), y = prom, fill = nombre_ind,
                        text = paste0("<b>", nombre_ind, "</b><br>",
                                      "Año: ", year_val, "<br>",
                                      "Promedio: ", round(prom, 2), "<br>",
                                      "Meta: ", meta, " (", pct_meta, "%)"))) +
      geom_col(position = "dodge", color = "white", linewidth = 0.2) +
      geom_hline(data = tibble(nombre_ind = names(gc_metas), meta = gc_metas),
                 aes(yintercept = meta, color = nombre_ind),
                 linetype = "dashed", linewidth = 0.8) +
      scale_fill_manual(values  = gc_serv_colors, na.value = "grey60") +
      scale_color_manual(values = gc_serv_colors, na.value = "grey60") +
      labs(x = "Año", y = "GC promedio", fill = NULL) +
      theme_classic() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  output$gc_tabla_kpi <- renderReactable({
    df <- data_kpi_gc %>%
      filter(nombre_ind %in% names(gc_metas), tipo == "rate_mean", !is.na(valor)) %>%
      mutate(col_key = paste0(year_val, "_", sprintf("%02d", mes_num)),
             servicio_corto = case_when(
               nombre_ind == "Giro Cama Institucional" ~ "Institucional",
               nombre_ind == "Giro cama de UCI"        ~ "UCI",
               nombre_ind == "Giro cama de UCIN"       ~ "UCIN",
               nombre_ind == "Giro cama de HOSP"       ~ "HOSP"
             )) %>%
      select(Servicio = servicio_corto, year_val, mes_num, valor) %>%
      mutate(label = paste0(year_val, "-M", sprintf("%02d", mes_num))) %>%
      select(-year_val, -mes_num) %>%
      tidyr::pivot_wider(names_from = label, values_from = valor)

    reactable(df,
      searchable = FALSE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(
        align = "center", minWidth = 65, format = colFormat(digits = 2),
        style = function(v) {
          if (!is.numeric(v) || is.na(v)) return(list())
          if (v >= 10) list(background = "#d5f5e3", fontWeight = "bold")
          else if (v >= 8) list(background = "#fef9e7")
          else list(background = "#fadbd8")
        }
      ),
      columns = list(
        Servicio = colDef(minWidth = 120, sticky = "left", align = "left",
                          style = list(fontWeight = "bold"),
                          format = colFormat())
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  # ── Sub-tab 2: Tendencias y Quiebres ─────────────────────────────────────
  output$gc_plot_breakpoint <- renderPlotly({
    df <- data_kpi_wide %>% filter(!is.na(tasa_gc)) %>% arrange(fecha)
    req(nrow(df) >= 6)

    bp_date <- tryCatch({
      ts_vals <- ts(df$tasa_gc, frequency = 12,
                    start = c(df$year_val[1], df$mes_num[1]))
      bp      <- strucchange::breakpoints(ts_vals ~ 1)
      bp_idx  <- bp$breakpoints
      if (!is.na(bp_idx[1])) df$fecha[bp_idx[1]] else NULL
    }, error = function(e) NULL)

    p <- ggplot(df, aes(x = fecha, y = tasa_gc,
                        text = paste0(format(fecha, "%b %Y"), "<br>GC: ", round(tasa_gc, 2)))) +
      geom_line(color = "#2C3E50", linewidth = 1.1) +
      geom_point(aes(color = factor(year_val)), size = 2.5) +
      geom_hline(yintercept = 10, linetype = "dashed", color = "#E15759", linewidth = 0.8) +
      geom_smooth(method = "loess", se = TRUE, color = "#4E79A7",
                  fill = "#4E79A7", alpha = 0.15, linewidth = 0.8) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_color_manual(values = c("2024" = "#F28E2B","2025" = "#59A14F","2026" = "#4E79A7"),
                         name = "Año") +
      labs(x = NULL, y = "Giro Cama Institucional") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")

    pl <- ggplotly(p, tooltip = "text")

    if (!is.null(bp_date)) {
      pl <- pl %>% layout(
        shapes = list(list(
          type = "line", x0 = as.character(bp_date), x1 = as.character(bp_date),
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "red", dash = "dot", width = 2)
        )),
        annotations = list(list(
          x = as.character(bp_date), y = 1, yref = "paper",
          text = paste0("Quiebre: ", format(bp_date, "%b %Y")),
          showarrow = TRUE, arrowhead = 2, ax = 40, ay = -30,
          font = list(color = "red", size = 11)
        ))
      )
    }
    pl %>% layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_plot_los_inac <- renderPlotly({
    df <- data_kpi_wide %>%
      filter(!is.na(mean_los), !is.na(dias_inac_total)) %>%
      arrange(fecha)
    req(nrow(df) > 0)

    escala <- max(df$dias_inac_total, na.rm = TRUE) / max(df$mean_los, na.rm = TRUE)

    p <- ggplot(df, aes(x = fecha)) +
      geom_col(aes(y = dias_inac_total, fill = "Días Inactivos"), alpha = 0.7) +
      geom_line(aes(y = mean_los * escala, color = "LOS Medio"),
                linewidth = 1.2, group = 1) +
      geom_point(aes(y = mean_los * escala, color = "LOS Medio"), size = 2) +
      scale_y_continuous(
        name     = "Días Inactivos Totales",
        sec.axis = sec_axis(~ . / escala, name = "LOS Medio (días)")
      ) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_fill_manual(values  = c("Días Inactivos" = "#E15759")) +
      scale_color_manual(values = c("LOS Medio"      = "#2C3E50")) +
      labs(x = NULL, fill = NULL, color = NULL) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_plot_corr_los <- renderPlotly({
    df <- data_kpi_wide %>% filter(!is.na(tasa_gc), !is.na(mean_los))
    req(nrow(df) >= 4)
    r_val <- round(cor(df$tasa_gc, df$mean_los, use = "complete.obs"), 3)

    p <- ggplot(df, aes(x = mean_los, y = tasa_gc, color = factor(year_val),
                        text = paste0(format(fecha, "%b %Y"),
                                      "<br>LOS: ", round(mean_los, 2),
                                      "<br>GC: ",  round(tasa_gc,  2)))) +
      geom_point(size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "grey40",
                  fill = "grey80", linewidth = 0.8, alpha = 0.3) +
      scale_color_manual(values = c("2024" = "#F28E2B","2025" = "#59A14F","2026" = "#4E79A7"),
                         name = "Año") +
      annotate("text", x = min(df$mean_los, na.rm=TRUE),
               y = max(df$tasa_gc, na.rm=TRUE),
               label = paste0("r = ", r_val), hjust = 0, size = 4, color = "#2C3E50") +
      labs(x = "LOS Medio Institucional (días)", y = "Giro Cama Institucional") +
      theme_classic() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })

  output$gc_plot_corr_inac <- renderPlotly({
    df <- data_kpi_wide %>% filter(!is.na(tasa_gc), !is.na(dias_inac_total))
    req(nrow(df) >= 4)
    r_val <- round(cor(df$tasa_gc, df$dias_inac_total, use = "complete.obs"), 3)

    p <- ggplot(df, aes(x = dias_inac_total, y = tasa_gc, color = factor(year_val),
                        text = paste0(format(fecha, "%b %Y"),
                                      "<br>Días inac.: ", dias_inac_total,
                                      "<br>GC: ", round(tasa_gc, 2)))) +
      geom_point(size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "grey40",
                  fill = "grey80", linewidth = 0.8, alpha = 0.3) +
      scale_color_manual(values = c("2024" = "#F28E2B","2025" = "#59A14F","2026" = "#4E79A7"),
                         name = "Año") +
      annotate("text", x = min(df$dias_inac_total, na.rm=TRUE),
               y = max(df$tasa_gc, na.rm=TRUE),
               label = paste0("r = ", r_val), hjust = 0, size = 4, color = "#2C3E50") +
      labs(x = "Días Inactivos Totales / Mes", y = "Giro Cama Institucional") +
      theme_classic() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })

  output$gc_plot_corr_matrix <- renderPlotly({
    df_num <- data_kpi_wide %>%
      select(GC = tasa_gc, LOS = mean_los,
             Inac_Total = dias_inac_total,
             Inac_IPS   = dias_inac_ips,
             Inac_EPS   = dias_inac_eps) %>%
      filter(complete.cases(.))
    req(nrow(df_num) >= 4)

    cm   <- round(cor(df_num), 2)
    nms  <- colnames(cm)
    plot_ly(z = cm, x = nms, y = nms, type = "heatmap",
            colorscale = list(c(0,"#E15759"), c(0.5,"white"), c(1,"#4E79A7")),
            zmin = -1, zmax = 1,
            text = matrix(cm, nrow = length(nms)),
            hovertemplate = "%{y} × %{x}<br>r = %{z}<extra></extra>") %>%
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })

  # ── Sub-tab 3: Modelo Estadístico ────────────────────────────────────────
  # Response: tasa_gc (Giro de Cama rate, continuous positive)
  # Gamma GLM with log link; egresos added as demand control
  gc_modelo <- reactive({
    df <- data_kpi_wide %>%
      filter(!is.na(tasa_gc), !is.na(mean_los), !is.na(dias_inac_total),
             !is.na(egresos), tasa_gc > 0, egresos > 0)
    req(nrow(df) >= 8)
    tryCatch(
      glm(tasa_gc ~ mean_los + dias_inac_total + log(egresos) + año_f,
          family = Gamma(link = "log"), data = df),
      error = function(e) NULL
    )
  })

  output$gc_tabla_modelo <- renderReactable({
    m <- gc_modelo()
    req(!is.null(m))

    coefs <- coef(summary(m))
    ci    <- tryCatch(confint(m), error = function(e) matrix(NA, nrow(coefs), 2))
    tbl <- tibble(
      Variable      = rownames(coefs),
      `Razón (exp)` = round(exp(coefs[, 1]), 4),
      `IC 2.5%`     = round(exp(ci[, 1]),   4),
      `IC 97.5%`    = round(exp(ci[, 2]),   4),
      `p-valor`     = round(coefs[, 4],     4)
    ) %>%
      mutate(
        Significancia = case_when(
          `p-valor` < 0.001 ~ "***",
          `p-valor` < 0.01  ~ "**",
          `p-valor` < 0.05  ~ "*",
          `p-valor` < 0.10  ~ ".",
          TRUE               ~ ""
        ),
        Interpretación = case_when(
          Variable == "(Intercept)"      ~ "Intercepto del modelo",
          Variable == "mean_los"         ~ "+1 día estancia media → efecto en Giro Cama",
          Variable == "dias_inac_total"  ~ "+1 día inactivo → efecto en Giro Cama",
          Variable == "log(egresos)"     ~ "Control por volumen de demanda (egresos)",
          str_detect(Variable, "año_f")  ~
            paste0("Ajuste año ", str_extract(Variable, "[0-9]{4}")),
          TRUE ~ Variable
        )
      )

    reactable(tbl,
      searchable = FALSE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 80),
      columns = list(
        Variable       = colDef(minWidth = 150, align = "left"),
        Interpretación = colDef(minWidth = 260, align = "left"),
        `Razón (exp)`  = colDef(
          style = function(v) {
            if (!is.na(v) && v < 1) list(color = "#E15759", fontWeight = "bold")
            else if (!is.na(v) && v > 1) list(color = "#59A14F", fontWeight = "bold")
            else list()
          }
        ),
        `p-valor` = colDef(
          style = function(v) {
            if (!is.na(v) && v < 0.05) list(fontWeight = "bold", color = "#2C3E50")
            else list(color = "#999")
          }
        ),
        Significancia = colDef(minWidth = 60)
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$gc_plot_pred_obs <- renderPlotly({
    m  <- gc_modelo()
    df <- data_kpi_wide %>%
      filter(!is.na(tasa_gc), !is.na(mean_los), !is.na(dias_inac_total),
             !is.na(egresos), tasa_gc > 0, egresos > 0)
    req(!is.null(m), nrow(df) >= 8)
    df$pred <- predict(m, type = "response")

    p <- ggplot(df, aes(text = paste0(format(fecha, "%b %Y"),
                                       "<br>GC obs: ",  round(tasa_gc, 2),
                                       "<br>GC pred: ", round(pred,    2)))) +
      geom_point(aes(x = tasa_gc, y = pred, color = factor(year_val)), size = 3, alpha = 0.8) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
      scale_color_manual(values = c("2024"="#F28E2B","2025"="#59A14F","2026"="#4E79A7"),
                         name = "Año") +
      labs(x = "Giro de Cama observado", y = "Giro de Cama predicho") +
      theme_classic() + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })

  output$gc_plot_marg_los <- renderPlotly({
    m  <- gc_modelo()
    df <- data_kpi_wide %>%
      filter(!is.na(tasa_gc), !is.na(mean_los), !is.na(dias_inac_total),
             !is.na(egresos), tasa_gc > 0, egresos > 0)
    req(!is.null(m), nrow(df) >= 4)

    los_seq    <- seq(min(df$mean_los,    na.rm=TRUE), max(df$mean_los,    na.rm=TRUE), length.out=60)
    inac_mean  <- mean(df$dias_inac_total, na.rm=TRUE)
    egr_mean   <- mean(df$egresos,         na.rm=TRUE)

    pred_df <- bind_rows(lapply(unique(df$year_val), function(yr) {
      tibble(mean_los=los_seq, dias_inac_total=inac_mean, egresos=egr_mean,
             año_f=factor(yr, levels=levels(df$año_f)), year_val=yr)
    })) %>% mutate(pred = predict(m, newdata=., type="response"))

    p <- ggplot(pred_df, aes(x=mean_los, y=pred, color=factor(year_val),
                              text=paste0("Año: ",year_val,
                                          "<br>LOS: ",round(mean_los,1),
                                          "<br>GC pred: ",round(pred,2)))) +
      geom_line(linewidth=1.2) +
      geom_hline(yintercept=10, linetype="dashed", color="#E15759", linewidth=0.7) +
      scale_color_manual(values=c("2024"="#F28E2B","2025"="#59A14F","2026"="#4E79A7"), name="Año") +
      labs(x="Estancia Media (días)", y="Giro de Cama predicho") +
      theme_classic() + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>% layout(legend=list(orientation="h", y=-0.25))
  })

  output$gc_plot_marg_inac <- renderPlotly({
    m  <- gc_modelo()
    df <- data_kpi_wide %>%
      filter(!is.na(tasa_gc), !is.na(mean_los), !is.na(dias_inac_total),
             !is.na(egresos), tasa_gc > 0, egresos > 0)
    req(!is.null(m), nrow(df) >= 4)

    inac_seq  <- seq(min(df$dias_inac_total, na.rm=TRUE), max(df$dias_inac_total, na.rm=TRUE), length.out=60)
    los_mean  <- mean(df$mean_los,  na.rm=TRUE)
    egr_mean  <- mean(df$egresos,   na.rm=TRUE)

    pred_df <- bind_rows(lapply(unique(df$year_val), function(yr) {
      tibble(mean_los=los_mean, dias_inac_total=inac_seq, egresos=egr_mean,
             año_f=factor(yr, levels=levels(df$año_f)), year_val=yr)
    })) %>% mutate(pred = predict(m, newdata=., type="response"))

    p <- ggplot(pred_df, aes(x=dias_inac_total, y=pred, color=factor(year_val),
                              text=paste0("Año: ",year_val,
                                          "<br>Días inac: ",round(dias_inac_total),
                                          "<br>GC pred: ",round(pred,2)))) +
      geom_line(linewidth=1.2) +
      geom_hline(yintercept=10, linetype="dashed", color="#E15759", linewidth=0.7) +
      scale_color_manual(values=c("2024"="#F28E2B","2025"="#59A14F","2026"="#4E79A7"), name="Año") +
      labs(x="Días Inactivos / Mes", y="Giro de Cama predicho") +
      theme_classic() + theme(legend.position="bottom")
    ggplotly(p, tooltip="text") %>% layout(legend=list(orientation="h", y=-0.25))
  })

  # ── Sub-tab 4: Estancias Inactivas BD Paciente ────────────────────────────
  output$gc_bd_kpi_cards <- renderUI({
    df <- gc_bd_filt()
    req(nrow(df) > 0)
    n_pac       <- n_distinct(df$identificacion, na.rm = TRUE)
    tot_inac    <- sum(df$estancia_inactiva, na.rm = TRUE)
    med_inac    <- round(mean(df$estancia_inactiva, na.rm = TRUE), 1)
    pct_ips     <- round(mean(df$responsable_bd == "IPS", na.rm = TRUE) * 100, 1)
    pct_eps     <- round(mean(df$responsable_bd == "EPS", na.rm = TRUE) * 100, 1)

    fluidRow(
      valueBox(format(n_pac, big.mark="."),     "Pacientes únicos",         icon("user"),     color="blue",       width=2),
      valueBox(format(tot_inac, big.mark="."),  "Total días inactivos",     icon("pause"),    color="red",        width=3),
      valueBox(paste0(med_inac, " días"),        "Media días/paciente",      icon("calendar"), color="light-blue", width=2),
      valueBox(paste0(pct_ips, "%"),             "Responsabilidad IPS",      icon("hospital"), color="orange",     width=2),
      valueBox(paste0(pct_eps, "%"),             "Responsabilidad EPS",      icon("file-alt"), color="yellow",     width=3)
    )
  })

  output$gc_plot_pareto <- renderPlotly({
    df <- gc_bd_filt() %>%
      filter(!is.na(causa_principal_bd), causa_principal_bd != "") %>%
      mutate(causa_corta = causa_principal_bd) %>%
      group_by(causa_corta) %>%
      summarise(dias  = sum(estancia_inactiva, na.rm = TRUE),
                casos = n(), .groups = "drop") %>%
      arrange(desc(dias)) %>%
      slice_head(n = 15) %>%
      mutate(
        acum_pct = cumsum(dias) / sum(dias) * 100,
        label    = str_wrap(causa_corta, 40)
      )
    req(nrow(df) > 0)

    escala_p <- max(df$dias) / 100

    p <- ggplot(df, aes(x = reorder(label, dias))) +
      geom_col(aes(y = dias, fill = dias,
                   text = paste0("<b>", causa_corta, "</b><br>",
                                 casos, " casos · ", dias, " días")),
               color = "white", linewidth = 0.2) +
      geom_line(aes(y = acum_pct * escala_p, group = 1), color = "#2C3E50", linewidth = 1) +
      geom_point(aes(y = acum_pct * escala_p), color = "#2C3E50", size = 2) +
      scale_fill_gradient(low = "#F28E2B", high = "#E15759") +
      scale_y_continuous(
        name     = "Días inactivos",
        sec.axis = sec_axis(~ . / escala_p, name = "% Acumulado",
                            labels = function(x) paste0(x, "%"))
      ) +
      coord_flip() +
      labs(x = NULL) +
      theme_classic() +
      theme(legend.position = "none", axis.text.y = element_text(size = 7))
    ggplotly(p, tooltip = "text")
  })

  output$gc_plot_edad <- renderPlotly({
    df <- gc_bd_filt() %>%
      filter(!is.na(rangos_edad), rangos_edad != "Sin dato",
             !is.na(responsable_bd)) %>%
      group_by(Rango = rangos_edad, Responsable = responsable_bd) %>%
      summarise(n = n(), dias = sum(estancia_inactiva, na.rm = TRUE), .groups = "drop")

    if (nrow(df) == 0) {
      return(plotly_empty() %>%
        layout(title = "Sin datos de edad (requiere cruce GRD)"))
    }

    resp_cols <- c("IPS"="#E15759","EPS"="#F28E2B","Paciente"="#4E79A7","No clasificado"="grey60")
    p <- ggplot(df, aes(x = Rango, y = n, fill = Responsable,
                        text = paste0("<b>", Rango, "</b><br>",
                                      Responsable, ": ", n, " pacientes · ",
                                      dias, " días inactivos"))) +
      geom_col(position = "stack", color = "white", linewidth = 0.2) +
      scale_fill_manual(values = resp_cols) +
      labs(x = "Rango de edad (GRD)", y = "Pacientes", fill = "Responsable") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_plot_bd_trend <- renderPlotly({
    df <- gc_bd_filt() %>%
      filter(!is.na(year_bd), !is.na(mes_bd_num)) %>%
      group_by(year_bd, mes_bd_num, responsable_bd) %>%
      summarise(dias = sum(estancia_inactiva, na.rm = TRUE), .groups = "drop") %>%
      mutate(fecha = as.Date(paste0(year_bd, "-", sprintf("%02d", mes_bd_num), "-01")))
    req(nrow(df) > 0)

    resp_cols <- c("IPS"="#E15759","EPS"="#F28E2B","Paciente"="#4E79A7","No clasificado"="grey60")
    p <- ggplot(df, aes(x = fecha, y = dias, fill = responsable_bd,
                        text = paste0(format(fecha, "%b %Y"), "<br>",
                                      responsable_bd, ": ", dias, " días"))) +
      geom_col(position = "stack", color = "white", linewidth = 0.1) +
      scale_fill_manual(values = resp_cols) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(x = NULL, y = "Días inactivos", fill = "Responsable") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_plot_diag <- renderPlotly({
    df <- gc_bd_filt() %>%
      filter(!is.na(diag_ei), diag_ei != "") %>%
      mutate(diag_corto = str_sub(str_squish(diag_ei), 1, 50)) %>%
      group_by(diag_corto) %>%
      summarise(n    = n(),
                dias = sum(estancia_inactiva, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(dias)) %>%
      slice_head(n = 15)

    if (nrow(df) == 0)
      return(plotly_empty() %>% layout(title = "Sin diagnóstico disponible"))

    p <- ggplot(df, aes(x = reorder(diag_corto, dias), y = dias,
                        text = paste0("<b>", diag_corto, "</b><br>",
                                      n, " casos · ", dias, " días inactivos"))) +
      geom_col(fill = "#4E79A7", color = "white", linewidth = 0.2) +
      coord_flip() +
      labs(x = NULL, y = "Total días inactivos") +
      theme_classic() + theme(axis.text.y = element_text(size = 7))
    ggplotly(p, tooltip = "text")
  })

  output$gc_tabla_bd_detail <- renderDT({
    df <- gc_bd_filt() %>%
      select(
        Paciente          = paciente,
        Identificación    = identificacion,
        EPS               = eps,
        Año               = year_bd,
        Mes               = mes,
        Responsable       = responsable_bd,
        CACI              = caci_label,
        `Es CACI`         = es_caci,
        `Causa IPS`       = causa_1_de_estancia_inactiva_por_ips,
        `Causa EPS`       = causa_1_de_estancia_inactiva_por_eps,
        `Días Inac. IPS`  = dias_inac_ips_bd,
        `Días Inac. EPS`  = dias_inac_eps_bd,
        `Total Inactivo`  = estancia_inactiva,
        `Valor EI`        = valor_inac,
        `Diagnóstico GRD` = diag_ei,
        `Adm. GRD`        = n_adm_ei,
        `LOS prom GRD`    = los_grd_ei
      ) %>%
      mutate(`Es CACI` = if_else(`Es CACI`, "Sí", "No"))
    DT::datatable(df,
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Blfrtip", buttons = c("copy", "csv", "excel"),
        scrollX = TRUE, scroller = TRUE, scrollY = "380px", pageLength = 20,
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")
      ),
      class = "cell-border stripe compact", rownames = FALSE
    )
  })

  # ── Sub-tab 5: Impacto Económico ──────────────────────────────────────────
  output$gc_econ_kpi_cards <- renderUI({
    df_cost <- data_kpi_gc %>%
      filter(nombre_ind %in% c("Costo de estancia inactiva por EPS",
                                "Costo de estancia inactiva por IPS"),
             tipo == "inactivo_costo")

    df_inac <- data_kpi_gc %>%
      filter(nombre_ind == "Total de días de estancias inactivas",
             tipo == "inactivo_costo")

    costo_eps_tot <- sum(df_cost$valor[df_cost$nombre_ind == "Costo de estancia inactiva por EPS"],
                         na.rm = TRUE)
    costo_ips_tot <- sum(df_cost$valor[df_cost$nombre_ind == "Costo de estancia inactiva por IPS"],
                         na.rm = TRUE)
    dias_tot      <- sum(df_inac$valor, na.rm = TRUE)
    costo_dia     <- if (dias_tot > 0) round((costo_eps_tot + costo_ips_tot) / dias_tot, 0) else NA

    fluidRow(
      valueBox(cop(costo_eps_tot), "Costo EPS 2024-2026",      icon("file-invoice-dollar"), color="orange",     width=3),
      valueBox(cop(costo_ips_tot), "Costo IPS 2024-2026",      icon("hospital"),            color="red",        width=3),
      valueBox(format(dias_tot, big.mark="."), "Días inactivos acum.", icon("pause"),        color="light-blue", width=3),
      valueBox(cop(costo_dia),     "Costo promedio / día inac.",icon("calculator"),          color="yellow",     width=3)
    )
  })

  output$gc_plot_costo_trend <- renderPlotly({
    df <- data_kpi_gc %>%
      filter(nombre_ind %in% c("Costo de estancia inactiva por EPS",
                                "Costo de estancia inactiva por IPS"),
             tipo == "inactivo_costo", !is.na(valor)) %>%
      mutate(fecha = as.Date(paste0(year_val, "-", sprintf("%02d", mes_num), "-01")),
             tipo_label = if_else(str_detect(nombre_ind, "EPS"), "Costo EPS", "Costo IPS"))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha, y = valor / 1e6, color = tipo_label, group = tipo_label,
                        text = paste0(format(fecha, "%b %Y"), "<br>",
                                      tipo_label, ": $", round(valor / 1e6, 1), "M"))) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      scale_color_manual(values = c("Costo EPS" = "#F28E2B", "Costo IPS" = "#E15759")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
      labs(x = NULL, y = "Costo (Millones COP)", color = NULL) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_plot_inac_bar <- renderPlotly({
    df <- data_kpi_gc %>%
      filter(nombre_ind %in% c("Total de días de estancias inactivas EPS",
                                "Total de días de estancias inactivas IPS"),
             tipo == "inactivo_costo", !is.na(valor)) %>%
      mutate(fecha      = as.Date(paste0(year_val, "-", sprintf("%02d", mes_num), "-01")),
             tipo_label = if_else(str_detect(nombre_ind, "EPS"), "Días EPS", "Días IPS"))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha, y = valor, fill = tipo_label,
                        text = paste0(format(fecha, "%b %Y"), "<br>",
                                      tipo_label, ": ", valor, " días"))) +
      geom_col(position = "stack", color = "white", linewidth = 0.1) +
      scale_fill_manual(values = c("Días EPS" = "#F28E2B", "Días IPS" = "#E15759")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(x = NULL, y = "Días inactivos", fill = NULL) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$gc_tabla_anual <- renderReactable({
    tbl <- data_kpi_wide %>%
      group_by(Año = year_val) %>%
      summarise(
        `Egresos totales`   = sum(egresos, na.rm = TRUE),
        `GC prom.`          = round(mean(tasa_gc, na.rm = TRUE), 2),
        `LOS medio (d)`     = round(mean(mean_los, na.rm = TRUE), 2),
        `Días inac. total`  = sum(dias_inac_total, na.rm = TRUE),
        `Días inac. IPS`    = sum(dias_inac_ips,   na.rm = TRUE),
        `Días inac. EPS`    = sum(dias_inac_eps,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Año) %>%
      mutate(
        `Var. GC (%)`   = round(((`GC prom.` / lag(`GC prom.`)   - 1) * 100), 1),
        `Var. LOS (%)`  = round(((`LOS medio (d)` / lag(`LOS medio (d)`) - 1) * 100), 1),
        `Var. inac. (%)` = round(((as.numeric(`Días inac. total`) /
                                    lag(as.numeric(`Días inac. total`)) - 1) * 100), 1)
      )

    reactable(tbl,
      searchable = FALSE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 110),
      columns = list(
        Año               = colDef(minWidth = 70, sticky = "left", align = "left",
                                    style = list(fontWeight = "bold")),
        `Egresos totales` = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `GC prom.`        = colDef(format = colFormat(digits = 2),
                                    style = function(v) {
                                      if (!is.na(v) && v >= 10) list(color="#59A14F", fontWeight="bold")
                                      else if (!is.na(v) && v >= 8) list(color="#F28E2B")
                                      else if (!is.na(v)) list(color="#E15759")
                                      else list()
                                    }),
        `LOS medio (d)`   = colDef(format = colFormat(digits = 2)),
        `Días inac. total`= colDef(format = colFormat(separators = TRUE, digits = 0)),
        `Días inac. IPS`  = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `Días inac. EPS`  = colDef(format = colFormat(separators = TRUE, digits = 0)),
        `Var. GC (%)`     = colDef(format = colFormat(suffix = "%", digits = 1),
                                    style = function(v) {
                                      if (!is.na(v) && v > 0) list(color="#59A14F")
                                      else if (!is.na(v) && v < 0) list(color="#E15759")
                                      else list()
                                    }),
        `Var. LOS (%)`    = colDef(format = colFormat(suffix = "%", digits = 1),
                                    style = function(v) {
                                      if (!is.na(v) && v > 0) list(color="#E15759")
                                      else if (!is.na(v) && v < 0) list(color="#59A14F")
                                      else list()
                                    }),
        `Var. inac. (%)`  = colDef(format = colFormat(suffix = "%", digits = 1),
                                    style = function(v) {
                                      if (!is.na(v) && v > 0) list(color="#E15759")
                                      else if (!is.na(v) && v < 0) list(color="#59A14F")
                                      else list()
                                    })
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$gc_interpretacion <- renderUI({
    df  <- data_kpi_wide %>% filter(!is.na(tasa_gc), !is.na(mean_los))
    m   <- gc_modelo()

    r_gc_los  <- if (nrow(df) >= 4) round(cor(df$tasa_gc, df$mean_los,       use="complete.obs"), 3) else NA
    r_gc_inac <- if (nrow(df) >= 4) round(cor(df$tasa_gc, df$dias_inac_total, use="complete.obs"), 3) else NA

    r_los  <- round(exp(coef(m)["mean_los"]),          4)
    r_inac <- round(exp(coef(m)["dias_inac_total"]),   6)
    disp   <- round(summary(m)$dispersion, 3)
    pct_caci <- round(mean(data_bd_inac$es_caci, na.rm=TRUE)*100, 1)

    tags$div(
      style = "font-size: 14px; line-height: 1.9;",
      tags$h5(tags$strong("Hallazgos principales")),
      tags$ul(
        tags$li(HTML(paste0(
          "<b>Correlación GC ~ LOS:</b> r = ", r_gc_los,
          ". Cada día adicional de estancia reduce el giro de cama de forma consistente."
        ))),
        tags$li(HTML(paste0(
          "<b>Correlación GC ~ Días Inactivos:</b> r = ", r_gc_inac,
          ". Las estancias inactivas acumuladas comprimen la rotación efectiva."
        ))),
        tags$li(HTML(paste0(
          "<b>Modelo Gamma GLM (liga log) — respuesta: Giro de Cama:</b> ",
          "Razón LOS = ", r_los,
          " — cada día extra de estancia media multiplica el GC por ", r_los,
          " (", round((r_los-1)*100,1), "% de cambio). ",
          "Razón días inactivos = ", r_inac,
          " por cada día inactivo adicional. Dispersión del modelo: ", disp, "."
        ))),
        tags$li(HTML(paste0(
          "<b>", pct_caci, "% de los pacientes con estancia inactiva pertenecen al grupo CACI</b>",
          " (SCA, ICC, ACV principalmente). Esto convierte la gestión de inactivos en una",
          " palanca directa de calidad de la ruta clínica de alta complejidad."
        ))),
        tags$li(HTML(
          "<b>Recomendación operacional:</b> reducir 1 día promedio de LOS en UCIN y HOSP",
          " liberaría entre 30 y 45 camas-día/mes adicionales, con impacto directo en giro de cama",
          " y reducción de costos de estancia inactiva atribuible a IPS."
        ))
      )
    )
  })

  # ── Sub-tab 5: Análisis de oportunidad GC ─────────────────────────────────
  output$gc_oportunidad_kpis <- renderUI({
    gc_rec <- data_kpi_wide %>%
      filter(!is.na(tasa_gc), !is.na(egresos)) %>%
      filter(year_val == max(year_val, na.rm = TRUE))

    req(nrow(gc_rec) >= 2)

    gc_actual  <- mean(gc_rec$tasa_gc, na.rm = TRUE)
    gc_meta    <- 10
    camas_imp  <- mean(gc_rec$egresos / gc_rec$tasa_gc, na.rm = TRUE)
    fill_rate  <- 0.75
    delta_gc   <- max(0, gc_meta - gc_actual)
    delta_egr  <- round(camas_imp * delta_gc * fill_rate)
    ingr_x_egr <- median(data_grd_dedup$vf_win, na.rm = TRUE)
    ingr_pot   <- delta_egr * ingr_x_egr

    tagList(
      tags$div(
        class = "callout callout-warning",
        style = "background:#EAF7F0; border-left:4px solid #27AE60; padding:8px 12px; margin-bottom:10px;",
        tags$small(icon("info-circle"),
          HTML(paste0(
            " Simulación basada en <b>GC actual: ", round(gc_actual, 2), "</b>",
            " (último año disponible) y meta <b>GC = 10</b>.",
            " Camas imputadas: <b>", round(camas_imp), "</b> (egresos/mes ÷ GC/mes).",
            " Se asume <b>fill rate 75 %</b>: de la capacidad liberada,",
            " 3 de cada 4 camas-rotación se convierten en nuevas admisiones."
          ))
        )
      ),
      fluidRow(
        valueBox(
          value    = round(gc_actual, 2),
          subtitle = paste0("GC actual (últ. año) · Meta institucional: ", gc_meta),
          icon     = icon("bed"),
          color    = if (gc_actual >= gc_meta) "green"
                     else if (gc_actual >= 8)  "yellow" else "red",
          width    = 3
        ),
        valueBox(
          value    = format(round(camas_imp), big.mark = "."),
          subtitle = "Camas imputadas (egresos/mes ÷ GC/mes)",
          icon     = icon("hospital"),
          color    = "light-blue",
          width    = 3
        ),
        valueBox(
          value    = if (delta_egr > 0) paste0("+", format(delta_egr, big.mark = "."))
                     else "Meta alcanzada",
          subtitle = HTML("Egresos adicionales potenciales<br><small>(si GC = 10, fill rate 75 %)</small>"),
          icon     = icon("plus-circle"),
          color    = if (delta_egr > 0) "blue" else "green",
          width    = 3
        ),
        valueBox(
          value    = if (delta_egr > 0) cop(ingr_pot) else "—",
          subtitle = HTML("Ingreso potencial adicional<br><small>(egresos pot. × mediana GRD)</small>"),
          icon     = icon("coins"),
          color    = if (delta_egr > 0) "teal" else "green",
          width    = 3
        )
      )
    )
  })

  output$gc_plot_oportunidad <- renderPlotly({
    ingr_x_egr <- median(data_grd_dedup$vf_win, na.rm = TRUE)

    df <- data_kpi_wide %>%
      filter(!is.na(tasa_gc)) %>%
      arrange(fecha) %>%
      mutate(
        meta       = 10,
        gap        = pmax(0, meta - tasa_gc),
        camas_mes  = if_else(!is.na(egresos) & tasa_gc > 0,
                              egresos / tasa_gc, NA_real_),
        egr_pot    = round(camas_mes * gap * 0.75),
        ingr_pot_M = if_else(!is.na(egr_pot),
                              round(egr_pot * ingr_x_egr / 1e6, 1), NA_real_),
        tooltip_gc = paste0(
          format(fecha, "%b %Y"), "<br>GC: ", round(tasa_gc, 2),
          "<br>Brecha vs. meta 10: ", round(gap, 2),
          if_else(!is.na(egr_pot) & egr_pot > 0,
                  paste0("<br>Egresos pot.: +", egr_pot,
                         "<br>Ingreso pot.: $", ingr_pot_M, "M COP"),
                  "<br><i>Meta alcanzada</i>")
        ),
        zona = if_else(tasa_gc < meta, "Bajo meta", "Sobre meta")
      )

    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = fecha)) +
      geom_ribbon(data = df %>% filter(zona == "Bajo meta"),
                  aes(ymin = tasa_gc, ymax = meta), fill = "#E15759", alpha = 0.25) +
      geom_ribbon(data = df %>% filter(zona == "Sobre meta"),
                  aes(ymin = meta, ymax = tasa_gc), fill = "#59A14F", alpha = 0.2) +
      geom_line(aes(y = tasa_gc), color = "#2C3E50", linewidth = 1.1) +
      geom_point(aes(y = tasa_gc, color = factor(year_val), text = tooltip_gc), size = 2.5) +
      geom_hline(yintercept = 10, linetype = "dashed", color = "#E15759", linewidth = 0.9) +
      annotate("text", x = min(df$fecha, na.rm = TRUE), y = 10.25,
               label = "Meta: 10", hjust = 0, size = 3.5, color = "#E15759", fontface = "bold") +
      scale_color_manual(values = c("2024" = "#F28E2B", "2025" = "#59A14F", "2026" = "#4E79A7"),
                         name = "Año") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
      labs(x = NULL, y = "Giro de Cama institucional",
           caption = "Zona roja = GC por debajo de meta (oportunidad). Zona verde = meta alcanzada.") +
      theme_classic() +
      theme(axis.text.x    = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom",
            plot.caption    = element_text(size = 7, color = "grey50"))

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.35),
             margin = list(b = 70))
  })

  # ── Sub-tab 6: Ocupación ─────────────────────────────────────────────────
  ocu_serv_colors <- c(
    "Institucional" = "#2C3E50",
    "UCI"           = "#E15759",
    "UCIN"          = "#F28E2B",
    "HOSP"          = "#59A14F"
  )

  ocu_filt <- reactive({
    df <- data_ocupacion
    if (!is.null(input$ocu_yr) && input$ocu_yr != "0")
      df <- df %>% filter(year_val == as.integer(input$ocu_yr))
    df
  })

  output$ocu_kpi_cards <- renderUI({
    req(nrow(data_ocupacion) > 0)
    df <- ocu_filt() %>% filter(!is.na(ocupacion_pct))

    make_ocu_box <- function(serv, label) {
      val <- mean(df$ocupacion_pct[df$servicio == serv], na.rm = TRUE)
      col <- if (is.na(val))  "light-blue"
             else if (val > 100) "purple"
             else if (val >= 90) "red"
             else if (val >= 85) "yellow"
             else "green"
      valueBox(
        value    = if (is.na(val)) "—" else paste0(round(val, 1), "%"),
        subtitle = paste0(label, " | meta ≤90%"),
        icon     = icon("percent"),
        color    = col,
        width    = 3
      )
    }
    fluidRow(
      make_ocu_box("Institucional", "Ocupación Institucional"),
      make_ocu_box("UCI",           "Ocupación UCI"),
      make_ocu_box("UCIN",          "Ocupación UCIN"),
      make_ocu_box("HOSP",          "Ocupación HOSP")
    )
  })

  output$ocu_plot_trend <- renderPlotly({
    df <- ocu_filt() %>% filter(!is.na(ocupacion_pct))
    req(nrow(df) > 0)

    y_max <- max(max(df$ocupacion_pct, na.rm = TRUE) + 5, 110)

    p <- ggplot(df, aes(x = fecha, y = ocupacion_pct,
                        color = servicio, group = servicio,
                        text = paste0("<b>", servicio, "</b><br>",
                                      format(fecha, "%B %Y"), "<br>",
                                      "Ocupación: ", ocupacion_pct, "%<br>",
                                      "Días estancia: ", dias_tot, "<br>",
                                      "Camas×días disponibles: ", cap_total))) +
      geom_line(linewidth = 1) + geom_point(size = 2.2) +
      geom_hline(yintercept = 85, linetype = "dashed", color = "#F39C12", linewidth = 0.8) +
      geom_hline(yintercept = 90, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
      annotate("text", x = min(df$fecha), y = 85.8,
               label = "85% — Vigilancia", color = "#F39C12",
               hjust = 0, size = 2.8) +
      annotate("text", x = min(df$fecha), y = 90.8,
               label = "90% — Alerta (NICE)",  color = "#E74C3C",
               hjust = 0, size = 2.8) +
      scale_color_manual(values = ocu_serv_colors, na.value = "grey60") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_y_continuous(limits = c(NA, y_max),
                         labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "Tasa de ocupación (%)", color = "Servicio") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$ocu_plot_heatmap <- renderPlotly({
    meses_abr <- c("Ene","Feb","Mar","Abr","May","Jun",
                   "Jul","Ago","Sep","Oct","Nov","Dic")
    df <- data_ocupacion %>%
      filter(servicio == "Institucional", !is.na(ocupacion_pct)) %>%
      mutate(mes_lbl = factor(meses_abr[mes_num], levels = meses_abr))
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = mes_lbl, y = factor(year_val),
                        fill = ocupacion_pct,
                        text = paste0(format(fecha, "%B %Y"), "<br>",
                                      "Ocupación: ", ocupacion_pct, "%<br>",
                                      alerta))) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = paste0(ocupacion_pct, "%")),
                size = 3, fontface = "bold", color = "white") +
      scale_fill_gradient2(
        low      = "#27AE60",
        mid      = "#F39C12",
        high     = "#E74C3C",
        midpoint = 90,
        limits   = c(60, 110),
        oob      = scales::squish,
        name     = "Ocup. %"
      ) +
      labs(x = NULL, y = NULL, title = "Institucional") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(size = 10, face = "bold"))
    ggplotly(p, tooltip = "text")
  })

  output$ocu_alertas <- renderUI({
    df <- data_ocupacion %>%
      filter(servicio == "Institucional", !is.na(ocupacion_pct),
             ocupacion_pct > 100) %>%
      arrange(desc(ocupacion_pct))

    if (nrow(df) == 0) {
      return(tags$p(icon("check-circle"),
                    " No hay meses con sobreocupación institucional en el período seleccionado.",
                    style = "color:#27AE60; font-weight:bold; padding:8px;"))
    }

    tags$div(
      tags$p(icon("exclamation-triangle"),
             paste0(nrow(df), " meses con sobreocupación institucional (>100%):"),
             style = "font-weight:bold; color:#E74C3C; margin-bottom:6px;"),
      tags$ul(style = "margin:0; padding-left:20px;",
        lapply(seq_len(nrow(df)), function(i) {
          tags$li(HTML(paste0(
            "<b>", format(df$fecha[i], "%B %Y"), "</b>: ",
            "<span style='color:#E74C3C; font-weight:bold;'>", df$ocupacion_pct[i], "%</span> — ",
            df$dias_tot[i], " días-estancia / ",
            df$cap_total[i], " días-cama disponibles"
          )))
        })
      )
    )
  })

  output$ocu_tabla <- renderReactable({
    df <- data_ocupacion %>%
      filter(!is.na(ocupacion_pct)) %>%
      mutate(mes_lbl = format(fecha, "%b %Y")) %>%
      select(
        Servicio     = servicio,
        Año          = year_val,
        Mes          = mes_lbl,
        `Días estancia`          = dias_tot,
        Camas                    = camas_n,
        `Días del mes`           = dias_mes,
        `Días-cama disponibles`  = cap_total,
        `Ocupación (%)`          = ocupacion_pct,
        Semáforo                 = alerta
      ) %>%
      arrange(Servicio, Año, `Días-cama disponibles`)
    req(nrow(df) > 0)

    reactable(df,
      filterable = TRUE, searchable = TRUE, defaultPageSize = 24,
      columns = list(
        `Ocupación (%)` = colDef(
          style = function(value) {
            col <- if (is.na(value)) "#BDC3C7"
                   else if (value > 100) "#8E44AD"
                   else if (value >= 90) "#E74C3C"
                   else if (value >= 85) "#E67E22"
                   else "#27AE60"
            list(color = col, fontWeight = "bold")
          }
        ),
        Semáforo = colDef(
          cell = function(value) {
            ico <- switch(as.character(value),
              "Normal (<85%)"       = "🟢",
              "Vigilancia (85-90%)" = "🟡",
              "Alerta (90-100%)"    = "🔴",
              "Crítica (>100%)"     = "🟣",
              "—")
            paste(ico, as.character(value))
          }
        )
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 7 · Estancias Inactivas
  # ══════════════════════════════════════════════════════════════════════════
  # Selección de período. Se calcula UNA vez y la consumen los tres bloques de
  # la pestaña (registros, costo y CACI) para que no puedan desincronizarse.
  ei_sel <- reactive({
    list(
      yr  = if (is.null(input$ei_yr))  0L else as.integer(input$ei_yr),
      mes = if (is.null(input$ei_mes)) 0L else as.integer(input$ei_mes),
      rsp = if (is.null(input$ei_resp)) "0" else input$ei_resp
    )
  })

  # Aplica año / mes / responsable a cualquier tabla con esas columnas.
  ei_aplicar_filtro <- function(df, col_anio, col_mes, col_resp) {
    s <- ei_sel()
    if (s$yr  > 0) df <- df %>% filter(.data[[col_anio]] == s$yr)
    if (s$mes > 0) df <- df %>% filter(.data[[col_mes]]  == s$mes)
    if (s$rsp != "0") df <- df %>% filter(.data[[col_resp]] == s$rsp)
    df
  }

  ei_periodo_label <- reactive({
    s <- ei_sel()
    yr_txt <- if (s$yr > 0) as.character(s$yr) else "2024–2026"
    if (s$mes > 0) paste(meses_full[s$mes], yr_txt) else yr_txt
  })

  output$ei_periodo_txt <- renderUI({
    s <- ei_sel()
    rsp_txt <- if (s$rsp != "0") paste0(" · responsable: <b>", s$rsp, "</b>") else ""
    HTML(paste0("<p style='margin:0 0 8px 2px;color:#6c757d;font-size:13px;'>",
                "Período: <b>", ei_periodo_label(), "</b>", rsp_txt, "</p>"))
  })

  ei_filt <- reactive({
    ei_aplicar_filtro(data_ei, "año", "mes_n", "responsable")
  })

  # Un mes + un responsable concretos pueden no tener registros. Antes esto
  # dejaba recuadros en blanco sin explicación; ahora se dice explícitamente.
  ei_msg_vacio <- function() {
    tags$p(paste0("Sin registros de estancia inactiva para ", ei_periodo_label(),
                  if (ei_sel()$rsp != "0") paste0(" · ", ei_sel()$rsp) else "", "."),
           style = "color:#6c757d;margin:12px 4px;")
  }
  ei_plot_vacio <- function() {
    plotly_empty() %>%
      layout(title = list(text = paste0("Sin registros en ", ei_periodo_label()),
                          font = list(size = 12)))
  }

  output$kpi_ei <- renderUI({
    df <- ei_filt()
    if (nrow(df) == 0) return(ei_msg_vacio())
    n_con_grd  <- sum(!is.na(df$n_admisiones_grd))
    valor_tot  <- sum(df$valor_total_estancia_inactiva, na.rm = TRUE)
    dias_ips   <- sum(suppressWarnings(as.numeric(df$total_dias_de_estancia_por_ips)), na.rm = TRUE)
    dias_eps   <- sum(suppressWarnings(as.numeric(df$total_dias_de_estancia_por_eps)), na.rm = TRUE)

    fluidRow(
      valueBox(
        value    = format(nrow(df), big.mark = "."),
        subtitle = "Registros de estancias inactivas",
        icon     = icon("clipboard"),
        color    = "blue",
        width    = 2
      ),
      valueBox(
        value    = format(dias_ips, big.mark = "."),
        subtitle = "Días inactivos por IPS",
        icon     = icon("hospital"),
        color    = "red",
        width    = 2
      ),
      valueBox(
        value    = format(dias_eps, big.mark = "."),
        subtitle = "Días inactivos por EPS",
        icon     = icon("file-alt"),
        color    = "orange",
        width    = 2
      ),
      valueBox(
        value    = cop(valor_tot),
        subtitle = "Valor total estancia inactiva",
        icon     = icon("dollar-sign"),
        color    = "yellow",
        width    = 3
      ),
      valueBox(
        value    = format(n_con_grd, big.mark = "."),
        subtitle = "Pacientes cruzados con GRD",
        icon     = icon("database"),
        color    = "green",
        width    = 3
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # COSTO DE ESTANCIA INACTIVA (día-cama SOAT 2026) — módulo aditivo
  # No reutiliza ni altera costo_filt(); parte de ei_cost, calculado aparte.
  # ══════════════════════════════════════════════════════════════════════════
  # Bloque de agregados según la base elegida (costo DIME | tarifa facturada).
  # Ambas se precalculan en inactive_stay_cost.R; aquí sólo se conmuta.
  # Nombre de la columna de tarifa según la base elegida.
  ei_rate_col <- reactive({
    b <- if (is.null(input$ei_base)) "costo" else input$ei_base
    if (b == "costo") "costo_dime" else "tarifa_fact"
  })

  # Detalle fila a fila YA FILTRADO por período y responsable.
  ei_det <- reactive({
    req(!is.null(ei_cost))
    ei_aplicar_filtro(ei_cost$detalle, "anio", "mes_n", "responsable")
  })

  # Agregados recalculados sobre el período elegido. Antes esto devolvía el
  # objeto precalculado ei_cost[[b]], que cubre SIEMPRE 2024-2026: por eso
  # "Impacto por EAPB" y las dos gráficas de causas mostraban el acumulado
  # histórico aunque el usuario hubiera elegido un año.
  ei_base <- reactive({
    d <- ei_det(); req(nrow(d) > 0)
    ei_build_aggs(d, ei_rate_col())
  })

  # Serie mensual para el gráfico de evolución: respeta año y responsable, pero
  # NO el mes — una serie temporal de un solo punto no es una serie. El mes
  # elegido se resalta en el gráfico.
  ei_costo_mensual <- reactive({
    req(!is.null(ei_cost))
    s <- ei_sel()
    d <- ei_cost$detalle
    if (s$yr  > 0)    d <- d %>% filter(anio == s$yr)
    if (s$rsp != "0") d <- d %>% filter(responsable == s$rsp)
    req(nrow(d) > 0)
    ei_build_aggs(d, ei_rate_col())$mensual
  })

  output$kpi_ei_costo <- renderUI({
    if (is.null(ei_cost))
      return(tags$p("Módulo de costo no disponible (falta el libro de estancias inactivas).",
                    style = "color:#6c757d;"))
    if (nrow(ei_det()) == 0) return(ei_msg_vacio())
    # Usa el agregado del PERÍODO elegido (año + mes + responsable), no la serie
    # completa: los indicadores deben cuadrar con las tablas de abajo.
    m <- ei_base()$mensual; req(nrow(m) > 0)
    # Mediana ENTRE MESES: la serie es corta y un mes atípico desplaza la media.
    # Con un solo mes seleccionado la mediana ES el valor de ese mes.
    un_mes  <- nrow(m) == 1
    pref    <- if (un_mes) "Costo del mes — " else "Mediana mensual — "
    med_ips <- median(m$costo_ips,   na.rm = TRUE)
    med_eps <- median(m$costo_eps,   na.rm = TRUE)
    med_tot <- median(m$costo_total, na.rm = TRUE)
    fluidRow(
      valueBox(cop(med_ips), paste0(pref, "IPS (intervenible)"),
               icon = icon("hospital"), color = "red",    width = 3),
      valueBox(cop(med_eps), paste0(pref, "EAPB"),
               icon = icon("building-columns"), color = "yellow", width = 3),
      valueBox(cop(med_tot), paste0(pref, "total"),
               icon = icon("coins"), color = "orange", width = 3),
      # 4º indicador: días, NO el acumulado en pesos. El acumulado ya está en
      # Giro Cama → Impacto Económico (base KPI institucional, 36 meses) y
      # mostrar aquí otro total en pesos con base distinta sólo genera dudas.
      valueBox(format(sum(m$dias_ips, na.rm = TRUE), big.mark = "."),
               paste0("Días IPS intervenibles (",
                      if (un_mes) ei_periodo_label() else paste(nrow(m), "meses"), ")"),
               icon = icon("calendar-xmark"), color = "maroon", width = 3)
    )
  })

  output$ei_costo_nota <- renderUI({
    req(!is.null(ei_cost))
    b   <- if (is.null(input$ei_base)) "costo" else input$ei_base
    cb  <- ei_cost$cobertura
    pct <- function(o) { v <- cb$pct[cb$origen == o]; if (length(v)) v else 0 }
    base_txt <- if (b == "costo")
      paste0("<b>Costo DIME</b> (tabla CUPS): UCI $946.000 · UCIN $599.000 · ",
             "Hospitalización $300.000. Mide el <b>recurso consumido sin ",
             "producción</b> — el argumento de gestión interna.")
    else
      paste0("<b>Tarifa facturada</b> (ventas reales por EAPB): mide el ",
             "<b>ingreso dejado de percibir</b>. Útil para negociar con el pagador. ",
             "Ojo: en hospitalización la tarifa ($176.000) está <i>por debajo</i> ",
             "del costo ($300.000), así que esta base subestima el impacto interno.")
    HTML(paste0(
      "Indicadores, tabla por EAPB y gráficas de causas calculados sobre el ",
      "período seleccionado (<b>", ei_periodo_label(), "</b>). Las dos series ",
      "mensuales conservan los 12 meses del año y resaltan el mes elegido.<br>",
      base_txt,
      "<br>Cada día inactivo se valoriza con la tarifa de <b>su servicio y su ",
      "pagador</b>: el servicio sale de cruzar el censo por documento + mes ",
      "(servicio con más días). Cobertura: ", pct("EAPB × servicio"),
      " % EAPB×servicio · ", pct("mediana del servicio"), " % mediana del servicio · ",
      pct("sin servicio → SOAT"), " % respaldo SOAT.<br>",
      "Es <b>costo de oportunidad de la cama</b>, no gasto facturable: un día ",
      "inactivo no genera medicamentos ni procedimientos, y <i>no</i> se suma a los ",
      "costos itemizados de las demás pestañas. El desglose del día-cama está en ",
      "<b>Valor de Cama</b>; el acumulado institucional con base KPI, en ",
      "<b>Giro Cama → Impacto Económico</b> (36 meses, base distinta — no sumar)."
    ))
  })

  output$plot_ei_costo_mes <- renderPlotly({
    req(!is.null(ei_cost)); m <- ei_costo_mensual(); req(nrow(m) > 0)
    mes_sel <- ei_sel()$mes
    df <- m %>%
      select(periodo, IPS = costo_ips, EAPB = costo_eps) %>%
      tidyr::pivot_longer(-periodo, names_to = "resp", values_to = "costo") %>%
      mutate(resp = factor(resp, levels = c("EAPB", "IPS")),
             # Con un mes elegido se conserva la serie del año y se atenúan los
             # demás meses: una barra sola no deja ver la tendencia.
             foco = mes_sel == 0 | lubridate::month(periodo) == mes_sel)
    p <- ggplot(df, aes(periodo, costo / 1e6, fill = resp, alpha = foco,
                        text = paste0(format(periodo, "%Y-%m"), "<br>", resp, ": ",
                                      cop(costo)))) +
      geom_col() +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25), guide = "none") +
      scale_fill_manual(values = c("IPS" = "#E15759", "EAPB" = "#F1CE63"), name = NULL) +
      labs(x = NULL, y = "Millones COP") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$tabla_ei_costo_eapb <- renderReactable({
    req(!is.null(ei_cost))
    if (nrow(ei_det()) == 0)
      return(reactable(tibble(Mensaje = paste0("Sin registros en ", ei_periodo_label())),
                       compact = TRUE))
    # Se omiten las columnas de días: el costo es proporcional a ellos y con 7
    # columnas en un box de ancho 5 los valores en pesos quedaban truncados.
    # Los días siguen disponibles en el detalle por paciente.
    d <- ei_base()$por_eapb %>%
      transmute(EAPB = eapb, Casos = casos,
                IPS = costo_ips / 1e6, EAPB_c = costo_eps / 1e6,
                Total = costo_total / 1e6) %>%
      head(15)
    mm <- colDef(format = colFormat(suffix = " M", separators = TRUE, digits = 1),
                 align = "right", minWidth = 70)
    reactable(
      d, compact = TRUE, striped = TRUE, defaultPageSize = 10,
      defaultSorted = list(Total = "desc"),
      columns = list(
        EAPB   = colDef(minWidth = 110),
        Casos  = colDef(minWidth = 55, align = "right"),
        IPS    = mm,
        EAPB_c = colDef(name = "EAPB $", format = colFormat(suffix = " M", separators = TRUE,
                                                            digits = 1),
                        align = "right", minWidth = 70),
        Total  = mm
      )
    )
  })

  # Gráfico de causas, parametrizado por responsable
  ei_plot_causa <- function(tbl, fill_col) {
    # Con un mes o un responsable concreto puede no haber causas registradas:
    # mejor decirlo que dejar el recuadro en blanco.
    if (is.null(tbl) || nrow(tbl) == 0)
      return(plotly_empty() %>%
               layout(title = list(text = paste0("Sin causas registradas en ",
                                                 ei_periodo_label()),
                                   font = list(size = 12))))
    df <- tbl %>%
      mutate(causa = stringr::str_trunc(causa, 46)) %>%
      slice_max(costo, n = 8) %>%
      mutate(causa = forcats::fct_reorder(causa, costo))
    p <- ggplot(df, aes(costo / 1e6, causa,
                        text = paste0(causa, "<br>", cop(costo), "<br>", dias, " días"))) +
      geom_col(fill = fill_col) +
      labs(x = "Millones COP", y = NULL) +
      theme_minimal(base_size = 10)
    ggplotly(p, tooltip = "text")
  }

  output$plot_ei_causa_ips <- renderPlotly({
    req(!is.null(ei_cost))
    if (nrow(ei_det()) == 0) return(ei_plot_vacio())
    ei_plot_causa(ei_base()$por_causa_ips, "#E15759")
  })
  output$plot_ei_causa_eps <- renderPlotly({
    req(!is.null(ei_cost))
    if (nrow(ei_det()) == 0) return(ei_plot_vacio())
    ei_plot_causa(ei_base()$por_causa_eps, "#4E79A7")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # VALOR DE CAMA
  # ══════════════════════════════════════════════════════════════════════════
  output$bv_intro <- renderUI({
    HTML(paste0(
      "<p style='margin-bottom:6px'>La pestaña <b>Est. Inactivas</b> cuantifica ",
      "<i>cuántos días</i> de cama se pierden y <i>de quién</i> es la causa. ",
      "Ésta responde la pregunta siguiente: <b>cuánto vale ese día de cama</b>, ",
      "y por qué ese valor cambia según el servicio, el pagador y la patología.</p>",
      "<p style='margin-bottom:0'>No repite indicadores de estancia inactiva. ",
      "Aquí se comparan tres bases distintas —lo que <b>cuesta</b>, lo que se ",
      "<b>factura</b> y lo que dice el <b>tarifario</b>— que hasta ahora se usaban ",
      "de forma intercambiable sin explicitar que no son lo mismo.</p>"
    ))
  })

  output$bv_tabla_comparativo <- renderReactable({
    req(!is.null(bed_value))
    d <- bed_value$comparativo %>%
      transmute(Servicio = servicio_cama, Días = n,
                `Costo DIME` = costo_dime,
                `Tarifa facturada` = tarifa_med,
                `IQR tarifa` = paste0(format(round(tarifa_p25), big.mark = "."), " – ",
                                      format(round(tarifa_p75), big.mark = ".")),
                `SOAT 2026` = soat_dia,
                `Margen %` = margen_pct,
                `Tarifa/SOAT %` = tarifa_vs_soat)
    money <- colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0))
    reactable(
      d, compact = TRUE, striped = TRUE, sortable = TRUE, defaultPageSize = 5,
      columns = list(
        `Costo DIME` = money, `Tarifa facturada` = money, `SOAT 2026` = money,
        Días = colDef(format = colFormat(separators = TRUE)),
        `Margen %` = colDef(
          cell = function(v) paste0(if (!is.na(v) && v > 0) "+" else "", v, " %"),
          style = function(v) if (!is.na(v) && v < 0)
            list(color = "#B00020", fontWeight = "bold") else list(color = "#1B7F3B")),
        `Tarifa/SOAT %` = colDef(cell = function(v) paste0(v, " %"))
      )
    )
  })

  output$bv_nota_bases <- renderUI({
    HTML(paste0(
      "<b>Margen %</b> = (tarifa facturada − costo DIME) / costo DIME. En rojo, ",
      "los servicios donde la cama se factura <b>por debajo</b> de lo que cuesta. ",
      "<b>Tarifa/SOAT %</b> sitúa lo facturado frente al referente normativo."
    ))
  })

  output$bv_plot_bases <- renderPlotly({
    req(!is.null(bed_value))
    df <- bed_value$comparativo %>%
      select(servicio_cama, `Costo DIME` = costo_dime,
             `Tarifa facturada` = tarifa_med, `SOAT 2026` = soat_dia) %>%
      tidyr::pivot_longer(-servicio_cama, names_to = "base", values_to = "valor") %>%
      mutate(base = factor(base, levels = c("Costo DIME", "Tarifa facturada", "SOAT 2026")))
    p <- ggplot(df, aes(servicio_cama, valor / 1000, fill = base,
                        text = paste0(servicio_cama, "<br>", base, ": ", cop(valor)))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = c("Costo DIME" = "#E15759",
                                   "Tarifa facturada" = "#4E79A7",
                                   "SOAT 2026" = "#B0B0B0"), name = NULL) +
      labs(x = NULL, y = "Miles COP / día") +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$bv_plot_margen <- renderPlotly({
    req(!is.null(bed_value))
    df <- bed_value$comparativo %>%
      mutate(servicio_cama = forcats::fct_reorder(servicio_cama, margen_pct))
    p <- ggplot(df, aes(margen_pct, servicio_cama,
                        fill = margen_pct < 0,
                        text = paste0(servicio_cama, "<br>margen ", margen_pct, " %",
                                      "<br>tarifa ", cop(tarifa_med),
                                      " vs costo ", cop(costo_dime)))) +
      geom_col(width = 0.6) +
      geom_vline(xintercept = 0, linewidth = 0.4) +
      scale_fill_manual(values = c("TRUE" = "#E15759", "FALSE" = "#59A14F"), guide = "none") +
      labs(x = "Margen sobre costo (%)", y = NULL) +
      theme_minimal(base_size = 11)
    # ggplotly reintroduce la leyenda pese a guide="none" -> suprimir en layout
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  bv_eapb_filt <- reactive({
    req(!is.null(bed_value))
    bed_value$brecha_eapb %>% filter(servicio_cama == input$bv_serv)
  })

  output$bv_plot_eapb <- renderPlotly({
    df <- bv_eapb_filt(); req(nrow(df) > 0)
    costo <- df$costo_dime[1]
    df <- df %>% mutate(eapb_s = forcats::fct_reorder(stringr::str_trunc(eapb, 30), tarifa_med))
    p <- ggplot(df, aes(tarifa_med / 1000, eapb_s, fill = tarifa_med < costo,
                        text = paste0(eapb, "<br>tarifa ", cop(tarifa_med),
                                      "<br>n = ", format(n, big.mark = "."),
                                      " días<br>margen ", margen_pct, " %"))) +
      geom_col(width = 0.65) +
      geom_vline(xintercept = costo / 1000, linetype = "dashed", colour = "#B00020") +
      scale_fill_manual(values = c("TRUE" = "#E15759", "FALSE" = "#59A14F"), guide = "none") +
      labs(x = "Miles COP / día  (línea roja = costo DIME)", y = NULL) +
      theme_minimal(base_size = 10)
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  output$bv_tabla_eapb <- renderReactable({
    df <- bv_eapb_filt(); req(nrow(df) > 0)
    d <- df %>%
      transmute(EAPB = eapb, Días = n, `Tarifa` = tarifa_med,
                `Costo DIME` = costo_dime, `Margen %` = margen_pct) %>%
      arrange(`Margen %`)
    money <- colDef(format = colFormat(prefix = "$", separators = TRUE, digits = 0))
    reactable(d, compact = TRUE, striped = TRUE, defaultPageSize = 8,
      columns = list(
        Tarifa = money, `Costo DIME` = money,
        Días = colDef(format = colFormat(separators = TRUE)),
        `Margen %` = colDef(
          cell = function(v) paste0(if (!is.na(v) && v > 0) "+" else "", v, " %"),
          style = function(v) if (!is.na(v) && v < 0)
            list(color = "#B00020", fontWeight = "bold") else list(color = "#1B7F3B"))))
  })

  output$bv_plot_caci <- renderPlotly({
    req(!is.null(bed_value))
    df <- bed_value$caci_comp %>% filter(!is.na(caci))
    req(nrow(df) > 0)
    p <- ggplot(df, aes(caci, tarifa_med / 1000, fill = servicio_cama,
                        text = paste0(caci, " · ", servicio_cama,
                                      "<br>tarifa ", cop(tarifa_med),
                                      "<br>n = ", format(n, big.mark = "."), " días"))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      scale_fill_manual(values = c("UCI" = "#E15759",
                                   "UCIN (intermedio)" = "#F28E2B",
                                   "Hospitalización" = "#4E79A7"), name = NULL) +
      labs(x = NULL, y = "Miles COP / día") +
      theme_minimal(base_size = 11) + theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$bv_metodologia <- renderUI({
    req(!is.null(bed_value))
    ei_nota <- if (!is.null(ei_cost))
      paste0("La pestaña <b>Est. Inactivas</b> valoriza cada día perdido con la ",
             "tarifa de <b>su servicio y su pagador</b>, usando estas mismas cifras, ",
             "y deja elegir entre las dos bases: costo DIME (mediana mensual IPS ",
             cop(ei_cost$costo$resumen$mediana_mes_ips), ") o tarifa facturada (",
             cop(ei_cost$tarifa$resumen$mediana_mes_ips), "). No son intercambiables.")
      else ""
    HTML(paste0(
      "<ul style='margin-bottom:6px'>",
      "<li><b>Costo DIME</b>: tabla CUPS de planeación <code>costo_general_2026.xlsx</code>. ",
      "Códigos 110A01 (UCI), 107M01 (intermedio), 129A02/10A002 (hospitalización).</li>",
      "<li><b>Tarifa facturada</b>: mediana de <code>valor_cargo_tarifario</code> de los ",
      "cargos de internación efectivamente facturados. Se usa mediana porque la ",
      "distribución tiene valores extremos por contratos atípicos.</li>",
      "<li><b>SOAT 2026</b>: Manual Tarifario, Circular Externa 047 de 2025, UVB $12.110. ",
      "Institución de tercer nivel.</li></ul>",
      "<p style='margin-bottom:6px'><b>Limitación conocida:</b> la columna <code>costo</code> ",
      "del dataset de ventas devuelve $0 en los cargos de hospitalización y difiere de CUPS ",
      "en UCI ($773.000 vs $946.000). Por eso el costo se toma de la tabla CUPS y no de ese ",
      "campo. Conviene depurar esos registros en origen.</p>",
      "<p style='margin-bottom:0'><b>Relación con otras pestañas:</b> ", ei_nota,
      " En <b>Giro Cama → Impacto Económico</b> el costo de estancia inactiva proviene del ",
      "archivo KPI institucional y cubre 36 meses; en <b>Est. Inactivas</b> se recalcula ",
      "desde el libro de detalle (29 meses con fecha válida). Los totales no coinciden ",
      "porque las series y las bases son distintas — no son un error, pero no deben sumarse.</p>"
    ))
  })

  output$plot_causas_ei <- renderPlotly({
    df <- ei_filt() %>%
      filter(!is.na(causa_principal)) %>%
      group_by(causa_principal, responsable) %>%
      summarise(casos = n(),
                dias  = sum(total_dias_de_estancia_por_ips, na.rm = TRUE),
                .groups = "drop") %>%
      group_by(causa_principal) %>%
      mutate(dias_tot = sum(dias)) %>%
      ungroup() %>%
      arrange(desc(dias_tot)) %>%
      slice_max(order_by = dias_tot, n = 30, with_ties = TRUE)
    if (nrow(df) == 0) return(ei_plot_vacio())

    resp_cols <- c("IPS" = "#E15759", "EPS" = "#F28E2B",
                   "Paciente" = "#4E79A7", "No clasificado" = "grey60")

    p <- ggplot(df,
                aes(x = reorder(str_wrap(causa_principal, 38), dias_tot),
                    y = dias, fill = responsable,
                    text = paste0("<b>", causa_principal, "</b><br>",
                                  responsable, ": ", dias, " días (", casos, " casos)"))) +
      geom_col(position = "stack", color = "black", linewidth = 0.15) +
      coord_flip() +
      scale_fill_manual(values = resp_cols, na.value = "grey80") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Total días inactivos", fill = "Responsable") +
      theme_classic() +
      theme(axis.text.y = element_text(size = 7), legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.15))
  })

  output$plot_trend_ei <- renderPlotly({
    # Igual que el gráfico de costo mensual: respeta año y responsable pero
    # conserva los 12 meses, resaltando el mes elegido. Filtrar la serie a un
    # solo mes la reduce a una barra y borra justamente la tendencia.
    s <- ei_sel()
    base_df <- data_ei
    if (s$yr  > 0)    base_df <- base_df %>% filter(año == s$yr)
    if (s$rsp != "0") base_df <- base_df %>% filter(responsable == s$rsp)

    df_long <- base_df %>%
      group_by(año, mes_n) %>%
      summarise(
        registros = n(),
        dias_ips  = sum(suppressWarnings(as.numeric(total_dias_de_estancia_por_ips)), na.rm = TRUE),
        dias_eps  = sum(suppressWarnings(as.numeric(total_dias_de_estancia_por_eps)), na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(fecha = as.Date(paste0(año, "-", sprintf("%02d", mes_n), "-01"))) %>%
      arrange(fecha) %>%
      tidyr::pivot_longer(c(dias_ips, dias_eps),
                          names_to  = "tipo",
                          values_to = "dias") %>%
      mutate(tipo = if_else(tipo == "dias_ips", "Días IPS", "Días EPS"),
             foco = s$mes == 0 | mes_n == s$mes)

    req(nrow(df_long) > 0)

    df_reg <- df_long %>% distinct(fecha, registros)
    max_dias <- max(df_long$dias, na.rm = TRUE)
    max_reg  <- max(df_reg$registros, na.rm = TRUE)
    escala   <- if (max_reg > 0) max_dias / max_reg else 1

    p <- ggplot(df_long, aes(x = fecha)) +
      geom_col(aes(y = dias, fill = tipo, alpha = foco,
                   text = paste0(format(fecha, "%b %Y"), "<br>",
                                 tipo, ": ", dias, " días")),
               position = "stack") +
      scale_alpha_manual(values = c(`FALSE` = 0.25, `TRUE` = 0.85), guide = "none") +
      geom_line(data = df_reg,
                aes(y = registros * escala,
                    color = "Registros de estancias inactivas"),
                linewidth = 1.2, group = 1) +
      geom_point(data = df_reg,
                 aes(y = registros * escala,
                     color = "Registros de estancias inactivas",
                     text = paste0(format(fecha, "%b %Y"),
                                   "<br>Registros: ", registros,
                                   " (filas en la BD de estancias inactivas)")),
                 size = 2.5) +
      scale_y_continuous(
        name     = "Total días inactivos (IPS + EPS)",
        breaks   = scales::pretty_breaks(n = 6),
        sec.axis = sec_axis(~ . / escala, name = "N° registros de estancias inactivas")
      ) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_fill_manual(values  = c("Días IPS" = "#E15759", "Días EPS" = "#F28E2B")) +
      scale_color_manual(values = c("Registros de estancias inactivas" = "#2C3E50")) +
      labs(x = NULL, fill = NULL, color = NULL) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.35))
  })

  output$plot_ei_caci <- renderPlotly({
    df <- ei_filt() %>%
      filter(!is.na(caci_ei), caci_ei != "") %>%
      tidyr::separate_rows(caci_ei, sep = ", ") %>%
      group_by(CACI = caci_ei, Responsable = responsable) %>%
      summarise(Casos = n(),
                `Días IPS` = sum(total_dias_de_estancia_por_ips, na.rm = TRUE),
                .groups = "drop")

    if (nrow(df) == 0) {
      return(plotly_empty() %>%
        layout(title = "Sin cruce GRD — pacientes no identificados en admisiones"))
    }

    resp_cols <- c("IPS" = "#E15759", "EPS" = "#F28E2B",
                   "Paciente" = "#4E79A7", "No clasificado" = "grey60")

    p <- ggplot(df, aes(x = reorder(CACI, Casos), y = Casos, fill = Responsable,
                        text = paste0("<b>", CACI, "</b><br>",
                                      Responsable, ": ", Casos, " casos · ",
                                      `Días IPS`, " días IPS"))) +
      geom_col(position = "stack", color = "black", linewidth = 0.2) +
      coord_flip() +
      scale_fill_manual(values = resp_cols, na.value = "grey80") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(x = NULL, y = "Casos con estancia inactiva", fill = "Responsable") +
      theme_classic() + theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$tabla_ei_eps <- renderReactable({
    ei_eps <- ei_filt() %>%
      group_by(EPS = eps) %>%
      summarise(
        `Casos EI`  = n(),
        `Días IPS`  = sum(total_dias_de_estancia_por_ips,  na.rm = TRUE),
        `Días EPS`  = sum(total_dias_de_estancia_por_eps,  na.rm = TRUE),
        `Valor EI`  = round(sum(valor_total_estancia_inactiva, na.rm = TRUE), 0),
        `% por IPS` = round(mean(responsable == "IPS", na.rm = TRUE) * 100, 1),
        .groups = "drop"
      )

    # El denominador debe cubrir el MISMO período que el numerador: comparar los
    # casos de un mes contra las admisiones de 2024-2026 inflaba "EI / Adm. (%)"
    # hacia cero. El filtro de Responsable no aplica aquí (es un atributo de la
    # estancia inactiva, no de la admisión).
    s <- ei_sel()
    grd_p <- data_grd_base %>%
      mutate(mes_adm = month(coalesce(fecha_ingreso, fecha_de_egreso)))
    if (s$yr  > 0) grd_p <- grd_p %>% filter(año == s$yr)
    if (s$mes > 0) grd_p <- grd_p %>% filter(mes_adm == s$mes)

    grd_eps <- grd_p %>%
      group_by(EPS = str_to_upper(coalesce(eps, "?"))) %>%
      summarise(`Admisiones GRD` = n(),
                `LOS med. GRD`   = round(mean(dif_days, na.rm = TRUE), 1),
                .groups = "drop")

    df <- left_join(ei_eps, grd_eps, by = "EPS") %>%
      mutate(`EI / Adm. (%)` = round(`Casos EI` / pmax(`Admisiones GRD`, 1) * 100, 1)) %>%
      arrange(desc(`Días IPS`))
    if (nrow(df) == 0)
      return(reactable(tibble(Mensaje = paste0("Sin registros en ", ei_periodo_label())),
                       compact = TRUE))

    reactable(df,
      searchable = TRUE, pagination = TRUE, defaultPageSize = 15,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 80),
      columns = list(
        EPS              = colDef(minWidth = 150, align = "left", sticky = "left"),
        `Casos EI`       = colDef(format = colFormat(separators = TRUE)),
        `Días IPS`       = colDef(format = colFormat(separators = TRUE)),
        `Días EPS`       = colDef(format = colFormat(separators = TRUE)),
        `Valor EI`       = colDef(minWidth = 120, align = "right",
                                  format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        `% por IPS`      = colDef(format = colFormat(suffix = "%", digits = 1)),
        `Admisiones GRD` = colDef(format = colFormat(separators = TRUE)),
        `LOS med. GRD`   = colDef(format = colFormat(digits = 1)),
        `EI / Adm. (%)`  = colDef(format = colFormat(suffix = "%", digits = 1),
                                   style = function(v) {
                                     if (!is.na(v) && v > 20) list(color = "#E15759", fontWeight = "bold")
                                     else if (!is.na(v) && v > 10) list(color = "#F28E2B")
                                     else list()
                                   })
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$tabla_ei_detail <- renderDT({
    df <- ei_filt() %>%
      select(
        Identificación    = identificacion,
        Paciente          = paciente,
        EPS               = eps,
        Clasificación     = clasificacion,
        Responsable       = responsable,
        Mes               = mes_label,
        Año               = año,
        `Causa 1 (IPS)`   = causa_1_de_estancia_inactiva_por_ips,
        `Causa 2 (IPS)`   = causa_2_de_estancia_inactiva_por_ips,
        `Causa 1 (EPS)`   = causa_1_de_estancia_inactiva_por_eps,
        `Días IPS`        = total_dias_de_estancia_por_ips,
        `Días EPS`        = total_dias_de_estancia_por_eps,
        `Días paciente`   = total_dias_de_estancia_por_paciente,
        `Total (EPS+pac)` = total_estancias_eps_y_paciente,
        `Valor estancia`  = valor_total_estancia_inactiva,
        `CACI (GRD)`      = caci_ei,
        `Admis. GRD`      = n_admisiones_grd,
        `LOS total GRD`   = los_total_grd,
        `Factura GRD`     = fact_total_grd,
        `Diagnóstico GRD` = diag_ei
      )

    DT::datatable(df,
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Blfrtip", buttons = c("copy", "csv", "excel"),
        scrollX = TRUE, scroller = TRUE, scrollY = "400px", pageLength = 25,
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")
      ),
      class = "cell-border stripe compact", rownames = FALSE
    ) %>%
      DT::formatCurrency(c("Valor estancia", "Factura GRD"),
                         currency = "$", digits = 0, mark = ".", dec.mark = ",", before = TRUE) %>%
      DT::formatRound(c("Días IPS", "Días EPS", "Días paciente",
                        "Total (EPS+pac)", "LOS total GRD"), digits = 1)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 8 · Datos
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_los_dt <- renderDT({
    df <- los_flagged() %>%
      select(
        Cuenta             = cuenta,
        Paciente           = paciente,
        Servicio           = estacion_2,
        Año                = year,
        Mes                = month,
        `LOS servicio (d)` = dif_bed_serv,
        `LOS total (d)`    = dif_days_total,
        CACI               = caci,
        Diagnóstico        = diag_egreso,
        Edad               = edad_grd,
        Sexo               = sexo,
        EPS                = eps,
        `Estado alta`      = estado_al_alta,
        `Larga estancia`   = larga_estancia,
        Umbral             = umbral
      ) %>%
      mutate(`Larga estancia` = if_else(`Larga estancia`, "Sí", "No"))

    DT::datatable(df,
      filter = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom = "Blfrtip", buttons = c("copy", "csv", "excel"),
        scrollX = TRUE, scroller = TRUE, scrollY = "500px", pageLength = 30,
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")
      ),
      class = "cell-border stripe compact", rownames = FALSE
    ) %>%
      DT::formatRound(c("LOS servicio (d)", "LOS total (d)", "Umbral"), digits = 2)
  })
}

shinyApp(ui, server)
