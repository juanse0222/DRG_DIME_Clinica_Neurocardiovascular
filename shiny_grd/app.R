################################################################################
# app.R — GRD Dashboard · DIME Clínica Neurocardiovascular
# 10 tabs: Resumen · Costos · Ticket · Tendencias · Rentabilidad ·
#          Histórico · Resumen anual · Por unidad · Epidemiología · Datos
################################################################################

source("global.R")

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- dashboardPage(
  title = "Análisis Grupos Relacionados de Diagnóstico y Caracterización de Pacientes — DIME Clínica Neurocardiovascular",
  skin = "blue",

  dashboardHeader(
    title = "Análisis GRD",
    titleWidth = 240,
    tags$li(class = "dropdown header-center-title-item",
      tags$span(class = "header-center-title",
        "Análisis Grupos Relacionados de Diagnóstico y Caracterización de Pacientes",
        tags$span(class = "header-title-sub", "DIME Clínica Neurocardiovascular")
      )
    ),
    tags$li(class = "dropdown",
      tags$a(style = "padding-top:8px; padding-bottom:8px; display:block;",
        tags$img(src = "logo.png", height = "34px")
      )
    )
  ),

  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem("Resumen",       tabName = "resumen",       icon = icon("chart-line")),
      menuItem("Costos",        tabName = "costos",        icon = icon("dollar-sign")),
      menuItem("Ticket",        tabName = "ticket",        icon = icon("receipt")),
      menuItem("Tendencias",    tabName = "tendencias",    icon = icon("arrow-up")),
      menuItem("Rentabilidad",  tabName = "rentabilidad",  icon = icon("dollar-sign")),
      menuItem("Histórico",     tabName = "historico",     icon = icon("history")),
      menuItem("Anual",         tabName = "anual",         icon = icon("calendar")),
      menuItem("Por unidad",    tabName = "por_unidad",    icon = icon("hospital")),
      menuItem("Epidemiología", tabName = "epidemiologia", icon = icon("stethoscope")),
      menuItem("Perfil de pacientes", tabName = "perfil_pac", icon = icon("user-injured")),
      menuItem("Mapa",                tabName = "mapa_pac",   icon = icon("map-marked-alt")),
      menuItem("Datos",         tabName = "datos",         icon = icon("table"))
    ),
    tags$hr(style = "border-color:rgba(255,255,255,.2); margin:8px 0;"),
    tags$div(
      style = "padding: 0 14px;",
      selectInput("yr", "Año de análisis",
                  choices = year_choices, selected = max(year_choices)),
      selectInput("yr_desde", "Histórico desde",
                  choices = rev(year_choices), selected = min(year_choices)),
      selectInput("mon", "Mes",
                  choices = mes_choices, selected = "0"),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      tags$label("CACI incluidos",
                 style = "color:rgba(255,255,255,.8); font-weight:600; font-size:.85rem;"),
      checkboxGroupInput("caci_sel", NULL,
                         choices = caci_choices, selected = caci_choices),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      actionButton("btn_update", "Actualizar",
                   icon = icon("sync"), class = "btn-primary btn-block"),
      br(),
      tags$small(
        tags$em(
          "Datos hasta: ", tags$strong(textOutput("ultimo_mes", inline = TRUE)), br(),
          "Últ. carga: ", textOutput("last_update", inline = TRUE)
        ),
        style = "color:rgba(255,255,255,.6);"
      )
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
        uiOutput("kpi_boxes"),
        br(),
        fluidRow(
          box(title      = tagList(icon("exclamation-triangle"),
                                   " Alertas ejecutivas por CACI"),
              solidHeader = TRUE, status = "primary", width = 5,
              reactableOutput("tabla_alertas", height = "280px")),
          box(title      = tagList(icon("users"),
                                   " Pacientes por CACI — evolución mensual"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("plot_resumen_pte", height = "280px"))
        ),
        fluidRow(
          box(title      = tagList(icon("dot-circle"),
                                   " Costo mediano y volumen por CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_resumen_bubble", height = "320px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 2 · Costos
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "costos",
        fluidRow(
          box(title      = tagList(icon("table"),
                                   " Costo por paciente según CACI y mes"),
              solidHeader = TRUE, status = "primary", width = 12,
              reactableOutput("tabla_costo_medio"))
        ),
        br(),
        fluidRow(
          box(title      = tagList(icon("chart-bar"),
                                   " Distribución de costos por paciente y CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_boxplot_caci", height = "500px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 3 · Ticket General
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "ticket",
        uiOutput("ticket_kpi_boxes"),
        br(),
        fluidRow(
          box(title      = tagList(icon("chart-line"), " Ticket mediana por CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_ticket_caci", height = "380px")),
          box(title      = tagList(icon("history"),
                                   " Comparativo ticket general por año"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_ticket_historico", height = "380px"))
        ),
        fluidRow(
          box(title      = tagList(icon("table"), " Ticket por CACI y mes"),
              solidHeader = TRUE, status = "primary", width = 6,
              reactableOutput("tabla_ticket_caci")),
          box(title      = tagList(icon("table"), " Ticket histórico por año"),
              solidHeader = TRUE, status = "primary", width = 6,
              reactableOutput("tabla_ticket_historico"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 4 · Tendencias
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "tendencias",
        fluidRow(
          box(title      = tagList(icon("person"),
                                   " Evolución mensual de pacientes por CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_trend_pacientes", height = "380px"))
        ),
        br(),
        fluidRow(
          box(title      = tagList(icon("chart-bar"),
                                   " Ventas y costos mensuales por CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_financiero", height = "520px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 5 · Rentabilidad
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "rentabilidad",
        fluidRow(
          box(title      = tagList(icon("table"), " Rentabilidad por mes y CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              reactableOutput("tabla_rentabilidad"))
        ),
        br(),
        fluidRow(
          box(title      = tagList(icon("chart-bar"),
                                   " Margen bruto mensual por CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_margen", height = "380px")),
          box(title      = tagList(icon("percent"),
                                   " Porcentaje de rentabilidad mensual"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_pct_rent", height = "380px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 6 · Histórico
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "historico",
        fluidRow(
          box(title      = tagList(icon("table"),
                                   " Admisiones y rentabilidad por año y CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              reactableOutput("tabla_historico"))
        ),
        br(),
        fluidRow(
          box(title      = tagList(icon("users"), " Admisiones por año y CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_hist_pte", height = "380px")),
          box(title      = tagList(icon("percent"),
                                   " Rentabilidad histórica por CACI"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_hist_rent", height = "380px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 7 · Resumen anual
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "anual",
        fluidRow(
          box(title      = tagList(icon("table"),
                                   " Resumen financiero anual por CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              reactableOutput("tabla_anual"))
        ),
        br(),
        fluidRow(
          box(title      = tagList(icon("percent"), " Rentabilidad por año"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_rent_anual", height = "380px")),
          box(title      = tagList(icon("chart-line"),
                                   " Costo total mensual por año (tendencia)"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_costo_anual", height = "380px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 8 · Por Unidad
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "por_unidad",
        fluidRow(
          box(title      = tagList(icon("hospital"),
                                   " Costos por unidad de negocio y CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_une", height = "600px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 9 · Epidemiología
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "epidemiologia",
        fluidRow(
          box(title      = tagList(icon("users"),
                                   " Pirámide poblacional por CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_piramide", height = "520px"))
        ),
        br(),
        fluidRow(
          box(title      = tagList(icon("table"), " Resumen epidemiológico"),
              solidHeader = TRUE, status = "primary", width = 5,
              reactableOutput("tabla_epi")),
          box(title      = tagList(icon("bed"),
                                   " Días de estancia hospitalaria por CACI"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("plot_estancia", height = "320px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab · Perfil de pacientes — TODOS los pacientes DIME (no solo CACI)
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "perfil_pac",
        fluidRow(
          box(width = 12, solidHeader = FALSE, status = "primary",
              tags$div(style = "display:flex; gap:24px; align-items:flex-end; flex-wrap:wrap;",
                tags$div(style = "min-width:220px;",
                  selectInput("pp_yr", "Año",
                              choices = c("Todos" = "0", setNames(as.character(perfil_year_choices), perfil_year_choices)),
                              selected = "0")
                ),
                tags$div(style = "flex:1; min-width:260px;",
                  tags$small(tags$em(
                    "SLE (Segmento Libre Elección) se divide en Particulares (pago directo, ",
                    "convenios médicos, PREVISER) y MP/Pólizas (medicina prepagada y ",
                    "aseguradoras), según metodología del proyecto Prexo SLE / AV DIME ",
                    "(Personal JSH/analisis_comercial_2024)."
                  ))
                )
              )
          )
        ),
        uiOutput("pp_kpi_boxes"),
        br(),
        fluidRow(
          box(title = tagList(icon("venus-mars"), " Pirámide poblacional"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("pp_plot_piramide", height = "420px")),
          box(title = tagList(icon("hand-holding-medical"), " Tipo de pagador (EAPB / SLE)"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("pp_plot_pagador", height = "420px"))
        ),
        fluidRow(
          box(title = tagList(icon("door-open"), " Canal de ingreso"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("pp_plot_canal", height = "380px")),
          box(title = tagList(icon("stethoscope"), " Diagnósticos de ingreso más frecuentes"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("pp_plot_dx", height = "380px"))
        ),
        fluidRow(
          box(title = tagList(icon("bed"), " Estancia hospitalaria (días)"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("pp_plot_estancia", height = "380px")),
          box(title = tagList(icon("chart-area"), " Ingresos por año"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("pp_plot_tendencia", height = "380px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab · Mapa — pacientes geocodificados (EAPB / SLE / CACI)
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "mapa_pac",
        fluidRow(
          tags$div(class = "filters-compact",
            box(width = 12, solidHeader = FALSE, status = "primary",
              tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
                tags$span(class = "filters-section-label", style = "margin-bottom:0;", "Vista del mapa"),
                downloadButton("map_download", "Descargar datos (CSV)", class = "btn-sm")
              ),
              tags$div(style = "display:flex; gap:18px; align-items:flex-end; flex-wrap:wrap;",
                tags$div(style = "min-width:120px;",
                  selectInput("map_yr", "Año",
                              choices = c("Todos" = "0", setNames(as.character(map_year_choices), map_year_choices)),
                              selected = "0")
                ),
                tags$div(style = "min-width:180px;",
                  radioButtons("map_color_mode", "Colorear por",
                               choices  = c("Tipo de pagador", "CACI", "Distancia a DIME"),
                               selected = "Tipo de pagador")
                ),
                tags$div(style = "min-width:150px;",
                  checkboxInput("map_cluster", "Agrupar en clústeres", value = TRUE)
                ),
                tags$div(style = "min-width:190px;",
                  checkboxInput("map_comunas", "Mostrar concentración por comuna", value = FALSE)
                ),
                tags$div(style = "flex:1; min-width:240px;",
                  tags$small(tags$em(
                    uiOutput("map_caveat", inline = TRUE)
                  ))
                )
              ),
              tags$hr(),
              tags$span(class = "filters-section-label", "Segmentación de pacientes"),
              tags$div(style = "display:flex; gap:24px; align-items:flex-start;",
                tags$div(style = "display:flex; gap:18px; align-items:flex-start; flex-wrap:wrap; flex:1;",
                  tags$div(style = "min-width:160px;",
                    checkboxGroupInput("map_atencion", "Tipo de atención",
                                       choices = tipo_atencion_levels, selected = tipo_atencion_levels)
                  ),
                  tags$div(style = "min-width:220px;",
                    checkboxGroupInput("map_pagador", "Tipo de pagador",
                                       choices  = names(payer_colors)[names(payer_colors) != "Sin dato"],
                                       selected = names(payer_colors)[names(payer_colors) != "Sin dato"])
                  ),
                  tags$div(style = "min-width:190px;",
                    checkboxGroupInput("map_frecuencia", "Frecuencia de visitas",
                                       choices = frecuencia_levels, selected = frecuencia_levels)
                  ),
                  tags$div(style = "min-width:210px;",
                    checkboxGroupInput("map_caci_tipo", "Tipo CACI",
                                       choices = c(caci_levels, "No CACI"),
                                       selected = c(caci_levels, "No CACI"))
                  )
                ),
                tags$div(style = "min-width:300px; max-width:400px; flex-shrink:0;",
                  selectizeInput("map_servicio", "Servicio",
                                choices = map_servicio_choices, selected = map_servicio_choices,
                                multiple = TRUE, width = "100%",
                                options = list(plugins = list("remove_button"),
                                               placeholder = "Todos los servicios"))
                )
              )
            )
          )
        ),
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = tagList(icon("map-marked-alt"), " Pacientes geocodificados — DIME"),
              leafletOutput("map_pacientes", height = "600px"))
        ),
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = tagList(icon("chart-bar"), " Visitas por mes — EAPB / SLE"),
              plotlyOutput("map_plot_mensual", height = "320px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 10 · Datos detallados
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "datos",
        fluidRow(
          box(title      = tagList(icon("search"),
                                   " Explorador de datos — histórico completo"),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_dt"))
        )
      )
    )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Reactivo disparador: re-filtrar al presionar "Actualizar" ─────────────
  trigger <- reactiveVal(0)
  observeEvent(input$btn_update, trigger(trigger() + 1), ignoreNULL = FALSE)

  # ── Etiqueta del período seleccionado ─────────────────────────────────────
  periodo_label <- reactive({
    trigger()
    mon <- as.integer(isolate(input$mon))
    yr  <- as.integer(isolate(input$yr))
    if (mon > 0) {
      paste(meses_abr[mon], yr)
    } else {
      as.character(yr)
    }
  })

  # ── Info sidebar ──────────────────────────────────────────────────────────
  output$last_update <- renderText({
    format(Sys.Date(), "%d/%m/%Y")
  })

  output$ultimo_mes <- renderText({
    trigger()
    # pte_base existe tanto en modo deploy (pre-agregado) como en desarrollo
    # (data_costo_base solo existe en desarrollo local) — usar siempre pte_base
    # para que este indicador no dependa del modo de carga.
    df <- pte_base %>%
      filter(año == as.integer(isolate(input$yr))) %>%
      summarise(m = max(mes_cargue, na.rm = TRUE))
    mes_label <- tryCatch(
      paste(meses_abr[df$m], isolate(input$yr)),
      error = function(e) "—"
    )
    mes_label
  })

  # ── Datasets filtrados ────────────────────────────────────────────────────
  # Filtro de une_base (Tab 8 — Por unidad)
  une_filt <- reactive({
    trigger()
    yr    <- as.integer(isolate(input$yr))
    mon   <- as.integer(isolate(input$mon))
    cacis <- isolate(input$caci_sel)
    df <- une_base %>%
      filter(año == yr, as.character(caci) %in% cacis)
    if (mon > 0L) df <- df %>% filter(mes_cargue == mon)
    df
  })

  # Filtro de dt_base (Tab 10 — Datos)
  dt_filt <- reactive({
    trigger()
    yr_desde <- as.integer(isolate(input$yr_desde))
    yr       <- as.integer(isolate(input$yr))
    cacis    <- isolate(input$caci_sel)
    dt_base %>%
      filter(año >= yr_desde, año <= yr, as.character(caci) %in% cacis)
  })

  data_grd_filt <- reactive({
    trigger()
    yr    <- as.integer(isolate(input$yr))
    mon   <- as.integer(isolate(input$mon))
    cacis <- isolate(input$caci_sel)

    df <- data_grd_base %>%
      filter(año == yr, is.na(caci) | as.character(caci) %in% cacis)

    if (mon > 0) df <- df %>% filter(month(fecha_ingreso) == mon)
    df
  })

  # ── Costo por paciente × CACI × mes ──────────────────────────────────────
  # Filtra desde pte_base (ya agregado en global.R): rápido en cada sesión.
  pte_mes <- reactive({
    trigger()
    yr    <- as.integer(isolate(input$yr))
    mon   <- as.integer(isolate(input$mon))
    cacis <- isolate(input$caci_sel)

    df <- pte_base %>%
      filter(año == yr, as.character(caci) %in% cacis)

    if (mon > 0L) df <- df %>% filter(mes_cargue == mon)
    df
  })

  # ── Resumen mensual por CACI ──────────────────────────────────────────────
  resumen_caci <- reactive({
    pte_mes() %>%
      group_by(caci, mes_cargue) %>%
      summarise(
        pacientes     = n_distinct(identificacion),
        costo_total   = sum(costo,  na.rm = TRUE),
        venta_total   = sum(venta,  na.rm = TRUE),
        costo_mediana = median(costo, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        margen     = venta_total - costo_total,
        margen_pct = safe_pct(margen, venta_total),
        mes_nombre = mes_factor(mes_cargue)
      )
  })

  # ── KPIs del año seleccionado ─────────────────────────────────────────────
  kpi_data <- reactive({
    costo_med <- median(pte_mes()$costo, na.rm = TRUE)
    resumen_caci() %>%
      summarise(
        admisiones = sum(pacientes,   na.rm = TRUE),
        ventas     = sum(venta_total, na.rm = TRUE),
        costos     = sum(costo_total, na.rm = TRUE),
        margen     = sum(margen,      na.rm = TRUE)
      ) %>%
      mutate(
        margen_pct    = safe_pct(margen, ventas),
        costo_mediana = costo_med
      )
  })

  # ── Alertas ejecutivas (excluye Otros CV por su heterogeneidad) ───────────
  alertas_caci <- reactive({
    mediana_caci <- pte_mes() %>%
      filter(!is.na(caci), as.character(caci) != "Otros CV") %>%
      group_by(caci) %>%
      summarise(`Costo mediano` = median(costo, na.rm = TRUE), .groups = "drop")

    resumen_caci() %>%
      filter(!is.na(caci), as.character(caci) != "Otros CV") %>%
      group_by(caci) %>%
      summarise(
        Pacientes    = sum(pacientes,   na.rm = TRUE),
        Ventas       = sum(venta_total, na.rm = TRUE),
        Costos       = sum(costo_total, na.rm = TRUE),
        Rentabilidad = safe_pct(sum(margen, na.rm = TRUE), Ventas),
        .groups = "drop"
      ) %>%
      left_join(mediana_caci, by = "caci") %>%
      mutate(
        Alerta = case_when(
          is.na(Rentabilidad)   ~ "Sin ventas",
          Rentabilidad < 30     ~ "Crítica",
          Rentabilidad < 50     ~ "Intermedia",
          TRUE                  ~ "Favorable"
        )
      ) %>%
      arrange(Rentabilidad)
  })

  # ── Resumen anual por CACI (histórico) ───────────────────────────────────
  resumen_anual <- reactive({
    trigger()
    yr_desde <- as.integer(isolate(input$yr_desde))
    yr       <- as.integer(isolate(input$yr))
    cacis    <- isolate(input$caci_sel)

    pte_base %>%
      filter(año >= yr_desde, año <= yr, as.character(caci) %in% cacis) %>%
      group_by(año, caci) %>%
      summarise(
        pacientes   = n_distinct(identificacion),
        costo_total = sum(costo, na.rm = TRUE),
        venta_total = sum(venta, na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      mutate(
        margen     = venta_total - costo_total,
        margen_pct = safe_pct(margen, venta_total)
      )
  })

  # ── Ticket (costo mediano por paciente) ───────────────────────────────────
  ticket_caci <- reactive({
    pte_mes() %>%
      filter(!is.na(caci)) %>%
      group_by(caci, mes_cargue) %>%
      summarise(
        pacientes      = n_distinct(identificacion),
        ticket_mediana = median(costo, na.rm = TRUE),
        ticket_p25     = quantile(costo, 0.25, na.rm = TRUE),
        ticket_p75     = quantile(costo, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(mes_nombre = mes_factor(mes_cargue))
  })

  ticket_historico <- reactive({
    trigger()
    yr_desde <- as.integer(isolate(input$yr_desde))
    yr       <- as.integer(isolate(input$yr))
    cacis    <- isolate(input$caci_sel)

    pte_base %>%
      filter(año >= yr_desde, año <= yr, as.character(caci) %in% cacis) %>%
      group_by(mes_cargue, año) %>%
      summarise(
        pacientes      = n_distinct(identificacion),
        ticket_mediana = median(costo, na.rm = TRUE),
        ticket_p25     = quantile(costo, 0.25, na.rm = TRUE),
        ticket_p75     = quantile(costo, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        año_label  = factor(as.character(año)),
        año_mes    = as.Date(paste(año, as.integer(mes_cargue), "01", sep = "-")),
        mes_nombre = mes_factor(mes_cargue)
      )
  })

  # Resumen mensual histórico multi-año (para tab Anual — tendencia de costos)
  resumen_caci_hist <- reactive({
    trigger()
    yr_desde <- as.integer(isolate(input$yr_desde))
    yr       <- as.integer(isolate(input$yr))
    cacis    <- isolate(input$caci_sel)

    pte_base %>%
      filter(año >= yr_desde, año <= yr, as.character(caci) %in% cacis) %>%
      group_by(caci, mes_cargue, año) %>%
      summarise(
        pacientes   = n_distinct(identificacion),
        costo_total = sum(costo, na.rm = TRUE),
        venta_total = sum(venta, na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      mutate(
        margen     = venta_total - costo_total,
        margen_pct = safe_pct(margen, venta_total),
        año_label  = factor(as.character(año)),
        año_mes    = as.Date(paste(año, as.integer(mes_cargue), "01", sep = "-")),
        mes_nombre = mes_factor(mes_cargue)
      )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 1 · Resumen
  # ══════════════════════════════════════════════════════════════════════════
  output$kpi_boxes <- renderUI({
    kpi <- kpi_data()
    fluidRow(
      valueBox(
        value    = format(as.integer(kpi$admisiones), big.mark = "."),
        subtitle = "Admisiones CACI",
        icon     = icon("heartbeat"),
        color    = "blue",
        width    = 3
      ),
      valueBox(
        value    = cop_kpi(kpi$costo_mediana),
        subtitle = "Costo mediano por paciente",
        icon     = icon("stethoscope"),
        color    = "purple",
        width    = 3
      ),
      valueBox(
        value    = cop_kpi(kpi$costos),
        subtitle = "Costos totales",
        icon     = icon("dollar-sign"),
        color    = "red",
        width    = 3
      ),
      valueBox(
        value    = cop_kpi(kpi$margen),
        subtitle = "Margen bruto",
        icon     = icon("chart-line"),
        color    = "green",
        width    = 3
      )
    )
  })

  output$tabla_alertas <- renderReactable({
    df <- alertas_caci()
    reactable(
      df,
      pagination    = FALSE,
      bordered      = TRUE,
      highlight     = TRUE,
      compact       = TRUE,
      defaultColDef = colDef(align = "center"),
      columns = list(
        caci              = colDef(name = "CACI",          sticky = "left", minWidth = 70),
        Pacientes         = colDef(name = "Pac.",          format = colFormat(separators = TRUE)),
        `Costo mediano`   = colDef(name = "Costo med.",    cell = function(v) cop(v),
                                   align = "right", minWidth = 120),
        Ventas            = colDef(name = "Ventas",        cell = function(v) cop(v), align = "right"),
        Costos            = colDef(name = "Costos",        cell = function(v) cop(v), align = "right"),
        Rentabilidad      = colDef(
          name = "Rent. %",
          cell = function(v) pct_fmt(v),
          style = function(v) {
            list(color = if (is.na(v)) "#7F8C8D" else if (v >= 50) "#59A14F"
                        else if (v >= 30) "#F28E2B" else "#E15759",
                 fontWeight = "bold")
          }
        ),
        Alerta = colDef(
          name  = "Alerta",
          style = function(v) {
            list(background = switch(v,
                   "Favorable"  = "#EAF7EA",
                   "Intermedia" = "#FEF5E7",
                   "Crítica"    = "#FDEDEC",
                   "#F4F6F6"),
                 fontWeight = "bold")
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_resumen_pte <- renderPlotly({
    df <- resumen_caci() %>% filter(!is.na(caci))
    p <- ggplot(df, aes(x = mes_nombre, y = pacientes,
                        color = caci, group = caci,
                        text  = paste0("<b>", caci, "</b><br>",
                                       "Mes: ", mes_nombre, "<br>",
                                       "Pacientes: ", pacientes))) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
      scale_color_manual(values = caci_colors) +
      labs(x = NULL, y = "Pacientes", color = "CACI") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  output$plot_resumen_bubble <- renderPlotly({
    df <- resumen_caci() %>% filter(!is.na(caci), costo_mediana > 0)
    p <- ggplot(df, aes(x = mes_nombre, y = costo_mediana,
                        color = caci, group = caci,
                        text = paste0("<b>", caci, "</b><br>",
                                      "Mes: ", mes_nombre, "<br>",
                                      "Costo mediano: ", cop(costo_mediana), "<br>",
                                      "Pacientes: ", pacientes))) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
      scale_color_manual(values = caci_colors) +
      scale_y_continuous(labels = function(x) cop_m(x)) +
      labs(x = NULL, y = "Costo mediano por paciente", color = "CACI",
           title = paste("Costo mediano por paciente ·", periodo_label())) +
      theme_classic() +
      theme(plot.title      = element_text(face = "bold", hjust = 0.5),
            axis.text.x     = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 2 · Costos
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_costo_medio <- renderReactable({
    tabla <- pte_mes() %>%
      group_by(Mes = mes_nombre, CACI = caci) %>%
      summarise(
        Pacientes       = n_distinct(identificacion),
        `Costo mediano` = median(costo, na.rm = TRUE),
        `Costo total`   = sum(costo,   na.rm = TRUE),
        .groups = "drop"
      )
    reactable(
      tabla,
      searchable    = TRUE,
      pagination    = FALSE,
      striped       = TRUE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 90),
      columns = list(
        Mes  = colDef(minWidth = 90, sticky = "left"),
        CACI = colDef(minWidth = 90),
        Pacientes = colDef(
          align  = "right",
          format = colFormat(separators = TRUE)
        ),
        `Costo mediano` = colDef(
          align = "right", minWidth = 150,
          cell  = function(v) cop(v)
        ),
        `Costo total` = colDef(
          align = "right", minWidth = 150,
          cell  = function(v) cop(v)
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_boxplot_caci <- renderPlotly({
    df <- pte_mes() %>% filter(!is.na(caci), costo > 0)
    p <- ggplot(df, aes(x = mes_nombre, y = costo, fill = caci,
                        text = paste0("CACI: ", caci, "<br>Mes: ", mes_nombre,
                                      "<br>Costo: ", cop(costo)))) +
      geom_boxplot(outlier.size = 0.7, outlier.alpha = 0.4) +
      facet_wrap(~ caci, scales = "free_y") +
      scale_y_continuous(labels = function(x) cop_m(x)) +
      scale_fill_manual(values = caci_colors) +
      labs(x = "Mes", y = "Costo por paciente",
           title = paste("Distribución de costos ·", periodo_label())) +
      theme_classic() +
      theme(plot.title       = element_text(face = "bold", hjust = 0.5),
            axis.text.x      = element_text(angle = 45, hjust = 1),
            legend.position  = "none",
            strip.text       = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 3 · Ticket General
  # ══════════════════════════════════════════════════════════════════════════
  output$ticket_kpi_boxes <- renderUI({
    req(nrow(pte_mes()) > 0)
    kpi <- pte_mes() %>%
      summarise(
        mediana = median(costo, na.rm = TRUE),
        p25     = quantile(costo, 0.25, na.rm = TRUE),
        p75     = quantile(costo, 0.75, na.rm = TRUE)
      )
    fluidRow(
      valueBox(
        value    = cop(kpi$p25),
        subtitle = "Ticket P25",
        icon     = icon("arrow-down"),
        color    = "green",
        width    = 4
      ),
      valueBox(
        value    = cop(kpi$mediana),
        subtitle = "Ticket mediana",
        icon     = icon("chart-line"),
        color    = "purple",
        width    = 4
      ),
      valueBox(
        value    = cop(kpi$p75),
        subtitle = "Ticket P75",
        icon     = icon("arrow-up"),
        color    = "orange",
        width    = 4
      )
    )
  })

  output$plot_ticket_caci <- renderPlotly({
    req(nrow(ticket_caci()) > 0)
    df <- ticket_caci() %>% filter(!is.na(caci))
    p <- ggplot(df, aes(x = mes_nombre, y = ticket_mediana,
                        color = caci, group = caci,
                        text = paste0("CACI: ", caci, "<br>Mes: ", mes_nombre,
                                      "<br>Mediana: ", cop(ticket_mediana),
                                      "<br>P25: ", cop(ticket_p25),
                                      "<br>P75: ", cop(ticket_p75),
                                      "<br>Pacientes: ", pacientes))) +
      geom_ribbon(aes(ymin = ticket_p25, ymax = ticket_p75, fill = caci),
                  alpha = 0.12, show.legend = FALSE) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      scale_color_manual(values = caci_colors) +
      scale_fill_manual(values = caci_colors) +
      scale_y_continuous(labels = function(x) cop_m(x), n.breaks = 8) +
      labs(title    = paste("Ticket mediana por CACI ·", periodo_label()),
           subtitle = "Banda: rango intercuartílico P25–P75",
           x = NULL, y = "Costo mediana por paciente", color = "CACI") +
      theme_classic() +
      theme(plot.title    = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D", size = 10),
            axis.text.x   = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$plot_ticket_historico <- renderPlotly({
    req(nrow(ticket_historico()) > 0)
    df <- ticket_historico()
    p <- ggplot(df, aes(x = año_mes, y = ticket_mediana,
                        color = año_label, group = año_label,
                        text = paste0("Año: ", año_label,
                                      "<br>Mes: ", format(año_mes, "%b %Y"),
                                      "<br>Mediana: ", cop(ticket_mediana),
                                      "<br>P25: ", cop(ticket_p25),
                                      "<br>P75: ", cop(ticket_p75),
                                      "<br>Pacientes: ", pacientes))) +
      geom_ribbon(aes(ymin = ticket_p25, ymax = ticket_p75, fill = año_label),
                  alpha = 0.12, show.legend = FALSE) +
      geom_line(linewidth = 1.3) + geom_point(size = 2.8) +
      scale_color_manual(values = year_colors) +
      scale_fill_manual(values = year_colors) +
      scale_x_date(date_labels = "%b '%y", date_breaks = "3 months") +
      scale_y_continuous(labels = function(x) cop_m(x), n.breaks = 8) +
      labs(title    = "Ticket general mediana · histórico",
           subtitle = "Banda: rango intercuartílico P25–P75",
           x = NULL, y = "Costo mediana por paciente", color = "Año") +
      theme_classic() +
      theme(plot.title    = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "#7F8C8D", size = 10),
            axis.text.x   = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$tabla_ticket_caci <- renderReactable({
    req(nrow(ticket_caci()) > 0)
    df <- ticket_caci() %>%
      filter(!is.na(caci)) %>%
      arrange(mes_cargue, caci) %>%
      transmute(
        Mes              = mes_nombre,
        CACI             = caci,
        Pacientes        = pacientes,
        `Ticket P25`     = cop(ticket_p25),
        `Ticket mediana` = cop(ticket_mediana),
        `Ticket P75`     = cop(ticket_p75)
      )
    reactable(
      df,
      pagination    = FALSE,
      striped       = TRUE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 100),
      columns = list(
        Mes  = colDef(minWidth = 60, sticky = "left"),
        CACI = colDef(minWidth = 80),
        Pacientes        = colDef(format = colFormat(separators = TRUE)),
        `Ticket P25`     = colDef(align = "right", minWidth = 140),
        `Ticket mediana` = colDef(align = "right", minWidth = 150,
                                  style = list(fontWeight = "bold",
                                               color = "#2C3E50")),
        `Ticket P75`     = colDef(align = "right", minWidth = 140)
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$tabla_ticket_historico <- renderReactable({
    req(nrow(ticket_historico()) > 0)
    df <- ticket_historico() %>%
      arrange(año, mes_cargue) %>%
      transmute(
        Año              = as.character(año),
        Mes              = mes_nombre,
        Pacientes        = pacientes,
        `Ticket P25`     = cop(ticket_p25),
        `Ticket mediana` = cop(ticket_mediana),
        `Ticket P75`     = cop(ticket_p75)
      )
    reactable(
      df,
      pagination    = FALSE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 100),
      rowStyle = function(index) {
        clr <- year_colors[df$Año[index]]
        if (!is.na(clr)) {
          v <- col2rgb(clr)[, 1]
          list(background = sprintf("rgba(%d,%d,%d,0.10)", v[1], v[2], v[3]))
        } else list(background = "white")
      },
      columns = list(
        Año  = colDef(minWidth = 60, sticky = "left",
                      style = function(v) {
                        clr <- year_colors[as.character(v)]
                        list(color = if (!is.na(clr)) clr else "black",
                             fontWeight = "bold")
                      }),
        Mes              = colDef(minWidth = 60),
        Pacientes        = colDef(format = colFormat(separators = TRUE)),
        `Ticket P25`     = colDef(align = "right", minWidth = 140),
        `Ticket mediana` = colDef(align = "right", minWidth = 150,
                                  style = list(fontWeight = "bold",
                                               color = "#2C3E50")),
        `Ticket P75`     = colDef(align = "right", minWidth = 140)
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 4 · Tendencias
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_trend_pacientes <- renderPlotly({
    df <- resumen_caci() %>% filter(!is.na(caci))
    p <- ggplot(df, aes(x = mes_nombre, y = pacientes,
                        color = caci, group = caci)) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      scale_color_manual(values = caci_colors) +
      scale_y_continuous(n.breaks = 8) +
      labs(title = paste("Pacientes por CACI ·", periodo_label()),
           x = "Mes", y = "Número de pacientes", color = "CACI") +
      theme_classic() +
      theme(plot.title  = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })

  output$plot_financiero <- renderPlotly({
    fin_long <- resumen_caci() %>%
      select(CACI = caci, Mes = mes_nombre, costo_total, venta_total) %>%
      pivot_longer(c(costo_total, venta_total),
                   names_to  = "tipo", values_to = "valor") %>%
      mutate(tipo = recode(tipo, costo_total = "Costo", venta_total = "Venta"))
    p <- ggplot(fin_long,
                aes(x = Mes, y = valor, fill = tipo,
                    group = interaction(CACI, tipo),
                    text  = paste0("CACI: ", CACI, "<br>Mes: ", Mes,
                                   "<br>Tipo: ", tipo,
                                   "<br>Valor: ", cop(valor)))) +
      geom_col(position = "dodge", color = "black", linewidth = 0.2) +
      facet_wrap(~ CACI, scales = "free_y") +
      scale_y_continuous(labels = function(x) cop_m(x)) +
      scale_fill_manual(values = c(Costo = "#E15759", Venta = "#4E79A7")) +
      labs(x = "Mes", y = "Millones COP", fill = NULL) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text  = element_text(face = "bold"))
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 5 · Rentabilidad
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_rentabilidad <- renderReactable({
    tabla <- resumen_caci() %>%
      filter(!is.na(caci)) %>%
      select(Mes = mes_nombre, CACI = caci,
             costo_total, venta_total, margen, margen_pct)
    reactable(
      tabla,
      searchable    = TRUE,
      pagination    = FALSE,
      striped       = TRUE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 100),
      columns = list(
        Mes  = colDef(minWidth = 90, sticky = "left"),
        CACI = colDef(minWidth = 80),
        costo_total = colDef(
          name = "Costo total", minWidth = 150, align = "right",
          cell = function(v) cop(v)
        ),
        venta_total = colDef(
          name = "Ventas", minWidth = 150, align = "right",
          cell = function(v) cop(v)
        ),
        margen = colDef(
          name = "Margen bruto", minWidth = 150, align = "right",
          cell = function(v) cop(v)
        ),
        margen_pct = colDef(
          name   = "Rentabilidad (%)", minWidth = 140, align = "center",
          format = colFormat(suffix = "%", digits = 1),
          style  = function(value) {
            list(color = if (!is.na(value) && value >= 50) "#59A14F"
                        else if (!is.na(value) && value >= 30) "#F28E2B"
                        else "#E15759",
                 fontWeight = "bold")
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_margen <- renderPlotly({
    p <- ggplot(resumen_caci() %>% filter(!is.na(caci)),
                aes(x = mes_nombre, y = margen, fill = caci,
                    text = paste0("CACI: ", caci, "<br>Mes: ", mes_nombre,
                                  "<br>Margen: ", cop(margen)))) +
      geom_col(position = "dodge", color = "black", linewidth = 0.2) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      scale_y_continuous(labels = function(x) cop_m(x), n.breaks = 10) +
      scale_fill_manual(values = caci_colors) +
      labs(x = "Mes", y = "Margen (millones COP)", fill = "CACI") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1),
            legend.title = element_text(face = "bold"))
    ggplotly(p, tooltip = "text")
  })

  output$plot_pct_rent <- renderPlotly({
    p <- ggplot(resumen_caci() %>% filter(!is.na(caci), venta_total > 0),
                aes(x = mes_nombre, y = margen_pct, color = caci, group = caci,
                    text = paste0("CACI: ", caci, "<br>Mes: ", mes_nombre,
                                  "<br>Rentabilidad: ", pct_fmt(margen_pct)))) +
      geom_line(linewidth = 1.1) + geom_point(size = 3) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "grey50") +
      scale_color_manual(values = caci_colors) +
      scale_y_continuous(n.breaks = 10, limits = c(0, 100),
                         labels   = function(x) paste0(x, "%")) +
      labs(x = "Mes", y = "Margen sobre ventas (%)", color = "CACI") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 6 · Histórico
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_historico <- renderReactable({
    tabla <- resumen_anual() %>%
      filter(!is.na(caci)) %>%
      select(Año = año, CACI = caci, Pacientes = pacientes,
             costo_total, venta_total, margen, margen_pct)
    reactable(
      tabla,
      searchable    = FALSE,
      pagination    = FALSE,
      striped       = TRUE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 100),
      columns = list(
        Año  = colDef(minWidth = 70, sticky = "left"),
        CACI = colDef(minWidth = 80),
        Pacientes = colDef(
          align  = "right",
          format = colFormat(separators = TRUE)
        ),
        costo_total = colDef(
          name = "Costo total", minWidth = 150, align = "right",
          cell = function(v) cop(v)
        ),
        venta_total = colDef(
          name = "Ventas", minWidth = 150, align = "right",
          cell = function(v) cop(v)
        ),
        margen = colDef(
          name = "Margen bruto", minWidth = 150, align = "right",
          cell = function(v) cop(v)
        ),
        margen_pct = colDef(
          name   = "Rentabilidad (%)", minWidth = 140, align = "center",
          format = colFormat(suffix = "%", digits = 1),
          style  = function(value) {
            list(color = if (!is.na(value) && value >= 50) "#59A14F"
                        else if (!is.na(value) && value >= 30) "#F28E2B"
                        else "#E15759",
                 fontWeight = "bold")
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_hist_pte <- renderPlotly({
    p <- ggplot(resumen_anual() %>% filter(!is.na(caci)),
                aes(x = factor(año), y = pacientes, fill = caci,
                    text = paste0("CACI: ", caci, "<br>Año: ", año,
                                  "<br>Pacientes: ", pacientes))) +
      geom_col(position = "dodge", color = "black", linewidth = 0.2) +
      scale_fill_manual(values = caci_colors) +
      scale_y_continuous(n.breaks = 8) +
      labs(x = "Año", y = "Pacientes", fill = "CACI") +
      theme_classic()
    ggplotly(p, tooltip = "text")
  })

  output$plot_hist_rent <- renderPlotly({
    p <- ggplot(resumen_anual() %>% filter(!is.na(caci), venta_total > 0),
                aes(x = factor(año), y = margen_pct, fill = caci,
                    text = paste0("CACI: ", caci, "<br>Año: ", año,
                                  "<br>Rentabilidad: ", pct_fmt(margen_pct)))) +
      geom_col(position = "dodge", color = "black", linewidth = 0.2) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
      scale_fill_manual(values = caci_colors) +
      scale_y_continuous(n.breaks = 10, labels = function(x) paste0(x, "%")) +
      labs(x = "Año", y = "Rentabilidad (%)", fill = "CACI") +
      theme_classic()
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 7 · Resumen anual
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_anual <- renderReactable({
    df <- resumen_anual() %>%
      filter(!is.na(caci)) %>%
      arrange(año, caci) %>%
      transmute(
        Año              = as.character(año),
        CACI             = caci,
        Pacientes        = pacientes,
        `Costo total`    = cop(costo_total),
        Ventas           = cop(venta_total),
        `Margen bruto`   = cop(margen),
        `Rentabilidad %` = pct_fmt(margen_pct)
      )
    reactable(
      df,
      searchable    = FALSE,
      pagination    = FALSE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 100),
      rowStyle = function(index) {
        clr <- year_colors[df$Año[index]]
        if (!is.na(clr)) {
          v <- col2rgb(clr)[, 1]
          list(background = sprintf("rgba(%d,%d,%d,0.10)", v[1], v[2], v[3]))
        } else list(background = "white")
      },
      columns = list(
        Año  = colDef(minWidth = 70, sticky = "left",
                      style = function(v) {
                        clr <- year_colors[as.character(v)]
                        list(color = if (!is.na(clr)) clr else "black",
                             fontWeight = "bold")
                      }),
        CACI           = colDef(minWidth = 80),
        Pacientes      = colDef(format = colFormat(separators = TRUE)),
        `Costo total`  = colDef(align = "right", minWidth = 150),
        Ventas         = colDef(align = "right", minWidth = 150),
        `Margen bruto` = colDef(align = "right", minWidth = 150),
        `Rentabilidad %` = colDef(
          minWidth = 130, align = "center",
          style = function(v) {
            pct <- as.numeric(gsub(",", ".", gsub("%", "", gsub("—", NA, v))))
            list(color = if (!is.na(pct) && pct >= 50) "#59A14F"
                        else if (!is.na(pct) && pct >= 30) "#F28E2B"
                        else "#E15759",
                 fontWeight = "bold")
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_rent_anual <- renderPlotly({
    p <- ggplot(resumen_anual() %>% filter(!is.na(caci), venta_total > 0),
                aes(x = factor(año), y = margen_pct, color = caci, group = caci,
                    text = paste0("CACI: ", caci, "<br>Año: ", año,
                                  "<br>Rentabilidad: ", pct_fmt(margen_pct)))) +
      geom_line(linewidth = 1.2) + geom_point(size = 4) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "grey50") +
      scale_color_manual(values = caci_colors) +
      scale_y_continuous(n.breaks = 10, limits = c(0, 100),
                         labels = function(x) paste0(x, "%")) +
      labs(x = "Año", y = "Rentabilidad (%)", color = "CACI",
           title = "Rentabilidad por año") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p, tooltip = "text")
  })

  output$plot_costo_anual <- renderPlotly({
    p <- ggplot(resumen_caci_hist() %>% filter(!is.na(caci)),
                aes(x = año_mes, y = costo_total, color = año_label,
                    group = año_label,
                    text = paste0("CACI: ", caci, "<br>Año: ", año_label,
                                  "<br>Mes: ", format(año_mes, "%b %Y"),
                                  "<br>Costo: ", cop(costo_total)))) +
      geom_line(linewidth = 1.1) + geom_point(size = 2) +
      facet_wrap(~ caci, scales = "free_y") +
      scale_color_manual(values = year_colors) +
      scale_x_date(date_labels = "%b '%y", date_breaks = "3 months") +
      scale_y_continuous(labels = function(x) cop_m(x)) +
      labs(x = NULL, y = "Costo", color = "Año",
           title = "Costo mensual por CACI · histórico") +
      theme_classic() +
      theme(plot.title  = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
            strip.text  = element_text(face = "bold"))
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 8 · Por unidad
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_une <- renderPlotly({
    une_caci <- une_filt() %>%
      filter(!is.na(caci)) %>%
      mutate(Mes = mes_factor(mes_cargue)) %>%
      group_by(Mes, CACI = caci, Unidad) %>%
      summarise(Costo = sum(costo, na.rm = TRUE), .groups = "drop")

    p <- ggplot(une_caci, aes(x = Mes, y = Costo, fill = Unidad,
                              text = paste0("CACI: ", CACI, "<br>Mes: ", Mes,
                                            "<br>Unidad: ", Unidad,
                                            "<br>Costo: ", cop(Costo)))) +
      geom_col(position = "stack", color = "black", linewidth = 0.15) +
      facet_wrap(~ CACI) +
      scale_y_continuous(labels = function(x) cop_m(x), n.breaks = 8) +
      labs(x = "Mes", y = "Costo (millones COP)", fill = "Unidad",
           title = paste("Costos por unidad de negocio ·", periodo_label())) +
      theme_classic() +
      theme(plot.title  = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text  = element_text(face = "bold"))
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 9 · Epidemiología
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_piramide <- renderPlotly({
    df_epi <- data_grd_filt()
    # Accept 'documento' or 'identificacion' as the patient ID column
    id_col <- intersect(c("documento", "identificacion"), names(df_epi))[1]
    data_pir <- df_epi %>%
      filter(!is.na(caci), !is.na(edad), !is.na(sexo)) %>%
      mutate(
        Sexo = case_when(
          str_to_upper(sexo) %in% c("F", "FEMENINO", "MUJER") ~ "Femenino",
          str_to_upper(sexo) %in% c("M", "MASCULINO", "HOMBRE") ~ "Masculino",
          TRUE ~ "Otro"
        ),
        `Grupo de edad` = cut(
          edad,
          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
          labels = c("0-9","10-19","20-29","30-39","40-49",
                     "50-59","60-69","70-79","80+"),
          right  = FALSE
        )
      ) %>%
      group_by(CACI = caci, `Grupo de edad`, Sexo) %>%
      summarise(n = if (!is.na(id_col)) n_distinct(.data[[id_col]]) else n(),
                .groups = "drop") %>%
      mutate(n_dir = if_else(Sexo == "Femenino", -n, n))

    p <- ggplot(data_pir, aes(x = `Grupo de edad`, y = n_dir, fill = Sexo,
                              text = paste0("CACI: ", CACI, "<br>Edad: ", `Grupo de edad`,
                                            "<br>Sexo: ", Sexo, "<br>Pacientes: ", n))) +
      geom_col(color = "black", linewidth = 0.2) +
      coord_flip() +
      facet_wrap(~ CACI) +
      scale_y_continuous(labels = abs) +
      scale_fill_manual(values = c(Femenino = "#F28E2B", Masculino = "#4E79A7",
                                   Otro = "#95A5A6")) +
      labs(x = "Grupo de edad", y = "Pacientes", fill = "Sexo",
           title = paste("Pirámide poblacional ·", periodo_label())) +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            strip.text = element_text(face = "bold"))
    ggplotly(p, tooltip = "text")
  })

  output$tabla_epi <- renderReactable({
    df_epi2 <- data_grd_filt()
    id_col2  <- intersect(c("documento", "identificacion"), names(df_epi2))[1]
    has_stay <- "dif_days" %in% names(df_epi2)
    df <- df_epi2 %>%
      filter(!is.na(caci)) %>%
      group_by(caci) %>%
      summarise(
        Pacientes         = if (!is.na(id_col2)) n_distinct(.data[[id_col2]]) else n(),
        `Edad mediana`    = median(edad, na.rm = TRUE),
        `Edad P25`        = quantile(edad, 0.25, na.rm = TRUE),
        `Edad P75`        = quantile(edad, 0.75, na.rm = TRUE),
        `Estancia mediana`= if (has_stay) median(dif_days, na.rm = TRUE) else NA_real_,
        `Estancia P75`    = if (has_stay) quantile(dif_days, 0.75, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(caci)
    reactable(
      df,
      pagination    = FALSE,
      striped       = TRUE,
      highlight     = TRUE,
      bordered      = TRUE,
      defaultColDef = colDef(align = "center"),
      columns = list(
        caci              = colDef(name = "CACI", sticky = "left"),
        Pacientes         = colDef(format = colFormat(separators = TRUE)),
        `Edad mediana`    = colDef(format = colFormat(digits = 1)),
        `Edad P25`        = colDef(format = colFormat(digits = 1)),
        `Edad P75`        = colDef(format = colFormat(digits = 1)),
        `Estancia mediana`= colDef(format = colFormat(digits = 1)),
        `Estancia P75`    = colDef(format = colFormat(digits = 1))
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  output$plot_estancia <- renderPlotly({
    df_raw <- data_grd_filt()
    if (!"dif_days" %in% names(df_raw)) {
      return(plotly_empty() %>% layout(title = "Columna dif_days no disponible en estos datos"))
    }
    df <- df_raw %>%
      filter(!is.na(caci), !is.na(dif_days), dif_days >= 0)
    p <- ggplot(df, aes(x = caci, y = dif_days, fill = caci,
                        text = paste0("CACI: ", caci, "<br>Días: ", dif_days))) +
      geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5) +
      scale_fill_manual(values = caci_colors) +
      scale_y_continuous(n.breaks = 12) +
      labs(x = "CACI", y = "Días de estancia",
           title = paste("Estancia hospitalaria ·", periodo_label())) +
      theme_classic() +
      theme(plot.title      = element_text(face = "bold", hjust = 0.5),
            legend.position = "none")
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab · Perfil de pacientes
  # ══════════════════════════════════════════════════════════════════════════
  pp_filt <- reactive({
    req(perfil_pacientes)
    yr <- as.integer(input$pp_yr)
    df <- perfil_pacientes
    if (!is.na(yr) && yr > 0) df <- df %>% filter(año == yr)
    df
  })

  output$pp_kpi_boxes <- renderUI({
    if (is.null(perfil_pacientes)) {
      return(fluidRow(box(width = 12, status = "warning",
        "Perfil de pacientes no disponible: ejecuta shiny_grd/prep_data.R.")))
    }
    df <- pp_filt()
    fluidRow(
      valueBox(format(n_distinct(df$documento), big.mark = "."),
               "Pacientes", icon = icon("users"), color = "blue", width = 3),
      valueBox(format(as.integer(round(median(df$edad, na.rm = TRUE))), big.mark = "."),
               "Edad mediana", icon = icon("birthday-cake"), color = "purple", width = 3),
      valueBox(pct_fmt(100 * mean(df$mortalidad, na.rm = TRUE)),
               "Mortalidad intrahospitalaria", icon = icon("heart-broken"), color = "red", width = 3),
      valueBox(pct_fmt(100 * mean(df$cirugia, na.rm = TRUE)),
               "Remitidos a cirugía", icon = icon("scalpel"), color = "green", width = 3)
    )
  })

  output$pp_plot_piramide <- renderPlotly({
    df <- pp_filt() %>% filter(!is.na(edad_grupo), !is.na(sexo))
    req(nrow(df) > 0)
    d <- df %>%
      count(edad_grupo, sexo) %>%
      mutate(n_dir = if_else(sexo == "Femenino", -n, n))
    p <- ggplot(d, aes(x = edad_grupo, y = n_dir, fill = sexo,
                       text = paste0("Edad: ", edad_grupo, "<br>Sexo: ", sexo, "<br>Pacientes: ", n))) +
      geom_col(color = "black", linewidth = 0.2) +
      coord_flip() +
      scale_y_continuous(labels = abs) +
      scale_fill_manual(values = sexo_colors) +
      labs(x = "Grupo de edad", y = "Pacientes", fill = "Sexo") +
      theme_classic() +
      theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })

  output$pp_plot_pagador <- renderPlotly({
    df <- pp_filt()
    req(nrow(df) > 0)
    d <- df %>% count(tipo_pagador) %>% mutate(pct = n / sum(n) * 100)
    p <- ggplot(d, aes(x = reorder(tipo_pagador, n), y = n, fill = tipo_pagador,
                       text = paste0(tipo_pagador, "<br>Pacientes: ", n,
                                     "<br>", round(pct, 1), "%"))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = payer_colors) +
      labs(x = NULL, y = "Encuentros") +
      theme_classic() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  output$pp_plot_canal <- renderPlotly({
    df <- pp_filt()
    req(nrow(df) > 0)
    d <- df %>% count(via_ingreso, sort = TRUE)
    p <- ggplot(d, aes(x = reorder(via_ingreso, n), y = n,
                       text = paste0(via_ingreso, "<br>Encuentros: ", n))) +
      geom_col(fill = "#4E79A7") +
      coord_flip() +
      labs(x = NULL, y = "Encuentros") +
      theme_classic()
    ggplotly(p, tooltip = "text")
  })

  output$pp_plot_dx <- renderPlotly({
    df <- pp_filt() %>% filter(!is.na(dx_desc))
    req(nrow(df) > 0)
    d <- df %>% count(dx_desc, sort = TRUE) %>% slice_head(n = 10)
    p <- ggplot(d, aes(x = reorder(str_trunc(dx_desc, 40), n), y = n,
                       text = paste0(dx_desc, "<br>Casos: ", n))) +
      geom_col(fill = "#E15759") +
      coord_flip() +
      labs(x = NULL, y = "Casos") +
      theme_classic()
    ggplotly(p, tooltip = "text")
  })

  output$pp_plot_estancia <- renderPlotly({
    df <- pp_filt() %>% filter(!is.na(estancia_dias), estancia_dias >= 0, estancia_dias < 90)
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = estancia_dias)) +
      geom_histogram(binwidth = 1, fill = "#59A14F", color = "white") +
      labs(x = "Días de estancia", y = "Pacientes") +
      theme_classic()
    ggplotly(p)
  })

  output$pp_plot_tendencia <- renderPlotly({
    req(perfil_pacientes)
    d <- perfil_pacientes %>% count(año)
    p <- ggplot(d, aes(x = año, y = n, text = paste0("Año: ", año, "<br>Ingresos: ", n))) +
      geom_line(color = "#4E79A7", linewidth = 1) +
      geom_point(color = "#4E79A7", size = 2) +
      labs(x = NULL, y = "Ingresos") +
      theme_classic()
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab · Mapa de pacientes geocodificados
  # ══════════════════════════════════════════════════════════════════════════
  map_filt <- reactive({
    req(geocoded_map_data)
    yr <- as.integer(input$map_yr)
    df <- geocoded_map_data %>%
      filter(tipo_pagador %in% input$map_pagador,
             as.character(frecuencia) %in% input$map_frecuencia,
             as.character(servicio) %in% input$map_servicio,
             as.character(tipo_atencion) %in% input$map_atencion,
             as.character(caci) %in% input$map_caci_tipo)
    if (!is.na(yr) && yr > 0) df <- df %>% filter(año == yr)
    df
  })

  output$map_caveat <- renderUI({
    req(geocoded_map_data)
    HTML(sprintf(paste(
      "Mostrando <b>%s</b> de %s filas paciente×año geocodificadas.",
      "Excluye direcciones de solo municipio/sin detalle de calle;",
      "coordenadas con jitter de privacidad (~55 m).",
      "Distancia calculada desde DIME (Av. 5N #20N-75, Versalles)."
    ), format(nrow(map_filt()), big.mark = "."), format(nrow(geocoded_map_data), big.mark = ".")))
  })

  output$map_download <- downloadHandler(
    filename = function() paste0("dime_mapa_pacientes_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      readr::write_excel_csv(map_filt(), file, na = "")
    }
  )

  output$map_pacientes <- renderLeaflet({
    df <- map_filt()
    req(nrow(df) > 0)

    color_mode <- input$map_color_mode
    if (color_mode == "CACI") {
      df$color_col  <- as.character(df$caci)
      pal           <- colorFactor(palette = unname(caci_map_colors), levels = names(caci_map_colors))
      legend_title  <- "CACI"
    } else if (color_mode == "Distancia a DIME") {
      df$color_col  <- as.character(df$distancia_banda)
      pal           <- colorFactor(palette = unname(distancia_banda_colors), levels = names(distancia_banda_colors))
      legend_title  <- "Distancia a DIME"
    } else {
      df$color_col  <- as.character(df$tipo_pagador)
      pal           <- colorFactor(palette = unname(payer_colors), levels = names(payer_colors))
      legend_title  <- "Tipo de pagador"
    }

    clust_opts <- if (isTRUE(input$map_cluster)) {
      markerClusterOptions(disableClusteringAtZoom = 17)
    } else NULL

    rings <- tibble(radio_m = c(1000, 2000, 5000, 10000),
                    etiqueta = c("1 km", "2 km", "5 km", "10 km"))

    m <- leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles(attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>%
      setView(lng = -76.53, lat = 3.45, zoom = 12)

    # ── Mapa de concentración por comuna (coroplético) ─────────────────────
    if (isTRUE(input$map_comunas) && !is.null(comunas_cali)) {
      conteo_comuna <- df %>% count(comuna, name = "n_pac")
      comunas_plot <- comunas_cali %>%
        left_join(conteo_comuna, by = c("nombre" = "comuna")) %>%
        mutate(n_pac = coalesce(n_pac, 0L))
      pal_comuna <- colorNumeric(palette = "YlOrRd", domain = c(0, max(comunas_plot$n_pac, 1)))
      centroides <- suppressWarnings(st_coordinates(st_centroid(comunas_plot)))
      m <- m %>%
        addPolygons(
          data = comunas_plot,
          fillColor = ~pal_comuna(n_pac), fillOpacity = 0.6,
          color = "#555555", weight = 1,
          label = ~paste0(nombre, ": ", n_pac, " pacientes"),
          highlightOptions = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)
        ) %>%
        addLabelOnlyMarkers(
          lng = centroides[, "X"], lat = centroides[, "Y"],
          label = as.character(comunas_plot$comuna),
          labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                      style = list("font-weight" = "bold", "font-size" = "13px",
                                                   "color" = "#222", "text-shadow" = "0 0 3px #fff, 0 0 3px #fff"))
        ) %>%
        addLegend(position = "topright", pal = pal_comuna, values = comunas_plot$n_pac,
                  title = "Pacientes por comuna", opacity = 0.8)
    }

    m <- m %>%
      addCircles(lng = DIME_LON, lat = DIME_LAT, radius = rings$radio_m,
                 fill = FALSE, color = "#2C3E50", weight = 1.5,
                 dashArray = "6", opacity = 0.6) %>%
      addAwesomeMarkers(lng = DIME_LON, lat = DIME_LAT,
                        icon = makeAwesomeIcon(icon = "plus", markerColor = "darkred", library = "fa"),
                        popup = "<b>DIME Clínica Neurocardiovascular</b><br>Av. 5N #20N-75, Versalles") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 5, stroke = TRUE, weight = 1, color = "white",
        fillColor = ~pal(color_col), fillOpacity = 0.85,
        clusterOptions = clust_opts,
        popup = ~paste0("<b>", tipo_pagador, "</b><br>Atención: ", tipo_atencion,
                        "<br>CACI: ", caci,
                        "<br>Servicio: ", servicio, "<br>Comuna: ", comuna,
                        "<br>Edad: ", edad_grupo, "<br>Año: ", año,
                        "<br>Visitas ese año: ", n_visitas_anio,
                        "<br>Frecuencia: ", frecuencia,
                        "<br>Distancia a DIME: ", distancia_km, " km (", distancia_banda, ")")
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = ~color_col,
                title = legend_title, opacity = 0.9)
    m
  })

  output$map_plot_mensual <- renderPlotly({
    req(visitas_mensuales)
    yr <- as.integer(input$map_yr)
    d <- visitas_mensuales %>%
      filter(tipo_pagador %in% input$map_pagador,
             as.character(tipo_atencion) %in% input$map_atencion,
             as.character(servicio) %in% input$map_servicio)
    if (!is.na(yr) && yr > 0) d <- d %>% filter(año == yr)
    d <- d %>%
      group_by(mes, tipo_pagador) %>%
      summarise(n_visitas = sum(n_visitas), .groups = "drop") %>%
      mutate(mes_lbl = mes_factor(mes))
    req(nrow(d) > 0)
    p <- ggplot(d, aes(x = mes_lbl, y = n_visitas, fill = tipo_pagador,
                       text = paste0(mes_lbl, "<br>", tipo_pagador, "<br>Visitas: ", n_visitas))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = payer_colors) +
      labs(x = NULL, y = "Visitas", fill = "Tipo de pagador") +
      theme_classic() +
      theme(legend.position = "bottom")
    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 10 · Datos detallados
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_dt <- renderDT({
    tabla <- dt_filt() %>%
      filter(!is.na(caci)) %>%
      transmute(
        Año      = año,
        Mes      = mes_factor(mes_cargue),
        CACI     = caci,
        Paciente = identificacion,
        Tipo     = coalesce(departamento_cargue_2, "—"),
        Margen         = venta - costo,
        `Costo (COP)`  = cop(costo),
        `Ventas (COP)` = cop(venta),
        `Margen (COP)` = cop(Margen)
      ) %>%
      select(Año, Mes, CACI, Paciente, Tipo,
             `Costo (COP)`, `Ventas (COP)`, `Margen (COP)`)

    DT::datatable(
      tabla,
      filter     = "top",
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom        = "Blfrtip",
        buttons    = c("copy", "csv", "excel"),
        scrollX    = TRUE,
        scroller   = TRUE,
        scrollY    = "500px",
        pageLength = 25,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
        )
      ),
      class    = "cell-border stripe compact",
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
