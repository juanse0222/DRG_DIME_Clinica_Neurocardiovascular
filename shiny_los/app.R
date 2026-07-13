################################################################################
# app.R — Estancia Hospitalaria (LOS) · DIME Clínica Neurocardiovascular
# Tabs: Resumen | Tendencias | Por Servicio | Larga Estancia |
#       Diagnóstico (CACI) | Ventas y Costos | Estancias Inactivas | Datos
################################################################################

source("global.R")

SERV_CORE <- c("UCI", "UCIN 2°", "UCIN 5°", "PISO HOSP")

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
      menuItem("Est. Inactivas",     tabName = "inactivas",     icon = icon("pause")),
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
    tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
    tabItems(

      # ════════════════════════════════════════════════════════════════════════
      # Tab 1 · Resumen
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "resumen",
        uiOutput("kpi_los"),
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
          box(title = tagList(icon("table"), " Estadísticos LOS por CACI y servicio"),
              solidHeader = TRUE, status = "primary", width = 6,
              reactableOutput("tabla_caci_stats")),
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
          box(title = tagList(icon("circle"), " LOS vs. Costo total por CACI"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("plot_scatter_los_cost", height = "380px")),
          box(title = tagList(icon("chart-bar"), " Costo diario promedio por CACI"),
              solidHeader = TRUE, status = "primary", width = 5,
              plotlyOutput("plot_cost_day_caci", height = "380px"))
        ),
        br(),
        fluidRow(
          box(title = tagList(icon("table"), " Resumen financiero por departamento y CACI"),
              solidHeader = TRUE, status = "primary", width = 12,
              reactableOutput("tabla_fin_resumen"))
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
              selectInput("ei_resp", "Responsable",
                          choices  = c("Todos" = "0",
                                       "IPS" = "IPS", "EPS" = "EPS", "Paciente" = "Paciente"),
                          selected = "0"),
              tags$small(tags$em("Datos 2024–2026."), style = "color:#6c757d;")),
          box(solidHeader = FALSE, width = 9,
              uiOutput("kpi_ei"))
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

    n_adm    <- n_distinct(df$cuenta)
    df_core  <- df %>% filter(estacion_2 %in% SERV_CORE)
    med_core <- mean(df_core$dif_bed_serv, na.rm = TRUE)
    pct_long <- round(mean(df$larga_estancia, na.rm = TRUE) * 100, 1)
    ei_days  <- sum(data_ei$total_dias_de_estancia_por_ips, na.rm = TRUE)

    fluidRow(
      valueBox(
        value    = format(n_adm, big.mark = "."),
        subtitle = "Registros servicio-estancia",
        icon     = icon("hospital"),
        color    = "blue",
        width    = 3
      ),
      valueBox(
        value    = paste0(round(med_core, 1), " días"),
        subtitle = "Media LOS (UCI/UCIN/HOSP)",
        icon     = icon("calendar"),
        color    = "light-blue",
        width    = 3
      ),
      valueBox(
        value    = paste0(pct_long, "%"),
        subtitle = "% Larga estancia",
        icon     = icon("exclamation-triangle"),
        color    = if (pct_long > 20) "red" else if (pct_long > 10) "yellow" else "green",
        width    = 3
      ),
      valueBox(
        value    = format(ei_days, big.mark = "."),
        subtitle = "Días inactivos totales (IPS)",
        icon     = icon("pause"),
        color    = "orange",
        width    = 3
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
    df <- los_flagged() %>%
      filter(larga_estancia, !is.na(diag_egreso), diag_egreso != "") %>%
      mutate(icd3  = str_extract(str_to_upper(str_trim(diag_egreso)), "^[A-Z]\\d{2}"),
             desc  = str_squish(diag_egreso)) %>%
      filter(!is.na(icd3)) %>%
      group_by(icd3) %>%
      summarise(n     = n(),
                label = paste0(icd3, " · ", str_sub(first(desc), nchar(first(icd3)) + 2, 50)),
                .groups = "drop") %>%
      arrange(desc(n)) %>%
      slice_head(n = 20)

    if (nrow(df) == 0) {
      return(plotly_empty() %>%
        layout(title = "Sin diagnóstico disponible\n(requiere datos GRD)"))
    }

    p <- ggplot(df, aes(x = reorder(label, n), y = n,
                        text = paste0("<b>", icd3, "</b><br>", label, "<br>N = ", n))) +
      geom_col(fill = "#E15759", color = "black", linewidth = 0.2) +
      coord_flip() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      labs(x = NULL, y = "Casos larga estancia") +
      theme_classic() + theme(axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 5 · Diagnóstico (CACI)
  # ══════════════════════════════════════════════════════════════════════════
  output$plot_caci_box <- renderPlotly({
    df <- grd_filt() %>% filter(!is.na(caci), !is.na(dif_days))
    req(nrow(df) > 5)
    cap <- quantile(df$dif_days, 0.99, na.rm = TRUE)

    p <- ggplot(df, aes(x = caci, y = dif_days, fill = caci)) +
      geom_boxplot(outlier.size = 0.7, outlier.alpha = 0.4) +
      scale_fill_manual(values = caci_colors, na.value = "grey70") +
      scale_y_continuous(limits = c(0, cap)) +
      labs(x = "CACI", y = "Días de estancia total") +
      theme_classic() + theme(legend.position = "none")

    ggplotly(p)
  })

  output$plot_caci_trend <- renderPlotly({
    df <- grd_filt() %>%
      filter(!is.na(caci), !is.na(dif_days)) %>%
      group_by(caci, año) %>%
      summarise(med = mean(dif_days, na.rm = TRUE),
                n   = n(), .groups = "drop")
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = año, y = med, color = caci, group = caci,
                        text = paste0("<b>", caci, "</b><br>",
                                      "Año: ", año, "<br>",
                                      "Media: ", round(med, 1), " d  N=", n))) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      scale_color_manual(values = caci_colors, na.value = "grey70") +
      scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), 1)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = "Año", y = "Media LOS (días)", color = "CACI") +
      theme_classic() + theme(legend.position = "bottom")

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$tabla_caci_stats <- renderReactable({
    df <- los_flagged() %>%
      filter(!is.na(caci)) %>%
      group_by(CACI = str_to_upper(caci), Servicio = estacion_2) %>%
      summarise(
        N         = n(),
        Media     = round(mean(dif_bed_serv, na.rm = TRUE), 1),
        P75       = round(quantile(dif_bed_serv, 0.75, na.rm = TRUE), 1),
        P90       = round(quantile(dif_bed_serv, 0.90, na.rm = TRUE), 1),
        `% larga` = round(mean(larga_estancia, na.rm = TRUE) * 100, 1),
        .groups   = "drop"
      )
    req(nrow(df) > 0)

    reactable(df,
      groupBy = "CACI", searchable = TRUE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 80),
      columns = list(
        CACI     = colDef(minWidth = 110, sticky = "left", align = "left",
                          aggregate = "unique",
                          grouped   = JS("function(ci){return ci.value;}")),
        Servicio = colDef(minWidth = 110, aggregate = "unique"),
        N        = colDef(aggregate = "sum"),
        Media    = colDef(aggregate = "mean", format = colFormat(digits = 1)),
        P75      = colDef(aggregate = "mean", format = colFormat(digits = 1)),
        P90      = colDef(aggregate = "mean", format = colFormat(digits = 1)),
        `% larga` = colDef(
          aggregate = "mean",
          format = colFormat(suffix = "%", digits = 1),
          style  = function(v) {
            if (!is.na(v) && v > 20) list(color = "#E15759", fontWeight = "bold")
            else if (!is.na(v) && v > 10) list(color = "#F28E2B")
            else list()
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
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
  output$kpi_financiero <- renderUI({
    df <- cost_per_cuenta() %>% filter(!is.na(caci))
    req(nrow(df) > 0)

    med_costo_dia <- mean(df$costo_dia, na.rm = TRUE)
    med_venta_dia <- mean(df$venta_dia, na.rm = TRUE)
    tot_venta     <- sum(df$venta_total, na.rm = TRUE)
    tot_margen    <- sum(df$margen, na.rm = TRUE)
    pct_rent      <- if (tot_venta > 0) round(tot_margen / tot_venta * 100, 1) else 0

    fluidRow(
      valueBox(
        value    = cop(med_costo_dia),
        subtitle = "Costo diario promedio",
        icon     = icon("dollar-sign"),
        color    = "red",
        width    = 3
      ),
      valueBox(
        value    = cop(med_venta_dia),
        subtitle = "Venta diaria promedio",
        icon     = icon("chart-line"),
        color    = "light-blue",
        width    = 3
      ),
      valueBox(
        value    = cop(tot_venta),
        subtitle = "Ventas totales",
        icon     = icon("dollar-sign"),
        color    = "blue",
        width    = 3
      ),
      valueBox(
        value    = paste0(pct_rent, "%"),
        subtitle = "Rentabilidad",
        icon     = icon("percent"),
        color    = if (pct_rent >= 50) "green" else if (pct_rent >= 30) "yellow" else "red",
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
    df <- costo_filt() %>%
      filter(!is.na(caci)) %>%
      group_by(Departamento = coalesce(departamento_cargue, "Otro"), CACI = caci) %>%
      summarise(
        Cuentas       = n_distinct(cuenta),
        `Costo total` = sum(costo, na.rm = TRUE),
        Ventas        = sum(venta, na.rm = TRUE),
        Margen        = sum(venta, na.rm = TRUE) - sum(costo, na.rm = TRUE),
        .groups       = "drop"
      ) %>%
      mutate(`Rent. (%)` = round(Margen / pmax(Ventas, 1) * 100, 1)) %>%
      arrange(desc(Ventas))
    req(nrow(df) > 0)

    reactable(df,
      groupBy = "Departamento", searchable = TRUE, pagination = FALSE,
      striped = TRUE, highlight = TRUE, bordered = TRUE,
      defaultColDef = colDef(align = "center", minWidth = 90),
      columns = list(
        Departamento = colDef(minWidth = 160, sticky = "left", align = "left",
                               aggregate = "unique",
                               grouped   = JS("function(ci){return ci.value;}")),
        CACI     = colDef(minWidth = 90, aggregate = "unique"),
        Cuentas  = colDef(aggregate = "sum"),
        `Costo total` = colDef(minWidth = 150, align = "right",
          aggregate = "sum",
          format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        Ventas = colDef(minWidth = 150, align = "right",
          aggregate = "sum",
          format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        Margen = colDef(minWidth = 150, align = "right",
          aggregate = "sum",
          format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        `Rent. (%)` = colDef(
          minWidth = 100, aggregate = "mean",
          format = colFormat(suffix = "%", digits = 1),
          style  = function(v) {
            if (!is.na(v) && v >= 50) list(color = "#59A14F", fontWeight = "bold")
            else if (!is.na(v) && v >= 30) list(color = "#F28E2B")
            else list(color = "#E15759")
          }
        )
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#2C3E50", color = "white", fontWeight = "bold")
      )
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Tab 7 · Estancias Inactivas
  # ══════════════════════════════════════════════════════════════════════════
  ei_filt <- reactive({
    df  <- data_ei
    yr  <- as.integer(input$ei_yr)
    rsp <- input$ei_resp
    if (yr  > 0)    df <- df %>% filter(año == yr)
    if (!is.null(rsp) && rsp != "0") df <- df %>% filter(responsable == rsp)
    df
  })

  output$kpi_ei <- renderUI({
    df <- ei_filt()
    req(nrow(df) > 0)
    n_con_grd  <- sum(!is.na(df$n_admisiones_grd))
    valor_tot  <- sum(df$valor_total_estancia_inactiva, na.rm = TRUE)

    fluidRow(
      valueBox(
        value    = format(nrow(df), big.mark = "."),
        subtitle = "Casos registrados",
        icon     = icon("clipboard"),
        color    = "blue",
        width    = 2
      ),
      valueBox(
        value    = format(sum(df$total_dias_de_estancia_por_ips, na.rm = TRUE), big.mark = "."),
        subtitle = "Días inactivos por IPS",
        icon     = icon("pause"),
        color    = "red",
        width    = 2
      ),
      valueBox(
        value    = cop(valor_tot),
        subtitle = "Valor total estancia inactiva",
        icon     = icon("dollar-sign"),
        color    = "orange",
        width    = 3
      ),
      valueBox(
        value    = round(mean(df$total_dias_de_estancia_por_ips, na.rm = TRUE), 1),
        subtitle = "Media días/caso (IPS)",
        icon     = icon("calculator"),
        color    = "light-blue",
        width    = 2
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
    req(nrow(df) > 0)

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
    df <- ei_filt() %>%
      group_by(año, mes_n) %>%
      summarise(casos = n(),
                dias  = sum(total_dias_de_estancia_por_ips, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(fecha = as.Date(paste0(año, "-", sprintf("%02d", mes_n), "-01"))) %>%
      arrange(fecha)
    req(nrow(df) > 0)

    max_dias  <- max(df$dias,  na.rm = TRUE)
    max_casos <- max(df$casos, na.rm = TRUE)
    escala    <- if (max_casos > 0) max_dias / max_casos else 1

    p <- ggplot(df, aes(x = fecha)) +
      geom_col(aes(y = dias, fill = "Días IPS"), alpha = 0.8) +
      geom_line(aes(y = casos * escala, color = "Casos"), linewidth = 1.2, group = 1) +
      geom_point(aes(y = casos * escala, color = "Casos"), size = 2) +
      scale_y_continuous(name     = "Total días inactivos",
                         breaks   = scales::pretty_breaks(n = 6),
                         sec.axis = sec_axis(~ . / escala, name = "Número de casos")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      scale_fill_manual(values  = c("Días IPS" = "#E15759")) +
      scale_color_manual(values = c("Casos"    = "#2C3E50")) +
      labs(x = NULL, fill = NULL, color = NULL) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom")

    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.3))
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

    grd_eps <- data_grd_base %>%
      group_by(EPS = str_to_upper(coalesce(eps, "?"))) %>%
      summarise(`Admisiones GRD` = n(),
                `LOS med. GRD`   = round(mean(dif_days, na.rm = TRUE), 1),
                .groups = "drop")

    df <- left_join(ei_eps, grd_eps, by = "EPS") %>%
      mutate(`EI / Adm. (%)` = round(`Casos EI` / pmax(`Admisiones GRD`, 1) * 100, 1)) %>%
      arrange(desc(`Días IPS`))
    req(nrow(df) > 0)

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
