################################################################################
# app.R — Mortalidad Hospitalaria Ajustada al Riesgo · DIME
# Tabs: Resumen | Análisis Anual | Tendencias | Año Actual | Características
################################################################################

source("global.R")

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "32px",
               style = "margin-right:8px; vertical-align:middle;"),
      "Mortalidad · DIME"
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem("Resumen Acumulado",     tabName = "resumen",     icon = icon("house")),
      menuItem("Análisis Anual",        tabName = "anual",       icon = icon("bar-chart")),
      menuItem("Tendencias Bimensuales",tabName = "bimensual",   icon = icon("chart-line")),
      menuItem(paste("Año", ano_actual),tabName = "actual",      icon = icon("calendar")),
      menuItem("Características Clínicas", tabName = "clinicas", icon = icon("stethoscope"))
    ),
    tags$hr(style = "border-color:rgba(255,255,255,.2); margin:8px 0;"),
    tags$div(
      style = "padding: 0 14px;",
      checkboxGroupInput("grd_sel", "Grupos diagnósticos (GRD)",
                         choices  = grd_choices,
                         selected = grd_choices),
      tags$small(
        tags$em("El filtro GRD aplica a los análisis de mortalidad ajustada (TMAR/O:E)."),
        style = "color:rgba(255,255,255,.55); display:block; margin-bottom:8px;"
      ),
      tags$hr(style = "border-color:rgba(255,255,255,.2); margin:6px 0;"),
      tags$small(
        tags$em(paste0("Datos actualizados: año ", ano_actual,
                       " | Modelos GEE entrenados excluyendo 2020–2021")),
        style = "color:rgba(255,255,255,.55);"
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
      # Tab 1 · Resumen Acumulado
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "resumen",
        fluidRow(
          valueBox(format(total_discharges_dime, big.mark = "."),
                   "Total egresos DIME", icon("hospital"),    color = "teal",       width = 2),
          valueBox(format(tot_egresos, big.mark = "."),
                   "Egresos monitorizados (8 GRD)", icon("users"), color = "blue",  width = 2),
          valueBox(format(total_mort_dime, big.mark = "."),
                   "Total muertes DIME", icon("heart"),       color = "red",        width = 2),
          valueBox(format(tot_muertes, big.mark = "."),
                   "Muertes observadas (GRD)", icon("heart-pulse"), color = "orange", width = 2),
          valueBox(paste0(tasa_bruta_dime, "%"),
                   "Tasa bruta DIME (%)", icon("percent"),    color = "light-blue", width = 2),
          valueBox(paste0(tasa_bruta_grd, "%"),
                   "Tasa bruta GRD (%)", icon("percent"),     color = "purple",     width = 2)
        ),
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Distribución de egresos por GRD"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_dist_acum", height = "320px")),
          box(title = tagList(icon("chart-line"), " Mortalidad bruta anual por GRD"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_trend_anual", height = "320px"))
        ),
        fluidRow(
          box(title = tagList(icon("table"), " Resumen acumulado por GRD"),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_resumen_acum"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 2 · Análisis Anual
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "anual",
        fluidRow(
          box(title = tagList(icon("scale-balanced"), " Razón Observado/Esperado (O/E) por año"),
              solidHeader = TRUE, status = "primary", width = 7,
              plotlyOutput("plot_oe_anual", height = "380px")),
          box(title = tagList(icon("chart-line"), " TMAR anual por GRD"),
              solidHeader = TRUE, status = "primary", width = 5,
              plotlyOutput("plot_tmar_anual", height = "380px"))
        ),
        fluidRow(
          box(title = tagList(icon("table"), " Tabla detallada anual"),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_anual_detalle"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 3 · Tendencias Bimensuales
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "bimensual",
        fluidRow(
          box(title = tagList(icon("chart-area"),
                              " Evolución TMAR bimensual por grupo diagnóstico (IC 95%)"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_bimensual", height = "640px"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 4 · Año Actual
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "actual",
        fluidRow(
          valueBox(format(act_egresos_dime, big.mark = "."),
                   paste("Egresos", ano_actual, "(DIME)"),     icon("hospital"),     color = "teal",   width = 2),
          valueBox(format(act_egresos, big.mark = "."),
                   paste("Egresos", ano_actual, "(8 GRD)"),    icon("users"),        color = "blue",   width = 2),
          valueBox(format(act_mort_dime, big.mark = "."),
                   paste("Muertes", ano_actual, "(DIME)"),     icon("heart"),        color = "red",    width = 2),
          valueBox(format(act_muertes, big.mark = "."),
                   paste("Muertes observadas", ano_actual),    icon("heart-pulse"),  color = "orange", width = 2),
          valueBox(format(act_exp, big.mark = "."),
                   paste("Muertes esperadas", ano_actual),     icon("calculator"),   color = "light-blue", width = 2),
          valueBox(format(act_tmar, big.mark = "."),
                   paste("TMAR promedio x100,", ano_actual),   icon("activity"),
                   color = if (identical(tmar_color, "danger")) "red" else "green",  width = 2)
        ),
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Muertes observadas por mes y GRD"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_mes_obs", height = "340px")),
          box(title = tagList(icon("chart-line"), " TMAR mensual con IC 95%"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_tmar_mes", height = "340px"))
        ),
        fluidRow(
          box(title = tagList(icon("table"), paste(" Detalle mensual", ano_actual, "por GRD")),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_mes_act"))
        )
      ),

      # ════════════════════════════════════════════════════════════════════════
      # Tab 5 · Características Clínicas
      # ════════════════════════════════════════════════════════════════════════
      tabItem(tabName = "clinicas",
        fluidRow(
          box(title = tagList(icon("users"), " Perfil demográfico por GRD"),
              solidHeader = TRUE, status = "primary", width = 12,
              DTOutput("tabla_demo"))
        ),
        fluidRow(
          box(title = tagList(icon("chart-bar"), " Distribución de edad por GRD"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_violin", height = "380px")),
          box(title = tagList(icon("grip"), " Prevalencia de comorbilidades en fallecidos"),
              solidHeader = TRUE, status = "primary", width = 6,
              plotlyOutput("plot_comorb", height = "380px"))
        ),
        fluidRow(
          box(title = tagList(icon("hospital"), " Mortalidad por departamento"),
              solidHeader = TRUE, status = "primary", width = 12,
              plotlyOutput("plot_dept", height = "340px"))
        )
      )

    ) # tabItems
  )   # dashboardBody
)     # dashboardPage

# ══════════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  grds <- reactive({
    req(length(input$grd_sel) > 0)
    input$grd_sel
  })

  # ── Tab 1: Resumen Acumulado ───────────────────────────────────────────────
  output$plot_dist_acum <- renderPlotly({
    df <- dist_acum %>% filter(GRD %in% grds())
    plot_ly(df,
            x = ~reorder(GRD, n), y = ~n,
            type  = "bar",
            color = ~GRD, colors = pal,
            text  = ~paste0(format(n, big.mark = "."), "<br>(", Pct, "%)"),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Grupo Diagnóstico"),
             yaxis = list(title = "Número de egresos"),
             plot_bgcolor  = "white", paper_bgcolor = "white",
             showlegend = FALSE)
  })

  output$plot_trend_anual <- renderPlotly({
    df <- trend_anual %>% filter(GRD %in% grds())
    p <- ggplot(df, aes(x = año, y = Tasa, color = GRD, group = GRD,
                        text = paste0("GRD: ", GRD, "<br>Año: ", año,
                                      "<br>Tasa: ", Tasa, "%",
                                      "<br>Muertes: ", Muertes,
                                      "<br>Egresos: ", Egresos))) +
      geom_line(linewidth = 0.9) + geom_point(size = 2) +
      scale_color_manual(values = pal) +
      scale_x_continuous(breaks = sort(unique(df$año))) +
      theme_classic() +
      labs(x = "Año", y = "Tasa de mortalidad bruta (%)", color = "GRD") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.35))
  })

  output$tabla_resumen_acum <- renderDT({
    df <- tbl_anual %>%
      filter(GRD %in% grds(), !año == "2016") %>%
      group_by(GRD) %>%
      summarise(
        `Total egresos`      = sum(Pacientes),
        `Muertes observadas` = sum(Observado),
        `Muertes esperadas`  = round(sum(Esperado), 1),
        `Tasa bruta (%)`     = round(sum(Observado) / sum(Pacientes) * 100, 2),
        `TMAR promedio`      = round(mean(TMAR, na.rm = TRUE), 2),
        `Razón O/E`          = round(sum(Observado) / sum(Esperado), 2),
        .groups = "drop"
      ) %>% arrange(desc(`Muertes observadas`))

    datatable(df, rownames = FALSE,
              options = list(pageLength = 10, dom = "t", scrollX = TRUE),
              class   = "compact stripe hover") %>%
      formatStyle("TMAR promedio",
                  background = styleColorBar(range(df$`TMAR promedio`, na.rm = TRUE), "#f4a261"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Razón O/E",
                  backgroundColor = styleInterval(1, values = c("#d4edda", "#f8d7da")))
  })

  # ── Tab 2: Análisis Anual ──────────────────────────────────────────────────
  output$plot_oe_anual <- renderPlotly({
    df <- tbl_anual %>% filter(GRD %in% grds(), !is.na(Razon_OE))
    p <- ggplot(df, aes(x = año, y = Razon_OE, color = GRD, group = GRD,
                        text = paste0("GRD: ", GRD, "<br>Año: ", año,
                                      "<br>O/E: ", Razon_OE,
                                      "<br>Obs: ", Observado, " | Esp: ", Esperado))) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
      geom_line(linewidth = 0.9) + geom_point(size = 2.5) +
      scale_color_manual(values = pal) +
      scale_x_continuous(breaks = sort(unique(df$año))) +
      theme_classic() +
      labs(x = "Año", y = "Razón O/E (SMR)", color = "Patología") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.35), hovermode = "closest")
  })

  output$plot_tmar_anual <- renderPlotly({
    df <- tbl_anual %>% filter(GRD %in% grds(), !is.na(TMAR))
    p <- ggplot(df, aes(x = año, y = TMAR, color = GRD, group = GRD,
                        text = paste0("GRD: ", GRD, "<br>Año: ", año,
                                      "<br>TMAR: ", TMAR, "<br>IC 95%: [", LI, " – ", LS, "]"))) +
      geom_ribbon(aes(ymin = LI, ymax = LS, fill = GRD), alpha = 0.15, color = NA) +
      geom_line(linewidth = 0.9) + geom_point(size = 2) +
      scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
      scale_x_continuous(breaks = sort(unique(df$año))) +
      theme_classic() +
      labs(x = "Año", y = "TMAR (x 100 egresos)", color = "GRD") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.35))
  })

  output$tabla_anual_detalle <- renderDT({
    df <- tbl_anual %>%
      filter(GRD %in% grds(), !año == "2016") %>%
      select(GRD, Año = año, Pacientes, Observado, Esperado,
             `Tasa (%)` = Tasa, TMAR, `LI 95%` = LI, `LS 95%` = LS,
             `Razón O/E` = Razon_OE, `T. referencia` = tasa_ref) %>%
      arrange(GRD, Año)

    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 12, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-center", targets = "_all"))),
              class = "compact stripe hover") %>%
      formatStyle("Razón O/E",
                  backgroundColor = styleInterval(1, values = c("#d4edda", "#f8d7da"))) %>%
      formatStyle("TMAR",
                  background = styleColorBar(range(tbl_anual$TMAR, na.rm = TRUE), "#f4a261"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ── Tab 3: Tendencias Bimensuales ─────────────────────────────────────────
  output$plot_bimensual <- renderPlotly({
    df <- df_bim %>% filter(GRD %in% grds(), !is.na(RSMR))
    p <- ggplot(df, aes(x = mes_2)) +
      geom_ribbon(aes(ymin = li_RSMR, ymax = ls_RSMR, fill = "IC (95%)"), alpha = 0.3) +
      geom_line(aes(y = RSMR, color = "TMAR"), linewidth = 0.8) +
      geom_point(aes(y = RSMR, color = "TMAR",
                     text = paste0("GRD: ", GRD,
                                   "<br>Período: ", format(mes_2, "%b %Y"),
                                   "<br>TMAR: ", round(RSMR, 2),
                                   "<br>IC: [", round(li_RSMR, 2), " – ", round(ls_RSMR, 2), "]",
                                   "<br>Obs: ", obs, " / Esp: ", round(exp, 1))),
                 size = 1.8) +
      geom_hline(aes(yintercept = tasa_ref), linetype = "dashed",
                 color = "red", linewidth = 0.6) +
      facet_wrap(~ GRD, scales = "free_y", ncol = 2) +
      scale_color_manual(values = c("TMAR" = "black")) +
      scale_fill_manual(values  = c("IC (95%)" = "gray50")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
      theme_classic() +
      labs(title    = "Mortalidad Hospitalaria Ajustada al Riesgo — Tendencia Bimensual",
           subtitle = "Modelos GEE entrenados excluyendo 2020–2021 | Línea roja = tasa de referencia",
           x = NULL, y = "TMAR (x 100 egresos)") +
      theme(axis.text.x      = element_text(angle = 90, vjust = 0.5, size = 8),
            strip.text        = element_text(face = "bold", size = 11),
            strip.background  = element_rect(fill = "lightgray", color = "black"),
            legend.position   = "none",
            plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle     = element_text(size = 10, hjust = 0.5, color = "gray40"))
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = -0.05))
  })

  # ── Tab 4: Año Actual ──────────────────────────────────────────────────────
  output$plot_mes_obs <- renderPlotly({
    df <- df_mes_act %>% filter(GRD %in% grds(), !is.na(mes_label))
    if (nrow(df) == 0) return(plotly_empty())
    p <- ggplot(df, aes(x = mes_label, y = Observado, fill = GRD,
                        text = paste0("GRD: ", GRD, "<br>Mes: ", mes_label,
                                      "<br>Muertes: ", Observado,
                                      "<br>Egresos: ", Pacientes,
                                      "<br>Tasa: ", Tasa, "%"))) +
      geom_col(position = "dodge", color = "black", alpha = 0.9) +
      scale_fill_manual(values = pal) +
      theme_classic() +
      labs(x = NULL, y = "Muertes observadas", fill = "GRD") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.3))
  })

  output$plot_tmar_mes <- renderPlotly({
    df <- df_mes_act %>%
      filter(GRD %in% grds(), !is.na(TMAR)) %>%
      mutate(
        li_TMAR = round(if_else(Observado > 0,
                    (Observado / Esperado) *
                      ((1 - 1/(9*Observado) - 1.96/(3*sqrt(Observado)))^3) * tasa_ref,
                    NA_real_), 2),
        ls_TMAR = round(if_else(Observado > 0,
                    ((Observado + 1) / Esperado) *
                      ((1 - 1/(9*(Observado+1)) + 1.96/(3*sqrt(Observado+1)))^3) * tasa_ref,
                    NA_real_), 2)
      )
    if (nrow(df) == 0) return(plotly_empty())
    p <- ggplot(df, aes(x = mes_label, y = TMAR, group = GRD, color = GRD)) +
      geom_ribbon(aes(ymin = li_TMAR, ymax = ls_TMAR, fill = GRD), alpha = 0.15, color = NA) +
      geom_line(linewidth = 0.9) +
      geom_point(aes(text = paste0("GRD: ", GRD, "<br>Mes: ", mes_label,
                                   "<br>TMAR: ", TMAR,
                                   "<br>IC: [", li_TMAR, " – ", ls_TMAR, "]")), size = 2.5) +
      geom_hline(aes(yintercept = tasa_ref, color = GRD),
                 linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
      scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
      theme_classic() +
      labs(x = NULL, y = "TMAR (x 100 egresos)", color = "GRD") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.3))
  })

  output$tabla_mes_act <- renderDT({
    df <- df_mes_act %>%
      filter(GRD %in% grds()) %>%
      mutate(`Razón O/E` = round(Observado / Esperado, 2)) %>%
      select(Mes = mes_label, GRD, Pacientes, Observado, Esperado,
             `Tasa (%)` = Tasa, TMAR, `Razón O/E`) %>%
      arrange(GRD, Mes)
    if (nrow(df) == 0) return(datatable(tibble()))
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 12, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-center", targets = "_all"))),
              class = "compact stripe hover") %>%
      formatStyle("Razón O/E",
                  backgroundColor = styleInterval(1, values = c("#d4edda", "#f8d7da"))) %>%
      formatStyle("TMAR",
                  background = styleColorBar(range(tbl_anual$TMAR, na.rm = TRUE), "#f4a261"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ── Tab 5: Características Clínicas ───────────────────────────────────────
  output$tabla_demo <- renderDT({
    df <- demo_year %>% filter(GRD %in% grds())
    tasa_max <- max(df$`Tasa (%)`, na.rm = TRUE)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(scrollX = TRUE, pageLength = 8, dom = "tip",
                             columnDefs = list(list(className = "dt-center", targets = "_all"))),
              class = "compact stripe hover") %>%
      formatStyle("Tasa (%)",
                  background = styleColorBar(c(0, tasa_max), "#f8d7da"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("% Hombres",
                  background = styleColorBar(c(0, 100), "#cce5ff"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("% Mujeres",
                  background = styleColorBar(c(0, 100), "#fce4ec"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$plot_violin <- renderPlotly({
    df <- box_data %>% filter(GRD %in% grds())
    plot_ly(df, x = ~GRD, y = ~edad, color = ~GRD, colors = pal,
            type = "violin", box = list(visible = TRUE),
            points = FALSE, meanline = list(visible = TRUE)) %>%
      layout(xaxis = list(title = "GRD"), yaxis = list(title = "Edad (años)"),
             plot_bgcolor = "white", paper_bgcolor = "white", showlegend = FALSE)
  })

  output$plot_comorb <- renderPlotly({
    if (nrow(comorb_data) == 0) {
      plotly_empty(type = "scatter") %>% layout(title = "Datos Elixhauser no disponibles")
    } else {
      df <- comorb_data %>% filter(GRD %in% grds())
      p <- ggplot(df, aes(x = GRD, y = Comorbilidad, fill = Prevalencia,
                           text = paste0(Comorbilidad, " | ", GRD,
                                         "<br>Prevalencia en fallecidos: ", Prevalencia, "%"))) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient(low = "#e8f4fd", high = "#1565c0", name = "%") +
        theme_minimal() +
        labs(x = "GRD", y = NULL) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      ggplotly(p, tooltip = "text")
    }
  })

  output$plot_dept <- renderPlotly({
    plot_ly(dept_mort,
            x = ~Tasa, y = ~reorder(Departamento, Tasa),
            type = "bar", orientation = "h",
            marker = list(color = "#00BFC4"),
            text  = ~paste0(Tasa, "% (n=", Egresos, ")"),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Tasa de mortalidad (%)"),
             yaxis = list(title = NULL),
             plot_bgcolor = "white", paper_bgcolor = "white")
  })

}

shinyApp(ui, server)
