pacman::p_load(tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms, 
               epikit, scales, gt, zoo, readxl, here)

##### Analysis process about CACI and impact in DIME #########################################################################

### Total costo CACI mes
data_costo_total_3 <- import("data_costo_total_3_2025_II.rds")

################################## Database to planning team. ###################################################################
data_plan_grd <- data_grd_6 %>% 
  select(tipo_doc, documento, paciente, edad, sexo, eps, fecha_ingreso, numero_de_ingreso, caci_3) 


export(data_plan_grd, here("Database of procedure and check it up", "data_plan_grd_sep_2025.xlsx"))

########################### Analysis, outputs and final outcomes ##########################################################

tabla_ordenes_costo_total <- data_costo_total_3 %>%
  mutate(mes_año = as.Date(fecha_cargue, format = "%b-%Y")) %>% 
  filter(año == "2025") %>% 
  #mutate(costo = replace_na(costo, 0)) %>% 
  group_by(caci_3, mes_cargue) %>% 
  summarise(costo_orden = sum(costo_2),
            venta = sum(venta),
            pacientes = n_distinct(identificacion),
            promedio = mean(costo_2), .groups = "drop") %>% 
  flextable()

tabla_ordenes_costo_total


### Columna de total para todos los CACI
data_costo_total_4 <- data_costo_total_3 %>% 
  filter(año == "2025") %>% 
  group_by(identificacion, mes_cargue, año, caci) %>% 
  summarise(costo = sum(costo), 
            venta = sum(venta), .groups = "drop")

data_caci_total <- data_costo_total_3 %>% 
  filter(año == "2025") %>% 
  group_by(identificacion, mes_cargue) %>% 
  summarise(Total = sum(costo), .groups = "drop")

data_caci_total_2 <- data_caci_total %>% 
  group_by(mes_cargue) %>% 
  summarise(Total = median(Total), .groups = "drop")

### Tabla general de costo medio paciente.
data_costo_total_5 <- data_costo_total_4 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_orden = sum(costo),
            pacientes = n_distinct(identificacion),
            promedio = costo_orden/pacientes,
            mediana = median(costo), .groups = "drop")  %>% 
  pivot_wider(id_cols = mes_cargue, names_from  = caci, 
              values_from = c(pacientes, mediana, costo_orden)) %>% 
  select(mes_cargue,
         pacientes_ACV, mediana_ACV, costo_orden_ACV,
         pacientes_SCA, mediana_SCA, costo_orden_SCA, 
         pacientes_ICC, mediana_ICC, costo_orden_ICC,
         pacientes_TEP, mediana_TEP, costo_orden_TEP,
         pacientes_TXC, mediana_TXC, costo_orden_TXC) 

### Datos requeridos para el informe de gestión en la parte inicial
data_costo_total_4 %>% 
  #filter(mes_cargue %in% c("oct", "nov", "dic")) %>% 
  group_by(mes_cargue) %>% 
  summarise(Mediana = median(costo),
            RIQ_I = quantile(costo, 0.25),
            RIQ_S = quantile(costo, 0.75),
            Pacientes = n_distinct(identificacion),
            Total = sum(costo)) %>% 
  rename("Mes" = mes_cargue) %>% 
  flextable() %>% 
  align(align = "center", j = c(2:6), part = "all")%>% 
  bold(i = 1, bold = TRUE, part = "header") 

### Total data for 
data_costo_total_4 %>% 
  summarise(median(costo),
            riq_l = quantile(costo, 0.25),
            riq_u = quantile(costo, 0.75))

### Unión con los totales de la tabla
data_costo_total_5 <- full_join(data_costo_total_5, data_caci_total_2)

### Tabla general de costo medio paciente.
border_style = officer::fp_border(color="black", width=1)
data_costo_total_5 %>% 
  mutate(across(,replace_na,0)) %>% 
  bind_rows(summarize(., mes_cargue = "Total", 
                      pacientes_ACV = sum(pacientes_ACV),
                      mediana_ACV = median(mediana_ACV), 
                      costo_orden_ACV = sum(costo_orden_ACV),
                      pacientes_SCA = sum(pacientes_SCA),
                      mediana_SCA = median(mediana_SCA), 
                      costo_orden_SCA = sum(costo_orden_SCA),
                      pacientes_ICC = sum(pacientes_ICC),
                      mediana_ICC = median(mediana_ICC), 
                      costo_orden_ICC = sum(costo_orden_ICC),
                      pacientes_TEP = sum(pacientes_TEP),
                      mediana_TEP = median(mediana_TEP), 
                      costo_orden_TEP = sum(costo_orden_TEP),
                      pacientes_TXC = sum(pacientes_TXC),
                      mediana_TXC = median(mediana_TXC), 
                      costo_orden_TXC = sum(costo_orden_TXC),
                      Total = median(Total))) %>%
  flextable() %>% 
  add_header_row( top = TRUE,   # La nueva cabecera va encima de la fila de cabecera existente
                  values = c("Mes",     # Valores de cabecera para cada columna a continuación
                             "ACV", "", "",
                             "SCA", "", "",    # Este será el encabezado de nivel superior para esta columna y las dos siguientes
                             "ICC","", "",
                             "TEP", "", "",
                             "Trasplante", "", "",
                             "Costo medio total")) %>% 
  set_header_labels(mes_cargue = "",
                    pacientes_ACV = "Pte", mediana_ACV = "Costo medio", costo_orden_ACV = "Costo total",
                    pacientes_SCA = "Pte", mediana_SCA = "Costo medio", costo_orden_SCA = "Costo total",
                    pacientes_ICC = "Pte", mediana_ICC = "Costo medio", costo_orden_ICC = "Costo total",
                    pacientes_TEP = "Pte", mediana_TEP = "Costo medio", costo_orden_TEP = "Costo total",
                    pacientes_TXC = "Pte", mediana_TXC = "Costo medio", costo_orden_TXC = "Costo total",
                    Total = "") %>% 
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
  merge_at(i = 1, j = 11:13, part = "header") %>% 
  merge_at(i = 1, j = 14:16, part = "header") %>% 
  theme_booktabs() %>% 
  vline(part = "all", j = 1, border = border_style) %>% 
  vline(part = "all", j = 4, border = border_style) %>% 
  vline(part = "all", j = 7, border = border_style) %>% 
  vline(part = "all", j = 10, border = border_style) %>% 
  vline(part = "all", j = 13, border = border_style) %>% 
  vline(part = "all", j = 16, border = border_style) %>% 
  hline(i = 1, part = "header", border = border_style) %>% 
  align(align = "center", j = c(2:17), part = "all") %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j =1, part = "header") %>% 
  merge_at(i = 1:2, j =17, part = "header") %>% 
  #flextable::color(color = "white" ,i = 13, j = c(3,6,9,12), part = "body") %>% 
  flextable::set_caption(caption = as_paragraph(
    as_chunk("Tabla costo medio paciente por CACI, DIME, año 2025", 
             props = fp_text_default(bold = TRUE, font.size = 14)))) %>% 
  footnote(i =2, j=c(3,6,9,11), part = "header", value = as_paragraph(value ="Costo medio corresponde a la mediana de los costos paciente"))

#########################################################################################################################################################################

### tabla para exportar costo medio paciente
tabla_costo_medio_2025 <-  data_costo_total_5 %>% 
  mutate(across(,replace_na,0)) %>% 
  bind_rows(summarize(., mes_cargue = "Total", 
                      pacientes_ACV = sum(pacientes_ACV),
                      mediana_ACV = median(mediana_ACV), 
                      costo_orden_ACV = sum(costo_orden_ACV),
                      pacientes_SCA = sum(pacientes_SCA),
                      mediana_SCA = median(mediana_SCA), 
                      costo_orden_SCA = sum(costo_orden_SCA),
                      pacientes_ICC = sum(pacientes_ICC),
                      mediana_ICC = median(mediana_ICC), 
                      costo_orden_ICC = sum(costo_orden_ICC),
                      pacientes_TEP = sum(pacientes_TEP),
                      mediana_TEP = median(mediana_TEP), 
                      costo_orden_TEP = sum(costo_orden_TEP),
                      pacientes_TXC = sum(pacientes_TXC),
                      mediana_TXC = median(mediana_TXC), 
                      costo_orden_TXC = sum(costo_orden_TXC),
                      Total = median(Total))) %>%
  export("tabla costo medio paciente 2025.xlsx")

### Tabla de costo, venta y rentabilidad
data_costo_total_6 <- data_costo_total_4 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_orden = sum(costo),
            venta = sum(venta), .groups = "drop")  %>% 
  mutate(rentabilidad = venta - costo_orden)%>% 
  mutate(rentabilidad = as.numeric(rentabilidad)) %>% 
  mutate(rentabilidad_2 = round(rentabilidad/venta*100, digits = 2)) %>% 
  pivot_wider(id_cols = mes_cargue, names_from  = caci, 
              values_from = c(costo_orden, venta, rentabilidad, rentabilidad_2)) %>% 
  select(mes_cargue,
         costo_orden_ACV, venta_ACV, rentabilidad_ACV, rentabilidad_2_ACV, 
         costo_orden_SCA, venta_SCA, rentabilidad_SCA, rentabilidad_2_SCA,
         costo_orden_ICC, venta_ICC, rentabilidad_ICC, rentabilidad_2_ICC,
         costo_orden_TEP, venta_TEP, rentabilidad_TEP, rentabilidad_2_TEP,
         costo_orden_TXC, venta_TXC, rentabilidad_TXC, rentabilidad_2_TXC) %>% 
  # adorn_percentages(starts_with("rentabilidad"), denominator = "col",  na.rm = T) %>% 
  # adorn_pct_formatting(,,,c(rentabilidad_ACV, rentabilidad_SCA,
  #                          rentabilidad_ICC, rentabilidad_TxC)) %>% 
  #adorn_ns(position = "front",,,c(rentabilidad_ACV, rentabilidad_SCA,
  #                               rentabilidad_ICC, rentabilidad_TxC)) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  bind_rows(summarize(., mes_cargue = "Total", 
                      costo_orden_ACV = sum(costo_orden_ACV),
                      venta_ACV = sum(venta_ACV), 
                      rentabilidad_ACV = sum(venta_ACV) - sum(costo_orden_ACV),
                      rentabilidad_2_ACV = round(mean(rentabilidad_2_ACV), digits = 2),
                      costo_orden_SCA = sum(costo_orden_SCA),
                      venta_SCA = sum(venta_SCA), 
                      rentabilidad_SCA = sum(venta_SCA) - sum(costo_orden_SCA),
                      rentabilidad_2_SCA = round(mean(rentabilidad_2_SCA), digits = 2),
                      costo_orden_ICC = sum(costo_orden_ICC),
                      venta_ICC = sum(venta_ICC),
                      rentabilidad_ICC = sum(venta_ICC) - sum(costo_orden_ICC),
                      rentabilidad_2_ICC = round(mean(rentabilidad_2_ICC), digits = 2),
                      costo_orden_TEP = sum(costo_orden_TEP),
                      venta_TEP = sum(venta_TEP),
                      rentabilidad_TEP = sum(venta_TEP) - sum(costo_orden_TEP),
                      rentabilidad_2_TEP = round(mean(rentabilidad_2_TEP), digits = 2),
                      costo_orden_TXC = sum(costo_orden_TXC),
                      venta_TXC = sum(venta_TXC),
                      rentabilidad_TXC = sum(venta_TXC) - sum(costo_orden_TXC),
                      rentabilidad_2_TXC = round(mean(rentabilidad_2_TXC), digits = 2)))



tabla_rentabilidad_2025 <- data_costo_total_6 %>%
  flextable() %>% 
  add_header_row(
    top = TRUE,
    values = c("Mes", "ACV", "", "", "", "SCA", "", "","", "ICC", "", "","","TEP", "", "","", "Trasplante", "", "","")
  ) %>%
  set_header_labels(
    mes_cargue = "",
    costo_orden_ACV = "Costo", venta_ACV = "Venta", rentabilidad_ACV = "Rentabilidad", rentabilidad_2_ACV = "%",
    costo_orden_SCA = "Costo", venta_SCA = "Venta", rentabilidad_SCA = "Rentabilidad", rentabilidad_2_SCA = "%",
    costo_orden_ICC = "Costo", venta_ICC = "Venta", rentabilidad_ICC = "Rentabilidad", rentabilidad_2_ICC = "%",
    costo_orden_TEP = "Costo", venta_TEP = "Venta", rentabilidad_TEP = "Rentabilidad", rentabilidad_2_TEP = "%",
    costo_orden_TXC = "Costo", venta_TXC = "Venta", rentabilidad_TXC = "Rentabilidad", rentabilidad_2_TXC = "%"
  ) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  merge_at(i = 1, j = 6:9, part = "header") %>%
  merge_at(i = 1, j = 10:13, part = "header") %>%
  merge_at(i = 1, j = 14:17, part = "header") %>%
  merge_at(i = 1, j = 18:21, part = "header") %>%
  theme_booktabs() %>%
  vline(j = c(1, 5, 9, 13, 17), border = border_style, part = "all") %>%
  hline(i = 1, part = "header", border = border_style) %>%
  hline(i = 9, part = "body", border = border_style) %>%
  align(align = "center", j = 2:21, part = "all") %>%
  bold(i = 1, part = "header") %>%
  bold(i = nrow(data_costo_total_6), part = "body") %>%
  # merge_at(i = 1:2, j = 1, part = "header") %>%
  set_caption(as_paragraph(
    as_chunk("Rentabilidad por mes de los GRD/CACI, DIME, año 2025", 
             props = fp_text_default(bold = TRUE, font.size = 14)))) %>%
  footnote(
    i = 2, j = c(3, 6, 9), part = "header",
    value = as_paragraph("Costo medio corresponde a la mediana de los costos paciente")
  )

tabla_rentabilidad_2025 <- tabla_rentabilidad_2025 %>%
  width(j = 1, width = 1.0) %>%
  width(j = 2:17, width = 1.1) %>%
  fontsize(size = 9, part = "all") %>%
  set_table_properties(width = 1, layout = "autofit")

tabla_rentabilidad_2025
############################################################################################################################

#### Procedures evaluation in quantity by month and CACI
qx <- data_grd_2 %>%
  filter(año == "2025" & !is.na(procedimiento_qx)) %>% 
  group_by(mes, caci_3, procedimiento_qx) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(n = replace_na(n, 0)) %>% 
  pivot_wider(id_cols = c(procedimiento_qx, caci_3), names_from = mes, values_from = n) %>% 
  mutate(across(everything(), ~ replace_na(., 0))) %>% 
  flextable()

#### Figure/graph median distribution cost by CACI. 2025. 

# Step 1: summarize at order level per month (not across caci yet)
monthly_by_caci <- data_costo_total_3 %>%
  filter(año == "2025") %>%
  mutate(mes_cargue_2 = floor_date(fecha_cargue, "month")) 

# Step 2: for each caci_3 and month, compute median & IQR of order-level costo_2
monthly_summary <- monthly_by_caci %>%
  group_by(mes_cargue_2, caci_3) %>%
  summarise(
    median_costo = median(costo_2, na.rm = TRUE),
    low_ci = quantile(costo_2, 0.25, na.rm = TRUE),
    high_ci = quantile(costo_2, 0.75, na.rm = TRUE),
    total_costo = sum(costo_2, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Plot faceted by caci_3
ggplot(monthly_summary, aes(x = mes_cargue_2)) +
  geom_ribbon(aes(ymin = low_ci, ymax = high_ci), fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = median_costo), size = 1.1, color = "#1F77B4") +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    n.breaks = 10,
    labels = scales::label_dollar(big.mark = ".", decimal.mark = ",", suffix = "$")
  ) +
  labs(
    x = "Mes",
    y = "Costo",
    title = "Evolución mensual del costo mediano con rango intercuartílico (IQR) por CACI — 2025"
  ) +
  facet_wrap(~ caci_3, scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

####################### length stay time in-hospital patient by CACI #####################################################

time_total <- data_grd_6 %>% 
  filter(año == "2025") %>% 
  group_by(mes) %>% 
  summarise(Total = median(dif_days), .groups = "drop") 

time <- data_grd_6 %>% 
  filter(año == "2025") %>% 
  group_by(caci_3, mes, año) %>% 
  summarise(median = median(dif_days),
            low_ci = quantile(dif_days, 0.25),
            upper_ci = quantile(dif_days, 0.75), .groups = "drop") %>% 
  pivot_wider(id_cols = c(mes, año), names_from = caci_3, 
              values_from = c(median, low_ci, upper_ci))

time_2 <- time %>% 
  select(mes, año, median_ACV, low_ci_ACV, upper_ci_ACV, 
         median_ICC, low_ci_ICC, upper_ci_ICC, 
         median_SCA, low_ci_SCA, upper_ci_SCA,
         median_TXC, low_ci_TXC, upper_ci_TXC) 

export(time_2, "dias_estancia_2025.xlsx")
  
time_2 <- full_join(time_2, time_total)

time_2 %>% 
  mutate(across(,replace_na,0)) %>% 
  bind_rows(summarize(., mes = "Total", 
                      median_ACV = sum(pacientes_ACV),
                      low_ci_ACV = median(mediana_ACV), 
                      upper_ci_ACV = sum(costo_orden_ACV),
                      median_SCA = sum(pacientes_SCA),
                      low_ci_SCA = median(mediana_SCA), 
                      upper_ci_SCA = sum(costo_orden_SCA),
                      median_ICC = sum(pacientes_ICC),
                      low_ci_ICC = median(mediana_ICC), 
                      upper_ci_ICC = sum(costo_orden_ICC),
                      pacientes_TEP = sum(pacientes_TEP),
                      #mediana_TEP = median(mediana_TEP), 
                      #costo_orden_TEP = sum(costo_orden_TEP),
                      #pacientes_TXC = sum(pacientes_TXC),
                      mediana_TXC = median(mediana_TXC), 
                      costo_orden_TXC = sum(costo_orden_TXC),
                      Total = median(Total))) %>%
  flextable() %>% 
  add_header_row( top = TRUE,   # La nueva cabecera va encima de la fila de cabecera existente
                  values = c("Mes",     # Valores de cabecera para cada columna a continuación
                             "ACV", "", "",
                             "SCA", "", "",    # Este será el encabezado de nivel superior para esta columna y las dos siguientes
                             "ICC","", "",
                             "TEP", "", "",
                             "Trasplante", "", "",
                             "Costo medio total")) %>% 
  set_header_labels(mes_cargue = "",
                    pacientes_ACV = "Pte", mediana_ACV = "Costo medio", costo_orden_ACV = "Costo total",
                    pacientes_SCA = "Pte", mediana_SCA = "Costo medio", costo_orden_SCA = "Costo total",
                    pacientes_ICC = "Pte", mediana_ICC = "Costo medio", costo_orden_ICC = "Costo total",
                    pacientes_TEP = "Pte", mediana_TEP = "Costo medio", costo_orden_TEP = "Costo total",
                    pacientes_TXC = "Pte", mediana_TXC = "Costo medio", costo_orden_TXC = "Costo total",
                    Total = "") %>% 
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
  merge_at(i = 1, j = 11:13, part = "header") %>% 
  merge_at(i = 1, j = 14:16, part = "header") %>% 
  theme_booktabs() %>% 
  vline(part = "all", j = 1, border = border_style) %>% 
  vline(part = "all", j = 4, border = border_style) %>% 
  vline(part = "all", j = 7, border = border_style) %>% 
  vline(part = "all", j = 10, border = border_style) %>% 
  vline(part = "all", j = 13, border = border_style) %>% 
  vline(part = "all", j = 16, border = border_style) %>% 
  hline(i = 1, part = "header", border = border_style) %>% 
  align(align = "center", j = c(2:17), part = "all") %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j =1, part = "header") %>% 
  merge_at(i = 1:2, j =17, part = "header") %>% 
  #flextable::color(color = "white" ,i = 13, j = c(3,6,9,12), part = "body") %>% 
  flextable::set_caption(caption = as_paragraph(
    as_chunk("Tabla costo medio paciente por CACI, DIME, año 2025", 
             props = fp_text_default(bold = TRUE, font.size = 14)))) %>% 
  footnote(i =2, j=c(3,6,9,11), part = "header", value = as_paragraph(value ="Costo medio corresponde a la mediana de los costos paciente"))

###################################### Graphics distribution values_from = ###################################### Graphics distribution inpatient days ###############################

library(RColorBrewer)

data_occupation <- data_costo_total_3 %>% 
  distinct(fecha_cargue, departamento_cargue, caci_3, cuenta, .keep_all = T) %>% 
  mutate(departamento_cargue = case_when(
    departamento_cargue == "UCI" ~ "UCI",
    str_detect(departamento_cargue, "UCIN") ~ "UCIN",
    str_detect(departamento_cargue, "HOSPITALIZACION") ~ "HOSPITALIZACION", 
    departamento_cargue == "URGENCIAS" ~ "URGENCIAS",
    TRUE ~ NA_character_
  )) %>% 
  group_by(fecha_cargue, caci_3, departamento_cargue) %>% 
  summarise(n = n_distinct(cuenta), .groups = "drop") %>% 
  filter(str_detect(departamento_cargue, "HOSPITALIZA|UCI|URG"))

data_occupation_2 <- data_occupation %>% 
  mutate(month = floor_date(fecha_cargue, unit = "month")) %>% 
  group_by(month, caci_3, departamento_cargue) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  mutate(beds = case_when(
    departamento_cargue == "UCI" ~ 12,
    departamento_cargue == "UCIN" ~ 14,
    departamento_cargue == "HOSPITALIZACION" ~ 22, 
    departamento_cargue == "URGENCIAS" ~ 4,
    TRUE ~ NA_real_
  )) %>% 
  mutate(days_month = days_in_month(month),
         bed_available = beds * days_month,
         occupation = n / bed_available,
         occupation_2 = round((n / bed_available) * 100, 2))

# --- cálculo de media y CI (corregido: se usa el mismo n para el SE) ---
data_general_occupation <- data_occupation_2 %>% 
  filter(!caci_3 %in% c("TEP", "TXC")) %>% 
  group_by(month, departamento_cargue) %>% 
  summarise(
    n_2 = n_distinct(caci_3),
    mean = mean(occupation, na.rm = TRUE),
    mean_2 = mean(occupation_2, na.rm = TRUE),
    sd = sd(occupation, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    se = sd / sqrt(n_2),
    low_ci = mean - 1.96 * se,
    upper_ci = mean + 1.96 * se
  )

# --- opcional: unir para otras operaciones (si lo necesitas) ---
data_occupation_2 <- full_join(data_occupation_2, data_general_occupation, 
                               by = c("month", "departamento_cargue")) %>% 
  filter(!is.na(caci_3))

# --- paleta dinámica para caci_3 + Mean en negro ---
caci_levels <- sort(unique(data_occupation_2$caci_3))
n_caci <- length(caci_levels)

# generar paleta (Set2 por defecto; se extiende si n > 8)
if (n_caci <= 8) {
  cols_caci <- brewer.pal(max(3, n_caci), "Set2")[1:n_caci]
} else {
  cols_caci <- colorRampPalette(brewer.pal(8, "Set2"))(n_caci)
}
names(cols_caci) <- caci_levels

cols_all <- c(cols_caci, "Mean" = "black")  # agrego la entrada 'Mean'

# --- PLOT: línea por caci_3, línea de la media y ribbon de CI con leyenda ---
p <- ggplot(data_occupation_2, aes(x = month)) +
  # líneas por CACI
  geom_line(aes(y = occupation, colour = caci_3, group = caci_3), size = 1.3) +
  # línea de la media (mapeo de color y linetype a "Mean" para que aparezca en leyenda)
  geom_line(
    data = data_general_occupation,
    aes(x = month, y = mean, colour = "Mean", linetype = "Mean"),
    size = 0.9,
    inherit.aes = FALSE
  ) +
  # ribbon CI (mapeo de fill a "95% CI" para que aparezca en leyenda)
  geom_ribbon(
    data = data_general_occupation,
    aes(x = month, ymin = low_ci, ymax = upper_ci, fill = "95% CI"),
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  labs(x = "Meses", y = "Ocupación", colour = "CACI / Media", fill = NULL, linetype = NULL) +
  theme_classic() +
  scale_x_date(date_labels = "%b-%y", breaks = "month") +
  scale_y_continuous(n.breaks = 15, labels = scales::label_percent()) +
  # colores: combinamos colores por CACI y la entrada 'Mean'
  scale_colour_manual(
    name = "CACI",
    values = cols_all,
    breaks = c(caci_levels, "Mean"),
    labels = c(caci_levels, "Mean (overall)")
  ) +
  scale_fill_manual(
    name = "Intervalo",
    values = c("95% CI" = "grey70"),
    breaks = "95% CI",
    labels = "95% CI (mean)"
  ) +
  scale_linetype_manual(
    values = c("Mean" = "dashed"),
    guide = guide_legend(order = 2)
  ) +
  theme(
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold", color = "black")
  ) +
  facet_wrap(~ departamento_cargue)

print(p)
  
#################################################### Bed Turnover rate #########################################################

date_btr <- data_costo_total_2 %>% 
  filter(str_detect(departamento_cargue, "HOSPITALIZA|UCI|URG")) %>% 
  group_by(ingreso, caci_3, departamento_cargue) %>% 
  summarise(min = min(fecha_cargue),
            max = max(fecha_cargue), 
            length = max - min, .groups = "drop") 

btr <- date_btr %>% 
  mutate(month = floor_date(as.Date(max), unit = "month")) %>% 
  mutate(departamento_cargue = case_when(
    departamento_cargue == "UCI" ~ "UCI",
    str_detect(departamento_cargue, "UCIN") ~ "UCIN",
    str_detect(departamento_cargue, "HOSPITALIZACION") ~ "HOSPITALIZACION", 
    departamento_cargue == "URGENCIAS" ~ "URGENCIAS",
    TRUE ~ NA_character_
  )) %>% 
  group_by(month, departamento_cargue, caci_3) %>% 
  summarise(n_3 = n(),.groups = "drop") %>% 
  mutate(beds = case_when(
    departamento_cargue == "UCI" ~ 12,
    departamento_cargue == "UCIN" ~ 14,
    departamento_cargue == "HOSPITALIZACION" ~ 22, 
    departamento_cargue == "URGENCIAS" ~ 4,
    TRUE ~ NA_real_
  )) %>% 
  mutate(btr = n_3 / beds) 

# --- cálculo de media y CI (corregido: se usa el mismo n para el SE) ---
data_general_btr <- btr %>% 
  #filter(!caci_3 %in% c("TEP", "TXC")) %>% 
  group_by(month, departamento_cargue) %>% 
  summarise(
    n4 = n_distinct(caci_3),
    mean_3 = mean(btr, na.rm = TRUE),
    sd_2 = sd(btr, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    se_2 = sd_2 / sqrt(n4),
    low_ci_2 = mean_3 - 1.96 * se_2,
    upper_ci_2 = mean_3 + 1.96 * se_2
  ) %>% 
  filter(!departamento_cargue == "URGENCIAS")

# --- opcional: unir para otras operaciones (si lo necesitas) ---
btr_2 <- full_join(btr, data_general_btr,
                 by = c("month", "departamento_cargue")) %>% 
  filter(!is.na(caci_3)) %>% 
  filter(!departamento_cargue == "URGENCIAS")

# --- paleta dinámica para caci_3 + Mean en negro ---
caci_levels <- sort(unique(btr_2$caci_3))
n_caci <- length(caci_levels)

# generar paleta (Set2 por defecto; se extiende si n > 8)
if (n_caci <= 8) {
  cols_caci <- brewer.pal(max(3, n_caci), "Set2")[1:n_caci]
} else {
  cols_caci <- colorRampPalette(brewer.pal(8, "Set2"))(n_caci)
}
names(cols_caci) <- caci_levels

cols_all <- c(cols_caci, "Mean" = "black")  # agrego la entrada 'Mean'

# --- PLOT: línea por caci_3, línea de la media y ribbon de CI con leyenda ---
g_btr <- ggplot(btr_2, aes(x = month)) +
  # líneas por CACI
  geom_line(aes(y = btr, colour = caci_3, group = caci_3), size = 1.4) +
  # línea de la media (mapeo de color y linetype a "Mean" para que aparezca en leyenda)
  geom_line(
    data = data_general_btr,
    aes(x = month, y = mean_3, colour = "Mean", linetype = "Mean"),
    size = 0.9,
    inherit.aes = FALSE
  ) +
  # ribbon CI (mapeo de fill a "95% CI" para que aparezca en leyenda)
  geom_ribbon(
    data = data_general_btr,
    aes(x = month, ymin = low_ci_2, ymax = upper_ci_2, fill = "95% CI"),
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  labs(x = "Meses", y = "Giro Cama", colour = "CACI / Media", fill = NULL, linetype = NULL) +
  theme_classic() +
  scale_x_date(date_labels = "%b-%y", breaks = "month") +
  scale_y_continuous(n.breaks = 15) +
  # colores: combinamos colores por CACI y la entrada 'Mean'
  scale_colour_manual(
    name = "CACI",
    values = cols_all,
    breaks = c(caci_levels, "Mean"),
    labels = c(caci_levels, "Mean (overall)")
  ) +
  scale_fill_manual(
    name = "Intervalo",
    values = c("95% CI" = "grey70"),
    breaks = "95% CI",
    labels = "95% CI (mean)"
  ) +
  scale_linetype_manual(
    values = c("Mean" = "dashed"),
    guide = guide_legend(order = 2)
  ) +
  theme(
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold", color = "black")
  ) +
  facet_wrap(~ departamento_cargue)

print(g_btr)

####### Occupation and bed turnover with lenght stay days ##########################################################

occupation_btr <- left_join(data_occupation_2, btr_2) 

occupation_btr <- occupation_btr %>% 
  filter(!departamento_cargue == "URGENCIAS") %>% 
  filter(!caci_3 %in% c("TXC", "TEP"))

ggplot(data = occupation_btr, aes(x = mean_2, y = btr)) +
  geom_point(aes(colour = caci_3)) +
  geom_smooth(method = "gam", aes(colour = caci_3)) +
  facet_wrap(~ departamento_cargue)


length <- date_btr %>% 
  filter(!caci_3 %in% c("TXC", "TEP")) %>% 
  mutate(month = floor_date(max, unit = "month")) %>% 
  mutate(departamento_cargue = case_when(
    departamento_cargue == "UCI" ~ "UCI",
    str_detect(departamento_cargue, "UCIN") ~ "UCIN",
    str_detect(departamento_cargue, "HOSPITALIZACION") ~ "HOSPITALIZACION", 
    departamento_cargue == "URGENCIAS" ~ "URGENCIAS",
    TRUE ~ NA_character_
  )) %>% 
  group_by(month, departamento_cargue, caci_3) %>% 
  summarise(
    n5 = n_distinct(ingreso),
    length = median(as.numeric(length)), .groups = "drop") %>% 
  filter(!departamento_cargue == "URGENCIAS")

length_general <- length %>% 
  filter(!caci_3 %in% c("TEP", "TXC")) %>% 
  group_by(month, departamento_cargue) %>% 
  summarise(
    median_2 = median(length, na.rm = TRUE),
    low_iqr = quantile(as.numeric(length), 0.25),
    upper_iqr = quantile(as.numeric(length), 0.75),
    .groups = "drop") %>% 
  filter(!departamento_cargue == "URGENCIAS")

length <- full_join(length, length_general)
length_occupation_btr <- full_join(occupation_btr, length)
length_btr <- full_join(btr_2, length)

export(length_occupation_btr, "length_occupation_btr.xlsx")

ggplot(data = length_occupation_btr %>% filter(!caci_3 %in% c("TXC", "TEP")), aes(x = occupation, y = btr )) +
  geom_point() +
  geom_smooth(aes(colour= caci_3), method = "loess") +
  labs(x = "Porcentaje de ocupación", y = "Giro cama", colour = "CACI") +
  theme_bw() +
  scale_x_continuous(n.breaks = 10, label = scales::label_percent()) +
  scale_y_continuous(n.breaks = 10) +
  theme(
    axis.title = element_text(size = 16, color = "black", face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 14, color = "black", face = "bold"),
    legend.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 0),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold", color = "black")
  ) +
  facet_wrap(~ departamento_cargue)


cols_all <- c(cols_caci, "Mean" = "black")  # agrego la entrada 'Mean'

# --- PLOT: línea por caci_3, línea de la media y ribbon de CI con leyenda ---
g_length <- ggplot(length %>% filter(!caci_3 %in% c("TXC", "TEP")), aes(x = month)) +
 # geom_line(aes(y = length, colour = caci_3, group = caci_3), size = 1.4) +
  # líneas por CACI
  tidyquant::geom_ma(aes(y = length, colour = caci_3, group = caci_3),
                     n = 3,           
                     size = 1.3, linetype = "solid") +
  # línea de la media (mapeo de color y linetype a "Mean" para que aparezca en leyenda)
  tidyquant::geom_ma(aes(x = month, y = median_2, colour = "Mean", linetype = "Mean"),
                     n = 3,           
                     size = 1.3, linetype = "dashed") +
  #geom_line(
   # data = length_general,
  #  aes(x = month, y = median_2, colour = "Mean", linetype = "Mean"),
  #  size = 0.9,
  #  inherit.aes = FALSE
  # ) +
  # ribbon CI (mapeo de fill a "95% CI" para que aparezca en leyenda)
  geom_ribbon(
    data = length %>% filter(!caci_3 %in% c("TXC", "TEP")),
    aes(x = month, ymin = low_iqr, ymax = upper_iqr, fill = "95% CI"),
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  labs(x = "Meses", y = "Días estancia", colour = "CACI / Media", fill = NULL, linetype = NULL) +
  theme_classic() +
  scale_x_date(date_labels = "%b-%y", breaks = "month") +
  scale_y_continuous(n.breaks = 15) +
  # colores: combinamos colores por CACI y la entrada 'Mean'
  scale_colour_manual(
    name = "CACI",
    values = cols_all,
    breaks = c(caci_levels, "Mean"),
    labels = c(caci_levels, "Mean (overall)")
  ) +
  scale_fill_manual(
    name = "Intervalo",
    values = c("95% CI" = "grey70"),
    breaks = "95% CI",
    labels = "95% CI (mean)"
  ) +
  scale_linetype_manual(
    values = c("Mean" = "dashed"),
    guide = guide_legend(order = 2)
  ) +
  theme(
    axis.title = element_text(size = 16, color = "black", face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 14, color = "black", face = "bold"),
    legend.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold", color = "black")
  ) +
  facet_wrap(~ departamento_cargue)

print(g_length)


#############################################################################################################

### Valores de costos superiores a los 20 millones de pesos 
valores_100_mill <- data_costo_total_3 %>% 
  group_by(caci, mes_cargue, cargo) %>% 
  summarise(Pacientes = n_distinct(identificacion),
            costo = sum(costo), 
            promedio = costo/Pacientes,
            .groups = "drop") %>% 
  arrange(desc(promedio)) 

valores_100_mill_2 <- valores_100_mill %>% 
  filter(costo > 30000000) %>% 
  rename("Mes" = mes_cargue, 
         "Prestación" = cargo,
         "Costo" = costo,
         "Promedio costo" = promedio) %>% 
  mutate(Costo = scales::dollar(Costo, prefix = "$", big.mark = ".", decimal.mark = ",")) %>% 
  flextable() %>% 
  autofit()

valores_100_mill_2

valores_100_mill_3 <- valores_100_mill %>% 
  filter(caci == "ACV") %>% 
  filter(promedio > 4000000) %>% 
  pivot_wider(id_cols = cargo, names_from = mes_cargue, values_from = c(promedio, Pacientes)) %>% 
  select(cargo, Pacientes_Jan, promedio_Jan, Pacientes_Feb, promedio_Feb, Pacientes_Mar, promedio_Mar,
         Pacientes_Apr, promedio_Apr, Pacientes_May, promedio_May, Pacientes_Jun, promedio_Jun) %>% 
  mutate(across(-1, replace_na, 0)) %>% 
  flextable() %>% 
  theme_box() %>% 
  width(j = 1, width = 1.0) %>%
  width(j = 2:13, width = 1.1) %>%
  fontsize(size = 9, part = "all") %>%
  set_table_properties(width = 1, layout = "autofit") %>% 
  save_as_docx(path = "table_outlines.docx")

valores_100_mill_3

### Costo CACI por mes y número de pacientes. 
caci_costo <- data_costo_total_3 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(Pacientes = n_distinct(identificacion),
            costo = sum(costo), .groups = "drop") %>% 
  pivot_wider(id_cols = mes_cargue, names_from = caci, values_from = c(costo, Pacientes)) %>% 
  mutate(across(,replace_na, 0)) %>% 
  mutate(across(starts_with("costo"), ~ scales::dollar(., prefix = "$", big.mark = ".", decimal.mark = ","))) %>% 
  select(mes_cargue, costo_ACV, Pacientes_ACV, costo_ICC, Pacientes_ICC, costo_SCA, Pacientes_SCA, 
         costo_TxC, Pacientes_TxC
  ) %>% 
  flextable() %>% 
  autofit() 

caci_costo %>% 
  add_header_row(
    top = TRUE,                
    values = c("Mes",     
               "ACV", "",
               "ICC", "",    
               "SCA", "",
               "TxC", ""
    )) %>% 
  set_header_labels(       
    mes_cargue = "",
    costo_acv = "Costo", 
    Pacientes_acv = "Pacientes",
    costo_icc = "Costo", 
    Pacientes_icc = "Pacientes",
    costo_sca = "Costo", 
    Pacientes_sca = "Pacientes",
    costo_txc = "Costo",
    Pacientes_txc = "Pacientes") %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header") %>% 
  merge_at(i = 1, j = 6:7, part = "header") %>% 
  merge_at(i = 1, j = 8:9, part = "header") %>% 
  border_remove() %>% 
  theme_booktabs() %>% 
  align(i = 1, align = "center", part = "header") %>% 
  bold( i = 1, part = "header") %>% 
  autofit()

### Costo mediana paciente CACI por mes y número de pacientes. 
caci_costo_2 <- data_costo_total_3 %>% 
  group_by(identificacion, caci, mes_cargue) %>% 
  summarise(costo = sum(costo), .groups = "drop") 

caci_costo_3 <- caci_costo_2 %>% 
  group_by(caci, mes_cargue) %>% 
  summarise(costo = median(costo), .groups = "drop",
            Pacientes = n_distinct(identificacion)) %>% 
  pivot_wider(id_cols = mes_cargue, names_from = caci, values_from = c(costo, Pacientes)) %>% 
  mutate(across(,replace_na, 0)) %>% 
  mutate(across(starts_with("costo"), ~ scales::dollar(., prefix = "$", big.mark = ".", decimal.mark = ","))) %>% 
  select(mes_cargue, costo_ACV, Pacientes_ACV, costo_ICC, Pacientes_ICC, costo_SCA, Pacientes_SCA, 
         costo_TxC, Pacientes_TxC) %>% 
  flextable() 

caci_costo_3 %>% 
  add_header_row(
    top = TRUE,                
    values = c("Mes",     
               "ACV", "",
               "ICC", "",    
               "SCA", "",
               "TxC", "")) %>% 
  set_header_labels(       
    mes_cargue = "",
    costo_acv = "Costo", 
    Pacientes_acv = "Pacientes",
    costo_icc = "Costo", 
    Pacientes_icc = "Pacientes",
    costo_sca = "Costo", 
    Pacientes_sca = "Pacientes",
    costo_txc = "Costo", 
    Pacientes_txc = "Pacientes") %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header") %>% 
  merge_at(i = 1, j = 6:7, part = "header") %>% 
  merge_at(i = 1, j = 8:9, part = "header") %>% 
  border_remove() %>% 
  theme_booktabs() %>% 
  align(i = 1, align = "center", part = "header") %>% 
  bold( i = 1, part = "header") %>% 
  autofit()

################################################################################################

#### Paciente con costos superiores outliers

valores_riq_mill <- data_costo_total_3 %>% 
  group_by(identificacion, caci, mes_cargue) %>% 
  summarise(costo = sum(costo), .groups = "drop") %>% 
  filter(costo > 31500000)

valores_riq_mill <- data_costo_total_3 %>% 
  group_by(identificacion, caci, mes_cargue) %>% 
  summarise(costo = sum(costo), .groups = "drop") %>% 
  filter(costo > 80000000)

data_costo_total_3 %>% 
  filter(mes_cargue == "Feb" & caci == "icc") %>% 
  group_by(identificacion, cargo) %>% 
  summarise(costo = sum(costo), .groups = "drop") %>% 
  arrange(desc(costo))

v2 <- valores_riq_mill$identificacion

data_out <- data_costo_total_3 %>% 
  filter(identificacion %in% v2) %>% 
  mutate(across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y")))

data_dias <- data_out %>% 
  mutate(dias_dif = fecha_de_egreso - fecha_ingreso) %>% 
  filter(!is.na(dias_dif)) %>% 
  distinct(identificacion, mes_cargue, .keep_all = T)

ggplot(data_dias, mapping = aes(x = caci, y = dias_dif, fill = caci)) +
  geom_boxplot()

#######################################################################################################

data_costo_total_3 <- data_costo_total_3 %>% 
  mutate(outliers = if_else(identificacion %in% v2, "Si", "No"),
         dif_dias = fecha_de_egreso - fecha_ingreso) %>% 
  mutate(departamento = case_when(departamento_cargue =="0089 CONSULTA HEMODINAMIA" ~ "CONSULTA",
                                  departamento_cargue == "ANGIOGRAFIA" ~ "HEMODINAMIA",
                                  departamento_cargue == "ANGIOGRAFIA CONSULTAS" ~ "CONSULTA",
                                  departamento_cargue == "CARDIOLOGIA NO INVASIVO" ~ "CARDIOLOGÍA",
                                  str_detect(departamento_cargue, "HOSPITALIZACION") ~ "HOSPITALIZACION",
                                  str_detect(departamento_cargue, "LABORATORIO CLINICO|
                                             LABORATORIO CLINICO Y PATOLOGIA|LABORATORIO GENETICA")~ "LABORATORIO",
                                  departamento_cargue == "UCI-UCIN ANGIO" ~ "UCIN",
                                  departamento_cargue == "UCIN 5 PISO" ~ "UCIN",
                                  TRUE ~ departamento_cargue))

### Gráfico de boxplot para observar outliers de día de estancia
ggplot(data_costo_total_3 %>% mutate(caci = str_to_upper(caci)) %>% 
         filter(!caci %in% c("SCA", "TXC")), 
       mapping = aes(x = caci, y = dif_dias, fill = caci)) +
  geom_boxplot(na.rm = T, show.legend = F) +
  facet_wrap("outliers") +
  ylab("Estancia") +
  xlab("CACI") +
  theme_bw()

costo_caci_outliers <- data_costo_total_3 %>% 
  group_by(caci, mes_cargue, departamento, outliers) %>% 
  summarise(costo = sum(costo), 
            pacientes = n_distinct(identificacion), .groups = "drop") %>% 
  mutate(mes_cargue = factor(mes_cargue, levels = c("ene", "feb", "mar", "abr",
                                                    "may", "jun", "jul", "ago", "sep")))


### Distribución de los costos de acuerdo a outliers y servicios
pacman::p_load(paletteer)

ggplot(costo_caci_outliers %>% mutate(caci = str_to_upper(caci)) %>% 
         filter(!caci %in% c("SCA", "TXC")), 
       mapping = aes(x = mes_cargue, y = costo, fill = departamento)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~outliers + caci) +
  scale_y_continuous(n.breaks = 12, labels = scales::dollar_format()) +
  theme_bw() +
  xlab("Mes 2024") +
  ylab("Costo") +
  labs(fill = "Servicio", title = "Comparación de costos por servicios entre pacientes Outliers vs valor promedio en DIME 2024") +
  scale_fill_manual(values = paletteer_d("ggsci::category20_d3")) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  )


### Distribución de pacientes de acuerdo a outliers y servicios
pacman::p_load(paletteer)

ggplot(costo_caci_outliers %>% mutate(caci = str_to_upper(caci)) %>% 
         filter(!caci %in% c("SCA", "TXC"),
                str_detect(departamento, "CIRUGIA|ESCANOGRAFIA|HEMODINAMIA|HOSPITALIZACION|
                           LABORATORIO|RESONANCIA|UCI|UCIN|UNIDAD DE ATENCION")), 
       mapping = aes(x = mes_cargue, y = pacientes, fill = departamento)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~outliers + caci) +
  scale_y_continuous(n.breaks = 12) +
  theme_bw() +
  xlab("Mes 2024") +
  ylab("Costo") +
  labs(fill = "Servicio", title = "Comparación de costos por servicios entre pacientes Outliers vs valor promedio en DIME 2024") +
  scale_fill_manual(values = paletteer_d("ggsci::category20_d3")) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  )

servicio_select <- c()

costo_caci_outliers_2 <- data_costo_total_3 %>% 
  group_by(caci, cargo, outliers, servicio) %>% 
  summarise(costo = sum(costo), 
            pacientes = n_distinct(identificacion), .groups = "drop",
            costo_paciente = round(costo/pacientes, digits = 2))

tabla_costo_outliers <- costo_caci_outliers_2 %>% 
  filter(servicio == "QUIRURGICOS") %>% 
  pivot_wider(id_cols = cargo, names_from = c(caci, outliers), values_from = c(costo_paciente)) %>% 
  as.data.frame()


###################################################################################
#### Tabla de cups solicitada por planeación financiera

tabla_cups <- data_costo_total_3 %>% 
  group_by(identificacion, caci, mes_cargue, cargo, cod_cargo) %>% 
  summarise(n = n(), .groups = "drop") 

tabla_cups_caci <- tabla_cups %>% 
  group_by(mes_cargue, caci, cargo, cod_cargo) %>% 
  summarise(Pacientes = n_distinct(identificacion),
            n = median(n), .groups = "drop") 

tabla_cups_caci_2 <- tabla_cups %>% 
  group_by(mes_cargue, caci, cargo, cod_cargo) %>% 
  summarise(Pacientes = n_distinct(identificacion),
            n = sum(n), .groups = "drop") 

### Número de pacientes atendidos en cada uno de los CACI
pacientes_caci_mes <- data_costo_total_3 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(Pacientes_mes = n_distinct(ingreso), .groups = "drop")

### Unión de la base de frecuncia de uso y pacientes CACI
tabla_cups_2 <- full_join(tabla_cups_caci_2, pacientes_caci_mes)

rio::export(tabla_cups_caci_2, "tabla_cups_caci_2024.xlsx")

tabla_cups_3 <- tabla_cups_2 %>% 
  mutate(prop_paciente = Pacientes/Pacientes_mes) %>% 
  mutate(n_uso = n/Pacientes) %>% 
  mutate(frecuencia_uso = n_uso*prop_paciente)

tabla_cups_3.2 <- tabla_cups_2 %>% 
  mutate(prop_paciente = Pacientes/Pacientes_mes) %>% 
  mutate(frecuencia_uso = n*prop_paciente)

costo_dime <- data_costo_total_3 %>% 
  select(cod_cargo, costo) %>% 
  distinct(cod_cargo, .keep_all = T)


tabla_cups_4 <- full_join(tabla_cups_3.2, costo_dime)

tabla_cups_5 <- tabla_cups_4 %>% 
  mutate(costo = if_else(costo <0, abs(costo), costo)) %>% 
  mutate(costo_2 = costo * frecuencia_uso)

tabla_cups_6 <- tabla_cups_5 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_2 = sum(costo_2), .groups = "drop") %>% 
  pivot_wider(id_cols = mes_cargue, names_from = caci, values_from = costo_2) %>%
  mutate(across(,replace_na, 0)) %>% 
  bind_rows(summarise(., mes_cargue = "Total",
                      ACV = median(ACV),
                      ICC = median(ICC),
                      SCA = median(SCA),
                      TxC = median(TxC))) %>% 
  rename("Mes cargue" = mes_cargue,
         "ACV" = ACV,
         "ICC" = ICC,
         "SCA" = SCA,
         "TXC" = TxC ) %>% 
  mutate(across(2:4, ~ scales::dollar(., prefix = "$"))) %>% 
  flextable()

tabla_cups_6 %>% 
  autofit() %>% 
  bold(i = 1, part = "header") %>% 
  bold(i = 7, part = "body") %>% 
  hline(i = 6, part = "body", border = fp_border_default(width = 1.5)) 

tabla_cups_6.1 <- tabla_cups_5 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_2 = sum(costo_2), .groups = "drop") %>% 
  pivot_wider(id_cols = mes_cargue, names_from = caci, values_from = costo_2) %>%
  mutate(across(,replace_na, 0)) %>% 
  bind_rows(summarise(., mes_cargue = "Total",
                      ACV = mean(ACV),
                      ICC = mean(ICC),
                      SCA = mean(SCA),
                      TxC = mean(TxC))) 
