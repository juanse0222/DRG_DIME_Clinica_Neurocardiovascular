pacman::p_load(tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms, 
               epikit, scales, gt, zoo, readxl, here)

##### Analysis process about CACI and impact in DIME #########################################################################

### Total costo CACI mes
data_costo_total_3 <- import("data_costo_total_3_2025_II.rds")
data_results <- import(here("data", "results_2.xlsx"))


acv <-data_costo_total_3 %>% 
  filter(caci_3 == "ACV" & año == "2025") %>% 
  mutate(diag_all = paste(diagnostico_egreso_principal, diagnostico_egreso_secundario, 
               diagnostico_ingreso_princial, sep = " "),
         type = if_else(str_detect(diag_all, "HEMORRA|INTRACE|SANGRA"), "Hemorrhagic", "Stroke"))

acv_check <- acv %>% 
  distinct(ingreso, diag_all, type, .keep_all = T)
         

table_1 <- acv %>% 
  filter(departamento_cargue_2 != "FARMACIA") %>% 
  filter(!str_detect(cargo, "HONORARIO|DERECHOS|MATERIAL|ESPECIALISTA|INTERCONSULTA|INTERNACION|
                     |TERAPIA|INTRAHOSP|EQUIPO|CONSULTA|OXIGENO|CARGOS NO")) %>% 
  filter(!str_detect(departamento, "LABORATORIO")) %>% 
  group_by(cargo, type) %>% 
  summarise(pacientes = n_distinct(ingreso),
            frecuencia = n()) %>% 
  arrange(desc(frecuencia)) %>% 
  ungroup()

table_1 <- table_1 %>% 
  filter(frecuencia > 10) 


ggplot(table_1 %>% filter(type == "Stroke"), aes(x = cargo, y = frecuencia)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + # Wrap to max 10 characters per line
  geom_text(aes(label = frecuencia), vjust = -0.5) +
  theme_classic() +
  labs(x = "Cargo/Procedimiento", y = "Frecuencia", title = "CUPS con mayor frecuencia ordenados en ACV Isquémico (enero - octubre 2025)") +
  #facet_wrap(~type) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 16, face = "bold.italic", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) 

#### Exportating dataset withs CUPS lot frequency. 
table_2 <- acv %>% 
  filter(departamento_cargue_2 != "FARMACIA") %>% 
  filter(!str_detect(cargo, "HONORARIO|DERECHOS|MATERIAL|ESPECIALISTA|INTERCONSULTA|INTERNACION|
                     |TERAPIA|INTRAHOSP|EQUIPO|CONSULTA|OXIGENO|CARGOS NO")) %>% 
  filter(!str_detect(departamento, "LABORATORIO")) %>% 
  group_by(cargo, departamento, mes_cargue, plan, type) %>% 
  summarise(pacientes = n_distinct(ingreso),
            frecuencia = n()) %>% 
  arrange(desc(frecuencia)) %>% 
  ungroup()

export(table_2, here("ask-manager", "cups_acv_2025.xlsx"))

### Bar (col) figure
table_2 <- acv %>% 
  filter(departamento_cargue_2 != "FARMACIA") %>% 
  filter(!str_detect(cargo, "HONORARIO|DERECHOS|MATERIAL|ESPECIALISTA|INTERCONSULTA|INTERNACION|
                     |TERAPIA|INTRAHOSP|EQUIPO|CONSULTA|OXIGENO|CARGOS NO")) %>% 
  filter(!str_detect(departamento, "LABORATORIO")) %>% 
  group_by(cargo, type) %>% 
  summarise(pacientes = n_distinct(ingreso),
            frecuencia = n()) %>% 
  arrange(desc(frecuencia)) %>% 
  ungroup()

table_2 <- table_2 %>% 
  filter(frecuencia > 10) 


ggplot(table_2 %>% filter(type == "Stroke"), aes(x = cargo, y = frecuencia)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + # Wrap to max 10 characters per line
  geom_text(aes(label = frecuencia), vjust = -0.5) +
  theme_classic() +
  labs(x = "Cargo/Procedimiento", y = "Frecuencia", title = "CUPS con mayor frecuencia ordenados en ACV Isquémico (enero - octubre 2025)") +
  #facet_wrap(~type) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 16, face = "bold.italic", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) 

### Laboratory test CUPS ordered (requested)
table_3 <- acv %>% 
  filter(str_detect(departamento, "LABORATORIO")) %>% 
  filter(!str_detect(cargo, "GLUCOMETRIA")) %>% 
  group_by(cargo, departamento, mes_cargue, plan, type) %>% 
  summarise(pacientes = n_distinct(ingreso),
            frecuencia = n()) %>% 
  arrange(desc(frecuencia)) %>% 
  ungroup()

export(table_3, here("ask-manager", "cups_lab_acv_2025.xlsx"))

table_3 <- acv %>% 
  filter(str_detect(departamento, "LABORATORIO")) %>% 
  filter(!str_detect(cargo, "GLUCOMETRIA")) %>% 
  group_by(cargo, type) %>% 
  summarise(pacientes = n_distinct(ingreso),
            frecuencia = n()) %>% 
  arrange(desc(frecuencia)) %>% 
  ungroup()

table_3 <- table_3 %>% 
  filter(frecuencia > 10) 


ggplot(table_3 %>% filter(type == "Stroke"), aes(x = cargo, y = frecuencia)) +
  geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + # Wrap to max 10 characters per line
  geom_text(aes(label = frecuencia), vjust = -0.5) +
  theme_classic() +
  labs(x = "Cargo/Procedimiento", y = "Frecuencia", title = "CUPS con mayor frecuencia ordenados en ACV Isquémico (enero - octubre 2025)") +
  #facet_wrap(~type) +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.title = element_text(size = 16, face = "bold.italic", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) 


table_4 <- acv %>% 
  filter(departamento_cargue_2 == "FARMACIA") %>% 
  filter(!str_detect(cargo, "JERINGA|PAÑOS|GASAS|CLORURO DE SODIO USP|EQUIPO FREEGO|GASA|SONDA|FREEGO")) %>% 
  filter(!str_detect(departamento, "LABORATORIO")) %>% 
  group_by(cargo, departamento, mes_cargue, plan, type) %>% 
  summarise(pacientes = n_distinct(ingreso),
            frecuencia = n()) %>% 
  arrange(desc(frecuencia)) %>% 
  ungroup()

export(table_4, here("ask-manager", "cups_tx_acv_2025.xlsx"))



