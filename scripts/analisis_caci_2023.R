pacman::p_load(tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms, epikit, scales, gt, zoo)

# Importación base de datos CACI
data_caci <- import("base_ie_2023.xlsx")
data_sca <- import("sca_2023.xlsx")
data_acv <- import("acv_2023.xls")
data_icc <- import("Consolidado IC -TxC.xlsx")
data_txc <- import("txc_2023.xlsx")

data_caci_2 <- data_caci %>% 
  clean_names() %>% 
  mutate(hora_ingreso = as_hms(fecha_ingreso)) %>% 
  mutate(hora_egreso = as_hms(fecha_de_egreso)) %>% 
  mutate(across(starts_with("fecha"), as.Date)) %>% 
  filter(!paciente %in% c("CARDENAS NIÑO FERNANDO","VELOSA ORTIZ MONICA ALEJANDRA", "EPS  SANITAS")) %>%
  distinct(paciente, documento, fecha_ingreso, .keep_all = TRUE) 

data_caci_2 <- data_caci_2 %>% 
  mutate(cruce = str_c(documento, fecha_ingreso, .sep = "")) 
  

data_sca <- data_sca %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  mutate(documento = str_extract(cedula, regex("\\d+"))) %>% 
  mutate(cruce = str_c(documento, fecha_de_ingreso, .sep = ""))
  

data_acv <- data_acv %>% 
  clean_names() %>% 
  filter(fecha_ingreso > "2023-01-01") %>% 
  mutate(fecha_ingreso = as.Date(fecha_ingreso)) %>% 
  mutate(documento = str_extract(cedula, regex("\\d+"))) %>% 
  mutate(cruce = str_c(cedula, fecha_ingreso, .sep = "")) %>% 
  mutate(fecha_egreso = as.numeric(fecha_egreso)) %>% 
  mutate(fecha_egreso = as.Date(fecha_egreso, origin = "1899-12-30")) 



data_icc <- data_icc %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  mutate(documento = str_extract(numero_de_identificacion, regex("\\d+"))) %>% 
  mutate(cruce = str_c(documento, fecha_de_ingreso, .sep = ""))

data_txc <- data_txc %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  mutate(documento = str_extract(numero_de_identificacion, regex("\\d+"))) %>% 
  mutate(cruce = str_c(documento, fecha_de_ingreso, .sep = ""))

# Unión de base SCA
data_caci_2 <- left_join(x = data_caci_2, select(data_sca, documento, diagnostico), by = "documento") 

data_caci_2 <- data_caci_2 %>% 
  distinct(documento, fecha_de_egreso, hora_egreso, .keep_all = TRUE)

data_caci_2 <- data_caci_2%>% 
  rename("diagnostico_sca" = diagnostico)


export(data_caci_2, "data_caci_2.xlsx")

# Unión de base ACV
data_caci_2 <- left_join(x = data_caci_2, select(data_acv, documento, clasificacion_acv), by = "documento") 

data_caci_2 <- data_caci_2 %>% 
  distinct(documento, fecha_de_egreso, hora_egreso, .keep_all = TRUE)

data_caci_2 <- data_caci_2%>% 
  rename("diagnostico_acv" = clasificacion_acv) 

# Unión de base ICC
data_caci_2 <- left_join(x = data_caci_2, select(data_icc, documento, eapb), by = "documento")

data_caci_2 <- data_caci_2 %>% 
  distinct(documento, fecha_de_egreso, hora_egreso, .keep_all = TRUE)

data_caci_2 <- data_caci_2%>% 
  rename("diagnostico_icc" = eapb) 

# Unión de base txc
data_caci_2 <- left_join(x = data_caci_2, select(data_txc, documento, eapb), by = "documento")

data_caci_2 <- data_caci_2%>% 
  rename("diagnostico_txc" = eapb) 


# Limpieza de base general cruzada
data_caci_2 <- data_caci_2 %>% 
  mutate(filtro = str_detect(departamento_actual, pattern ="LABORATORIO|REAHABILITACION|CARDIOLOGIA|DIGESTIVA|CONSULTA|DENSITROMETRIA|ECOGRAFIA|ESCANOGRAFIA|CIRUGIA|
                             MAMOGRAFIA|RAYOS|REHABILITA|RESONANCIA|PROCEDIMIENTOS|ANGIOGRAFIA|DENSITOMETRIA|6680|URGENCIAS" )) %>% 
  filter(filtro == FALSE)

data_caci_4 <- data_caci_2 %>% 
  mutate(filtro_dx = ifelse(is.na(diagnostico_icc) & is.na(diagnostico_txc) & 
                              is.na(diagnostico_sca) & is.na(diagnostico_acv), TRUE, FALSE)) %>% 
  filter(filtro_dx == FALSE)
         
# Filtrado de cada patología en la base de ingresos y egresos

data_caci_2 <- data_caci_2 %>% 
  filter(fecha_de_egreso > "2023-12-31") %>% 
  filter(!tipo_de_atencion == "AMBULATORIO") %>% 
  distinct(fecha_ingreso, fecha_de_egreso, documento, .keep_all = TRUE) %>% 
  mutate(trimestre = quarter(fecha_de_egreso, with_year = T)) %>% 
  mutate(trimestre_txc = quarter(fecha_ingreso, with_year = T)) %>% 
  mutate(sca = if_else(!is.na(diagnostico_sca), TRUE, FALSE), 
         acv = if_else(!is.na(diagnostico_acv), TRUE, FALSE),
         icc = if_else(!is.na(diagnostico_icc), TRUE, FALSE),
         txc = if_else(!is.na(diagnostico_txc), TRUE, FALSE)) %>% 
  mutate(grup_edad = age_categories(edad, lower = min(edad), upper = max(edad), by = 10)) %>% 
  mutate(estancia_horas_2 = (estancia_horas/24)) %>% 
  mutate(estancia_horas_2 = round(estancia_horas_2, digits = 1)) %>% 
  mutate(eps = if_else(str_detect(string = eps, pattern = "COLSANITAS|SANITAS|MEDISANITAS"), "SANITAS", eps),
         eps = if_else(str_detect(string = eps, pattern = "MEDPLUS"), "MEDPLUS", eps),
         eps = if_else(str_detect(string = eps, pattern = "NUEVA"), "NUEVA EPS", eps),
         eps = if_else(str_detect(string = eps, pattern = "SURA"), "SURA", eps),
         eps = if_else(str_detect(string = eps, pattern = "UNIVALLE"), "UNIVALLE", eps),
         eps = if_else(str_detect(string = eps, pattern = "COOMEVA"), "COOMEVA", eps),
         eps = if_else(str_detect(string = eps, pattern = "COLMEDICA"), "COLMEDICA", eps),
         eps = if_else(str_detect(string = eps, pattern = "ALIANZ|ALLIANZ"), "ALIANZ", eps),
         eps = if_else(str_detect(string = eps, pattern = "COOSALUD"), "COOSALUD", eps),
         eps = if_else(str_detect(string = eps, pattern = "COMFENALCO"), "COMFENALCO", eps),
         procedimiento_qx_2 = str_detect(string = procedimiento_qx, pattern = "TRASPLANTE DE CORAZON|CERCLAJE|TRASPLANTE CARDIACO"),
         mes = as.yearmon(fecha_de_egreso),
         mes = factor(mes, levels = c("ene. 2024", "feb. 2024", "mar. 2024")), 
         servicio = case_when(str_detect(departamento_actual, "URG") ~ "Urgencias",
                              str_detect(departamento_actual, "HOSPIT") ~ "Hospitalización",
                              str_detect(departamento_actual, "UCIN") ~ "UCIN",
                              str_detect(departamento_actual, "UCI") ~ "UCI",
                              TRUE ~ departamento_actual))

# Exportación de la base de datos para quarto
export(data_caci_2, "data_caci_2.xlsx")

data_caci_2 %>% 
  arrange(fecha_de_egreso, documento) %>% 
  mutate(dupli = duplicated(documento)) %>% 
  filter(sca == TRUE & !departamento_actual == "URGENCIAS") %>% 
  filter(dupli == FALSE) %>% 
  mutate(mes = as.yearmon(fecha_de_egreso),
         mes = factor(mes, levels = c("ene. 2024", "feb. 2024", "mar. 2024"))) %>% 
  select(edad, grup_edad, sexo, eps, servicio, estancia_horas_2, estado_al_alta, 
         remision_a_cirugia, total_cuenta, trimestre, mes) %>% 
  tbl_summary(by = mes, 
              label = list(edad ~ "Edad",
                           grup_edad ~ "Grupo edad",
                           sexo ~ "Sexo",
                           eps ~ "EAPB",
                           estancia_horas_2 ~ "Estancia días", 
                           estado_al_alta ~ "Condición del alta",
                           remision_a_cirugia ~ "Remisión a cirugía",
                           #procedimiento_qx ~ "Procedimiento quirúrgico", 
                           total_cuenta ~ "Total cuenta"
                           ),
              statistic = list(all_continuous() ~ "{median}", all_categorical() ~ "{n} ({p}%)"),
              missing = "no"
              )  %>% 
   bold_labels() %>%
   italicize_labels() %>%
   add_overall(last = TRUE) %>% 
   as_flex_table() 
   #save_as_docx(path = "caci_sca_2023.docx")


## Tabla de sindrome coronario agudo
tabla_total_sca <- data_caci_2 %>% 
  mutate(dupli = duplicated(documento)) %>% 
  filter(sca == TRUE & !departamento_actual == "URGENCIAS") %>% 
  filter(dupli == FALSE) %>% 
  mutate(valor_factura = if_else(is.na(valor_factura), 0, valor_factura)) %>% 
  group_by(mes) %>%
  summarise(pacientes = n_distinct(numero_de_ingreso),
            total_cuenta_sum = sum(total_cuenta),
            promedio_cuenta = mean(total_cuenta),
            des_est = sd(total_cuenta),
            low_ci = (promedio_cuenta - 1.96*(des_est/(sqrt(pacientes)))),
            up_ci = (promedio_cuenta + 1.96*(des_est/(sqrt(pacientes)))),
            median_cuenta = median(total_cuenta),
            )
         
print(tabla_total_sca)

## Análisis de ACV
data_caci_2 %>% 
  filter(acv == TRUE & !departamento_actual == "URGENCIAS") %>% 
  select(edad, grup_edad, sexo, eps, estancia_horas_2, estado_al_alta, remision_a_cirugia,
         total_cuenta, mes, trimestre) %>% 
  tbl_summary(by = mes, 
              label = list(edad ~ "Edad",
                           grup_edad ~ "Grupo edad",
                           sexo ~ "Sexo",
                           eps ~ "EAPB",
                           estancia_horas_2 ~ "Estancia días", 
                           estado_al_alta ~ "Condición del alta",
                           remision_a_cirugia ~ "Remisión a cirugía",
                           #procedimiento_qx ~ "Procedimiento quirúrgico",
                           total_cuenta ~ "Total cuenta"
              ),
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)"),
              missing = "no"
  ) %>% 
  bold_labels() %>%
  italicize_labels() %>%
  add_overall(last = TRUE) %>% 
  as_flex_table() 


## Tabla ACV
tabla_total_acv <- data_caci_2 %>% 
  filter(acv == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(pacientes = n_distinct(numero_de_ingreso),
            total_cuenta_sum = sum(total_cuenta),
            promedio_cuenta = mean(total_cuenta),
            des_est = sd(total_cuenta),
            low_ci = (promedio_cuenta - 1.96*(des_est/(sqrt(pacientes)))),
            up_ci = (promedio_cuenta + 1.96*(des_est/(sqrt(pacientes)))),
            median_cuenta = median(total_cuenta),
  )

print(tabla_total_acv)


## Analisis ICC
data_caci_2 %>% 
  filter(icc == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  select(edad, grup_edad, sexo, eps, estancia_horas_2, estado_al_alta, remision_a_cirugia,
         total_cuenta, trimestre, mes) %>% 
  tbl_summary(by = mes, 
              label = list(edad ~ "Edad",
                           grup_edad ~ "Grupo edad",
                           sexo ~ "Sexo",
                           eps ~ "EAPB",
                           estancia_horas_2 ~ "Estancia días", 
                           estado_al_alta ~ "Condición del alta",
                           remision_a_cirugia ~ "Remisión a cirugía",
                           total_cuenta ~ "Total cuenta"
              ),
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)"),
              missing = "no"
  ) %>% 
  bold_labels() %>%
  italicize_labels() %>%
  add_overall(last = TRUE) %>% 
  as_flex_table() 


tabla_total_icc <- data_caci_2 %>% 
  filter(acv == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>%
  group_by(mes) %>% 
  summarise(pacientes = n_distinct(numero_de_ingreso),
            total_cuenta_sum = sum(total_cuenta),
            promedio_cuenta = mean(total_cuenta),
            des_est = sd(total_cuenta),
            low_ci = (promedio_cuenta - 1.96*(des_est/(sqrt(pacientes)))),
            up_ci = (promedio_cuenta + 1.96*(des_est/(sqrt(pacientes)))),
            median_cuenta = median(total_cuenta),
  ) 


print(tabla_total_icc)

data_caci_2 %>% 
  #filter(txc == TRUE) %>% 
  mutate(total_cuenta = as.numeric(total_cuenta)) %>% 
  filter(procedimiento_qx_2 == TRUE) %>% 
  #filter(!procedimiento_qx == "TRASPLANTE DE CORAZON VIA ABIERTA") %>% 
  select(edad, grup_edad, paciente, sexo, eps, estancia_horas_2, estado_al_alta, remision_a_cirugia,
         total_cuenta, trimestre_txc, mes) %>% 
  tbl_summary(by = mes, 
              label = list(edad ~ "Edad",
                           grup_edad ~ "Grupo edad",
                           sexo ~ "Sexo",
                           eps ~ "EAPB",
                           estancia_horas_2 ~ "Estancia días", 
                           estado_al_alta ~ "Condición del alta",
                           remision_a_cirugia ~ "Remisión a cirugía",
                           #procedimiento_qx ~ "Procedimiento quirúrgico",
                           paciente ~ "Paciente",
                           total_cuenta ~ "Total cuenta"
              ),
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)"), 
  ) %>% 
  bold_labels() %>%
  italicize_labels() %>%
  as_flex_table() 


tabla_total_txc <- data_caci_2 %>% 
  filter(procedimiento_qx_2 == TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            promedio = mean(total_cuenta), 
            desviación = sd(total_cuenta)) 

tabla_total_txc_2 <- data_caci_2 %>% 
  filter(procedimiento_qx_2 == TRUE) %>% 
  group_by(mes) %>% 
  summarise(pacientes = n_distinct(numero_de_ingreso),
            total_cuenta_sum = sum(total_cuenta),
            promedio_cuenta = mean(total_cuenta)) 


#Tablas de costos por mes por CACI
## CACI SCA
data_caci_2 %>% 
  mutate(dupli = duplicated(documento)) %>% 
  filter(sca == TRUE & !departamento_actual == "URGENCIAS") %>% 
  filter(dupli == FALSE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            promedio = mean(total_cuenta),
            sd = sd(total_cuenta, na.rm = TRUE),
                      n_tcuenta = n()) %>%
  mutate(se_tcuenta = sd / sqrt(n_tcuenta),
         lower.ci.cuenta = promedio - qt(1 - (0.05 / 2), n_tcuenta - 1) * se_tcuenta,
         upper.ci.cuenta = promedio + qt(1 - (0.05 / 2), n_tcuenta - 1) * se_tcuenta) %>% 
  select(mes, n_tcuenta, total_cuenta_sum, promedio,  lower.ci.cuenta, upper.ci.cuenta) %>%
  mutate(mes = str_to_title(mes)) %>% 
  adorn_totals("row") %>% 
  mutate_at(vars(total_cuenta_sum, promedio, lower.ci.cuenta, upper.ci.cuenta), dollar) %>%
  rename("Mes" = mes, 
         "Promedio" = promedio,
         "Costo total" = total_cuenta_sum,
         "Atenciones" = n_tcuenta,
         "IC Inferior" = lower.ci.cuenta,
         "IC Superior" = upper.ci.cuenta)%>%
  flextable() 



### Calculo con mediana SCA
data_caci_2 %>% 
  filter(sca == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            n_tcuenta = n(),
            mediana = median(total_cuenta),
            rango_iq = IQR(total_cuenta),
            p25 = quantile(total_cuenta, 0.25),
            p75 = quantile(total_cuenta, 0.75)) %>% 
  select(mes, n_tcuenta, total_cuenta_sum, mediana, p25, p75, rango_iq) %>%
  mutate(mes = str_to_title(mes)) %>% 
  adorn_totals("row") %>% 
  mutate_at(vars(mediana, total_cuenta_sum, rango_iq, p25, p75), dollar) %>%
  rename("Mes" = mes, 
         "Atenciones" = n_tcuenta,
         "Total cuenta" = total_cuenta_sum,
         "Mediana" = mediana,
         "Rango IQ" = rango_iq,
         "P25" = p25,
         "P75" = p75,
         )%>% 
  flextable() 



# Costo medio paciente ACV
data_caci_2 %>% 
  filter(acv == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            promedio = mean(total_cuenta),
            sd = sd(total_cuenta, na.rm = TRUE),
            n_tcuenta = n()) %>%
  mutate(se_tcuenta = sd / sqrt(n_tcuenta),
         lower.ci.cuenta = promedio - qt(1 - (0.05 / 2), n_tcuenta - 1) * se_tcuenta,
         upper.ci.cuenta = promedio + qt(1 - (0.05 / 2), n_tcuenta - 1) * se_tcuenta) %>% 
  select(mes, n_tcuenta, total_cuenta_sum, promedio,  lower.ci.cuenta, upper.ci.cuenta) %>%
  mutate(mes = str_to_title(mes)) %>% 
  adorn_totals("row") %>% 
  mutate_at(vars(total_cuenta_sum, promedio, lower.ci.cuenta, upper.ci.cuenta), dollar) %>%
  rename("Mes" = mes, 
         "Promedio" = promedio,
         "Costo total" = total_cuenta_sum,
         "Atenciones" = n_tcuenta,
         "IC Inferior" = lower.ci.cuenta,
         "IC Superior" = upper.ci.cuenta)%>%
  flextable() 


### Calculo con mediana ACV
data_caci_2 %>% 
  filter(acv == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            n_tcuenta = n(),
            mediana = median(total_cuenta),
            rango_iq = IQR(total_cuenta),
            p25 = quantile(total_cuenta, 0.25),
            p75 = quantile(total_cuenta, 0.75)) %>% 
  select(mes, n_tcuenta, total_cuenta_sum, mediana, p25, p75, rango_iq) %>%
  mutate(mes = str_to_title(mes)) %>% 
  adorn_totals("row") %>% 
  mutate_at(vars(mediana, rango_iq, p25, p75), dollar) %>%
  rename("Mes" = mes, 
         "Atenciones" = n_tcuenta,
         "Total cuenta" = total_cuenta_sum,
         "Mediana" = mediana,
         "Rango IQ" = rango_iq,
         "P25" = p25,
         "P75" = p75,
  )%>% 
  flextable() 


## Cálculo promedio ICC
data_caci_2 %>% 
  filter(icc == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            promedio = mean(total_cuenta),
            sd = sd(total_cuenta, na.rm = TRUE),
            n_tcuenta = n()) %>%
  mutate(se_tcuenta = sd / sqrt(n_tcuenta),
         lower.ci.cuenta = promedio - qt(1 - (0.05 / 2), n_tcuenta - 1) * se_tcuenta,
         upper.ci.cuenta = promedio + qt(1 - (0.05 / 2), n_tcuenta - 1) * se_tcuenta) %>% 
  select(mes, n_tcuenta, total_cuenta_sum, promedio,  lower.ci.cuenta, upper.ci.cuenta) %>%
  mutate(mes = str_to_title(mes)) %>% 
  adorn_totals("row") %>% 
  mutate_at(vars(total_cuenta_sum, promedio, lower.ci.cuenta, upper.ci.cuenta), dollar) %>%
  rename("Mes" = mes, 
         "Promedio" = promedio,
         "Costo total" = total_cuenta_sum,
         "Atenciones" = n_tcuenta,
         "IC Inferior" = lower.ci.cuenta,
         "IC Superior" = upper.ci.cuenta)%>%
  flextable() 


### Calculo con mediana ICC
data_caci_2 %>% 
  filter(icc == TRUE & !departamento_actual == "URGENCIAS") %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            n_tcuenta = n(),
            mediana = median(total_cuenta),
            rango_iq = IQR(total_cuenta),
            p25 = quantile(total_cuenta, 0.25),
            p75 = quantile(total_cuenta, 0.75)) %>% 
  select(mes, n_tcuenta, total_cuenta_sum, mediana, p25, p75, rango_iq) %>%
  mutate(mes = str_to_title(mes)) %>% 
  adorn_totals("row") %>% 
  mutate_at(vars(mediana, total_cuenta_sum, rango_iq, p25, p75), dollar) %>%
  rename("Mes" = mes, 
         "Atenciones" = n_tcuenta,
         "Total cuenta" = total_cuenta_sum,
         "Mediana" = mediana,
         "Rango IQ" = rango_iq,
         "P25" = p25,
         "P75" = p75,
  )%>% 
  flextable() 



data_caci_3 <- data_caci_2 %>% 
  filter(!tipo_de_atencion == "AMBULATORIO") %>% 
  distinct(fecha_ingreso, fecha_de_egreso, documento, .keep_all = TRUE) %>% 
  mutate(trimestre = quarter(fecha_de_egreso, with_year = T)) %>% 
  mutate(trimestre_txc = quarter(fecha_ingreso, with_year = T)) %>% 
  mutate(sca = if_else(!is.na(diagnostico_sca), TRUE, FALSE), 
         acv = if_else(!is.na(diagnostico_acv), TRUE, FALSE),
         icc = if_else(!is.na(diagnostico_icc), TRUE, FALSE),
         txc = if_else(!is.na(diagnostico_txc), TRUE, FALSE)) %>% 
  mutate(grup_edad = age_categories(edad, lower = min(edad), upper = max(edad), by = 10)) %>% 
  mutate(estancia_horas_2 = (estancia_horas/24)) %>% 
  mutate(estancia_horas_2 = round(estancia_horas_2, digits = 1)) %>% 
  mutate(eps = if_else(str_detect(string = eps, pattern = "COLSANITAS|SANITAS|MEDISANITAS"), "SANITAS", eps),
         eps = if_else(str_detect(string = eps, pattern = "MEDPLUS"), "MEDPLUS", eps),
         eps = if_else(str_detect(string = eps, pattern = "NUEVA"), "NUEVA EPS", eps),
         eps = if_else(str_detect(string = eps, pattern = "SURA"), "SURA", eps),
         eps = if_else(str_detect(string = eps, pattern = "UNIVALLE"), "UNIVALLE", eps),
         eps = if_else(str_detect(string = eps, pattern = "COOMEVA"), "COOMEVA", eps),
         eps = if_else(str_detect(string = eps, pattern = "COLMEDICA"), "COLMEDICA", eps),
         eps = if_else(str_detect(string = eps, pattern = "ALIANZ|ALLIANZ"), "ALIANZ", eps),
         eps = if_else(str_detect(string = eps, pattern = "COOSALUD"), "COOSALUD", eps),
         eps = if_else(str_detect(string = eps, pattern = "COMFENALCO"), "COMFENALCO", eps),
         procedimiento_qx_2 = str_detect(string = procedimiento_qx, pattern = "TRASPLANTE DE CORAZON|CERCLAJE|TRASPLANTE CARDIACO"),
         mes = as.yearmon(fecha_de_egreso),
         mes = factor(mes, levels = c("ene. 2024", "feb. 2024", "mar. 2024")))



data_caci_3%>%
  filter(txc == TRUE & !departamento_actual == "URGENCIAS") %>% 
  filter(procedimiento_qx_2 == TRUE) %>% 
  distinct(documento, fecha_ingreso, .keep_all = TRUE) %>% 
  group_by(mes) %>% 
  summarise(total_cuenta_sum = sum(total_cuenta),
            promedio = mean(total_cuenta),
            sd = sd(total_cuenta, na.rm = TRUE),
            n_tcuenta = n())
  

 #Tabla costos totales por mes y por trimestre CACI
tabla_totales_cuenta_sca_acv <- left_join(tabla_total_sca, tabla_total_acv, by = "trimestre") 
tabla_totales_cuenta_sca_acv_icc <- left_join(tabla_totales_cuenta_sca_acv, tabla_total_icc, by = "trimestre")
tabla_totales_cuenta_final <- left_join(tabla_totales_cuenta_sca_acv_icc, tabla_total_txc_2, by = c("trimestre" = "trimestre_txc"))

tabla_totales_cuenta_sca_acv <- left_join(tabla_total_sca, tabla_total_acv, by = "mes") 
tabla_totales_cuenta_sca_acv_icc <- left_join(tabla_totales_cuenta_sca_acv, tabla_total_icc, by = "mes")
tabla_totales_cuenta_final <- left_join(tabla_totales_cuenta_sca_acv_icc, tabla_total_txc_2, by = "mes")


tabla_total_acv_2 <-  tabla_total_acv %>% 
  select(mes, pacientes, total_cuenta_sum)

tabla_total_sca_2 <- tabla_total_sca %>% 
  select(mes, pacientes, total_cuenta_sum)

tabla_total_icc_2 <- tabla_total_icc %>% 
  select(mes, pacientes, total_cuenta_sum)

tabla_total_txc_2 <- tabla_total_txc_2 %>% 
  select(mes, pacientes, total_cuenta_sum)


## Uno todas las tablas de costos promedio paciente para los CACI
tabla_total_cuenta_fina_3 <- rbind(tabla_total_sca_2, tabla_total_acv_2, 
                                   tabla_total_icc_2, tabla_total_txc_2)


tabla_total_cuenta_fina_3 %>% 
  group_by(mes) %>% 
  summarise( paciente = sum(pacientes), 
             total = sum(total_cuenta_sum),
             promedio = total/paciente) %>% 
  adorn_totals() %>% 
  flextable()
           


data_caci_2 %>% 
  group_by(mes) %>% 
  summarise(valor = sum(total_cuenta))



tabla_totales_cuenta_final %>% 
  rename("Total SCA" = total_cuenta_sum.x,
         "Total ACV" = total_cuenta_sum.y,
         "Total ICC" = total_cuenta_sum.x.x,
         "Total Trasplante" = total_cuenta_sum.y.y,
         "Mes" = mes) %>% 
  mutate(across(-1, replace_na, 0)) %>% 
  mutate(across(-1,scales::dollar)) %>%
  flextable()
  #save_as_docx(path = "caci_totales_2023.docx")

data_caci

### Tabla
tabla_totales_cuenta_final_2 <- tabla_totales_cuenta_final %>% 
  adorn_totals("both") %>% 
  rename("Total SCA" = total_cuenta_sum.x,
         "Total ACV" = total_cuenta_sum.y,
         "Total ICC" = total_cuenta_sum.x.x,
         "Total Trasplante" = total_cuenta_sum.y.y,
         "Mes" = mes) %>% 
  mutate(across(-1, replace_na, 0)) 

print(tabla_totales_cuenta)

tabla_total_caci <- rbind(tabla_total_icc, tabla_total_sca, tabla_total_acv, tabla_total_txc_2)

tabla_total_caci_2 <- tabla_total_caci %>% 
  group_by(mes) %>% 
  summarise(pacientes = sum(pacientes),
            total_cuenta_sum = sum(total_cuenta_sum),
            promedio = total_cuenta_sum /pacientes)

print(tabla_total_caci_2)

# Gráfico de cajas y bigotes del total de facturación por mes ACV

ggplot(data_caci_2 %>% filter(acv == TRUE & !departamento_actual == "URGENCIAS"), 
       aes(x=mes, y=total_cuenta, fill = mes)) + 
  geom_boxplot(alpha=0.3) +
  theme_bw()+
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") +
  scale_y_continuous(labels = dollar_format(scale = 1e-6), n.breaks = 10)


# Gráfico de cajas y bigotes del total de facturación por mes ACV
data_caci_2_acv <- data_caci_2 %>% 
  filter(acv == TRUE) %>% 
  mutate(fecha_mes = format(fecha_de_egreso, "%B")) %>% 
  mutate(fecha_mes = factor(fecha_mes, levels = c("enero", "febrero", "marzo", "abril",
                                                  "mayo", "junio", "julio", "agosto", "septiembre"))) 
 # mutate(total_cuenta = dollar(total_cuenta, big.mark = ",", decimal.mark = "."))

data_caci_2_acv_2 <- data_caci_2_acv %>% 
  filter(total_cuenta <= 20000000)

ggplot(data_caci_2_acv_2, aes(x= mes, y=total_cuenta, fill = mes)) + 
  geom_boxplot(alpha=0.3) +
  xlab("Mes") +
  ylab("Total facturación (millones COPS)") +
  theme_bw()+
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") +
  scale_y_continuous(labels = dollar_format(scale = 1e-6), n.breaks = 10)

