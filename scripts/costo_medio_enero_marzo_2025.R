pacman::p_load(tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms, 
               epikit, scales, gt, zoo, readxl, here)

rm(list = ls())
Sys.setlocale("LC_TIME", "es_ES")

#######################################################################################################################
## Cargar bases de datos
data_sca <- import(here("Bases CACI", "sca_2023.xlsx"))
data_acv <- import(here("Bases CACI", "acv_2023.xls"))
data_icc <- import(here("Bases CACI", "Consolidado IC -TxC.xlsx"))
data_txc  <- import("Consolidado IC -TxC.xlsx", sheet = "TxC")
data_egresos <- import("ingresos_2025.xls")


## limpieza de base de datos de egreso
data_egresos_2 <- data_egresos %>% 
  clean_names() %>% 
  mutate(across(starts_with("fecha"), as.Date)) %>% 
  distinct(paciente, documento, fecha_ingreso, .keep_all = TRUE) 

data_egresos_2 <- data_egresos_2 %>% 
  mutate(documento = as.character(documento),
         mes_ingreso = month(fecha_ingreso, label = T),
         numero_de_ingreso = as.character(numero_de_ingreso)) 
# select(-c(x3, x18))

######################################################################################################################
## limpieza de bases de los CACI 

### SCA
data_sca_2 <- data_sca %>% 
  clean_names() %>% 
  mutate(fecha_de_egreso = as.numeric(fecha_de_egreso)) %>% 
  mutate(fecha_de_egreso = as.Date(fecha_de_egreso, origin = "1899-12-30"),
         fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  filter(fecha_de_ingreso > "2024-12-31") %>% 
  mutate(mes_sca = month(fecha_de_ingreso, label = T)) 

data_acv_2 <- data_acv %>% 
  clean_names() %>% 
  mutate(fecha_ingreso = as.numeric(fecha_ingreso)) %>% 
  mutate(fecha_ingreso = as.Date(fecha_ingreso, origin = "1899-12-30"),
         ingreso = as.character(ingreso)) %>% 
  filter(fecha_ingreso > "2024-12-31") %>% 
  mutate(fecha_egreso = as.numeric(fecha_egreso),
         fecha_egreso = as.Date(fecha_egreso, origin = "1899-12-30"),
         mes_acv = month(fecha_ingreso, label = T))

data_icc_2 <- data_icc %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  filter(fecha_de_ingreso > "2024-12-31") %>% 
  mutate(mes_icc = month(fecha_de_ingreso, label = T)) 


#data_txc_2 <- data_txc %>% 
#  clean_names() %>% 
 # mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
#  filter(fecha_de_ingreso > "2023-12-31") %>% 
#  mutate(numero_de_identificacion = str_extract(numero_de_identificacion, ("//d+")),
 #        mes_txc = month(fecha_de_ingreso, label = T)) %>% 
#  mutate(cruce = str_c(numero_de_identificacion, mes_txc, .sep = ""),
 # )

###################################################################################################################
# Unión de base SCA
data_egresos_sca <- left_join(x = data_egresos_2, select(data_sca_2, ingreso,
                                                         diagnostico, mes_sca, fecha_de_ingreso),
                              by = c("numero_de_ingreso" = "ingreso"))

data_egresos_sca_2 <- data_egresos_sca %>% 
  filter(!is.na(mes_sca)) %>% 
  distinct(numero_de_cuenta, fecha_ingreso, documento, .keep_all = TRUE) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS"))

data_egresos_sca_2 <- data_egresos_sca_2 %>% 
  rename("diagnostico" = diagnostico,
         "mes_caci" = mes_sca,
         "fecha_de_ingreso_caci" = fecha_de_ingreso) %>% 
  mutate(caci = "sca")

# Unión de base ACV
data_egresos_acv <- left_join(x = data_egresos_2, 
                              select(data_acv_2, cedula, clasificacion_acv, 
                                     mes_acv, fecha_ingreso), 
                              by = c("documento" = "cedula")) 


data_egresos_acv_2 <- data_egresos_acv %>%
  filter(!is.na(mes_acv)) %>% 
  distinct(documento, numero_de_ingreso, fecha_ingreso.y, .keep_all = T) 

data_egresos_acv_2 %>% 
  tabyl(departamento_actual)


data_egresos_acv_2 <- data_egresos_acv_2 %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(dif_dias = fecha_ingreso.y - fecha_ingreso.x) %>% 
  filter(dif_dias >= -5 & dif_dias < 10) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  select(-c(dif_dias))


data_egresos_acv_2 <- data_egresos_acv_2 %>% 
  rename("diagnostico" = clasificacion_acv,
         "mes_caci" = mes_acv, 
         "fecha_ingreso" = fecha_ingreso.x,
         "fecha_de_ingreso_caci" = fecha_ingreso.y) %>% 
  mutate(caci = "acv")


# Unión de base ICC
data_icc_2 <- data_icc_2 %>% 
  mutate(numero_de_identificacion = as.character(numero_de_identificacion)) %>% 
  mutate(numero_de_cuenta = as.character(numero_de_cuenta))

data_egresos_icc <- left_join(x = data_egresos_2, select(data_icc_2, numero_de_cuenta, 
                                                         eapb, mes_icc, fecha_de_ingreso), 
                              by = c("numero_de_cuenta" = "numero_de_cuenta"))

data_egresos_icc_2 <- data_egresos_icc %>% 
  filter(!is.na(eapb)) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URGE")) %>% 
  distinct(fecha_de_ingreso, fecha_de_egreso, numero_de_ingreso, .keep_all = TRUE) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) 


data_egresos_icc_2 <- data_egresos_icc_2%>% 
  rename("diagnostico" = eapb,
         "mes_caci" = mes_icc,
         "fecha_de_ingreso_caci" = fecha_de_ingreso) %>% 
  mutate(caci = "icc")

#export(data_egresos_icc_2, "data_egresos_icc_2.xlsx")

# Unión de base txc
data_txc_2 <- data_txc_2 %>% 
  mutate(numero_de_identificacion = as.character(numero_de_identificacion))

data_egresos_txc <- left_join(x = data_egresos_2, select(data_txc_2, numero_de_identificacion, 
                                                         eapb, mes_txc, fecha_de_ingreso), 
                              by = c("documento" = "numero_de_identificacion"))

data_egresos_txc_2 <- data_egresos_txc%>% 
  filter(!is.na(eapb)) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  rename("diagnostico" = eapb,
         "mes_caci" = mes_txc,
         "fecha_de_ingreso_caci" = fecha_de_ingreso) %>% 
  mutate(caci = "txc",
         icc = "icc")

#############################################################################################################

### Cruzar base de trasplante con ICC para eliminar pacientes de esta última. 
data_egresos_icc_2 <- left_join(data_egresos_icc_2, select(data_egresos_txc_2, cruce, 
                                                           icc), by = "cruce")

### Validar los casos que cruzan con trasplante

### Limpio la base de ICC posterior a la validacoin de los datos
data_egresos_icc_2 <- data_egresos_icc_2 %>% 
  distinct(numero_de_cuenta, .keep_all = TRUE) %>% 
  filter(is.na(icc)) %>% 
  select(-c(icc))

### Limpio la base de trasplante de la columna creada para cruce con ICC
data_egresos_txc_2 <- data_egresos_txc_2 %>% 
  select(-c(icc))

######################################################################################
# Uno las bases de los 4 caci 
data_grd <- rbind(data_egresos_sca_2, data_egresos_icc_2, data_egresos_acv_2
                  #data_egresos_txc_2
                  )


#Verificar pacientes en los meses del CACI ACV

data_grd %>% 
  group_by(caci) %>% 
  summarise(n = n())

###############################################################################################################

### Limpieza la base general de los CACI
data_grd_2<- data_grd %>% 
  filter(fecha_de_egreso > "2023-12-31") %>% 
  mutate(mes_equal = if_else(mes_ingreso == mes_caci, TRUE, FALSE), 
         dias_estancia = round(estancia_horas/24, digits = 1),
         caci = case_when(numero_de_cuenta == "687896" ~ "txc",
                          numero_de_cuenta == "693686" ~ "txc", 
                          TRUE ~ caci)) %>% 
  filter(!str_detect(diagnostico_ingreso_princial, "DENGUE|NEUMONIA|PIEL|PURPURA")) 


data_grd_2 %>% 
  tabyl(caci)

### Comportamiento de la estancia 
data_grd_2 %>% 
  summarise(median = median(dias_estancia),
            RI = quantile(dias_estancia, 0.25),
            RS = quantile(dias_estancia, 0.75))

## Validador de atenciones en CACI
data_grd_2 %>% 
  group_by(caci) %>%
  summarise(n = n_distinct(documento))


## como pueden existir mas de una cuenta por paciente debo dividirlas en la bas de ingresos y egresos

data_grd_3 <- data_grd_2 %>% 
  mutate(numero_de_cuenta_2 = str_extract(numero_de_cuenta, "(?<=//|//|)//s*//d+"),
         numero_de_cuenta_1 = sub("//|//|.*", "", numero_de_cuenta)) %>% 
  select(-c(numero_de_cuenta))

data_grd_4 <- data_grd_3 %>% 
  filter(!is.na(numero_de_cuenta_2)) %>% 
  mutate(numero_de_cuenta = numero_de_cuenta_2) %>% 
  select(-c(numero_de_cuenta_2, numero_de_cuenta_1))

data_grd_3 <- data_grd_3 %>% 
  select(-c(numero_de_cuenta_2)) %>% 
  rename("numero_de_cuenta" = numero_de_cuenta_1)

data_grd_5 <- rbind(data_grd_3, data_grd_4) %>% 
  mutate(numero_de_ingreso = as.character(numero_de_ingreso))

data_grd_5 %>% 
  group_by(caci) %>% 
  summarise(n = n_distinct(documento))

####################################################################################################################

#### cruce con la prestación de servicios de los pacientes identificados en el caci por número de ingreso

## importar basde servicios realizados
data_ordenes <- read.csv("~/Library/CloudStorage/OneDrive-800024390_DIMECLINICANEUROCARDIOVASCULARS.A/Documentos/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/ventas_detallado_jul_sep_2024.csv", sep=",")
data_ordenes_1 <- read.csv("~/Library/CloudStorage/OneDrive-800024390_DIMECLINICANEUROCARDIOVASCULARS.A/Documentos/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/ventas_detallado_oct_dic_2024.csv", sep=",")

data_ordenes <- read.csv("C:/Users/jshurtado/OneDrive - DIME/Documentos/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/ventas_detallado_jul_sep_2024.csv", sep=",")
data_ordenes_1 <- read.csv("C:/Users/jshurtado/OneDrive - DIME/Documentos/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/ventas_detallado_oct_dic_2024.csv", sep=",")

data_ordenes_1.1 <- import("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME /Documentos EDI/Personal JSH/analisis_caci_2023/Analisis costos GRD 2024/ventas_detallado_ene_mar_2024.csv")

#data_ordenes_1.1 <- rbind(data_ordenes, data_ordenes_1)

data_ordenes_2 <- data_ordenes_1.1 %>% 
  clean_names() %>% 
  mutate(cod_cargo = as.character(cod_cargo)) %>% 
  mutate(across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y")),
         mes_cargue = month(fecha_cargue, label = T),
         ingreso = as.character(ingreso),
         cuenta = as.character(cuenta)) %>% 
  #mutate(across(starts_with("valor"), ~ as.numeric(gsub("//.", "", gsub(",", ".", ., fixed = TRUE))))) %>% 
  #mutate(across(starts_with("total"), ~ as.numeric(gsub("//.", "", gsub(",", ".", ., fixed = TRUE))))) %>% 
  filter(tipo_registro == "CARGOS")

data_ordenes_2 %>% 
  tabyl(tipo_registro)


## unir la base de data_grd_5 con los servicios prestados a los pacientes de los caci

data_ordenes_2 <- left_join(data_ordenes_2, select(data_grd_2, numero_de_ingreso, caci),
                            by = c("ingreso" = "numero_de_ingreso"))


### Cuantos perdí en el cruce con las ordenes
data_ordenes_2.2 <- data_ordenes_2 %>% 
  distinct(identificacion, cuenta, fecha_cargue, cargo, costo, fecha_registro, .keep_all = T) %>% 
  filter(!is.na(caci))

data_revision_ordenes <- left_join(data_grd_5, select(data_ordenes_2, ingreso, cargo),
                                   by = c("numero_de_ingreso" = "ingreso")) %>% 
  filter(is.na(cargo))


### Evalúo pacientes
data_ordenes_2.2 %>% 
  group_by(caci) %>%
  summarise(n = n_distinct(identificacion))

### Verifico si todos los pacientes tienen ordenes

data_ordenes_2.3 <- data_ordenes_2.2 %>% 
  group_by(identificacion, ingreso) %>% 
  summarise(pacientes = n_distinct(cuenta)) %>% 
  ungroup()


data_grd_6 <- left_join(data_grd_2, select(data_ordenes_2.3, ingreso, pacientes),
                        by = c("numero_de_ingreso" = "ingreso"))

data_grd_6 %>% 
  count(is.na(pacientes))

data_grd_6 %>% 
  filter(!is.na(pacientes)) %>% 
  group_by(caci) %>% 
  summarise(n = n_distinct(documento))

##############################################################################################
## Importar base de costos de financiera

data_costo <- read_excel("/Users/juansebastianhurtadozapaa/Library/CloudStorage/OneDrive-800024390_DIMECLINICANEUROCARDIOVASCULARS.A/Documentos/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/costo_general_2024.xlsx")
data_costo <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/DIME /Documentos EDI/Personal JSH/analisis_caci_2023/Analisis costos GRD 2024/costo_general_2024.xlsx")


data_costo_2 <- data_costo %>% 
  clean_names() 

data_ordenes_3 <- data_ordenes_2 %>% 
  distinct(ingreso, cargo, fecha_cargue, mes_cargue, cargo, .keep_all = T) %>% 
  filter(!is.na(caci))


data_ordenes_4 <- data_ordenes_3 %>% 
  mutate(cod_cargo = if_else(cod_cargo == "876123", "876123 FFR", cod_cargo))

data_ordenes_5 <- left_join(data_ordenes_4, select(data_costo_2, codigo, 
                                                   valor, servicio),
                            by = c("cod_cargo" = "codigo")) 


data_ordenes_5 <- data_ordenes_5 %>% 
  distinct(cuenta, cargo, fecha_cargue, mes_cargue, valor_cargo_tarifario, .keep_all = T) %>% 
  mutate(valor = as.numeric(valor))

### determinar que sea el mismo número de pacientes
data_ordenes_4 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion))


### Revisar si todos los pacientes que estan en la base data_grd_5 tienen ordenes
data_grd_5.1 <- left_join(data_grd_2, select(data_ordenes_4, ingreso, cuenta, 
                                             departamento_cargue),
                          by = c("numero_de_ingreso" = "ingreso"))

data_grd_5.2 <- data_grd_5.1 %>% 
  filter(is.na(cuenta))


## Elimino de la base de datos las ordenes que no cruzaron con el listado de financiera
data_ordenes_5 <- data_ordenes_5 %>% 
  filter(!is.na(servicio)) 

### determinar que sea el mismo número de pacientes
data_ordenes_5 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion))

#############################################################################################
### Procesamiento de la base de medicamentos

## importar la base de medicamentos
data_mmto_2 <- data_ordenes_1.1 %>% 
  clean_names() %>% 
  mutate(cod_cargo = as.character(cod_cargo)) %>% 
  mutate(across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y"))) %>% 
  filter(tipo_registro == "FARMACIA")


data_mmto_3 <- data_mmto_2 %>% 
  # filter(!tipo_de_destino %in% c("null", "MOVIMIENTO DE BODEGA")) %>% 
  mutate(fecha_adm = as.Date(fecha_registro, format = "%y/%m/%d"),
         cuenta = as.character(cuenta),
         ingreso = as.character(ingreso)) 

data_grd_2 <- data_grd_2 %>% 
  mutate(numero_de_ingreso = as.character(numero_de_ingreso))


data_mmto_4 <- left_join(data_mmto_3, select(data_grd_2, numero_de_ingreso, caci),
                         by = c("ingreso" = "numero_de_ingreso"))

data_mmto_4_1 <- data_mmto_4 %>% 
  distinct(cargo, fecha_registro, fecha_cargue, factura, fecha_adm, .keep_all = T) %>% 
  filter(!is.na(caci))


### Validar el mismo número de pacientes
data_grd_2 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(documento)) 

data_mmto_4_1 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion), .groups = "drop")

########################################################################################
### como fueron diferentes faltan 6 voy a cruzar para comparar
#data_grd_5.1 <- left_join(data_grd_5, select(data_mmto_4_1, cuenta, cargo),
# by = c("numero_de_cuenta" = "cuenta"))

#data_grd_5.1 <- data_grd_5.1 %>% 
# filter(is.na(cargo))

### Base de datos de medicamento 1 
#data_mmto_4.2<-  data_mmto_4 %>% 
# filter(is.na(caci)) 

### Traer los pacientes que no cruzaron con número de cédula porque tienen diferente número de ingreso
#data_mmto_4.3 <- left_join(data_mmto_4.2, select(data_grd_5.1, documento, caci),
#                          by = c("identificacion" = "documento")) 

#data_mmto_4.4 <- data_mmto_4.3 %>% 
# filter(!is.na(caci.y))

#data_mmto_4.4 %>% 
# group_by(caci.y) %>% 
#summarise(pacientes = n_distinct(identificacion))

### Base de medicamentos 2 con los casos que no cruzaron por cuenta sino que toca por documento de identidad
#data_mmto_4.4 <- data_mmto_4.4 %>% 
# select(-c(caci.x)) %>% 
#  rename("caci" = caci.y) %>% 
# distinct(identificacion, cargo, fecha_cargue, cuenta, .keep_all = T)

### Base de medicamentos unida 
#data_mmto_5 <- rbind(data_mmto_4.1, data_mmto_4.4)

#data_mmto_5 <- data_mmto_5 %>% 
# mutate(mes = month(fecha_adm, label = T)) %>% 
#  mutate(costo = as.numeric(gsub("//,", "", gsub(".", ".", costo, fixed = TRUE)))) 
####################################################################################################
###  Base de medicamentos definitiva para trabajar

data_mmto_5 <- data_mmto_4_1 %>% 
  mutate(mes = month(fecha_adm, label = T),
         costo = as.character(costo)) %>% 
  mutate(costo_2 = as.numeric(gsub(",", ".", gsub("\\.", "", costo))))
         
         
         ,
         costo_2 = as.numeric(costo_2),
         costo_3 = (gsub("//.", "", costo)),
         costo_3 = (gsub(",", ".", costo_3)),
         costo_3 = as.numeric(costo_3),
         costo_4 = case_when(is.na(costo_2) ~ costo_3,
                             TRUE ~ costo_2),
         costo_4 = as.numeric(costo_4))


### Hacer algunas tablas para mirar costo de medicamentos
tabla_mmto_costo <- data_mmto_5 %>%
  mutate(departamento_2 = case_when(departamento_cargue == "UCIN" ~ "UCIN",
                                    departamento_cargue == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento_cargue == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento_cargue == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento_cargue))


## Tabla de costos medicamentos por CACI mes para para unión de costos
tabla_mmto_costo_caci <- data_mmto_5 %>%
  group_by(caci, mes) %>% 
  filter(!is.na(mes)) %>% 
  summarise(pacientes_mes = n_distinct(identificacion),
            costo_mmto = sum(costo_2), .groups = "drop")

tabla_mmto_costo_caci


### data de medicamentos selecciono a variable de interés para unirla a la gran base de todos los costos
data_mmto_final <- data_mmto_5 %>% 
  select(departamento_cargue, fecha_factura, nombres, identificacion, 
         cod_cargo, cargo, fecha_adm, cuenta,
         mes, caci, costo_2) %>% 
  mutate(medico = NA)

### Revisión de costos de servicios

## Cruzo todas las cuentas que puede tener un paciente por atención con las ordenes enviadas

data_ordenes_6 <- left_join(data_ordenes_5, select(data_grd_2, edad, sexo, 
                                                   departamento_actual, numero_de_cuenta,
                                                   estado_al_alta, dias_estancia, 
                                                   fecha_ingreso, fecha_de_egreso, 
                                                   mes_caci), by = c("cuenta" = "numero_de_cuenta"))


data_ordenes_6.1 <- left_join(data_ordenes_5, select(data_grd_2, edad, sexo, 
                                                     departamento_actual, documento,
                                                     estado_al_alta, dias_estancia, 
                                                     fecha_ingreso, fecha_de_egreso, 
                                                     mes_caci, fecha_de_ingreso_caci), by = c("identificacion" = "documento"))


data_ordenes_6.1 <- data_ordenes_6.1 %>% 
  distinct(cuenta, cargo, fecha_cargue, 
           valor_cargo_tarifario, identificacion, .keep_all = T) %>% 
  filter(!is.na(mes_caci)) %>% 
  mutate(mes_caci_2 = month(fecha_ingreso, label = T)) %>% 
  mutate(meses_igual = if_else(mes_caci == mes_caci_2, T, F))


data_ordenes_6.1 <- data_ordenes_6 %>% 
  mutate(departamento = departamento_cargue) %>% 
  mutate(nombre_completo = paste(nombres, apellidos, sep = " "))


## Tabla de costo de ordenes
tabla_ordenes_costo <- data_ordenes_5 %>% 
  mutate(departamento_2 = case_when(departamento_cargue == "UCIN" ~ "UCIN",
                                    departamento_cargue == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento_cargue == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento_cargue == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento_cargue)) %>% 
  group_by(cuenta, caci,mes_cargue) %>% 
  mutate(valor = replace_na(valor, 0)) %>% 
  summarise(costo_orden = sum(valor), .groups = "drop")

tabla_ordenes_costo

### Unión de bases de medicamentos y ordenes de servicio
## Ajuste a la base de medicamentos
data_mmto_final <- data_mmto_5 %>% 
  mutate(mes_factura = month(fecha_factura, label = T)) %>% 
  select(departamento_cargue, fecha_registro, nombres, id, identificacion, 
         cod_cargo, cargo, fecha_cargue, ingreso,
         mes_factura, caci, cuenta, costo_2, valor_cargo_tarifario, profesional_asignado) %>% 
  mutate(profesional_asignado = NA,
         servicio = "MEDICAMENTO o INSUMO",
         departamento_cargue_2 = "FARMACIA") 


## Ajuste a la base de ordenes 
data_ordenes_final <- data_ordenes_6.1 %>% 
  mutate(mes_factura = month(fecha_factura, label = T)) %>%
  select(departamento_cargue, fecha_registro, nombres, id, identificacion, 
         cod_cargo, cargo, fecha_cargue, ingreso,
         mes_factura, caci, cuenta, valor, valor_cargo_tarifario, profesional_asignado, servicio) %>% 
  mutate(departamento_cargue_2 = "FARMACIA") %>% 
  rename("costo_2" = valor) 

### Base de datos final de costos
data_costo_total <- rbind(data_mmto_final, data_ordenes_final) %>% 
  rename("costo" = costo_2)

data_costo_total %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion)) %>% 
  ungroup()


### Traer variables de caracterización de los pacientes de la base de ingresos y egresos (data_grd_3)

data_costo_total_2 <- left_join(data_costo_total, select(data_grd_2, edad, sexo, 
                                                         departamento_actual, numero_de_ingreso,
                                                         estado_al_alta, dias_estancia, 
                                                         fecha_ingreso, fecha_de_egreso, fecha_de_ingreso_caci,
                                                         mes_caci, diagnostico_ingreso_princial, 
                                                         diagnostico_egreso_principal,
                                                         diagnostico_egreso_secundario, procedimiento_qx), 
                                by = c("ingreso" = "numero_de_ingreso"))


data_costo_total_2 <- data_costo_total_2 %>% 
  distinct(cuenta, fecha_registro, cargo, fecha_registro,
           fecha_de_egreso, fecha_ingreso, costo, .keep_all = T) 

##############################################################################################################################################
### Separo la base de datos de costo total en quienes tienen fecha CACI y los que 
### para poder cruzar esto de nuevo con la base de datos de grd_2 y extraer el mes
#data_costo_total_2.1 <- data_costo_total_2 %>% 
#  filter(!is.na(mes_caci))

#data_costo_total_2.2 <- data_costo_total_2 %>% 
#  filter(is.na(mes_caci)) %>% 
#  select(-c(edad, sexo, 
#            departamento_actual,
#            estado_al_alta, dias_estancia, 
#            fecha_ingreso, fecha_egreso,
#            mes_caci))

#data_costo_total_2.3 <- left_join(data_costo_total_2.2, 
#                                  select(data_grd_5, edad, sexo, 
#                                         departamento_actual, documento,
#                                         estado_al_alta, dias_estancia, 
#                                         fecha_ingreso, fecha_egreso,
#                                         mes_caci),
#                                  by = c("identificacion" = "documento"))

# data_costo_total_2.3 <- data_costo_total_2.3 %>% 
#  distinct(fecha_cargue, cuenta, identificacion, cargo, .keep_all = T)


# data_costo_total_3 <- rbind(data_costo_total_2.1, data_costo_total_2.3) %>% 
#  filter(!mes_caci == "jul")

#####################################################################################
### Ajuste cuando hay dudas de temporalidad de los pacientes, 
Sys.setlocale("LC_TIME", "es_ES")

data_costo_total_2.1 <- data_costo_total_2 %>% 
  mutate(venta = as.numeric(gsub(",", ".", gsub("\\.", "", valor_cargo_tarifario))))

data_costo_total_3 <- data_costo_total_2.1 %>% 
  mutate(mes = month(fecha_ingreso, label = T),
         mes_ing_caci = month(fecha_de_ingreso_caci),
         dif_mes = (fecha_de_ingreso_caci - fecha_ingreso)) %>% 
  mutate(mes_dif = if_else(dif_mes < 91, T, F)) %>% 
  mutate(mes_dif_2 = if_else(dif_mes < -30, T, F),
         mes_egreso = month(fecha_de_egreso, label =T),
         mes_cargue = month(fecha_cargue, label =T)) %>% 
  filter(mes_dif %in% c(TRUE, NA)) %>% 
  filter(mes_dif_2 %in% c(FALSE, NA)) %>% 
  select(-c(mes_dif, mes_dif_2))

data_costo_total_3 <- data_costo_total_3 %>%
  filter(!(mes_cargue == "oct" & caci == "txc"))


data_costo_total_3 %>% 
  group_by(caci) %>% 
  summarise(n = n_distinct(identificacion))

### Validación de la clase de variable mes
class(data_costo_total_3$mes_cargue)
data_costo_total_3 %>% 
  tabyl(mes_egreso)

#############################################################################################################
### Exportar las bases de datos a RDS
export(data_costo_total_3, "data_costo_total_3_2025_I.rds")
export(data_costo_total_3, "data_costo_total_3_2025_I.xlsx")
export(data_grd_2, "data_grd_2_2025_I.rda")

#############################################################################################################
### Total costo CACI mes
data_costo_total_3 <- import("data_costo_total_3_2025_I.rds")

tabla_ordenes_costo_total <- data_costo_total_3 %>%
  mutate(costo = replace_na(costo, 0)) %>% 
  group_by(caci,mes_cargue) %>% 
  summarise(costo_orden = sum(costo),
            venta = sum(venta),
            pacientes = n_distinct(identificacion),
            promedio = mean(costo), .groups = "drop") %>% 
  flextable()

tabla_ordenes_costo_total


### Columna de total para todos los CACI
data_costo_total_4 <- data_costo_total_3 %>% 
  group_by(identificacion, mes_cargue, caci) %>% 
  summarise(costo = sum(costo), 
            venta = sum(venta), .groups = "drop")

data_caci_total <- data_costo_total_3 %>% 
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
         pacientes_acv, mediana_acv, costo_orden_acv,
         pacientes_sca, mediana_sca, costo_orden_sca, 
         pacientes_icc, mediana_icc, costo_orden_icc) 

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
  
### Total data for awareing 
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
                      pacientes_acv = sum(pacientes_acv),
                      mediana_acv = median(mediana_acv), 
                      costo_orden_acv = sum(costo_orden_acv),
                      pacientes_sca = sum(pacientes_sca),
                      mediana_sca = median(mediana_sca), 
                      costo_orden_sca = sum(costo_orden_sca),
                      pacientes_icc = sum(pacientes_icc),
                      mediana_icc = median(mediana_icc), 
                      costo_orden_icc = sum(costo_orden_icc),
                    #  pacientes_txc = sum(pacientes_txc),
                    #  mediana_txc = median(mediana_txc), 
                    #  costo_orden_txc = sum(costo_orden_txc),
                      Total = median(Total))) %>%
  flextable() %>% 
  add_header_row( top = TRUE,   # La nueva cabecera va encima de la fila de cabecera existente
                  values = c("Mes",     # Valores de cabecera para cada columna a continuación
                             "ACV", "", "",
                             "SCA", "", "",    # Este será el encabezado de nivel superior para esta columna y las dos siguientes
                             "ICC","", "",
                          #   "Trasplante", "", "",
                             "Total")) %>% 
  set_header_labels(mes_cargue = "",
                    pacientes_acv = "Pte", mediana_acv = "Costo medio", costo_orden_acv = "Costo total",
                    pacientes_sca = "Pte", mediana_sca = "Costo medio", costo_orden_sca = "Costo total",
                    pacientes_icc = "Pte", mediana_icc = "Costo medio", costo_orden_icc = "Costo total",
                  #  pacientes_txc = "Pte", mediana_txc = "Costo medio", costo_orden_txc = "Costo total",
                    Total = "") %>% 
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
 # merge_at(i = 1, j = 11:13, part = "header") %>% 
  theme_booktabs() %>% 
  vline(part = "all", j = 1, border = border_style) %>% 
  vline(part = "all", j = 4, border = border_style) %>% 
  vline(part = "all", j = 7, border = border_style) %>% 
  vline(part = "all", j = 10, border = border_style) %>% 
 # vline(part = "all", j = 13, border = border_style) %>% 
  hline(i = 1, part = "header", border = border_style) %>% 
  align(align = "center", j = c(2:10), part = "all")%>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j =1, part = "header") %>% 
  merge_at(i = 1:2, j =11, part = "header") %>% 
  # flextable::color(color = "white" ,i = 13, j = c(3,6,9,12), part = "body") %>% 
  flextable::set_caption(caption = as_paragraph(
    as_chunk("Tabla costo medio paciente por CACI, DIME, año 2025", 
             props = fp_text_default(bold = TRUE, font.size = 14)))) %>% 
  footnote(i =2, j=c(3,6,9,11), part = "header", value = as_paragraph(value ="Costo medio corresponde a la mediana de los costos paciente"))


### tabla para exportar costo medio paciente
tabla_costo_medio_2025 <-  data_costo_total_5 %>% 
  mutate(across(,replace_na,0)) %>% 
  bind_rows(summarize(., mes_cargue = "Total", 
                      pacientes_acv = sum(pacientes_acv),
                      mediana_acv = median(mediana_acv), 
                      costo_orden_acv = sum(costo_orden_acv),
                      pacientes_sca = sum(pacientes_sca),
                      mediana_sca = median(mediana_sca), 
                      costo_orden_sca = sum(costo_orden_sca),
                      pacientes_icc = sum(pacientes_icc),
                      mediana_icc = median(mediana_icc), 
                      costo_orden_icc = sum(costo_orden_icc),
                     # pacientes_txc = sum(pacientes_txc),
                    #  mediana_txc = median(mediana_txc), 
                    #  costo_orden_txc = sum(costo_orden_txc),
                      Total = median(Total))) %>% 
  export("tabla costo medio paciente 2025.xlsx")

### Tabla de costo, venta y rentabilidad
data_costo_total_6 <- data_costo_total_4 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_orden = sum(costo),
            venta = sum(venta), .groups = "drop")  %>% 
  mutate(rentabilidad = venta - costo_orden)%>% 
  mutate(rentabilidad = as.numeric(rentabilidad)) %>% 
  # mutate('Rentabilidad %' = round(rentabilidad/sum(rentabilidad)*100, digits = 2)) %>% 
  pivot_wider(id_cols = mes_cargue, names_from  = caci, 
              values_from = c(costo_orden, venta, rentabilidad)) %>% 
  select(mes_cargue,
         costo_orden_acv, venta_acv, rentabilidad_acv,
         costo_orden_sca, venta_sca, rentabilidad_sca,
         costo_orden_icc, venta_icc, rentabilidad_icc) %>%
        # costo_orden_txc, venta_txc, rentabilidad_txc 
  adorn_percentages(starts_with("rentabilidad"), denominator = "col",  na.rm = T) %>% 
  adorn_pct_formatting(,,,c(rentabilidad_acv, rentabilidad_sca,
                            rentabilidad_icc)) %>% 
                            #rentabilidad_txc
  adorn_ns(position = "front",,,c(rentabilidad_acv, rentabilidad_sca,
                                  rentabilidad_icc)) %>%
                                  #rentabilidad_txc 
  mutate(across(, replace_na, 0)) %>% 
  bind_rows(summarize(., mes_cargue = "Total", 
                      costo_orden_acv = sum(costo_orden_acv),
                      venta_acv = sum(venta_acv), 
                      #   rentabilidad_acv = as.character(sum(venta_acv) - sum(costo_orden_acv)),
                      costo_orden_sca = sum(costo_orden_sca),
                      venta_sca = sum(venta_sca), 
                      #    rentabilidad_sca = as.character(sum(venta_sca) - sum(costo_orden_sca)),
                      costo_orden_icc = sum(costo_orden_icc),
                      venta_icc = sum(venta_icc)))
                      #    rentabilidad_icc = as.character(sum(venta_icc) - sum(costo_orden_icc)),
                      #costo_orden_txc = sum(costo_orden_txc),
                      #venta_txc = sum(venta_txc))
#      rentabilidad_txc = as.character(sum(venta_txc) - sum(costo_orden_txc)))) 



tabla_rentabilidad_2025 <- data_costo_total_6  %>%
  flextable()  %>% 
  add_header_row( top = TRUE,   # La nueva cabecera va encima de la fila de cabecera existente
                  values = c("Mes",     # Valores de cabecera para cada columna a continuación
                             "ACV", "", "",
                             "SCA", "", "",    # Este será el encabezado de nivel superior para esta columna y las dos siguientes
                             "ICC","", "")) %>% 
                      #       "Trasplante", "", ""
  set_header_labels(mes_cargue = "",
                    costo_orden_acv = "Costo", venta_acv = "Venta", rentabilidad_acv = "Rentabilidad",
                    costo_orden_sca = "Costo", venta_sca = "Venta", rentabilidad_sca = "Rentabilidad",
                    costo_orden_icc = "Costo", venta_icc = "Venta", rentabilidad_icc = "Rentabilidad") %>% 
                  #  costo_orden_txc = "Costo", venta_txc = "Venta", rentabilidad_txc = "Rentabilidad"
  merge_at(i = 1, j = 2:4, part = "header") %>% 
  merge_at(i = 1, j = 5:7, part = "header") %>% 
  merge_at(i = 1, j = 8:10, part = "header") %>% 
 # merge_at(i = 1, j = 10:10, part = "header") %>% 
  theme_booktabs() %>% 
  vline(part = "all", j = 1, border = border_style) %>% 
  vline(part = "all", j = 4, border = border_style) %>% 
  vline(part = "all", j = 7, border = border_style) %>% 
  #vline(part = "all", j = 10, border = border_style) %>% 
  hline(i = 1, part = "header", border = border_style) %>% 
  align(align = "center", j = c(2:10), part = "all") %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  #bold(i = 13, bold = TRUE, part = "body") %>% 
  merge_at(i = 1:2, j =1, part = "header") %>% 
  # flextable::color(color = "white" ,i = 13, j = c(3,6,9,12), part = "body") %>% 
  flextable::set_caption(caption = as_paragraph(
    as_chunk("Rentabilidad por mes de los GRD/CACI, DIME, año 2025", 
             props = fp_text_default(bold = TRUE, font.size = 14)))) %>% 
  footnote(i =2, j=c(3,6,9), part = "header", value = as_paragraph(value ="Costo medio corresponde a la mediana de los costos paciente")) %>% 
  autofit()

tabla_rentabilidad_2025
#############################################################################################

### Valores de costos superiores a los 20 millones de pesos 
valores_100_mill <- data_costo_total_3 %>% 
  group_by(mes_cargue, cargo) %>% 
  summarise(Pacientes = n_distinct(identificacion),
            costo = sum(costo), 
            promedio = costo/Pacientes,
            .groups = "drop") %>% 
  filter(costo > 30000000) %>% 
  rename("Mes" = mes_cargue, 
         "Prestación" = cargo,
         "Costo" = costo,
         "Promedio costo" = promedio) %>% 
  mutate(Costo = scales::dollar(Costo, prefix = "$", big.mark = ".", decimal.mark = ",")) %>% 
  flextable() %>% 
  autofit()

valores_100_mill


}### Costo CACI por mes y número de pacientes. 
caci_costo <- data_costo_total_3 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(Pacientes = n_distinct(identificacion),
            costo = sum(costo), .groups = "drop") %>% 
  pivot_wider(id_cols = mes_cargue, names_from = caci, values_from = c(costo, Pacientes)) %>% 
  mutate(across(,replace_na, 0)) %>% 
  mutate(across(starts_with("costo"), ~ scales::dollar(., prefix = "$", big.mark = ".", decimal.mark = ","))) %>% 
  select(mes_cargue, costo_acv, Pacientes_acv, costo_icc, Pacientes_icc, costo_sca, Pacientes_sca, 
        # costo_txc, Pacientes_txc
         ) %>% 
  flextable() %>% 
  autofit() 

caci_costo %>% 
  add_header_row(
    top = TRUE,                
    values = c("Mes",     
               "ACV", "",
               "ICC", "",    
               "SCA", ""
              # "TxC", ""
               )) %>% 
  set_header_labels(       
    mes_cargue = "",
    costo_acv = "Costo", 
    Pacientes_acv = "Pacientes",
    costo_icc = "Costo", 
    Pacientes_icc = "Pacientes",
    costo_sca = "Costo", 
    Pacientes_sca = "Pacientes",
    costo_txc = "Costo") %>% 
   # Pacientes_txc = "Pacientes"
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_at(i = 1, j = 4:5, part = "header") %>% 
  merge_at(i = 1, j = 6:7, part = "header") %>% 
 # merge_at(i = 1, j = 8:9, part = "header") %>% 
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
  select(mes_cargue, costo_acv, Pacientes_acv, costo_icc, Pacientes_icc, costo_sca, Pacientes_sca, 
         costo_txc, Pacientes_txc) %>% 
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
                      acv = median(acv),
                      icc = median(icc),
                      sca = median(sca),
                      txc = median(txc))) %>% 
  rename("Mes cargue" = mes_cargue,
         "ACV" = acv,
         "ICC" = icc,
         "SCA" = sca,
         "TXC" = txc ) %>% 
  mutate(across(2:4, ~ scales::dollar(., prefix = "$"))) %>% 
  flextable()

tabla_cups_6 %>% 
  autofit() %>% 
  bold(i = 1, part = "header") %>% 
  bold(i = 13, part = "body") %>% 
  hline(i = 12, part = "body", border = fp_border_default(width = 1.5)) 

tabla_cups_6.1 <- tabla_cups_5 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_2 = sum(costo_2), .groups = "drop") %>% 
  pivot_wider(id_cols = mes_cargue, names_from = caci, values_from = costo_2) %>%
  mutate(across(,replace_na, 0)) %>% 
  bind_rows(summarise(., mes_cargue = "Total",
                      acv = mean(acv),
                      icc = mean(icc),
                      sca = mean(sca),
                      txc = mean(txc))) 


