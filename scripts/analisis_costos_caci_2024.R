pacman::p_load(tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms, 
               epikit, scales, gt, zoo, readxl)

## Cargar bases de datos
data_sca <- import("sca_2023.xlsx")
data_acv <- import("acv_2023.xlsx")
data_icc <- import("Consolidado IC -TxC.xlsx")
data_txc <- import("Consolidado IC -TxC.xlsx", which = "TxC")
data_egresos <- import("egresos_2024.xlsx")

data_mto <- read_excel("~/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/mmtos_administrados_jul_sep_2024.xlsx")

## limpieza de base de datos de egreso
data_egresos_2 <- data_egresos %>% 
  clean_names() %>% 
  mutate(across(starts_with("fecha"), as.Date)) %>% 
  filter(fecha_egreso > "2024-06-30") %>% 
  filter(!paciente %in% c("CARDENAS NIÑO FERNANDO","VELOSA ORTIZ MONICA ALEJANDRA", "EPS  SANITAS")) %>%
  distinct(paciente, documento, fecha_ingreso, .keep_all = TRUE) 

data_egresos_2 <- data_egresos_2 %>% 
  mutate(documento = as.character(documento)) %>% 
  select(-c(x25, x26))

## limpieza de bases de los CACI 

data_egresos_2 <- data_egresos_2 %>% 
  mutate(cruce = str_c(documento, fecha_ingreso, .sep = ""),
         mes_ingreso = month(fecha_ingreso, label = T)) 

data_sca_2 <- data_sca %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.numeric(fecha_de_ingreso)) %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso, origin = "1899-12-30")) %>% 
  filter(fecha_de_ingreso > "2024-06-30") %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  mutate(documento = str_extract(cedula, regex("//d+"))) %>% 
  mutate(cruce = str_c(documento, fecha_de_ingreso, .sep = ""),
         mes_sca = month(fecha_de_ingreso, label = T))

data_acv_2 <- data_acv %>% 
  clean_names() %>% 
  filter(fecha_ingreso > "2024-31") %>% 
  mutate(fecha_ingreso = as.Date(fecha_ingreso)) %>% 
  mutate(documento = str_extract(cedula, regex("//d+"))) %>% 
  mutate(cruce = str_c(cedula, fecha_ingreso, .sep = "")) %>% 
  mutate(fecha_egreso = as.numeric(fecha_egreso)) %>% 
  mutate(fecha_egreso = as.Date(fecha_egreso, origin = "1899-12-30"),
         mes_acv = month(fecha_ingreso, label = T)) 

data_icc_2 <- data_icc %>% 
  clean_names() %>% 
  filter(fecha_de_ingreso > "2023-12-31") %>% 
  mutate(documento = str_extract(numero_de_identificacion, regex("//d+"))) %>% 
  mutate(cruce = str_c(documento, fecha_de_ingreso, .sep = ""),
         mes_icc = month(fecha_de_ingreso, label = T))

data_txc_2 <- data_txc %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  mutate(documento = str_extract(numero_de_identificacion, regex("//d+"))) %>% 
  mutate(cruce = str_c(documento, fecha_de_ingreso, .sep = ""),
         mes_txc = month(fecha_de_ingreso, label = T))

# Unión de base SCA
data_egresos_sca <- left_join(x = data_egresos_2, select(data_sca_2, documento,
                                                       diagnostico, mes_sca, fecha_de_ingreso),
                            by = "documento") 

data_egresos_sca <- data_egresos_sca %>% 
  filter(!is.na(diagnostico)) %>% 
  distinct(numero_de_cuenta, .keep_all = TRUE) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS"))

data_egresos_sca <- data_egresos_sca %>% 
  rename("diagnostico" = diagnostico,
         "mes_caci" = mes_sca,
         "fecha_de_ingreso_caci" = fecha_de_ingreso) %>% 
  mutate(caci = "sca")



#export(data_egresos_2, "data_egresos_2.xlsx")


# Unión de base ACV
data_egresos_acv <- left_join(x = data_egresos_2, select(data_acv_2, documento, 
                                                      clasificacion_acv, mes_acv, fecha_ingreso),
                            by = "documento") 

data_egresos_acv_2 <- data_egresos_acv %>% 
  filter(!is.na(clasificacion_acv)) %>% 
  distinct(numero_de_cuenta, .keep_all = T) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS"))

data_egresos_acv_2 <- data_egresos_acv_2 %>% 
  distinct(numero_de_cuenta, clasificacion_acv, .keep_all = TRUE)

data_egresos_acv_2 <- data_egresos_acv_2 %>% 
  rename("diagnostico" = clasificacion_acv,
         "mes_caci" = mes_acv, 
         "fecha_de_ingreso_caci" = fecha_ingreso.y,
         "fecha_ingreso" = fecha_ingreso.x) %>% 
  mutate(caci = "acv")


# Unión de base ICC
data_egresos_icc <- left_join(x = data_egresos_2, select(data_icc_2, documento, 
                                                         eapb, mes_icc, fecha_de_ingreso), 
                            by = "documento")

data_egresos_icc_2 <- data_egresos_icc %>% 
  filter(!is.na(eapb)) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URGE")) %>% 
  distinct(numero_de_cuenta, .keep_all = TRUE) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS"))

data_egresos_icc_2 <- data_egresos_icc_2%>% 
  rename("diagnostico" = eapb,
         "mes_caci" = mes_icc,
         "fecha_de_ingreso_caci" = fecha_de_ingreso) %>% 
  mutate(caci = "icc")

#export(data_egresos_icc_2, "data_egresos_icc_2.xlsx")

# Unión de base txc
data_egresos_txc <- left_join(x = data_egresos_2, select(data_txc_2, documento, 
                                                         eapb, mes_txc, fecha_de_ingreso), 
                            by = "documento")

data_egresos_txc_2 <- data_egresos_txc%>% 
  filter(!is.na(eapb)) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS") &
         str_detect(procedimiento_qx, "TRASPLA")) %>% 
  rename("diagnostico" = eapb,
         "mes_caci" = mes_txc,
         "fecha_de_ingreso_caci" = fecha_de_ingreso) %>% 
  mutate(caci = "txc",
         icc = "cruce")


### Cruzar base de trasplante con ICC para eliminar pacientes de esta última. 
data_egresos_icc_2 <- left_join(data_egresos_icc_2, select(data_egresos_txc_2, cruce, 
                                                   icc), by = "cruce")

### Validar los casos que cruzan con trasplante
data_egresos_icc_2 %>% 
  select(paciente, documento, fecha_egreso, caci, numero_de_cuenta, icc) %>% 
  filter(!is.na(icc))

### Limpio la base de ICC posterior a la validacoin de los datos
data_egresos_icc_2 <- data_egresos_icc_2 %>% 
  distinct(numero_de_cuenta, .keep_all = TRUE) %>% 
  filter(is.na(icc)) %>% 
  select(-c(icc))

### Limpio la base de trasplante de la columna creada para cruce con ICC
data_egresos_txc_2 <- data_egresos_txc_2 %>% 
  select(-c(icc))

# Uno las bases de los 4 caci 
data_grd <- rbind(data_egresos_sca, data_egresos_icc_2, data_egresos_acv_2, 
                  data_egresos_txc_2)

#Verificar pacientes en los meses del CACI ACV
data_grd %>% 
  filter(caci == "acv") %>% 
  tabyl(mes_caci)



### Limpieza la base general de los CACI
data_grd_2<- data_grd %>% 
  filter(!fecha_egreso < "2024-01-01") %>% 
  mutate(mes_equal = if_else(mes_ingreso == mes_caci, TRUE, FALSE), 
         dias_estancia = round(estancia_horas/24, digits = 1),
         caci = case_when(numero_de_cuenta == "687896" ~ "txc",
                          numero_de_cuenta == "693686" ~ "txc", 
                          TRUE ~ caci)) %>% 
  filter(!str_detect(diagnostico_ingreso_princial, "DENGUE|NEUMONIA|PIEL|PURPURA")) %>% 
  distinct(numero_de_cuenta, fecha_egreso, valor_factura, .keep_all = T)


data_grd_2.1 <- data_grd %>% 
  filter(!fecha_egreso < "2024-01-01") %>% 
  mutate(mes_equal = if_else(mes_ingreso == mes_caci, TRUE, FALSE), 
         dias_estancia = round(estancia_horas/24, digits = 1),
         caci = case_when(numero_de_cuenta == "687896" ~ "txc",
                          numero_de_cuenta == "693686" ~ "txc", 
                          TRUE ~ caci)) %>% 
  filter(!str_detect(diagnostico_ingreso_princial, "DENGUE|NEUMONIA|PIEL|PURPURA")) %>% 
  distinct(documento, fecha_ingreso, valor_factura, .keep_all = T)

data_grd_2 %>% 
  tabyl(dias_estancia) %>% 
  arrange(desc(n))

data_grd_2.1 %>% 
  tabyl(mes_equal)

data_grd_2 %>% 
  tabyl(caci)

### Comportamiento de la estancia 
data_grd_2 %>% 
  summarise(median = median(dias_estancia),
            RI = quantile(dias_estancia, 0.25),
            RS = quantile(dias_estancia, 0.75))


data_grd_2 %>% 
  group_by(caci) %>%
  summarise(n = n_distinct(documento))
  

## como pueden existir mas de una cuenta por paciente debo dividirlas en la bas de ingresos y egresos

data_grd_3 <- data_grd_2 %>% 
  mutate(numero_de_cuenta_2 = str_extract(numero_de_cuenta, "(?<=//|//| )//d+"),
         numero_de_cuenta = str_extract(numero_de_cuenta, "^[^//s]+"))

data_grd_4 <- data_grd_3 %>% 
  filter(!is.na(numero_de_cuenta_2)) %>% 
  mutate(numero_de_cuenta = numero_de_cuenta_2) %>% 
  select(-c(numero_de_cuenta_2))

data_grd_3 <- data_grd_3 %>% 
  select(-c(numero_de_cuenta_2))

data_grd_5 <- rbind(data_grd_3, data_grd_4) %>% 
  mutate(numero_de_ingreso = as.character(numero_de_ingreso))

data_grd_5 %>% 
  group_by(caci) %>% 
  summarise(n = n_distinct(documento))

#### cruce con la prestación de servicios de los pacientes identificados en el caci por número de ingreso

## importar basde servicios realizados

data_ordenes <- read_excel("~/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/ventas_detallado_ene_junio_2024.xlsx")

data_ordenes_2 <- data_ordenes %>% 
  clean_names() %>% 
  mutate(across(starts_with("fecha"), as.Date),
         mes_cargue = month(fecha_cargue, label = T),
         ingreso = as.character(ingreso),
         cuenta = as.character(cuenta)) 


## unir la base de data_grd_5 con los servicios prestados a los pacientes de los caci

data_ordenes_2 <- left_join(data_ordenes_2, select(data_grd_5, numero_de_ingreso, caci),
                              by = c("ingreso" = "numero_de_ingreso"))

data_ordenes_2.2 <- left_join(data_ordenes_2, select(data_grd_5, documento, caci),
                            by = c("identificacion" = "documento")) ### Base de datos para cruce por número de identificación

data_ordenes_2.2 <- data_ordenes_2 %>% 
  distinct(identificacion, cuenta, fecha_cargue, cargo, fecha_registro, .keep_all = T) %>% 
  filter(!is.na(caci))


data_ordenes_2.2 %>% 
  group_by(caci) %>% 
  summarise(n = n_distinct(identificacion))

## Importar base de costos de financiera

data_costo <- read_excel("~/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/costo_general_2024.xlsx")

data_costo_2 <- data_costo %>% 
  clean_names() 

data_ordenes_2 <- data_ordenes_2 %>% 
  distinct(ingreso, cargo, fecha_cargue, cargo, .keep_all = T) %>% 
  filter(!is.na(caci))


data_ordenes_3 <- data_ordenes_2 %>% 
  mutate(cod_cargo = if_else(cod_cargo == "876123", "876123 FFR", cod_cargo))

data_ordenes_4 <- left_join(data_ordenes_3, select(data_costo_2, codigo, 
                                                   valor, servicio),
                            by = c("cod_cargo" = "codigo")) 

data_ordenes_4 <- data_ordenes_4 %>% 
  distinct(cuenta, cargo, fecha_cargue, valor_cargo_tarifario, .keep_all = T)

### determinar que sea el mismo número de pacientes
data_ordenes_4 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion))


### Revisar si todos los pacientes que estan en la base data_grd_3 tienen ordenes

data_grd_5.1 <- left_join(data_grd_5, select(data_ordenes_4, ingreso, cuenta, departamento_cargue),
                          by = c("numero_de_ingreso" = "ingreso"))

data_grd_5.2 <- data_grd_5.1 %>% 
  filter(is.na(cuenta))

## Ordenes que no cruzaron con el listado de CUPS de financiera
data_ordenes_4 %>% 
  filter(is.na(servicio)) %>% 
  group_by(cod_cargo, cargo) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  export("ordenes_no_cruce.xlsx")

data_ordenes_4 %>% 
  filter(is.na(servicio)) %>% 
  group_by(cargo) %>% 
  summarise(n = n_distinct(cod_cargo))

### OJO REVISAR CON NATHALY PORQUE NO ME DA
data_ordenes_4 %>% 
  filter(cod_cargo == 933601) %>% 
  count()


## Elimino de la base de datos las ordenes que no cruzaron con el listado de financiera

data_ordenes_5 <- data_ordenes_4 %>% 
  filter(!is.na(servicio))

### determinar que sea el mismo número de pacientes
data_ordenes_5 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion))

### Procesamiento de la base de medicamentos

## importar la base de medicamentos
data_mmto <- read_excel("~/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/mmtos_administrados_ene_jun_2024.xlsx")

data_mmto_2 <- data_mmto %>% 
  clean_names() %>% 
  mutate(across(starts_with("fecha"), as.Date)) 


  data_mmto_3 <- data_mmto_2 %>% 
  distinct(cuenta, descripcion_producto, .keep_all = T) %>% 
  clean_names() %>% 
  filter(!tipo_de_destino %in% c("null", "MOVIMIENTO DE BODEGA")) %>% 
  mutate(fecha_adm = as.Date(lapso, format = "%y/%m/%d"),
         cuenta = as.character(cuenta)) %>% 
  arrange(fecha_adm) %>% 
  mutate(id = str_extract(identificacion, regex("//d+")),
         tip_id = str_extract(identificacion, regex( "[A-Z]+"))) 


data_mmto_4 <- left_join(data_mmto_3, select(data_grd_5, numero_de_cuenta, caci),
                       by = c("cuenta" = "numero_de_cuenta"))

data_mmto_4.1 <- data_mmto_4 %>% 
  distinct(cuenta, lapso, descripcion_producto, .keep_all = T) %>% 
  filter(!is.na(caci))


### Validar el mismo número de pacientes
data_grd_5 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(documento)) 
  

data_mmto_4.1 %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(id))

### como fueron diferentes faltan 6 voy a cruzar para comparar

data_grd_5.1 <- left_join(data_grd_5, select(data_mmto_4, cuenta, descripcion_producto),
                          by = c("numero_de_cuenta" = "cuenta"))

data_grd_5.1 <- data_grd_5.1 %>% 
  filter(is.na(descripcion_producto))

### Base de datos de medicamento 1 
data_mmto_4.2<-  data_mmto_4 %>% 
  filter(is.na(caci)) 

### Traer los pacientes que no cruzaron con número de cédula porque tienen diferente número de ingreso
data_mmto_4.3 <- left_join(data_mmto_4.2, select(data_grd_5.1, documento, caci),
                           by = c("id" = "documento")) 

data_mmto_4.4 <- data_mmto_4.3 %>% 
  filter(!is.na(caci.y))

data_mmto_4.4 %>% 
  group_by(caci.y) %>% 
  summarise(pacientes = n_distinct(id))

data_grd_5.3 <- left_join(data_grd_5.1, select(data_mmto_4.4, id, 
                                               descripcion_producto),
                           by = c("documento" = "id"))

### Base de medicamentos 2 con los casos que no cruzaron por cuenta sino que toca por documento de identidad
data_mmto_4.4 <- data_mmto_4.4 %>% 
  select(-c(caci.x)) %>% 
  rename("caci" = caci.y) %>% 
  distinct(id, descripcion_producto, lapso, cuenta, .keep_all = T)

### Base de medicamentos unida 
data_mmto_5 <- rbind(data_mmto_4.1, data_mmto_4.4)

data_mmto_5 <- data_mmto_5 %>% 
  mutate(mes = month(fecha_adm))


### Hacer algunas tablas para mirar costo de medicamentos
tabla_mmto_costo <- data_mmto_5 %>%
  mutate(departamento_2 = case_when(departamento == "UCIN" ~ "UCIN",
                                    departamento == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento)) %>% 
  mutate(egresos = as.numeric(egresos)) %>% 
  group_by(tipo_de_destino, cuenta, caci, mes) %>% 
  summarise(costo_mmto = sum(costo_total)) %>% 
  ungroup()

tabla_mmto_costo

## Tabla costo total por CACI acumulado de los medicamentos
tabla_mmto_costo_caci <- data_mmto_5 %>%
  group_by(caci) %>% 
  filter(!is.na(mes)) %>% 
  summarise(pacientes_mes = n_distinct(identificacion),
            costo_mmto = sum(costo_total)*-1) %>% 
  ungroup()

  tabla_mmto_costo_caci

  
## Tabla de costos medicamentos por CACI mes para para unión de costos
  tabla_mmto_costo_caci <- data_mmto_5 %>%
    group_by(caci, mes) %>% 
    filter(!is.na(mes)) %>% 
    summarise(pacientes_mes = n_distinct(id),
              costo_mmto = sum(costo_total)*-1) %>% 
    ungroup()
  
  tabla_mmto_costo_caci

  
### data de medicamentos selecciono a variable de interés para unirla a la gran base de todos los costos
  data_mmto_final <- data_mmto_5 %>% 
    select(departamento, lapso, tipo_de_destino, identificacion, codigo_producto, descripcion_producto, fecha_adm, cuenta,
           mes, caci, costo_total) %>% 
    mutate(medico = NA)
  
  

### Revisión de costos de servicios

## Importar base de costos de financiera

data_costo <- read_excel("C:/Users/jshurtado/OneDrive - Dime Clinica Neurocardiovascular/Documentos/Documentos Juan Sebastian Hurtado Z/9. CACI 2023/Analisis costos GRD 2024/costo_general_2024.xlsx")
data_costo_2 <- data_costo %>% 
  clean_names() 


## Cruzo todas las cuentas que puede tener un paciente por atención con las ordenes enviadas

data_ordenes_6 <- left_join(data_ordenes_5, select(data_grd_5, edad, sexo, 
                                                     departamento_actual, numero_de_cuenta,
                                                     estado_al_alta, dias_estancia, 
                                                     fecha_ingreso, fecha_egreso, 
                                                     mes_caci), by = c("cuenta" = "numero_de_cuenta"))


data_ordenes_6.1 <- left_join(data_ordenes_5, select(data_grd_5, edad, sexo, 
                                                   departamento_actual, documento,
                                                   estado_al_alta, dias_estancia, 
                                                   fecha_ingreso, fecha_egreso, 
                                                   mes_caci, fecha_de_ingreso_caci), by = c("identificacion" = "documento"))



data_ordenes_6.2 <- data_ordenes_6 %>% 
  distinct(cuenta, cargo, fecha_cargue, 
           valor_cargo_tarifario, identificacion, .keep_all = T) %>% 
  filter(!is.na(mes_caci))

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

tabla_ordenes_costo <- data_ordenes_6 %>% 
  mutate(departamento_2 = case_when(departamento_actual == "UCIN" ~ "UCIN",
                                    departamento_actual == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento_actual == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento_actual == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento_actual)) %>% 
  group_by(nombre_cliente, cuenta, caci,mes_cargue) %>% 
  summarise(costo_orden = sum(valor)) %>% 
  ungroup()

tabla_ordenes_costo

### Unión de bases de medicamentos y ordenes de servicio

## Ajuste a la base de medicamentos

data_mmto_final <- data_mmto_5 %>% 
  select(departamento, lapso, tipo_de_destino, tip_id, id, codigo_producto,
         descripcion_producto, fecha_adm, 
         mes, caci, cuenta, costo_total) %>% 
  mutate(profesional_asignado = NA,
         servicio = "MEDICAMENTO",
         departamento_cargue = "FARMACIA") %>% 
  rename("fecha_registro" = lapso,
         "nombre_completo" = tipo_de_destino,
         "id" = tip_id,
         "identificacion" = id,
         "cod_cargo" = codigo_producto,
         "cargo" = descripcion_producto,
         "fecha_cargue" = fecha_adm,
         "valor" = costo_total,
         "mes_cargue" = mes) %>% 
  mutate(valor = if_else(valor <0, valor*-1, 
                         valor*-1))

data_mmto_final_2 <- left_join(data_mmto_final, select(data_ordenes_6, cuenta, 
                                                       plan), by = "cuenta") 

data_mmto_final_2 <- data_mmto_final_2 %>% 
  distinct(cuenta, cargo, fecha_cargue, valor, .keep_all = T)


## Ajuste a la base de ordenes 
data_ordenes_final <- data_ordenes_6.1 %>% 
  select(departamento, fecha_registro, nombre_completo, id, identificacion, 
         plan, cuenta, cod_cargo, cargo, departamento_cargue,  fecha_cargue,
         profesional_asignado, servicio, mes_cargue, caci, valor)

data_costo_total <- rbind(data_mmto_final_2, data_ordenes_final)

data_costo_total %>% 
  group_by(caci) %>% 
  summarise(pacientes = n_distinct(identificacion))


### Traer variables de caracterización de los pacientes de la base de ingresos y egresos (data_grd_3)

data_costo_total_2 <- left_join(data_costo_total, select(data_grd_5, edad, sexo, 
                                                     departamento_actual, numero_de_cuenta,
                                                     estado_al_alta, dias_estancia, 
                                                     fecha_ingreso, fecha_egreso, fecha_de_ingreso_caci,
                                                     mes_caci), by = c("cuenta" = "numero_de_cuenta"))

data_costo_total_2 <- data_costo_total_2 %>% 
  distinct(cuenta, fecha_cargue, cargo, .keep_all = T) %>% 
  mutate(mes_cargue = month(fecha_cargue, label = T))


data_costo_total_2 <- data_costo_total_2 %>% 
  mutate(across(starts_with("fec"), as.Date))

### Separo la base de datos de costo total en quienes tienen fecha CACI y los que 
### para poder cruzar esto de nuevo con la base de datos de grd_2 y extraer el mes
data_costo_total_2.1 <- data_costo_total_2 %>% 
  filter(!is.na(mes_caci))

data_costo_total_2.2 <- data_costo_total_2 %>% 
  filter(is.na(mes_caci)) %>% 
  select(-c(edad, sexo, 
            departamento_actual,
            estado_al_alta, dias_estancia, 
            fecha_ingreso, fecha_egreso,
            mes_caci))

data_costo_total_2.3 <- left_join(data_costo_total_2.2, 
                                  select(data_grd_5, edad, sexo, 
                                         departamento_actual, documento,
                                         estado_al_alta, dias_estancia, 
                                         fecha_ingreso, fecha_egreso,
                                         mes_caci),
                                  by = c("identificacion" = "documento"))

data_costo_total_2.3 <- data_costo_total_2.3 %>% 
  distinct(fecha_cargue, cuenta, identificacion, cargo, .keep_all = T)


data_costo_total_3 <- rbind(data_costo_total_2.1, data_costo_total_2.3) %>% 
  filter(!mes_caci == "jul")

data_costo_total_3 <- data_costo_total_3 %>% 
  mutate(mes = month(fecha_ingreso, label = T),
         mes_egreso = month(fecha_egreso, label = T),
         mes_ing_caci = month(fecha_de_ingreso_caci),
         dif_mes = (fecha_de_ingreso_caci - fecha_ingreso)) %>% 
  mutate(mes_dif = if_else(dif_mes < 91, T, F)) %>% 
  mutate(mes_dif_2 = if_else(dif_mes < -30, T, F)) %>% 
  filter(mes_dif %in% c(TRUE, NA)) %>% 
  filter(mes_dif_2 %in% c(FALSE, NA))


data_costo_total_3 %>% 
  tabyl(mes_dif)

rio::export(data_costo_total_3, "data_costo_total_3.csv")
### Analisis de costos totales. 

### Total costo CACI mes
tabla_ordenes_costo_total <- data_costo_total_3 %>% 
  filter(str_detect(mes_caci, "ene|feb|mar|abr|may|jun")) %>% 
  mutate(departamento = case_when(departamento == "UCIN" ~ "UCIN",
                                    departamento == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento)) %>% 
  group_by(caci,mes_egreso) %>% 
  summarise(costo_orden = sum(valor),
            pacientes = n_distinct(identificacion),
            promedio = costo_orden/pacientes,
            mediana = median(valor)) %>% 
  ungroup()

### costo total mes por CACI
tabla_ordenes_costo_total %>% 
  print(n=21) %>% 
  flextable()

tabla_ordenes_costo_total %>% 
  pivot_wider(id_cols = "caci", names_from = "mes_caci", values_from = "promedio") %>% 
  mutate(across(, replace_na, 0)) %>% 
  adorn_totals("both")

#######################################################################################

tabla_ordenes_costo_total_2 <- data_costo_total_3 %>% 
  filter(str_detect(mes_caci, "ene|feb|mar|abr|may|jun")) %>% 
  mutate(mes_cargue = factor(mes_cargue, levels = c("ene", "feb", "mar","abr", "may", "jun"))) %>% 
  mutate(departamento = case_when(departamento == "UCIN" ~ "UCIN",
                                  departamento == "UCIN 5 PISO" ~ "UCIN", 
                                  departamento == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                  departamento == "UCI-UCIN ANGIO" ~ "UCIN",
                                  TRUE ~ departamento)) %>% 
  group_by(mes_cargue) %>% 
  summarise(costo_orden = sum(valor),
            pacientes = n_distinct(identificacion),
            promedio = costo_orden/pacientes,
            mediana = median(valor)) %>% 
  ungroup()

tabla_ordenes_costo_total_2 
  
### Tabla de costos medio paciente por CACI acumulado 
tabla_ordenes_agrupada_pacientes <- data_costo_total_3 %>%
  filter(!mes_caci == "jul") %>% 
  group_by(identificacion, mes_caci, caci) %>% 
  summarise(costo = sum(valor)) %>% 
  mutate(costo = as.numeric(costo)) %>% 
  ungroup()


########################################################################

# Definir una función para ajustar una distribución Gamma y calcular la mediana
gamma_boot <- function(data, indices) {
  d <- data[indices]  # Muestra bootstrap
  fit <- fitdistr(d, "gamma")  # Ajustar la distribución Gamma
  shape <- fit$estimate["shape"]
  rate <- fit$estimate["rate"]
  mediana_gamma <- qgamma(0.5, shape = shape, rate = rate)  # Calcular la mediana teórica de la distribución Gamma ajustada
  return(mediana_gamma)
}

# Aplicar bootstrap para cada mes
resultados_boot <- by(data$costo, data$mes_caci, function(x) {
  boot_obj <- boot(x, statistic = gamma_boot, R = 1000)  # 1000 remuestreos bootstrap
  return(boot_obj)
})

# Extraer intervalos de confianza bootstrap para cada mes
IC_boot <- lapply(resultados_boot, function(x) {
  boot.ci(x, type = "perc")
})

# Extraer las medianas y los intervalos de confianza
median_bootstrap <- sapply(resultados_boot, function(x) median(x$t))
ci_lower <- sapply(IC_boot, function(x) x$percent[4])
ci_upper <- sapply(IC_boot, function(x) x$percent[5])

# Crear un data frame para la visualización
resultados_df <- data.frame(
  mes = factor(c("January", "February", "March", "April", "May", "June", "July"), 
               levels = c("January", "February", "March", "April", "May", "June", "July")),
  mediana = median_bootstrap,
  ci_lower = ci_lower,
  ci_upper = ci_upper
)

# Graficar los resultados usando ggplot2
library(ggplot2)
ggplot(resultados_df, aes(x = mes, y = mediana)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mediana de Costos por Mes (Ajuste Gamma + Bootstrap)",
       x = "Mes",
       y = "Mediana de Costos (con Intervalo de Confianza)") +
  theme_minimal()


### Gráfico de boxplot 

tabla_ordenes_agrupada_pacientes <- tabla_ordenes_agrupada_pacientes %>% 
  mutate(mes_caci = factor(mes_caci, levels = c("ene", "feb", "mar", "abr",
                                                 "may", "jun")))
  
pacman::p_load(plotly, viridis, hrbrthemes)

color_caci = c(acv = "skyblue", icc = "orange", sca = "pink", txc = "burlywood") 

caci_boxplot <- ggplot(tabla_ordenes_agrupada_pacientes, 
                       mapping = aes(x = caci,y = costo, fill = caci)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  scale_y_continuous(labels = scales::dollar_format(), n.breaks = 20) +
  scale_fill_manual (values =  color_caci) +
  xlab("GRD") +
  ylab("COSTOS") +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  facet_wrap("mes_caci") 

ggplotly(caci_boxplot)


tabla_ordenes_agrupada_pacientes_2 <- tabla_ordenes_agrupada_pacientes %>% 
  group_by(caci, mes_caci) %>% 
  summarise(mediana = median(costo)) %>% 
  filter(!caci == "txc")

tabla_costo_medio_paciente <- tabla_ordenes_agrupada_pacientes_2 %>% 
  pivot_wider(id_cols = "caci", names_from = "mes_caci", values_from = "mediana") %>% 
  mutate(across(, replace_na, 0)) %>% 
  adorn_totals("both")

### Tabla costo medio paciente por mes y CACI sin númnero de pacientes
tabla_costo_medio_paciente %>% 
  mutate(across(2:8, ~ scales::dollar(.x, big.mark = "."))) %>% 
  flextable()


### Costo medio paciente por CACI acumulado 
tabla_ordenes_agrupada_pacientes %>% 
  group_by(mes_caci) %>% 
  summarise(Total = sum(costo),
            Pacientes = n_distinct(identificacion),
            #Costo_medio = Total/Pacientes,
            Mediana = median(costo), 
            'RIQ-I (0.25)' = quantile(costo, 0.25),
            'RIQ-S (0.75)' = quantile(costo, 0.75)) %>% 
  rename(Mes = mes_caci) %>% 
  mutate_at(vars(Total, Mediana,  'RIQ-I (0.25)', 'RIQ-S (0.75)'), ~ scales::dollar(.x, big.mark = ".")) %>% 
  flextable() %>% 
  bold(part = "head")

### Tabla costo medio paciente por CACI
### ACV
tabla_costo_medio_acv <- tabla_ordenes_agrupada_pacientes %>% 
  filter(caci == "acv") %>% 
  group_by(mes_caci) %>%
  summarise(mediana = median(costo),
            costo_total  = sum(costo),
            pacientes = n_distinct(identificacion)) %>% 
  ungroup() %>% 
  adorn_totals() %>% 
  mutate_at(vars(costo_total, promedio, mediana), ~ scales::dollar(.x, big.mark = "."))

tabla_costo_medio_acv %>% 
  select(mes_caci, pacientes, costo_total, promedio, mediana) %>% 
  flextable()

data_costo_total_3_acv <- data_costo_total_3 %>% 
  filter(caci == "acv") %>% 
  group_by(fecha_cargue, departamento_cargue) %>% 
  summarise(costo_pac = sum(valor),
            pacientes = n_distinct(identificacion)) %>% 
  ungroup()

ggplot(data_costo_total_3_acv, aes(x = fecha_cargue, y = costo_pac, 
                                   size = pacientes)) +
  geom_point()


### SCA

tabla_costo_medio_sca <- tabla_ordenes_agrupada_pacientes %>% 
  group_by(caci, mes_caci) %>% 
  filter(caci == "sca") %>% 
  summarise(mediana = median(costo),
            costo_total  = sum(costo),
            pacientes = n_distinct(cuenta),
            promedio = costo_total/pacientes) %>% 
  ungroup() %>% 
  adorn_totals() %>% 
  mutate_at(vars(costo_total, promedio, mediana), ~ scales::dollar(.x, big.mark = "."))

tabla_costo_medio_sca %>% 
  select(caci, mes_caci, pacientes, costo_total, promedio, mediana) %>% 
  flextable()

### ICC

tabla_costo_medio_icc <- tabla_ordenes_agrupada_pacientes %>% 
  group_by(caci, mes_caci) %>% 
  filter(caci == "icc") %>% 
  summarise(mediana = median(costo),
            costo_total  = sum(costo),
            pacientes = n_distinct(cuenta),
            promedio = costo_total/pacientes) %>% 
  ungroup() %>% 
  adorn_totals() %>% 
  mutate_at(vars(costo_total, promedio, mediana), ~ scales::dollar(.x, big.mark = "."))

tabla_costo_medio_icc %>% 
  select(caci, mes_caci, pacientes, costo_total, promedio, mediana) %>% 
  flextable()

### TXC

tabla_ordenes_agrupada_pacientes %>% 
  filter(caci == "txc") %>% 
  group_by(caci, mes_caci) %>%
  summarise(costo = sum(costo))