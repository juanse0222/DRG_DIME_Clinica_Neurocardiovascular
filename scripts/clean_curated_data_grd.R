pacman::p_load(tidyverse, janitor, lubridate, flextable, gtsummary, rio, hms, 
               epikit, scales, gt, zoo, readxl, here)
####################################################################################################################
#### Preparation database to analyses process.

### 1. Charge database admissions patients: Database coming from admission and discharges from 2017 to 2025 and it is result of pre-screening and pre-processing 
data_ingresos <- import("/Users/juansebastianhurtadozapata/Desktop/DIME /Documentos EDI/16. Egresos DIME Mensual/egresos_dime_mensual/data_2025/data_egresos_2017_2025.rda") ## Database update until june 2025

### 2. Sales historic and last trimester of the year that I'm analyzing.
data_sales <- import(here("data", "data_costs", "data_sales.rds"))


## 3. Processing discharge database
data_ingresos_2 <- data_ingresos %>%
  clean_names() %>%
  mutate(documento = as.character(documento)) %>%
  mutate(across(starts_with("fecha"), as.Date)) %>%
  arrange(documento, desc(fecha_ingreso)) %>%
  mutate(
    mes = month(fecha_ingreso, label = T, abbr = T),
    año = year(fecha_ingreso)) %>% 
  filter(fecha_ingreso > "2023-12-31")

### 4. Please continue with pre-processing in script "function_diagnosis_algorithm_caci"

#### 5. To charge database CACI patients. 
  
##  5.1. import CACI's databases from folder
### Every-database must conserve the same structure and names of variables
data_sca <- import(here("data", "sca_2023.xlsx"))
data_acv <- import(here("data", "acv_2023.xls"))
data_icc <- import(here("data", "Consolidado IC -TxC.csv"))
data_txc <- import(here("data", "Consolidado IC -TxC.xlsx"), which = "TxC")
data_tep <- import(here("data", "tep_2025.xlsx"))
  
#############################################################################################################
### 6. Sort out information from database admission patients and CACI

### 6.1. Curate CACI database and clean data and variables
### SCA
data_sca_2 <- data_sca %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  filter(fecha_de_ingreso > "2023-12-31") %>% 
  mutate(cedula = str_extract(cedula, "\\d+"),
         mes_sca = month(fecha_de_ingreso, label = T),
         year = year(fecha_de_ingreso)) %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  mutate(cruce = str_c(cedula, mes_sca, year, .sep = ""))

### ACV
data_acv_2 <- data_acv %>% 
  clean_names() %>% 
  mutate(fecha_ingreso = as.numeric(fecha_ingreso),
         fecha_ingreso = as.Date(fecha_ingreso, origin = "1899-12-30")) %>% 
  filter(fecha_ingreso > "2023-12-31" & fecha_ingreso < "2025-10-01") %>% 
  mutate(cedula = str_extract(cedula, ("\\d+")),
         mes_acv = month(fecha_ingreso, label = T),
         year = year(fecha_ingreso)) %>% 
  mutate(cruce = str_c(cedula, mes_acv, year, .sep = "")) 


### ICC
data_icc_2 <- data_icc %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  filter(fecha_de_ingreso > "2023-12-31") %>% 
  mutate(numero_de_identificacion = str_extract(numero_de_identificacion, ("//d+")),
         mes_icc = month(fecha_de_ingreso, label = T),
         year = year(fecha_de_ingreso)) %>% 
  mutate(cruce = str_c(numero_de_identificacion, mes_icc, year, .sep = ""),
         fecha_de_ingreso = if_else(fecha_de_ingreso > "2025-03-31", 
                                    as.Date(fecha_de_ingreso, format = "%d/%m/%y"), fecha_de_ingreso))

### TXC
data_txc_2 <- data_txc %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso = as.Date(fecha_de_ingreso)) %>% 
  filter(fecha_de_ingreso > "2023-12-31") %>% 
  mutate(numero_de_identificacion = str_extract(numero_de_identificacion, "\\d+"),
         mes_txc = month(fecha_de_ingreso, label = T),
         year = year(fecha_de_ingreso)) %>% 
  mutate(cruce = str_c(numero_de_identificacion, mes_txc, year, .sep = ""))


### TEP
data_tep_2 <- data_tep %>% 
  clean_names() %>% 
  mutate(fecha_de_ingreso_a_uci_ucin = as.Date(fecha_de_ingreso_a_uci_ucin)) %>% 
  filter(fecha_de_ingreso_a_uci_ucin > "2023-12-31") %>% 
  mutate(numero_de_identificacion = str_extract(numero_de_identificacion, "\\d+"),
         mes_tep = month(fecha_de_ingreso_a_uci_ucin, label = T),
         year = year(fecha_de_ingreso_a_uci_ucin)) %>% 
  mutate(cruce = str_c(numero_de_identificacion, mes_tep, year, .sep = ""))


############################################# 7. Joining database admission and CACI - SCA #############################################

### 7.1 Bringing database with diagnosis set it up from algorithm applied on step 4 and assign new name and make a unique code to match with
### CACI databases

data_ingresos_3 <- result_2 %>% 
  mutate(caci_2 = str_to_upper(caci_2), 
         cruce = str_c(documento, mes,año, .sep = ""))

### 7.1.1. Join SCA database with admission data-set adjusted above. 
data_ingresos_sca <- left_join(x = data_ingresos_3, select(data_sca_2, cruce,
                                                         diagnostico, mes_sca), by = "cruce")

### 7.2. Arrange database fulfilling with the criteria for including patients about SCA in the analysis.
data_ingreso_sca_2.1 <- data_ingresos_sca %>% 
  filter(caci_2 == "SCA" & tipo_de_atencion == "HOSPITALARIO") %>% 
  filter(str_detect(departamento_actual, "UCI|UCIN|HOSPITA|URGENCIA")|
           str_detect(departamento_de_ingreso, "UCI|UCIN|HOSPITA|URGENCIA")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS") & dif_days > 1)

nrow(data_ingreso_sca_2.1[!is.na(data_ingreso_sca_2.1$diagnostico), ])


### 7.3. Patients just is part of the CACI (checking correlation between CACI and criteria that was used to define SCA in my code)
data_ingreso_sca_2.2 <- data_ingreso_sca_2.1 %>% 
  filter(!is.na(mes_sca))


### 7.4. Processing only patients that are part of the CACI
data_ingresos_sca_2 <- data_ingresos_sca %>% 
  filter(!is.na(mes_sca)) %>% 
  distinct(numero_de_cuenta, fecha_ingreso, documento, .keep_all = TRUE) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>%  
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS"))

data_ingresos_sca_2 %>% 
  tabyl(mes, año)

### 7.5. Joning database CACI patients with SCA patients I found using myself criteria
data_ingresos_sca_3 <- rbind(data_ingreso_sca_2.1, data_ingresos_sca_2) 

data_ingresos_sca_3 <- data_ingresos_sca_3 %>% 
  distinct(documento, fecha_ingreso, fecha_de_egreso, numero_de_cuenta, numero_de_ingreso, .keep_all = T)

### Include variables that I gonna need later for reviewing conditions in the patients
data_ingresos_sca_3 <- data_ingresos_sca_3 %>% 
  mutate(caci_3 = if_else(is.na(diagnostico), caci_2, "SCA")) %>% 
  rename("diagnostico" = diagnostico,
         "mes_caci" = mes_sca)

nrow(data_ingresos_sca_3[!is.na(data_ingresos_sca_3$diagnostico), ])


############################################# 8. Joining database admission and CACI - ACV #############################################

#### I'm using fort this merge identification ("cedula")
#data_ingresos_acv <- left_join(x = data_ingresos_3, 
 #                             select(data_acv_2, cedula, dignostico, 
  #                                   mes_acv), 
   #                           by = c("documento" = "cedula")) 

### 8.1 Join ACV database with admission data-set adjusted above. 
data_ingresos_acv_1 <- left_join(x = data_ingresos_3, 
                               select(data_acv_2, cruce, dignostico, mes_acv), 
                               by = "cruce") 

### 8.2. Arrange database fulfilling with the criteria for including patients about ACV in the analysis.
data_ingresos_acv_1.2 <- data_ingresos_acv_1 %>% 
  filter(!is.na(dignostico)) %>% 
  distinct(documento, numero_de_ingreso, fecha_ingreso, .keep_all = T) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>%  
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS"))


#data_ingresos_acv_2 <- data_ingresos_acv %>%
 # filter(!is.na(mes_acv)) %>% 
  #distinct(documento, numero_de_ingreso, fecha_ingreso, .keep_all = T) %>% 
  #filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  #mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  #filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS"))

#data_egresos_acv_2 <- data_egresos_acv_2 %>% 
#  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
#  mutate(dif_dias = fecha_ingreso.y - fecha_ingreso.x) %>% 
#  filter(dif_dias >= -5 & dif_dias < 10) %>% 
#  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
#  select(-c(dif_dias))


### 8.3. Criteria to include patients about ACV in the analysis.
data_ingreso_acv_2.1 <- data_ingresos_acv_1 %>% 
  filter(caci_2 == "ACV" & tipo_de_atencion == "HOSPITALARIO") %>% 
  filter(str_detect(departamento_actual, "UCI|UCIN|HOSPITA|URGENCIA")|
           str_detect(departamento_de_ingreso, "UCI|UCIN|HOSPITA|URGENCIA")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS") & dif_days > 1)


### 8.4 Bind rows in database ACV CACI patients with ACV patients I did find using myself criteria 
data_ingresos_acv_3 <- rbind(data_ingreso_acv_2.1, data_ingresos_acv_1.2) 

data_ingresos_acv_3 <- data_ingresos_acv_3 %>% 
  distinct(documento, fecha_ingreso, fecha_de_egreso, numero_de_cuenta, numero_de_ingreso, .keep_all = T)


### 8.5. Include variables that I gonna need later for reviewing conditions in the patients
data_ingresos_acv_3 <- data_ingresos_acv_3 %>% 
  mutate(caci_3 = if_else(is.na(dignostico), caci_2, "ACV")) %>% 
  rename("diagnostico" = dignostico,
         "mes_caci" = mes_acv)

############################################# 9. Joining database admission and CACI - ICC #############################################

### 9.1 Adjust class of variables for getting the join with admission then. 
data_icc_2 <- data_icc_2 %>% 
  mutate(numero_de_identificacion = as.character(numero_de_identificacion)) %>% 
  mutate(numero_de_cuenta = as.character(numero_de_cuenta))


### 9.2. Arrange database fulfilling with the criteria for including patients about ICC in the analysis.
data_ingresos_icc <- left_join(x = data_ingresos_3, select(data_icc_2, numero_de_cuenta, mes_icc, eapb), 
                              by = c("numero_de_cuenta" = "numero_de_cuenta"))


### 9.3. Criteria to include patients with ICC in the analysis.
data_ingresos_icc_2 <- data_ingresos_icc %>% 
  filter(!is.na(eapb)) %>% 
  distinct(fecha_de_egreso, fecha_ingreso, numero_de_ingreso, .keep_all = TRUE) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>%  
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS"))

### 9.4. Criteria to include patients about ICC in the analysis.
data_ingreso_icc_2.1 <- data_ingresos_icc %>% 
  filter(caci_2 == "ICC" & tipo_de_atencion == "HOSPITALARIO") %>% 
  filter(str_detect(departamento_actual, "UCI|UCIN|HOSPITA|URGENCIA")|
           str_detect(departamento_de_ingreso, "UCI|UCIN|HOSPITA|URGENCIA")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>% 
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS") & dif_days > 1)


### 9.5. Joining database ICC CACI patients with ACV patients I did find using myself criteria 
data_ingresos_icc_3 <- rbind(data_ingreso_icc_2.1, data_ingresos_icc_2) 

### Delete duplicated 
data_ingresos_icc_3 <- data_ingresos_icc_3 %>% 
  distinct(documento, fecha_ingreso, fecha_de_egreso, numero_de_cuenta, numero_de_ingreso, .keep_all = T)


### 9.6. Include variables that I gonna need later for reviewing conditions in the patients
data_ingresos_icc_3 <- data_ingresos_icc_3 %>% 
  mutate(caci_3 = if_else(is.na(eapb), caci_2, "ICC")) %>%
  rename("diagnostico" = eapb,
         "mes_caci" = mes_icc)

#export(data_egresos_icc_2, "data_egresos_icc_2.xlsx")

############################################# 10. Joining database admission and CACI - TxC #############################################

### 10.1 Adjust class of variables for getting the join with admission then. 
data_txc_2 <- data_txc_2 %>% 
  mutate(numero_de_identificacion = as.character(numero_de_identificacion))

data_ingresos_txc <- left_join(x = data_ingresos_3, select(data_txc_2, numero_de_identificacion, 
                                                         eapb, mes_txc), 
                              by = c("documento" = "numero_de_identificacion"))

### 10.2. Criteria to include patients about TxC in the analysis.
data_ingresos_txc_3 <- data_ingresos_txc %>% 
  filter(!is.na(eapb)) %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>%  
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  distinct(fecha_ingreso, fecha_de_egreso, numero_de_cuenta, .keep_all = T) %>% 
  mutate(caci_3 = if_else(is.na(eapb), caci_2, "TXC")) %>% 
  rename("diagnostico" = eapb,
         "mes_caci" = mes_txc) 

############################################# 11. Joining database admission and CACI - TEP #############################################

### 11.1. Adjust class of variables for getting the join with admission then. 
data_tep_2 <- data_tep_2 %>% 
  mutate(numero_de_identificacion = as.character(numero_de_identificacion))


data_ingresos_tep <- left_join(x = data_ingresos_3, select(data_tep_2, numero_de_identificacion, 
                                                           genero, mes_tep), 
                               by = c("documento" = "numero_de_identificacion"))

### 11.2. Criteria to include patients about TEP in the analysis.
data_ingresos_tep_3 <- data_ingresos_tep %>% 
  filter(!is.na(genero)) %>% 
  filter(caci_2 == "TEP" & tipo_de_atencion == "HOSPITALARIO") %>% 
  filter(str_detect(departamento_actual, "HOSPI|UCI|UCIN|URG")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " "),
         dif_days = as.numeric(fecha_de_egreso - fecha_ingreso)) %>%  
  filter(!str_detect(departamento_filtro, "URGENCIAS URGENCIAS|ANGIOGRAFIA URGENCIAS|CIRUGIA URGENCIAS")) %>% 
  mutate(departamento_filtro = paste(departamento_de_ingreso, departamento_actual, sep = " ")) %>% 
  distinct(fecha_ingreso, fecha_de_egreso, numero_de_cuenta, .keep_all = T) %>% 
  mutate(caci_3 = if_else(is.na(mes_tep), caci_2, "TEP")) %>%
  rename("diagnostico" = genero,
         "mes_caci" = mes_tep) 


############################################ 12. Joining four CACI database ###########################################################################################
#### 12.1. Bind four database of each CACI
data_grd <- rbind(data_ingresos_tep_3, data_ingresos_sca_3, data_ingresos_acv_3, 
                  data_ingresos_icc_3, data_ingresos_txc_3) 

### 12.2. Evaluating count 
data_grd %>% 
  tabyl(caci_3)
 
### 13.3. I. make sure that I have the exact number of registers 
data_grd <- data_grd %>% 
  distinct(documento, numero_de_ingreso, numero_de_cuenta, total_cuenta, .keep_all = T)

######################################################################################################################################

## Review behavior 
### 14.1 Cleaning and depuration CACI - Patients with ICC diagnostics adjusted to Trasplant according with database of the CACI
data_grd_2<- data_grd %>% 
  mutate(caci_3 = case_when(numero_de_cuenta == "687896" ~ "TXC",
                          numero_de_cuenta == "693686" ~ "TXC", 
                          TRUE ~ caci_3)) %>% 
  #filter(!str_detect(diagnostico_ingreso_princial, "DENGUE|NEUMONIA|PIEL|PURPURA")) %>% 
  mutate(dif_days = if_else(is.na(dif_days), as.numeric(as.Date("2025-06-30") - fecha_ingreso), dif_days))


### Evaluating again total number patients by CACI
data_grd_2 %>% 
  tabyl(caci_3)

### 14.2. Inpatient days distribution and characteristics over time
data_grd_2 %>% 
  mutate(dif_days = fecha_de_egreso - fecha_ingreso) %>% 
  summarise(median = median(dif_days),
            RI = quantile(dif_days, 0.25),
            RS = quantile(dif_days, 0.75))

## 14.3. How many patients in each CACI - only double check 
data_grd_2 %>% 
  group_by(caci_3) %>%
  summarise(n = n_distinct(documento))

#### Split patients with more than one account number - We must used because some matches isn't able to do using "numero de ingreso".
## 15. How I can have more than one account per patient I need divide in each one. 
data_grd_3 <- data_grd_2 %>%
  mutate(numero_de_cuenta = str_trim(numero_de_cuenta)) %>%
  separate(numero_de_cuenta,
           into = c("numero_de_cuenta_1", "numero_de_cuenta_2"),
           sep = "\\|\\|",
           extra = "drop",
           fill = "right") %>%
  mutate(across(starts_with("numero_de_cuenta"), str_trim))

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
  group_by(caci_3) %>% 
  summarise(n = n_distinct(documento))

####################################################################################################################
#### 16. Sales database matched with the service's attentions and patients identified within of the CACI by admission number ("numero de ingreso")

#### 16.1. Curate and rename sales database 
data_ordenes_2 <- data_sales %>% 
  clean_names() %>% 
  mutate(cod_cargo = as.character(cod_cargo)) %>% 
  mutate(across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y")),
         mes_cargue = month(fecha_cargue, label = T),
         ingreso = as.character(ingreso),
         cuenta = as.character(cuenta)) %>% 
  #mutate(across(starts_with("valor"), ~ as.numeric(gsub("//.", "", gsub(",", ".", ., fixed = TRUE))))) %>% 
  #mutate(across(starts_with("total"), ~ as.numeric(gsub("//.", "", gsub(",", ".", ., fixed = TRUE))))) %>% 
  filter(tipo_registro == "CARGOS")


### 16.2. Join the database GRD with all services that was provided to the patients. 
data_grd_2 <- data_grd_2 %>% mutate(numero_de_ingreso = as.character(numero_de_ingreso)) %>% 
  mutate(mes_egreso = month(fecha_de_egreso, label = T, abbr = T),
         año_egreso = year(fecha_de_egreso),
         cruce_2 = paste(documento, mes_egreso, año_egreso, sep = ""))
  

data_ordenes_3 <- left_join(data_ordenes_2, select(data_grd_2, numero_de_ingreso, caci_3),
                            by = c("ingreso" = "numero_de_ingreso")) %>% 
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario, total_cuenta,
           costo, transaccion, fecha_registro, ingreso, .keep_all = T) ### Delete duplicated and errors after joining
  

############################################ 17. Review orders without not match #############################################
### How many I lost in the match - review. 
data_ordenes_2.2 <- data_ordenes_3 %>% 
  filter(!is.na(caci_3))

### Orders without match adjusted for new merge
data_revision_ordenes <- left_join(data_grd_5, select(data_ordenes_2, ingreso, cargo),
                                   by = c("numero_de_ingreso" = "ingreso")) %>% 
  filter(is.na(cargo)) %>% 
  select(-c(cargo, cruce)) %>% 
  mutate(mes_egreso = month(fecha_de_egreso, label = T, abbr = T),
         año_egreso = year(fecha_de_egreso),
         cruce_2 = paste(documento, mes_egreso, año_egreso, sep = ""))

### grd database adjusted for new merge with orders didn't match

data_ordenes_2.1 <- data_ordenes_2 %>% 
  mutate(mes_egreso = month(fecha_egreso, label = T, abbr = T),
         año_egreso = year(fecha_egreso),
         cruce_2 = paste(identificacion, mes_egreso, año_egreso, sep = ""))

#### Merge or match register didn't match using new code for get it on

data_ordenes_3.1 <- left_join(data_ordenes_2.1, select(data_revision_ordenes, cruce_2, caci_3),
                              by = "cruce_2") %>% 
  filter(!is.na(caci_3)) %>% 
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario, total_cuenta,
           costo, transaccion, fecha_registro, ingreso, .keep_all = T) %>% 
  select(-c(mes_egreso, año_egreso, cruce_2))

### Join both databases: registers that didn't match initially with the rest of registers
data_ordenes_4 <- rbind(data_ordenes_2.2, data_ordenes_3.1) %>% 
  mutate(cod_cargo = as.character(cod_cargo))

data_ordenes_4 <- data_ordenes_4 %>% 
  mutate(cod_cargo_2 = str_extract(cod_cargo, ".*(?=-)"),
         cod_cargo_2 = if_else(is.na(cod_cargo_2), cod_cargo, cod_cargo_2))


### Review if is the same number the patients before and now 
data_ordenes_4%>% 
  group_by(caci_3) %>%
  summarise(n = n_distinct(identificacion))

############################################## 18. Inclusion database costs by planning team ################################################
## 18.1. Import database by planning team. 
data_costo_2025 <- import(here("data", "data_costs", "costo_general_2024.xlsx"), which = "2025")

### 18.2 Clean and curate costs database
data_costo_2025 <- data_costo_2025 %>% 
  clean_names() %>% 
  mutate_at(vars("codigo", "codigo_2"), as.character)

### 18.3. Join data sales with the data costs to identify CUPS costs
data_orders <- left_join(data_ordenes_4, select(data_costo_2024, codigo, valor),
                                 by = c("cod_cargo_2" = "codigo")) 
 
data_orders <- data_orders %>% 
  distinct(identificacion, cuenta, fecha_cargue, cargo, valor_cargo_tarifario, total_cuenta,
           costo, transaccion, fecha_registro, ingreso, .keep_all = T) %>% 
  mutate(valor = replace_na(valor, 0))

data_orders_check <- data_orders %>% 
  filter(is.na(valor)) 


data_orders_check %>% 
  tabyl(cargo) %>% 
  arrange(desc(n)) %>% 
  flextable()


### Determine if is the same number of patients. 
data_orders%>% 
  group_by(caci_3) %>% 
  summarise(pacientes = n_distinct(identificacion))

############################################ 19. Medications/drugs administrated ################################################################################################

### 19.1. Filter within data sales drugs ("FARMACIA")
data_mmto_2 <- data_sales %>% 
  clean_names() %>% 
  mutate(cod_cargo = as.character(cod_cargo)) %>% 
  mutate(across(starts_with("fecha"), ~ as.Date(., format = "%d/%m/%y"))) %>% 
  filter(tipo_registro == "FARMACIA")


### 19.2. Setting up class and characteristics of variables
data_mmto_3 <- data_mmto_2 %>% 
  # filter(!tipo_de_destino %in% c("null", "MOVIMIENTO DE BODEGA")) %>% 
  mutate(fecha_adm = as.Date(fecha_registro, format = "%y/%m/%d"),
         cuenta = as.character(cuenta),
         ingreso = as.character(ingreso)) 

data_grd_2 <- data_grd_2 %>% 
  mutate(numero_de_ingreso = as.character(numero_de_ingreso))

### 19.3. Join data sales by drugs with information from admission and CACI patients 
data_mmto_4 <- left_join(data_mmto_3, select(data_grd_2, numero_de_ingreso, caci_3),
                         by = c("ingreso" = "numero_de_ingreso"))

data_mmto_4_1 <- data_mmto_4 %>% 
  distinct(cargo, fecha_registro, fecha_cargue, factura, fecha_adm, .keep_all = T) %>% 
  filter(!is.na(caci_3))


### Review total patients
data_grd_2 %>% 
  group_by(caci_3) %>% 
  summarise(pacientes = n_distinct(documento)) 

data_mmto_4_1 %>% 
  group_by(caci_3) %>% 
  summarise(pacientes = n_distinct(identificacion), .groups = "drop")


#######################################################################################################################
### 20. Database of the medications or drugs administrated final version
data_mmto_5 <- data_mmto_4_1 %>% 
  mutate(mes = month(fecha_adm, label = T),
         costo = as.character(costo),
         costo_2 = (gsub(",", "", costo)),
         costo_2 = as.numeric(costo_2))


#mutate(costo_2 = gsub(".", ".", gsub("\\.", "", costo)))
#costo_2 = as.numeric(costo_2),
#costo_3 = (gsub("//.", "", costo)),
#costo_3 = (gsub(",", ".", costo_3)),
#costo_3 = as.numeric(costo_3),
#costo_4 = case_when(is.na(costo_2) ~ costo_3,
#                    TRUE ~ costo_2),
#costo_4 = as.numeric(costo_4))


### 20.1. Tables for checking up medications costs
tabla_mmto_costo <- data_mmto_5 %>%
  mutate(departamento_2 = case_when(departamento_cargue == "UCIN" ~ "UCIN",
                                    departamento_cargue == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento_cargue == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento_cargue == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento_cargue))


### 20.2. Table monthly cost drugs by CACI
tabla_mmto_costo_caci <- data_mmto_5 %>%
  group_by(caci_3, mes) %>% 
  filter(!is.na(mes)) %>% 
  summarise(pacientes_mes = n_distinct(identificacion),
            costo_mmto = sum(costo_2), .groups = "drop")

tabla_mmto_costo_caci


### 20.3. Final database of medications select variable of interest to join to bigger database with all costs
data_mmto_final <- data_mmto_5 %>% 
  select(departamento_cargue, fecha_factura, nombres, identificacion, 
         cod_cargo, cargo, fecha_adm, cuenta,
         mes, caci_3, costo_2) %>% 
  mutate(medico = NA)

### Costs per service administrated or discharged
#########################################################################################################################################
#### 21. Cross-reference the all accounts that be able each patient by service or attention respect to orders it was deliveried

### 21.1. Join data sales with data from patients admitted and CACI. 
data_ordenes_6 <- left_join(data_orders, select(data_grd_2, edad, sexo, 
                                                departamento_actual, numero_de_ingreso,
                                                estado_al_alta, dif_days,
                                                fecha_ingreso, fecha_de_egreso, 
                                                mes_caci), by = c("ingreso" = "numero_de_ingreso"))

### 21.2. Delete registers repeated 
data_ordenes_6 <- data_ordenes_6 %>% 
  distinct(numero_id_cliente, cuenta, identificacion, apellidos, 
           departamento_ingreso, departamento_cargue, fecha_cargue, fecha_registro,
           total_cuenta, valor_cargo_tarifario, fecha_factura, transaccion,
           ingreso, .keep_all = T) 

### 21.3. Filter registers whom information are complete regard to admission database and CACI 
data_ordenes_6.1 <- data_ordenes_6 %>% 
  filter(!is.na(departamento_actual))

sum(is.na(data_ordenes_6$departamento_actual))

### 21.4.Filter patients without information from admission database and CACI for matching again with the same admission database but by id.
data_ordenes_6.2 <- data_ordenes_6 %>% 
  filter(is.na(departamento_actual)) %>% 
  select(-c(edad, sexo, 
            departamento_actual,
            estado_al_alta, dif_days,
            fecha_ingreso, fecha_de_egreso, 
            mes_caci))

### 21.5. Join data sales with data from admission and CACI using as key id. 
data_ordenes_6.2 <- left_join(data_ordenes_6.2, select(data_grd_2, edad, sexo, 
                                                     departamento_actual, documento,
                                                     estado_al_alta, dif_days, 
                                                     fecha_ingreso, fecha_de_egreso, 
                                                     mes_caci), by = c("identificacion" = "documento"))

### 21.6. Delete registers repeated
data_ordenes_6.2 <- data_ordenes_6.2 %>% 
  distinct(numero_id_cliente, cuenta, identificacion, apellidos, 
           departamento_ingreso, departamento_cargue, fecha_cargue, fecha_registro,
           total_cuenta, valor_cargo_tarifario, fecha_factura, transaccion,
           ingreso, .keep_all = T)

### 21.7. Finally bind both databases (preliminary with full data from admission and new register that were matched through id)
data_ordenes_6 <- rbind(data_ordenes_6.1, data_ordenes_6.2) %>% 
  mutate(departamento = departamento_cargue) %>% 
  mutate(nombre_completo = paste(nombres, apellidos, sep = " "))


#data_ordenes_6.1 <- data_ordenes_6.1 %>% 
#  distinct(cuenta, cargo, fecha_cargue, 
#           valor_cargo_tarifario, identificacion, .keep_all = T) %>% 
#  filter(!is.na(mes_caci)) %>% 
#  mutate(mes_caci_2 = month(fecha_ingreso, label = T)) %>% 
#  mutate(meses_igual = if_else(mes_caci == mes_caci_2, T, F))

## 22. Table orders cost per each account - Validation daata. 
tabla_ordenes_costo <- data_orders %>% 
  mutate(departamento_2 = case_when(departamento_cargue == "UCIN" ~ "UCIN",
                                    departamento_cargue == "UCIN 5 PISO" ~ "UCIN", 
                                    departamento_cargue == "HOSPITALIZACION 3 PISO" ~ "HOSPITALIZACION",
                                    departamento_cargue == "UCI-UCIN ANGIO" ~ "UCIN",
                                    TRUE ~ departamento_cargue)) %>% 
  group_by(cuenta, caci_3,mes_cargue) %>% 
  mutate(valor = replace_na(valor, 0)) %>% 
  summarise(costo_orden = sum(valor), .groups = "drop")

tabla_ordenes_costo

### 23. Joining medications database with the other servicies that was provied 
## Adjustment medications database
data_mmto_final <- data_mmto_5 %>% 
  mutate(mes_factura = month(fecha_factura, label = T)) %>% 
  select(nombre_cliente, tipo_cliente, plan, departamento_cargue, fecha_registro, 
         id, nombres, identificacion, cod_cargo, cargo, fecha_cargue, ingreso, 
         mes_factura, caci_3, cuenta, transaccion, costo_2, valor_cargo_tarifario, profesional_asignado) %>% 
  mutate(profesional_asignado = NA,
         departamento_cargue_2 = "FARMACIA") 


## 23.1. Adjustment orders (without medications)  
data_ordenes_final <- data_orders %>% 
  mutate(mes_factura = month(fecha_factura, label = T)) %>%
  select(nombre_cliente, tipo_cliente, plan, departamento_cargue, fecha_registro, 
         id, nombres, identificacion, cod_cargo, cargo, fecha_cargue, ingreso, 
         mes_factura, caci_3, cuenta, transaccion, valor, valor_cargo_tarifario, profesional_asignado) %>% 
  mutate(departamento_cargue_2 = "FARMACIA") %>% 
  rename("costo_2" = valor) 

################################################ 24. Final database - work data to build website and report ##################################
### 24.1. Final database of the cost DIME 
data_costo_total <- rbind(data_mmto_final, data_ordenes_final) 

data_costo_total %>% 
  group_by(caci_3) %>% 
  summarise(pacientes = n_distinct(identificacion)) %>% 
  ungroup()


### 24.2. Caught variables with main characteristics about patient of the admission database

data_costo_total_2 <- left_join(data_costo_total, select(data_grd_2, edad, sexo, 
                                                         departamento_actual, numero_de_ingreso,
                                                         estado_al_alta, dif_days, 
                                                         fecha_ingreso, fecha_de_egreso,
                                                         mes_caci, diagnostico_ingreso_princial, 
                                                         diagnostico_egreso_principal,
                                                         diagnostico_egreso_secundario, procedimiento_qx), 
                                by = c("ingreso" = "numero_de_ingreso"))


#### 24.3. Delete registers duplicated
data_costo_total_2 <- data_costo_total_2 %>% 
  distinct(cuenta, departamento_cargue, cod_cargo, cuenta, departamento_actual, dif_days,
           fecha_registro, cargo, fecha_registro, id, identificacion, ingreso, estado_al_alta,
           fecha_de_egreso, fecha_ingreso, costo_2, fecha_cargue, valor_cargo_tarifario, 
           diagnostico_ingreso_princial, transaccion, diagnostico_egreso_secundario, 
           .keep_all = T) 


sum(is.na(data_costo_total_2$departamento_actual))

### 24.4. Identify some registers those who aren't information from admission or CACI 
data_costo_total_2.1 <- data_costo_total_2 %>% 
  filter(!is.na(departamento_actual))

### 24.5. Choose only registers those who is admission data is NA
data_costo_total_2.2 <- data_costo_total_2 %>% 
  filter(is.na(departamento_actual)) %>% 
  select(-c(edad, sexo, 
            departamento_actual,
            estado_al_alta, dif_days, 
            fecha_ingreso, fecha_de_egreso,
            mes_caci, diagnostico_ingreso_princial, 
            diagnostico_egreso_principal,
            diagnostico_egreso_secundario, procedimiento_qx))

### 24.6. Join data sales with data from admission and CACI using as key id. 
data_costo_total_2.2 <- left_join(data_costo_total_2.2, select(data_grd_2, edad, sexo, 
                                                               departamento_actual, documento,
                                                               estado_al_alta, dif_days, 
                                                               fecha_ingreso, fecha_de_egreso,
                                                               mes_caci, diagnostico_ingreso_princial, 
                                                               diagnostico_egreso_principal,
                                                               diagnostico_egreso_secundario, procedimiento_qx), 
                                  by = c("identificacion" = "documento"))

### 24.7. Bind both databases to get a final data
data_costo_total_2.2 <- data_costo_total_2.2 %>% 
  distinct(cuenta, costo_2, transaccion, fecha_cargue, valor_cargo_tarifario, .keep_all = T) 

### Change name of variable. 
data_costo_total_2 <- rbind(data_costo_total_2.1, data_costo_total_2.2) %>% 
  mutate(departamento = departamento_cargue) 


##############################################################################################################################
### Ajuste cuando hay dudas de temporalidad de los pacientes, 
Sys.setlocale("LC_TIME", "es_ES")

data_costo_total_2.1 <- data_costo_total_2 %>% 
  mutate(venta = gsub(",", "", valor_cargo_tarifario),
         venta = as.numeric(venta))


#### Be carefull review variables and determine what apply 
data_costo_total_3 <- data_costo_total_2.1 %>% 
  mutate(mes = month(fecha_ingreso, label = T),
         #mes_ing_caci = month(fecha_de_ingreso_caci),
         #dif_mes = (fecha_de_ingreso_caci - fecha_ingreso)
#  mutate(mes_dif = if_else(dif_mes < 91, T, F)) %>% 
#  mutate(mes_dif_2 = if_else(dif_mes < -30, T, F),
         mes_egreso = month(fecha_de_egreso, label =T),
         mes_cargue = month(fecha_cargue, label =T))
 # filter(mes_dif %in% c(TRUE, NA)) %>% 
#  filter(mes_dif_2 %in% c(FALSE, NA)) %>% 
 # select(-c(mes_dif, mes_dif_2))

data_costo_total_3 <- data_costo_total_3 %>%
  mutate(año = year(fecha_cargue),
         año = as.factor(año)) %>% 
  filter(fecha_cargue < "2025-10-01")


data_costo_total_3 %>% 
  group_by(caci_3) %>% 
  summarise(n = n_distinct(identificacion))

### Validación de la clase de variable mes
class(data_costo_total_3$mes_cargue)
data_costo_total_3 %>% 
  tabyl(mes_egreso)

#############################################################################################################
### Exportar las bases de datos a RDS
export(data_costo_total_3, "data_costo_total_3_2025_II.rds")
export(data_costo_total_3, "data_costo_total_3_2025_I.xlsx")
export(data_grd_2, "data_grd_2_2025_II.rda")

#############################################################################################################
### Total costo CACI mes
data_costo_total_3 <- import("data_costo_total_3_2025_II.rds")

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
  group_by(identificacion, mes_cargue, año, caci_3) %>% 
  summarise(costo = sum(costo_2), 
            venta = sum(venta), .groups = "drop")

data_caci_total <- data_costo_total_3 %>% 
  filter(año == "2025") %>% 
  group_by(identificacion, mes_cargue) %>% 
  summarise(Total = sum(costo_2), .groups = "drop")

data_caci_total_2 <- data_caci_total %>% 
  group_by(mes_cargue) %>% 
  summarise(Total = median(Total), .groups = "drop")

### Tabla general de costo medio paciente.
data_costo_total_5 <- data_costo_total_4 %>% 
  group_by(mes_cargue, caci_3) %>% 
  summarise(costo_orden = sum(costo),
            pacientes = n_distinct(identificacion),
            promedio = costo_orden/pacientes,
            mediana = median(costo), .groups = "drop")  %>% 
  pivot_wider(id_cols = mes_cargue, names_from  = caci_3, 
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
                      pacientes_TxC = sum(pacientes_TxC),
                      mediana_TxC = median(mediana_TxC), 
                      costo_orden_TxC = sum(costo_orden_TxC),
                      Total = median(Total))) %>%
  export("tabla costo medio paciente 2025.xlsx")

### Tabla de costo, venta y rentabilidad
data_costo_total_6 <- data_costo_total_4 %>% 
  group_by(mes_cargue, caci) %>% 
  summarise(costo_orden = sum(costo),
            venta = sum(venta), .groups = "drop")  %>% 
  mutate(rentabilidad = venta - costo_orden)%>% 
  mutate(rentabilidad = as.numeric(rentabilidad)) %>% 
  mutate(rentabilidad_2 = round(rentabilidad/sum(rentabilidad)*100, digits = 2)) %>% 
  pivot_wider(id_cols = mes_cargue, names_from  = caci, 
              values_from = c(costo_orden, venta, rentabilidad, rentabilidad_2)) %>% 
  select(mes_cargue,
         costo_orden_ACV, venta_ACV, rentabilidad_ACV, rentabilidad_2_ACV, 
         costo_orden_SCA, venta_SCA, rentabilidad_SCA, rentabilidad_2_SCA,
         costo_orden_ICC, venta_ICC, rentabilidad_ICC, rentabilidad_2_ICC,
         costo_orden_TxC, venta_TxC, rentabilidad_TxC, rentabilidad_2_TxC) %>% 
 # adorn_percentages(starts_with("rentabilidad"), denominator = "col",  na.rm = T) %>% 
 # adorn_pct_formatting(,,,c(rentabilidad_ACV, rentabilidad_SCA,
  #                          rentabilidad_ICC, rentabilidad_TxC)) %>% 
  #adorn_ns(position = "front",,,c(rentabilidad_ACV, rentabilidad_SCA,
   #                               rentabilidad_ICC, rentabilidad_TxC)) %>%
  mutate(across(, replace_na, 0)) %>% 
  bind_rows(summarize(., mes_cargue = "Total", 
                      costo_orden_ACV = sum(costo_orden_ACV),
                      venta_ACV = sum(venta_ACV), 
                      rentabilidad_ACV = sum(venta_ACV) - sum(costo_orden_ACV),
                      rentabilidad_2_ACV = sum(rentabilidad_2_ACV), 
                      costo_orden_SCA = sum(costo_orden_SCA),
                      venta_SCA = sum(venta_SCA), 
                      rentabilidad_SCA = sum(venta_SCA) - sum(costo_orden_SCA),
                      rentabilidad_2_SCA = sum(rentabilidad_2_SCA), 
                      costo_orden_ICC = sum(costo_orden_ICC),
                      venta_ICC = sum(venta_ICC),
                      rentabilidad_ICC = sum(venta_ICC) - sum(costo_orden_ICC),
                      rentabilidad_2_ICC = sum(rentabilidad_2_ICC), 
                      costo_orden_TxC = sum(costo_orden_TxC),
                      venta_TxC = sum(venta_TxC),
                      rentabilidad_TxC = sum(venta_TxC) - sum(costo_orden_TxC),
                      rentabilidad_2_TxC = sum(rentabilidad_2_TxC)))



tabla_rentabilidad_2025 <- data_costo_total_6 %>%
  flextable() %>% 
  add_header_row(
    top = TRUE,
    values = c("Mes", "ACV", "", "", "", "SCA", "", "","", "ICC", "", "","", "Trasplante", "", "","")
  ) %>%
  set_header_labels(
    mes_cargue = "",
    costo_orden_ACV = "Costo", venta_ACV = "Venta", rentabilidad_ACV = "Rentabilidad", rentabilidad_2_ACV = "%",
    costo_orden_SCA = "Costo", venta_SCA = "Venta", rentabilidad_SCA = "Rentabilidad", rentabilidad_2_SCA = "%",
    costo_orden_ICC = "Costo", venta_ICC = "Venta", rentabilidad_ICC = "Rentabilidad", rentabilidad_2_ICC = "%",
    costo_orden_TxC = "Costo", venta_TxC = "Venta", rentabilidad_TxC = "Rentabilidad", rentabilidad_2_TxC = "%"
  ) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  merge_at(i = 1, j = 6:9, part = "header") %>%
  merge_at(i = 1, j = 10:13, part = "header") %>%
  merge_at(i = 1, j = 14:17, part = "header") %>%
  theme_booktabs() %>%
  vline(j = c(1, 5, 9, 13), border = border_style, part = "all") %>%
  hline(i = 1, part = "header", border = border_style) %>%
  hline(i = 6, part = "body", border = border_style) %>%
  align(align = "center", j = 2:17, part = "all") %>%
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

###################################### Graphics distribution inpatient days ###############################
data_grd_2 %>% 
  group_by(caci) %>% 
  summarise(mediana_dias = mean(dif_days))

ggplot(data_grd_2, aes(x = log(dif_days))) +
  geom_histogram(aes(y = after_stat(..count..))) +
  geom_density(aes(y = after_stat(count))) +
  scale_x_continuous(n.breaks = 10, labels = label_number(acurracy = 1)) +
  facet_wrap(~caci)


ggplot(data_grd_2, aes(x = dif_days)) +
  geom_histogram(aes(fill = caci), position = "identity") +
  geom

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
