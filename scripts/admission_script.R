
data_a_2024 <- import(here("data", "admission_data", "IE_epidemiologia.csv"),
                      fill =T, skip = 2, header = T)

data_a_2024 <- data_a_2024 %>% 
  filter(!`TIPO DOC` == "TIPO DOC")

data_a_2025 <- import(here("data", "admission_data", "IE_epidemiologia-2.csv"),
                      fill =T, skip = 2, header = T)

data_a_2025 <- data_a_2025 %>% 
  filter(!`TIPO DOC` == "TIPO DOC")

data_a_2024_2025 <- rbind(data_a_2024, data_a_2025) %>% 
  clean_names() %>% 
  mutate(tipo_doc = if_else(tipo_doc == "", NA_character_, tipo_doc)) %>% 
  filter(!is.na(tipo_doc) & !is.null(fecha_ingreso)) %>% 
  mutate(across(starts_with("fecha"), ~ dmy_hm(., tz = "America/Bogota")))

export(data_a_2024_2025, here("data", "admission_data", "data_a_2024_2025.rds"))
