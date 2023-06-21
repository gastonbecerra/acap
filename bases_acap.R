library(googlesheets4)
library(tidyverse)
library(readr)

gs4_auth()

limpiar <- function(texto) {
  # Convertir a minúsculas
  texto <- tolower(texto)

  # Reemplazar caracteres acentuados
  texto <- str_replace_all(texto, "[áäâà]", "a")
  texto <- str_replace_all(texto, "[éëêè]", "e")
  texto <- str_replace_all(texto, "[íïîì]", "i")
  texto <- str_replace_all(texto, "[óöôò]", "o")
  texto <- str_replace_all(texto, "[úüûù]", "u")
  texto <- str_replace_all(texto, "[ñ]", "n")

  # Eliminar espacios en blanco adicionales
  texto <- str_replace_all(texto, "\\s+", "")

  # Convertir plural a singular (solo una regla simple para demostración)
  texto <- str_replace_all(texto, "s$", "")

  # Devolver el texto limpio y normalizado
  return(texto)
}


# ai ---------------------

bd_ai <- read_sheet("https://docs.google.com/spreadsheets/d/1tKuUchFXnmcoQek2ZTMgjR9tgAxxsUqOD2Dp858UoGE/")
glimpse(bd_ai)

bd_ai %>%
  mutate(id = row_number() ) %>%
  rename(time = `Marca temporal`) %>%
  pivot_longer(cols = 2:6,
               names_to = "orden",
               values_to = "palabra") %>%
  mutate(orden = readr::parse_number(orden)) %>%
  mutate(palabra = limpiar(palabra)) %>%
  rename(ocupacion = "Ocupación") %>%
  mutate(ocupacion = limpiar(ocupacion)) %>%
  rename(edad = "Edad") %>%
  select(id, time, orden, palabra, ocupacion, edad) %>%
  write.csv('acap/ai.csv')

# milei ---------------------

bd_milei <- read_sheet("https://docs.google.com/spreadsheets/d/1J4e8T-OQDxGKaMI5qLN2I-6lSwqRd3o9jnsWkJQAOdk")
glimpse(bd_milei)

bd_milei$edad = bd_milei$Edad %>% unlist() %>% readr::parse_number()
glimpse(bd_milei)

bd_milei %>%
  mutate(id = row_number() ) %>%
  rename(time = `Marca temporal`) %>%
  pivot_longer(cols = 2:6,
               names_to = "orden",
               values_to = "palabra") %>%
  mutate(orden = readr::parse_number(orden)) %>%
  mutate(palabra = limpiar(palabra)) %>%
  rename(genero = "Género") %>%
  mutate(genero = limpiar(genero)) %>%
  rename(ocupacion = "Ocupación") %>%
  mutate(ocupacion = limpiar(ocupacion)) %>%
  rename(profesion = "Profesión") %>%
  mutate(profesion = limpiar(profesion)) %>%
  rename(politica = "Posición política") %>%
  mutate(politica = limpiar(politica)) %>%
  select(id, time, orden, palabra, ocupacion, edad, profesion, ocupacion, genero, politica) %>%
  write.csv('acap/milei.csv')

# redes --------------------

bd_redes <- read_sheet("https://docs.google.com/spreadsheets/d/1nWqreYE1pxQHCi-AcRBoqRB1Mj6IkTEF7-iPphShq-A/edit#gid=62074195")
glimpse(bd_redes)

bd_redes %>%
  mutate(id = row_number() ) %>%
  rename(time = `Marca temporal`) %>%
  pivot_longer(cols = 8:12,
               names_to = "orden",
               values_to = "palabra") %>%
  mutate(palabra = limpiar(palabra)) %>%
  mutate(orden = readr::parse_number(orden)) %>%
  rename(genero = `¿Con que genero te identificas?`) %>%
  mutate(genero = limpiar(genero)) %>%
  rename(edad = `¿Qué edad tienes?`) %>%
  rename(usa = `¿Usas Redes Sociales?`) %>%
  rename(horas = `¿Cuántas horas diarias?`) %>%
  rename(redes = `¿Qué Redes Sociales utilizas?`) %>%
  select(id, time, orden, palabra, genero, edad, horas, usa, redes) %>%
  write.csv('acap/redes.csv')

# cfk --------------------

bd_cfk <- read_sheet("https://docs.google.com/spreadsheets/d/15AXEONdQWYAdVg1l0tZ5cVDGS23-HFUfI94MlVqEYFE/")
glimpse(bd_cfk)

bd_cfk$p1 = bd_cfk$`primera palabra que se te viene a a cabeza cuando te mencionan CFK`
bd_cfk$p2 = bd_cfk$`segunda palabra que se te viene a a cabeza cuando te mencionan CFK`
bd_cfk$p3 = unlist(bd_cfk$`tercera palabra que se te viene a a cabeza cuando te mencionan CFK`)
bd_cfk$p4 = bd_cfk$`cuarta palabra que se te viene a a cabeza cuando te mencionan CFK`
bd_cfk$p5 = bd_cfk$`quinta palabra que se te viene a a cabeza cuando te mencionan CFK`
bd_cfk$`primera palabra que se te viene a a cabeza cuando te mencionan CFK` <- NULL
bd_cfk$`segunda palabra que se te viene a a cabeza cuando te mencionan CFK` <- NULL
bd_cfk$`tercera palabra que se te viene a a cabeza cuando te mencionan CFK` <- NULL
bd_cfk$`cuarta palabra que se te viene a a cabeza cuando te mencionan CFK` <- NULL
bd_cfk$`quinta palabra que se te viene a a cabeza cuando te mencionan CFK` <- NULL

glimpse(bd_cfk)
bd_cfk %>%
  mutate(id = row_number() ) %>%
  rename(time = `Marca temporal`) %>%
  pivot_longer(cols = 7:11,
               names_to = "orden",
               values_to = "palabra") %>%
  mutate(palabra = limpiar(palabra)) %>%
  mutate(orden = readr::parse_number(orden)) %>%
  rename(genero = Género) %>%
  rename(formacion = `formación académica`) %>%
  rename(partido = `partido político simpatizante o a quién pensas votar`) %>%
  rename(interes = `te interesa la política`) %>%
  select(id, time, orden, palabra, genero, edad, formacion, partido, interes) %>%
  write.csv('acap/cfk.csv')


# argentina --------------------

bd_argentina <- read_sheet("https://docs.google.com/spreadsheets/d/1xSPuRNF4PnL6wsPM6dXP12u847KJujWXnDuQQLbEbe4")
glimpse(bd_argentina)

bd_argentina$p1 = bd_argentina$`Primera palabra:`
bd_argentina$p2 = bd_argentina$`Segunda palabra:`
bd_argentina$p3 = bd_argentina$`Tercera palabra:`
bd_argentina$p4 = bd_argentina$`Cuarta palabra:`
bd_argentina$p5 = bd_argentina$`Quinta palabra:`
bd_argentina$`Primera palabra:` <- NULL
bd_argentina$`Segunda palabra:` <- NULL
bd_argentina$`Tercera palabra:` <- NULL
bd_argentina$`Cuarta palabra:` <- NULL
bd_argentina$`Quinta palabra:` <- NULL
glimpse(bd_argentina)

bd_argentina %>%
  mutate(id = row_number() ) %>%
  rename(time = `Marca temporal`) %>%
  pivot_longer(cols = 5:9,
               names_to = "orden",
               values_to = "palabra") %>%
  mutate(palabra = limpiar(palabra)) %>%
  mutate(orden = readr::parse_number(orden)) %>%
  rename(sexo = Sexo) %>%
  rename(edad = `¿Cuántos años tenés?`) %>%
  rename(nacionalidad = `¿Cuál es tu nacionalidad?`) %>%
  select(id, time, orden, palabra, sexo, edad, nacionalidad) %>%
  write.csv('acap/argentina.csv')


# eutanasia --------------------

bd_eutanasia <- read_sheet("https://docs.google.com/spreadsheets/d/1Tm5w7qVoBvN1U8PdcbFeAvEnSOayKYGT2XOdAB1YguA")
glimpse(bd_eutanasia)

bd_eutanasia %>%
  mutate(id = row_number() ) %>%
  rename(time = Timestamp) %>%
  pivot_longer(cols = 2:6,
               names_to = "orden",
               values_to = "palabra") %>%
  mutate(palabra = limpiar(palabra)) %>%
  mutate(orden = readr::parse_number(orden)) %>%
  mutate(religion = limpiar(Religion)) %>%
  rename(edad = Edad) %>%
  select(id, time, orden, palabra, edad, religion) %>%
  write.csv('acap/eutanasia.csv')
