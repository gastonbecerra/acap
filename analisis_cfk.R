library(tidyverse)

cfk <- read_csv('acap/cfk.csv')

glimpse(cfk) # veamos la base

# vamos a generar la tabla de asociaciones y el grafico -------------

asociaciones <- cfk %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  ) %>%
  filter(freq > 1)

freq_cut = mean(asociaciones$freq)
freq_orden = mean(asociaciones$orden_media)

asociaciones %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios

# vamos a cortar por genero -------------

table(cfk$genero)

asociaciones_fem <- cfk %>%
  filter(genero == "Femenino") %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  ) %>%
  filter(freq > 1)

freq_cut = mean(asociaciones_fem$freq)
freq_orden = mean(asociaciones_fem$orden_media)

asociaciones_fem %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios

asociaciones_masc <- cfk %>%
  filter(genero == "Masculino") %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  ) %>%
  filter(freq > 1)

freq_cut = mean(asociaciones_masc$freq)
freq_orden = mean(asociaciones_masc$orden_media)

asociaciones_masc %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios
