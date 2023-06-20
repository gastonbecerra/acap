library(tidyverse)

redes <- read_csv('https://github.com/gastonbecerra/acap/blob/main/redes.csv')

glimpse(redes) # veamos la base

# vamos a generar la tabla de asociaciones y el grafico -------------

asociaciones <- redes %>%
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

# vamos a cortar por horas -------------

table(redes$horas)

asociaciones_muchas <- redes %>%
  filter(horas > 4) %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

freq_cut = mean(asociaciones_muchas$freq)
freq_orden = mean(asociaciones_muchas$orden_media)

asociaciones_muchas %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios

asociaciones_pocas <- redes %>%
  filter(horas < 5) %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  ) %>%
  filter(freq > 1)

freq_cut = mean(asociaciones_pocas$freq)
freq_orden = mean(asociaciones_pocas$orden_media)

asociaciones_pocas %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios
