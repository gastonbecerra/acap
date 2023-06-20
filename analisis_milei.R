library(tidyverse)

milei <- read_csv('acap/milei.csv')

glimpse(milei) # veamos la base

# vamos a generar la tabla de asociaciones y el grafico -------------

asociaciones <- milei %>%
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

# vamos a cortar por orientacion politica -------------

table(milei$politica)

asociaciones_der <- milei %>%
  filter(politica == "derecha") %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

freq_cut = mean(asociaciones_der$freq)
freq_orden = mean(asociaciones_der$orden_media)

asociaciones_der %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios

asociaciones_no_der <- milei %>%
  filter(genero != "derecha") %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  ) %>%
  filter(freq > 1)

freq_cut = mean(asociaciones_no_der$freq)
freq_orden = mean(asociaciones_no_der$orden_media)

asociaciones_no_der %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios
