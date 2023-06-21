library(tidyverse)

ai <- read_csv('https://raw.githubusercontent.com/gastonbecerra/acap/main/ai.csv')

glimpse(ai) # veamos la base

# vamos a generar la tabla de asociaciones y el grafico -------------

asociaciones <- ai %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

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

# vamos a cortar por ocupacion -------------

table(ai$ocupacion)

asociaciones_estudiantes <- ai %>%
  filter(ocupacion == "estudiante") %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

freq_cut = mean(asociaciones_estudiantes$freq)
freq_orden = mean(asociaciones_estudiantes$orden_media)

asociaciones_estudiantes %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios

asociaciones_no_estudiantes <- ai %>%
  filter(ocupacion != "estudiante") %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

freq_cut = mean(asociaciones_no_estudiantes$freq)
freq_orden = mean(asociaciones_no_estudiantes$orden_media)

asociaciones_no_estudiantes %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios
