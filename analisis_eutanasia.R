library(tidyverse)

euta <- read_csv('https://raw.githubusercontent.com/gastonbecerra/acap/main/eutanasia.csv')

glimpse(euta) # veamos la base

# vamos a generar la tabla de asociaciones y el grafico -------------

asociaciones <- euta %>%
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

# vamos a cortar por religion -------------

table(euta$religion)

# son un millon de opciones distintas... mejor no

# vamos a cortar por edad -------------

table(euta$edad)

asociaciones_mayores <- euta %>%
  filter(edad > 17) %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

freq_cut = mean(asociaciones_mayores$freq)
freq_orden = mean(asociaciones_mayores$orden_media)

asociaciones_mayores %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios

asociaciones_menores <- euta %>%
  filter(edad < 18) %>%
  group_by(palabra) %>%
  summarize(
    freq = n(),
    orden_media = mean(orden)
  )

freq_cut = mean(asociaciones_menores$freq)
freq_orden = mean(asociaciones_menores$orden_media)

asociaciones_menores %>%
  ggplot(aes(x=freq,y=orden_media,label=palabra)) + # frecuencia x orden
  scale_x_continuous(trans='log') + # vamos a aplicar una transformación al eje X para ver mejor los puntos
  geom_hline(yintercept = freq_orden , linetype = 2) + # cortamos por el valor medio
  geom_vline(xintercept = freq_cut, linetype = 2) +  # cortamos por el valor medio
  geom_point(aes(size=freq), show.legend = FALSE) + # agregamos los puntos
  geom_text( show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) + # agregamos las palabras
  labs(y="Orden de evocación", x = "Frecuencia (log)") + # ponemos labels en los ejes
  theme_minimal() # borremos estilos innecesarios
