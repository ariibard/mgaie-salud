library(geoAr)
library(scales)

mapa_caba <- get_geo("CABA", level = "departamento") |>
  mutate(comuna = sub("^0+", "", coddepto_censo))

setwd("C:/Users/Usuario/Documents/GitHub/01. Activos/mgaie-salud/trabajo-final")

cobertura_salud <- read_excel("data/cobertura_salud.xlsx", 
                              sheet = "base_util")

mapa_caba <- mapa_caba |> 
  left_join(cobertura_salud)

mapa_filtrado <- mapa_caba |>
  filter(sexo == "Total" & tipo_cobertura == "Sólo sistema público") 

mapa_interactivo <- ggplot(mapa_filtrado) +
  geom_sf(aes(
    fill = porcentaje,
    text = paste("Comuna:", comuna, "<br>Porcentaje:", porcentaje, "%")
  )) +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1,
                       labels = percent_format(scale = 1)) +
  ggthemes::theme_pander() +
  labs(title = "Población total por cobertura de salud: Solo sistema Público",
       fill = "Porcentaje") +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5),
    text = element_text(size = 1)
  ) +
  ylab("") +
  xlab("") 

ggplotly(mapa_interactivo, tooltip = "text")
