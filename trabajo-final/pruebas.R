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


################### TENDENCIA ###################################33

grafico_tendencia <- cobertura_salud |> 
  filter(comuna == "Total" & tipo_cobertura != "Ns/Nc") 

esquisse::esquisser(grafico_tendencia)


library(RColorBrewer)
ggplot(grafico_tendencia) +
  aes(x = ano, y = porcentaje, colour = tipo_cobertura) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "Cobertura de salud de CABA a través del tiempo",
    color = "Tipo de cobertura"
  ) +
  ggthemes::theme_pander() +
  theme(
    plot.title = element_text(size = 17L,
                              face = "bold",
                              hjust = 0.5)
  )

library(highcharter)

grafico_tendencia <- grafico_tendencia %>%
  hchart(
    type = "line",
    hcaes(x = ano, y = porcentaje, group = tipo_cobertura)
  ) %>%
  hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
  hc_title(text = "Cobertura de salud de CABA a través del tiempo") %>%
  hc_xAxis(title = list(text = "Año")) %>%
  hc_yAxis(title = list(text = "Porcentaje")) %>%
  hc_legend(title = list(text = "Tipo de cobertura"))

# Mostrar el gráfico
grafico_tendencia

############################ DT ############################################


library(gt)

tabla <- cobertura_salud |> 
  filter(ano == '2022' & sexo == 'Total') |> 
  mutate(orden = as.numeric(ifelse(comuna == 'Total', '16', comuna)),
         porcentaje = ifelse(is.na(porcentaje), "-", paste0(porcentaje,"%"))) |> 
  pivot_wider(names_from = tipo_cobertura,values_from = porcentaje) |> 
  arrange(orden) |> 
  select(-orden,-sexo,-ano) |> 
  rename("Comuna" = comuna)


tabla |> 
  gt()  %>%
  tab_header(
    title = "Cobertura de Salud en Ciudad de Buenos Aires",
    subtitle = "Datos de cobertura de salud por comuna en 2022",
    # Puedes agregar otras opciones de formato aquí
  ) 
