library(tidyverse)
library(lubridate)
library(tidytext)
library(stringi)
library(stopwords)
library(shiny)
library(shinythemes)
library(readxl)

setwd("C:/Users/Usuario/Documents/GitHub/01. Activos/mgaie-salud/trabajo-final")

cobertura_salud <- read_excel("data/cobertura_salud.xlsx", 
                              sheet = "base_util")

# Traigo la cascara del mapa
mapa_caba <- get_geo("CABA", level = "departamento") |>
  mutate(comuna = sub("^0+", "", coddepto_censo))

# Uno
mapa_caba <- mapa_caba |> 
  left_join(cobertura_salud)


ui <- fluidPage(
  
  theme = shinytheme("yeti"), 
  
  titlePanel("Cobertura de Salud en Ciudad de Buenos Aires"),
  
  # Estructura de pestañas
  tabsetPanel(
    # Panel descriptivo
    tabPanel("Descripción",
             fluidRow(
               column(12,
                      tags$div(
                        HTML("<h3>Descripción del Tablero</h3>"),
                        HTML("<p>Este tablero muestra información sobre la cobertura de salud en la Ciudad Autónoma de Buenos Aires (CABA) por comuna, año y sexo. Puedes explorar los siguientes paneles:</p>"),
                        HTML("<ul>
            <li><strong>Mapa:</strong> Visualiza la cobertura de salud en un mapa interactivo.</li>
            <li><strong>Tendencia:</strong> Examina la tendencia de la cobertura de salud a lo largo de varios años y para diferentes géneros.</li>
            <li><strong>Resumen de Datos:</strong> Obtiene un resumen estadístico de los datos disponibles.</li>
             <li><strong>Fuente:</strong> Elaboración propia en base a Datos Abiertos del GCBA.</li>
          </ul>")
                      )
               )
             )
    ),
    tabPanel("Mapa",
             sidebarLayout(
               sidebarPanel(
                 # Controles de filtro para el mapa
                 selectInput("map_year", "Seleccionar Año", choices = unique(cobertura_salud$ano)),
                 selectInput("map_gender", "Seleccionar Sexo", choices = unique(cobertura_salud$sexo)),
                 selectInput("map_cobertura", "Seleccionar tipo de cobertura", choices = unique(cobertura_salud$tipo_cobertura))
                 
               ),
               mainPanel(
                 # Mapa de CABA
                 plotlyOutput("plot_mapa")
               )
             )
    ),
    tabPanel("Tendencia",
             sidebarLayout(
               sidebarPanel(
                 # Controles de filtro para la tendencia
                 selectizeInput("trend_year", "Seleccionar Año", 
                                choices = unique(cobertura_salud$ano), multiple = TRUE),
                 selectizeInput("trend_gender", "Seleccionar Sexo", 
                                choices =  unique(cobertura_salud$sexo), multiple = TRUE)
                 # Otros filtros relacionados con la tendencia
               ),
               mainPanel(
                 # Gráfico de líneas de tendencia
                 plotOutput("trendPlot")
               )
             )
    ),
    tabPanel("Resumen de Datos",
             sidebarLayout(
               sidebarPanel(
                 # Controles de filtro para el resumen
                 selectInput("summary_year", "Seleccionar Año", choices = unique(cobertura_salud$ano))
                 # Otros filtros relacionados con el resumen
               ),
               mainPanel(
                 # Tabla u otro tipo de resumen de datos
                 dataTableOutput("summaryTable")
               )
             )
    ),
    fluidRow(
      column(
        width = 12,
        style = "text-align: right; padding-top: 100px; font-size: 16px;",
        HTML("Desarrollado por <b><a href='https://www.linkedin.com/in/ariana-bardauil/' target='_blank'>Ariana Bardauil</a></b>")
      )
      )
)
)



server <- function(input, output) {
  
  # Crear un objeto reactivo para el mapa
  output$plot_mapa <- renderPlotly({
    
    # Filtro
    mapa_filtrado <- mapa_caba %>%
      filter(ano ==  input$map_year &
               sexo == input$map_gender &
               tipo_cobertura == input$map_cobertura)
    

    # Grafico
    plot_mapa <- ggplot(mapa_filtrado) +
      geom_sf(aes(
        fill = porcentaje,
        text = paste(
          "Comuna:",
          comuna,
          "<br>Porcentaje:",
          porcentaje,
          "%",
          "\nTipo de Cobertura:",
          tipo_cobertura
        )
      )) +
      scale_fill_viridis_c(option = "plasma",
                           direction = -1,
                           labels = percent_format(scale = 1)) +
      ggthemes::theme_pander() +
      labs(
        title = paste(
          "Población",
          input$map_gender,
          "por cobertura de salud:",
          input$map_cobertura,
          "en",
          input$map_year
        ),
        fill = "Porcentaje"
      ) +
      theme(
        plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5),
        text = element_text(size = 12)
      ) +
      ylab("") +
      xlab("")
    
    ggplotly(plot_mapa, tooltip = "text") 
    
    
  })
}

shinyApp(ui = ui, server = server)