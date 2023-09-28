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

ui <- fluidPage(
  
  theme = shinytheme("flatly"), 
  
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
                 selectInput("map_gender", "Seleccionar Sexo", choices = unique(cobertura_salud$sexo))
                 # Otros filtros relacionados con el mapa
               ),
               mainPanel(
                 # Mapa de CABA
                 plotOutput("healthMap")
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
    )
  )
)


server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)