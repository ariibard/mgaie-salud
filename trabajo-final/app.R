library(tidyverse)
library(lubridate)
library(tidytext)
library(stringi)
library(stopwords)
library(shiny)
library(shinythemes)
library(readxl)
library(geoAr)
library(plotly)
library(scales)
library(RColorBrewer)
library(highcharter)
library(gt)
library(openxlsx)

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
  
  theme = shinytheme("lumen"), 
  
  titlePanel("Cobertura de Salud en la Ciudad de Buenos Aires"),
  HTML("Desarrollado por <b><a href='https://www.linkedin.com/in/ariana-bardauil/' target='_blank'>Ariana Bardauil</a></b>"),
  
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
                 selectizeInput("map_year", "Seleccionar Año", choices = unique(cobertura_salud$ano),multiple = TRUE, selected  = max(cobertura_salud$ano)),
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
                                choices = unique(cobertura_salud$ano), multiple = TRUE, selected = unique(cobertura_salud$ano)),
                 selectizeInput("trend_gender", "Seleccionar Sexo", 
                                choices =  unique(cobertura_salud$sexo), multiple = FALSE, selected = "Total")
                 # Otros filtros relacionados con la tendencia
               ),
               mainPanel(
                 # Gráfico de líneas de tendencia
                 highchartOutput("plot_tendencia")
               )
             )
    ),
    tabPanel("Resumen de Datos",
             sidebarLayout(
               sidebarPanel(
                 # Controles de filtro para el resumen
                 selectInput("summary_year", "Seleccionar Año", choices = unique(cobertura_salud$ano)),
                 selectInput("summary_gender", "Seleccionar Sexo", choices = unique(cobertura_salud$sexo))
                 # Otros filtros relacionados con el resumen
               ),
               mainPanel(
                 # Tabla u otro tipo de resumen de datos
                 gt_output("tabla_resumen"),
                 downloadButton("descargar_tabla", "Descargar Tabla")
               )
             )
    )
)
)



server <- function(input, output) {
  
  # Crear un objeto reactivo para el mapa
  output$plot_mapa <- renderPlotly({
    
    # Filtro
    mapa_filtrado <- mapa_caba %>%
      filter(ano %in%  input$map_year &
               sexo == input$map_gender &
               tipo_cobertura == input$map_cobertura)
    
    if (input$map_gender == "Mujer") {
      color_palette <- "rocket"
    } else if (input$map_gender == "Varón") {
      color_palette <- "mako"
    } else {
      color_palette <- "plasma"
    }
    

    # Grafico
    if (length(input$map_year) > 1) {
      plot_mapa <- ggplot(mapa_filtrado) +
        geom_sf(aes(
          fill = porcentaje,
          text = paste("Comuna:", comuna, "<br>Porcentaje:", porcentaje, "%")
        )) +
        scale_fill_viridis_c(option = color_palette,  # Usar la paleta de colores seleccionada
                             direction = -1,
                             labels = scales::percent_format(scale = 1)) +
        ggthemes::theme_pander() +
        labs(title = "Población total por cobertura de salud: Solo sistema Público",
             fill = "Porcentaje") +
        theme(
          plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5),
          text = element_text(size = 12)  # Ajusta el tamaño de texto según tus preferencias
        ) +
        ylab("") +
        xlab("") +
        facet_wrap(~ano)  # Facetado por año
    } else {
      # Si solo se selecciona un año, no se utiliza facet_wrap
      plot_mapa <- ggplot(mapa_filtrado) +
        geom_sf(aes(
          fill = porcentaje,
          text = paste("Comuna:", comuna, "<br>Porcentaje:", porcentaje, "%")
        )) +
        scale_fill_viridis_c(option = color_palette,  # Usar la paleta de colores seleccionada
                             direction = -1,
                             labels = scales::percent_format(scale = 1)) +
        ggthemes::theme_pander() +
        labs(title = "Población total por cobertura de salud: Solo sistema Público",
             fill = "Porcentaje") +
        theme(
          plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5),
          text = element_text(size = 12)  # Ajusta el tamaño de texto según tus preferencias
        ) +
        ylab("") +
        xlab("")
    }
    
    ggplotly(plot_mapa, tooltip = "text") 
    
    
  })
  
  output$plot_tendencia <- renderHighchart({
    
    grafico_tendencia <- cobertura_salud |> 
      filter(comuna == "Total" & tipo_cobertura != "Ns/Nc") |> 
      filter(ano %in%  input$trend_year) |> 
      filter(sexo %in% input$trend_gender)
    
    # Grafico
    grafico_tendencia <- grafico_tendencia %>%
      hchart(
        type = "line",
        hcaes(x = ano, y = porcentaje, group = tipo_cobertura)
      ) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_title(text = paste("Cobertura de salud de CABA a través del tiempo según sexo:",input$trend_gender)) %>%
      hc_xAxis(title = list(text = "Año")) %>%
      hc_yAxis(title = list(text = "Porcentaje")) %>%
      hc_legend(title = list(text = "Tipo de cobertura"))
    
    # Mostrar el gráfico
    grafico_tendencia
  })
  
  generate_table <- reactive({
    tabla <- cobertura_salud |> 
      filter(ano == input$summary_year & sexo == input$summary_gender) |> 
      mutate(orden = as.numeric(ifelse(comuna == 'Total', '16', comuna)),
             porcentaje = ifelse(is.na(porcentaje), "-", paste0(porcentaje,"%"))) |> 
      pivot_wider(names_from = tipo_cobertura, values_from = porcentaje) |> 
      arrange(orden) |> 
      select(-orden,-sexo,-ano) |> 
      rename("Comuna" = comuna)
    
    return(tabla)
  })
  
  output$tabla_resumen <- render_gt({
    
    
    tabla <- generate_table()
    
    tabla |> 
      gt()  %>%
      tab_header(
        title = paste("Cobertura de Salud en Ciudad de Buenos Aires segun sexo:", input$summary_gender), 
        subtitle = paste("Datos de cobertura de salud por comuna en", input$summary_year)
        # Puedes agregar otras opciones de formato aquí
      ) 
    
  })
 
  
  # Para descargar la tabla
  output$descargar_tabla <- downloadHandler(
    filename = function() {
      paste("tabla_resumen",".csv", sep = "")
    },
    content = function(file) {
      tabla <- generate_table()
      write.csv(tabla, file)
    }
  )
}

shinyApp(ui = ui, server = server)