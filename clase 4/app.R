library(tidyverse)
library(shiny)
library(DT)

datos =iris
datos$Species = toupper(datos$Species)

ui <- fluidPage( #fluid page filas 
  # titulo
  fluidRow(
    column(width =  12,# ocupa 12 columnas que es todo el ancho de la página
           h1('Datos por especie'),
           br(),
           align = 'center', #centra todos los elementos de columna
    )
  ),
  
  hr(), # agregar una linea
  
  # primera fila de la ui
  fluidRow(
    column(width = 3,
           h2("Filtros"),
           selectInput(
             inputId = "selectSpecie",
             label = "seleccionar especie:",
             choices = unique(datos$Species),
             selected = "SETOSA"
             #choices = c("Setosa" = "setosa","Versicolor" = "versicolor") te permite buscar un valor pero mostrar el otro
           )
    ),
    column(width = 9,
           h2("Tabla"),
           DT::DTOutput("tabla"),
           align = "center"
    )
  )
)

server <- function(input, output, session) {
 #se pone output$nombre quele vas a poner a lo que querés mostrar
   output$tabla = DT::renderDataTable({
    especieSeleccionada = input$selectSpecie
    
    datos = datos |> 
      filter(Species == especieSeleccionada)
    
    DT::datatable(datos)
   }
  )
}

shinyApp(ui, server)