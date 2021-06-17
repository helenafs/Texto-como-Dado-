#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @noRd
app_ui <- function(request) {
  tagList(
    # Deixe essa função para adicionar recursos externos
    golem_add_external_resources(),
    # A partir daqui você pode adicionar sua logica de UI
    
    # Div que vai conter o mapa
    tags$div(
      width="100vw", height="100vh", style = "background-color:lightpink",
      
      leaflet::leafletOutput("map", width="100vw", height="100vh"),
    ),
    
    # Div com card lateral
    absolutePanel(
      id = "plot_div", class = "panel panel-default", fixed = TRUE,
      top = 10, right = 10, width = "400px", height = "auto",
      style = "background-color: rgba(255,255,255,0.9); padding: 10px; border-radius: 10px",
      
      tags$h2("Incêndios florestais na Amazônia, 2011 a 2013"),
      hr(),
      
      # Select de ano
      selectInput(
        inputId = "ano_selecionado",
        label = "Selecione um ano:",
        choices = c(2011, 2012, 2013),
        selected = 2013,
        width = "100%"
      ),
      
      # Plots
      plotOutput(outputId = "plot1", height = "350px"),
      plotOutput(outputId = "plot2", height = "350px"),
    ),
  )
}



#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = "Shiny - Incêndios florestais na Amazônia, 2014 a 2016"
    ),
    tags$style("html, body {margin: 0}"),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

