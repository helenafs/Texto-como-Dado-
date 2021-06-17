#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

load("data/amazon_fires.rda")

app_server <- function( input, output, session ) {
  
  # Filtrando o dataset a medida que o usuário seleciona um input diferente.
  dataset <- shiny::reactive({
    amazon_fires %>%
      dplyr::filter(year == input$ano_selecionado)
  })
  
  # Fazendo gráfico de mapa
  # O mapa foi feito através da biblioteca leaflet. Mais informações em:
  #         https://rstudio.github.io/leaflet/
  output$map <- leaflet::renderLeaflet({
    # O dataset
    plot_data = dataset()
    
    # O plot
    leaflet::leaflet(
      data = plot_data,
      options = leafletOptions(attributionControl=FALSE)
    ) %>%   # Base do mapa
      leaflet::addTiles() %>%   # Adiciona basemap 
      leaflet::addMarkers(
        ~longitude, ~latitude,popup = ~as.character(burned_area),
        clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula))
      ) %>%   # Adiciona pontos
      leaflet::setView(lng =  -51.92528, lat = -14.235004, zoom = 5)   # Zoom no Brasil
  })
  
  # Fazendo gráfico de linha
  # Esse gráfico foi feito através da biblioteca ggplot2. Mais informações em:
  #     https://ggplot2.tidyverse.org/ e https://livro.curso-r.com/8-1-o-pacote-ggplot2.html
  output$plot1 <- shiny::renderPlot({
    # O dataset
    plot_data <- dataset() %>%
      dplyr::group_by(month) %>% 
      dplyr::summarise(burned_area = sum(burned_area)) # Total de queimadas por mês
    
    # O plot
    ggplot2::ggplot(plot_data, ggplot2::aes(x = month, y = burned_area)) +
      ggplot2::geom_line(size = 1) + ggplot2::theme_bw() +
      ggplot2::labs(x = "Mês", y = "Área de Queimadas (km2)", title = "Total de área de queimadas (km2) por mês.") +
      ggplot2::scale_y_continuous(limits = c(0, 80000)) # Fixando escala pra facilitar comparação entre anos
  })
  
  # Fazendo gráfico de densidade
  # Esse gráfico foi feito através da biblioteca ggplot2. Mais informações em:
  #     https://ggplot2.tidyverse.org/ e https://livro.curso-r.com/8-1-o-pacote-ggplot2.html
  output$plot2 <- shiny::renderPlot({
    # O dataset
    plot_data <- dataset()
    
    # O plot
    ggplot2::ggplot(plot_data, ggplot2::aes(fill = basis_regions, x = Rh)) +
      ggplot2::geom_density(alpha = 0.6) + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.8, 0.2)) +
      ggplot2::labs(x = "Respiração Heterotrófica", y = "Densidade", fill = "Tipo de Floresta:", title = "Respiração heterotrófica por tipo de floresta.")
  })
}


# Só uso essa função pra fazer o gráfico do mapa contar o total de área de queimadas
# Por default, o leaflet usa a contagens de pontos pra fazer o gráfico.
sum.formula  = JS("function (cluster) {    
    var markers = cluster.getAllChildMarkers();
    var sum = 0; 
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.popup);
    }
    var size = sum/10000;
    var formatted_number = Math.round(sum);
    
    var c = ' marker-cluster-';  
    if (formatted_number < 100) {  
      c += 'small';  
    } else if (formatted_number < 1000) {  
      c += 'medium';  
    } else { 
      c += 'large';  
    }    
    return new L.DivIcon({ html: '<div><span>' + formatted_number + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  }")
