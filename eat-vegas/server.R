####### SERVER ########
#
# Justin Chan, Nick Zhou, Willa Yang, Tyler Marcinyshyn
# INFO 201
# Final Project
# Eat Vegas - Description 
#

library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data <- read.csv("Vegas.csv")
  
  # The actual points to be displayed on the map
  selection <- reactive({
    result <- filter(data, Shape == input$shape) %>%
      filter(substring(Date, 1, 2) == paste0(input$month, "/"))
    return(result)
  })
  
  # The map, to be rendered by leafletOutput()
  output$foodmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=-115.1, lat=36, zoom=4) %>% # sets initial viewpoint of map
      addMarkers(lng = selection()$lng, lat = selection()$lat) %>%
      fitBounds(~min(-115.4), ~min(36), ~max(-115), ~max(36.5)) # latitude and longitude boundaries
  })
  
})
