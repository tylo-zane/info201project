####### SERVER ########
#
# Justin Chan, Nick Zhou, Willa Yang, Tyler Marcinyshyn
# INFO 201
# Final Project
# Eat Vegas - Description here
#

  library(shiny)
  library(leaflet)

  # Define server logic required to draw a histogram
  shinyServer(function(input, output) {

    data <- read.csv("Vegas.csv")

    # The points to be displayed on the map (TO-DO: Set points to actual restaurants)
    selection <- reactive({
      result <- filter(data, categories == input$Cuisine) %>%
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

    # "Incremental changes to the map should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer." - shiny & leaflet tutorial

  })
