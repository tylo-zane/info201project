####################################################################################
####### SERVER ########
#
# Justin Chan, Nick Zhou, Willa Yang, Tyler Marcinyshyn
# INFO 201
# Final Project
# Eat Vegas - Description here
#

library(leaflet)
library(shiny)
library(dplyr)
library(ECharts2Shiny)
library(ndjson)
library(wordcloud)
library(tm)
library(RColorBrewer)

data <- read.csv("Food_Vegas.csv") # make sure data file is inside the "eat-vegas" app folder
color_list <- list("1" = "#E80000", "1.5" = "#FF4D00", "2" = "#FF8A00", "2.5" = "#FF9900", "3" = "#FFE500", "3.5" = "#E80000", "4" = "#9EFF00", "4.5" = "#70FF00", "5" = "#12DE00")

shinyServer(function(input, output) {
  # The points to be displayed on the map
  selection <- reactive({
    result <- data
    if (input$cuisine != "All") {
      result <- filter(data, grepl(input$cuisine, data$categories, fixed = TRUE) == TRUE)
    }
    if (!is.null(input$neighborhood)) {
      if (input$neighborhood != "All") {
        result <- filter(result, grepl(input$neighborhood, result$neighborhood, fixed = TRUE) == TRUE)
      }
    }
    return(result)
  })
  
  cloudOne <- reactive({
    if (input$cuisine != "All") {
      result <- filter(data, grepl(input$cuisine, data$categories, fixed = TRUE) == TRUE)
    } else {
      result <- data
    }
    return(result)
  })
  
  cloudTwo <- reactive({
    result <- data
    if (!is.null(input$neighborhood)) {
      if (input$neighborhood != "All") {
        result <- filter(result, grepl(input$neighborhood, result$neighborhood, fixed = TRUE) == TRUE)
      }
    }
    return(result)
  })
  
  # The map, to be rendered by leafletOutput()
  output$foodmap <- renderLeaflet({
    leaflet(data) %>%
      addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
               attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      #addRectangles(
      #  lng1=-115.4, lat1=36.4,
      #  lng2=-115, lat2=35.9,
      #  fillColor = "transparent"
      #) %>%
      setView(lng = -115.2, lat = 36.125, zoom = 10) %>% # sets initial viewpoint of map
      # color = color_list[toString(selection()$stars)]
      addCircles(weight= 5, lng = selection()$longitude, lat = selection()$latitude, popup = paste(paste("<b><big>", selection()$name, "</b></big>", sep=""), 
                                                                                        selection()$address,
                                                                                        paste("<b>Rated ", selection()$stars, " stars</b> based on ",selection()$review_count," reviews",sep=""),
                                                                                        paste("<b>Categories: </b>", selection()$categories, sep=""),
                                                                                        sep="<br>"),
                 label = selection()$name)
  })
  
  output$neighborhoods = renderUI({
    mydata <- as.list(levels(unique(data$neighborhood)))
    mydata <- replace(mydata, 1, "All")
    selectInput("neighborhood", "Select a neighborhood:", mydata)
  })
  
  output$wordcloud <- renderPlot({
    no_empty <- cloudOne()[!(is.na(cloudOne()$neighborhood) | cloudOne()$neighborhood==""), ]
    text <- Corpus(VectorSource(no_empty$neighborhood))
    tdm <- TermDocumentMatrix(text)
    m <- as.matrix(tdm)  
    freq <- sort(rowSums(m), decreasing = TRUE)
    par(mar = rep(0, 4))
    wordcloud(words = names(freq), freq = freq, min.freq = 4, random.order = FALSE,
              col=rainbow(8), scale=c(8, 0.5), max.words=Inf, rot.per=.1)
    
  })
  
  output$wordcloudTwo <- renderPlot({
    no_empty <- cloudTwo()[!(is.na(cloudTwo()$categories) | cloudTwo()$categories==""), ]
    text <- Corpus(VectorSource(no_empty$categories))
    tdm <- TermDocumentMatrix(text)
    m <- as.matrix(tdm)  
    freq <- sort(rowSums(m), decreasing = TRUE)
    par(mar = rep(0, 4))
    wordcloud(words = names(freq), freq = freq, min.freq = 4, random.order = FALSE,
              col=rainbow(8), scale=c(10, .75), max.words=120, rot.per=.1)
    
  })
  
  output$cloudOneText <- renderText({
    paste("Based on your choice of the",input$cuisine,
          "category, we recommend visiting the following",
          "neighborhoods:", sep=" ")
  })
  
  output$cloudTwoText <- renderText({
    paste("Based on your neighborhood choice of ",input$neighborhood,
          ", we recommend the following categories:",
          sep="")
  })
  
  output$summary <- renderText({
    paste("Showing",nrow(selection()),"of",nrow(data),"entries.",sep=" ")
  })
  
})
