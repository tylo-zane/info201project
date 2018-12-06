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
color_list <- list("1" = "#E80000", "1.5" = "#FF4D00", "2" = "#FF8A00", "2.5" = "#FF9900", "3" = "#FFE500", "3.5" = "#FFF500", "4" = "#9EFF00", "4.5" = "#70FF00", "5" = "#12DE00")

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
  
  # Returns subset of data to be used for the neighborhood wordcloud
  cloudOne <- reactive({
    if (input$cuisine != "All") {
      result <- filter(data, grepl(input$cuisine, data$categories, fixed = TRUE) == TRUE)
    } else {
      result <- data
    }
    return(result)
  })
  
  # Returns subset of data to be used for the cuisine wordcloud
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
      addProviderTiles(providers$Hydda.RoadsAndLabels) %>%
      setView(lng = -115.2, lat = 36.125, zoom = 10) %>% # sets initial viewpoint of map
      addCircles(weight= 5, lng = selection()$longitude, lat = selection()$latitude, color = selection()$color, popup = popupText(),
                 label = selection()$name) %>%
      addLegend("bottomright", 
                colors = c("#12DE00", "#70FF00", "#9EFF00", "yellow", "#FFE500", "#FF9900", "#FF8A00", "#FF4D00", "#E80000"),
                labels= c("5.0", "4.5", "4.0", "3.5", "3.0", "2.5", "2.0", "1.5", "1.0"),
                title= "Ratings",
                opacity = 1)
  })
  
  # The part of the UI where the user selects a neighborhood
  # List of neighborhoods is retrieved from the data
  output$neighborhoods = renderUI({
    mydata <- as.list(levels(unique(data$neighborhood)))
    mydata <- replace(mydata, 1, "All")
    selectInput("neighborhood", "Select a neighborhood:", mydata)
  })
  
  # Renders first wordcloud
  # Shows neighborhoods based on the chosen category
  output$wordcloud <- renderPlot({
    no_empty <- cloudOne()[!(is.na(cloudOne()$neighborhood) | cloudOne()$neighborhood==""), ]
    text <- Corpus(VectorSource(no_empty$neighborhood))
    tdm <- TermDocumentMatrix(text)
    m <- as.matrix(tdm)  
    freq <- sort(rowSums(m), decreasing = TRUE)
    par(mar = rep(0, 4), bg = "#232323")
    wordcloud(words = names(freq), freq = freq, min.freq = 4, random.order = FALSE,
              col=rainbow(8), scale=c(5, 0.5), max.words=Inf, rot.per=.1)
    
  })
  
  # Renders second wordcloud
  # Shows categories based on the chosen neighborhood
  output$wordcloudTwo <- renderPlot({
    no_empty <- cloudTwo()[!(is.na(cloudTwo()$categories) | cloudTwo()$categories==""), ]
    text <- Corpus(VectorSource(no_empty$categories))
    tdm <- TermDocumentMatrix(text)
    m <- as.matrix(tdm)  
    freq <- sort(rowSums(m), decreasing = TRUE)
    par(mar = rep(0, 4), bg = "#232323")
    wordcloud(words = names(freq), freq = freq, min.freq = 4, random.order = FALSE,
              col=rainbow(8), scale=c(10, .75), max.words=120, rot.per=.1)
    
  })
  
  # Renders text displayed before the first wordcloud
  output$cloudOneText <- renderText({
    paste("Based on your choice of the",input$cuisine,
          "category, we recommend visiting the following",
          "neighborhoods:", sep=" ")
  })
  
  # Renders popup text. Displayed when a map marker is clicked on.
  popupText <- reactive({
    return(paste(
      paste("<b><big>", selection()$name, "</b></big>", sep=""), 
      selection()$address,
      selection()$neighborhood,
      paste("<b>Rated ", selection()$stars, " stars</b> based on ",selection()$review_count," reviews",sep=""),
      paste("<b>Categories: </b>", selection()$categories, sep=""),
      paste("<b>Noise Level: </b>", selection()$attributes.NoiseLevel,sep=""),
      paste("<b>Offers take-out: </b>", selection()$attributes.RestaurantsTakeOut,sep=""),
      paste("<b>Offers delivery: </b>",selection()$attributes.RestaurantsDelivery,sep=""),
      paste("<b>Has outdoor seating: </b>",selection()$attributes.OutdoorSeating,sep=""),
      paste("<b>Wi-fi available: </b>",selection()$attributes.WiFi,sep=""),
      sep="<br>"))
  })
  
  # Renders text displayed before the second wordcloud
  output$cloudTwoText <- renderText({
    paste("Based on your neighborhood choice of ",input$neighborhood,
          ", we recommend the following categories:",
          sep="")
  })
  
  # Prints how many entries are displayed out of total number of entries
  output$summary <- renderText({
    paste("Showing",nrow(selection()),"of",nrow(data),"entries.",sep=" ")
  })
  
})
