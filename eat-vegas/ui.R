######## UI #########
#
# Justin Chan, Nick Zhou, Willa Yang, Tyler Marcinyshyn
# INFO 201
# Final Project
# Eat Vegas - Description 
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Eat Vegas"),
  
  # Sidebar with dropdown categories
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput("cuisine",
                  "Select a category of cuisine:",
                  list("Shape", "State", "AM.PM")),
      selectInput("neighborhood",
                  "Select a neighborhood:",
                  list("Standard", "Light")),
      textOutput("description")
    ),
    # Show a plot of the generated plot
    mainPanel(
      leafletOutput("foodmap")
    )
  )
))
