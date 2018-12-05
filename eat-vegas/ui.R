######## UI ########
#
# Justin Chan, Nick Zhou, Willa Yang, Tyler Marcinyshyn
# INFO 201
# Final Project
# Eat Vegas - Description here
#

library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("darkly"),
                  
                  # Sidebar with dropdown categories
                  sidebarLayout(
                    position = "left",
                    sidebarPanel(
                      img(src='eatLasVegas.png', align = "center", width = "125%"), # LOGO of the app, should be included in "eat-vegas" folder
                      selectInput("cuisine",
                                  "Select a category of cuisine:",
                                  list("All","American", "Italian", "French", "Japanese", "Chinese", "Indian", "Mexican", "Vegetarian",
                                       "Hawaiian", "Food Truck", "Steakhouses", "Desserts", "Seafood", "Greek", "Bars", "Breakfast & Brunch")),
                      uiOutput('neighborhoods'),
                      textOutput("summary"),
                      style = "background-color: #6b6b6b"
                    ),
                    # Show a plot of the generated plot
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", leafletOutput("foodmap")),
                                  tabPanel("Recommendations",
                                           textOutput("cloudOneText"),
                                           plotOutput("wordcloud"),
                                           textOutput("cloudTwoText"),
                                           plotOutput("wordcloudTwo")),
                                  tabPanel("About", includeMarkdown("www/about.md"))
                      )
                    )
                  )
))
