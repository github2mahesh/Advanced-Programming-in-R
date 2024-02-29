#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(leaflet)
library(rvest)
library(knitr)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)


#Ref:https://bookdown.org/nicohahn/making_maps_with_r5/docs/leaflet.html

# Define UI for application that draws a map
ui <- fluidPage(
  
  # Application title
  titlePanel("Find your Destiny!"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3(textInput(inputId = "begin",label = "Type here")),
      actionButton(inputId = "Find",label = "Find the location")
    ),
    
    # Show the map and location coordinates
    mainPanel(
      tags$style("#Location {font-size:25px;
               color:darkblue;
                 display:block; }"),     
      div(style="text-align:center;
        box-shadow: 10px 10px 5px #888888;
          width:350px;
          height:350px;
          padding-top:100px;
          position:relative;",
          textOutput(outputId = "Location")),
      div(style="text-align:center;
          width:1000px;
          height:1000px;
          padding-top:100px;
          position:relative;",
          leafletOutput(outputId = "mymap"))
    )
  )
)

# Define server logic required to plot the location in the map
server <- function(input, output) {
  
  
  reactivedata = reactive({
    return(latlong(input$begin))
  })
  
  
  observeEvent(input$Find,{
    
    
    
    x1=reactivedata()
    
    lat <- as.numeric(unname(x1[[1]]))
    lng <- as.numeric(unname(x1[[2]]))
    
    o1<-paste("Latitude:",lat,sep="")
    o2<-paste("Longitude:",lng,sep="")
    
    output$Location <- renderText(paste(c(o1,o2)))
    
    icon.fa <- makeAwesomeIcon(
      icon = "flag", markerColor = "red",
      library = "fa",
      iconColor = "black"
    )
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(
          "OpenStreetMap",
          # give the layer a name
          group = "OpenStreetMap"
        ) %>%
        addAwesomeMarkers(
          lat = lat,
          lng = lng,
          label = "Starting point",
          icon = icon.fa
        )
      
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



