#
#R shiny for Operation Management 
#Project 2
#Author: Yvonne Li 
#Date： 10/18.2019 



library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(plyr)
library(tools)
library(shinythemes)
library(shinydashboard)
library(reshape2)
library(plotly)
require(leaflet)

#Loading data and cleaning------------------------------
arrest <- read.csv("arrest.csv",fileEncoding="UTF-8-BOM",header=TRUE, check.names = FALSE)
#format time index
arrest$ARRESTTIME <- as.Date(arrest$ARRESTTIME,format='%m/%d/%Y')

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "PGH Arrest")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("DataTable", icon = icon("table"), tabName = "table"),
    
    # Select variable for color -----------------------------------
    radioButtons(inputId = "z",
                 label = "Color By: ",
                 choices = c("GENDER","RACE"),
                 selected = "GENDER"),
    
    # Select Agegroup ---------------------------------------
    checkboxGroupInput(inputId = "selected_agegroup",
                       label = "Select agegroup(s):",
                       choices = c("Adolescent","Teenager"),
                       selected = c("Adolescent","Teenager")),
    
    #Slider Input for year ------------------------------------------- 
    sliderInput("yearSelect",
                "Release Year:",
                min = min(arrest$ARRESTTIME, na.rm = T),
                max = max(arrest$ARRESTTIME, na.rm = T),
                value = c(min(arrest$ARRESTTIME, na.rm = T), max(arrest$ARRESTTIME, na.rm = T)),
                step = 1),
    
    # Select sample size -----------------------------------------------
    numericInput(inputId = "n_samp", 
                 label = "Sample size:", 
                 min = 1, max = nrow(arrest), 
                 value = 500),
    
    # Download Button -----------------------------------------------
    downloadButton('downloadData', 'Download data')
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   # tabbox id
                   id = "tab_being_displayed",
                   #tab to display histogran
                   tabPanel("Arrest Records Frequency by Time",
                            plotOutput(outputId = "hist")),
                   #tab to display boxplot
                   tabPanel("Demographic of Arrest Records",
                            plotOutput(outputId = "boxplot")),
                   #tab to display map 
                   tabPanel("Arrest Records Map",
                            leafletOutput(outputId = "arrestmap"))
                  )
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "arrest Game Store Data", DT::dataTableOutput(outputId = "arresttable")))
  )
)
)

# ui setting  ----------------------------------------------
ui <- dashboardPage(header, sidebar, body,skin = "yellow")

# Define server function required to create the hist ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected characters
  arrest_subset <- reactive({
    # ensure availablity of value before proceeding
    req(input$selected_agegroup) 
    # Slider Filter ----------------------------------------------
    filter(arrest, ARRESTTIME >= input$yearSelect[1] & ARRESTTIME <= input$yearSelect[2] &
             AGE20 %in% input$selected_agegroup)
  })
  
  # Create new df that is n_samp obs from selected characters
  arrest_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(arrest_subset(), input$n_samp)
  })
  
  
  # histogram displaying arrest frquency---------------------------
  output$hist <- renderPlot({
    dat <- arrest_sample()
    ggplot(data = dat, aes_string(x = dat$ARRESTTIME, fill = input$z)) +
      geom_histogram(binwidth = 20)+ scale_color_brewer()
  })
  
  #box chart display the demographic of arrest records----------------------------
  output$boxplot <- renderPlot({
    dat <- arrest_sample()
    ggplot(data = dat, aes_string(x = input$z, y=dat$AGE, color=input$z)) + 
      geom_boxplot()+labs(title = paste("Boxplot: ","Average Playtime"," By ",input$z) )
  }
  )
  
  #Map plot -------------------------------------
  output$arrestmap <- renderLeaflet({
    leaflet() %>%
      # Basemaps
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      # zoom to the center of pittsburgh city 
      setView(-79.9959, 40.4406, zoom = 10) 
    
  })
  
  #Setting leafletproxy to make the map reactive -------------------------------------
  observe({ 
    req(input$tab_being_displayed == "Arrest Records Map")
    dat <- arrest_sample()
    leafletProxy("arrestmap", data = dat) %>%
      clearHeatmap() %>%
      clearMarkerClusters()%>%
      clearMarkers()%>%
      addHeatmap(data = dat, lng = ~X, lat = ~Y, radius = 8, group = "heatmap")%>%
      addCircleMarkers(data = dat, lng = ~X, lat = ~Y, radius = 1.5, 
                       clusterOptions = markerClusterOptions(),group = "marker") %>%
      # Layers control
      addLayersControl(overlayGroups = c("heatmap","marker"))
      
  })
  

  
  # datatable setting -------------------------------------
  output$arresttable <- DT::renderDataTable({
    DT::datatable(data = arrest_sample(), 
                  options = list(pageLength = 17), 
                  rownames = FALSE)
  }
  
  )
  
  
  # download data when button is clicked
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("arrestdatasample",gsub(":","-",Sys.time()), ".csv", sep="")
    },
    content = function(file) {
      write.csv(arrest_sample(), file, row.names = FALSE)
    })
  
  
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

