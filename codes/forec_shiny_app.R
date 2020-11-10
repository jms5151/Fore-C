# Fore-C interactive explorer ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# https://github.com/eparker12/nCoV_tracker/blob/master/app.R

# one piece of an answer to this StackOverflow question
#  http://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny

# for this we'll use Kyle Walker's rpubs example
#   http://rpubs.com/walkerke/leaflet_choropleth
# combined with data from Diego Valle's crime in Mexico project
#   https://github.com/diegovalle/mxmortalitydb

# we'll also build on the shiny example included in dygraphs
#  https://github.com/rstudio/leaflet/blob/master/inst/examples/shiny.R

# load libraries
library(shiny)
library(shinythemes)
library(ggridges)
library(ggplot2)
library(leaflet)
library(raster)
library(flexdashboard)
library(tidyverse)
library(shinyWidgets)

# install.packages("leafpop")
# library(leafpop)

# load data
load("Compiled_data/observational_data.RData")
load("Compiled_data/historical_surveys.RData")
load("Compiled_data/number_surveys.RData")
source("codes/addScaleBar.R")
load("Compiled_data/grid.RData")

# library(rgdal)
# library(raster)
# library(sf)
# 
# p <- as(ga_nowcast, "SpatialPolygonsDataFrame")
# nByTown_latlon <- spTransform(p, CRS("+proj=longlat +datum=WGS84"))
# 
# bins <- seq(0, 0.5, 0.1)
# pal <- colorBin("YlOrRd", domain = nByTown_latlon$X2020.06.05_ga, bins = bins)
# 
# testplot <- list(plot(seq(1,10,1), seq(1,10,1), xlab = "x", ylab = "y", pch = 16))
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = nByTown_latlon,
#               fillColor = ~pal(nByTown_latlon$X2020.06.05_ga),
#               weight = 2,
#               opacity = 1,
#               color = ~pal(nByTown_latlon$X2020.06.05_ga),
#               fillOpacity = 0.7) %>%
#   popupGraph(testplot,
#              width = 300,
#              height = 300)

# load nowcasts
lastUpdate <- substr(max(list.files("Compiled_data/nowcasts/raster/")), 1, 10)
total_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_total.tif"))
bbd_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_bbd.tif"))
ga_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_ga.tif"))
ws_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_ws.tif"))

# other data
vs <- read.csv("Data/virtual_stations.csv", stringsAsFactors = F)

## raster data is slow to load and not needed
## issue making map 100% of page, copy LSTHM
# maybe make new polygon of reef grid and then make that interactive
# https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html

# create maps
basemap <- leaflet() %>%
  # addTiles() %>%
  addTiles(group = "OpenStreetMap") %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite") %>%
  addScaleBar() %>%
  # addPolygons(data = nByTown_latlon,
  #             fillColor = ~pal(nByTown_latlon$X2020.06.05_ga),
  #             weight = 2,
  #             opacity = 1,
  #             color = ~pal(nByTown_latlon$X2020.06.05_ga),
  #             fillOpacity = 0.7, 
  #             group = "Nowcast") %>%
  # addPolygons(data = nByTown_latlon,
  #             fillColor = ~pal(nByTown_latlon$X2020.06.05_ga),
  #             weight = 2,
  #             opacity = 1,
  #             color = ~pal(nByTown_latlon$X2020.06.05_ga),
  #             fillOpacity = 0.7, 
  #             group = "4-week forecast") %>%
  addRasterImage(total_nowcast, colors = "Spectral", opacity = 0.6, group = "Nowcast") %>%
  addRasterImage(bbd_nowcast, colors = "Spectral", opacity = 0.6, group = "4-week forecast") %>%
  addRasterImage(ga_nowcast, colors = "Spectral", opacity = 0.6, group = "8-week forecast") %>%
  addRasterImage(ws_nowcast, colors = "Spectral", opacity = 0.6, group = "12-week forecast") %>%
  setView(lng = 180, lat = 0 , zoom = 3)  %>%
  addLayersControl(
    overlayGroups = c("Nowcast", "4-week forecast", "8-week forecast", "12-week forecast"),
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE), # icon versus buttons with text
    position = c("bottomright")
    ) %>%
  hideGroup(c("4-week forecast", "8-week forecast", "12-week forecast"))

  
historicalMap = leaflet() %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircleMarkers(data=historical_data, lat = ~Latitude, lng = ~Longitude, radius = ~sqrt(N)
                   , color = ~'white', popup = ~survey_text
                   , clusterOptions = markerClusterOptions()) %>%
  addScaleBar() %>%
  setView(lng = -180, lat = 16.4502 , zoom = 3)

## create some simulated data
simdata_dates <- seq.Date(as.Date("2020-08-01", "%Y-%m-%d"), as.Date("2020-12-31", "%Y-%m-%d"), by = "days") 
vss <- unique(vs$Virtual_Stations)
diseasetypes <- c("BBD", "GA", "WS")
simdata <- data.frame(expand.grid(simdata_dates, vss, diseasetypes))
colnames(simdata) <- c("Date", "Virtual_Station", "Disease")
simdata$Disease <- rnorm(n = nrow(simdata), mean = 10, sd = 3)

# to use the css I need to download docs: https://shiny.rstudio.com/articles/css.html

# function to plot disease by virtual station
vs_plot_fun = function(df){
  g = ggplot(simdata, aes(Date, Disease, col = Virtual_Station)) +
    geom_line() + 
    theme_classic()
  }


# run shiny app
shinyApp(
  ui =  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,"", id="nav",
            
                   # Nowcasts and forecasts page
                   tabPanel("Coral disease predictions",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("disease_risk_map", width = "100%", height = "100%")),
                      ## transparent and dragable layer with dropdown menu
                      # absolutePanel(id = "controls", class = "panel panel-default",
                                    # top = 75, left = 55, width = 250, fixed = TRUE,
                                    # draggable = TRUE, height = "auto",
                                    # selectInput("select_forecast", label = h3("Select forecast"),
                                    #             choices = c("Nowcast",
                                    #                         "4-week forecast",
                                    #                         "8-week forecast",
                                    #                         "12-week forecast"),
                                    #             selected = "Nowcast"),
                                    # hr(),
                                    # textOutput("update")
                                    # ),
                      ),
                  # Scenarios page
                  tabPanel("Scenarios",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Black band disease",
                                                sidebarPanel(
                                                  
                                                  pickerInput("region_select", "Region:",   
                                                              choices = unique(vs[order(vs$Region),]$Region), 
                                                              selected = c("Hawaii"),
                                                              options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                              multiple = FALSE),
                                                  
                                                  pickerInput("vs_select", "Virtual station(s):",
                                                              choices = vs$Virtual_Stations[vs$Region == "Hawaii"],
                                                              options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                              selected = vs$Virtual_Stations[vs$Region == "Hawaii"],
                                                              multiple = TRUE),
                                                  
                                                  sliderInput("temp_slider", label = h3("Temperature"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             sliderInput("light_slider", label = h3("Irradiance"), min = -100, max = 100, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             actionButton("reset_input", label = "Reset inputs") # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
                                                ),
                                                mainPanel(
                                                  tabPanel("Disease risk", plotOutput("vs_risk_plot"))
                                                )
                                       ),
                                       tabPanel("Growth anomalies",
                                                sidebarPanel(
                                                  
                                                  pickerInput("region_select", "Region:",   
                                                              choices = c("Australia", "American Samoa", "Hawaii", "Marianas", "Pacific Remote Island Areas"), 
                                                              selected = c("Hawaii"),
                                                              multiple = FALSE),
                                                  
                                                  sliderInput("temp_slider", label = h3("Temperature"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             sliderInput("chl_slider", label = h3("Chlorophyll-a"), min = -100, max = 100, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             sliderInput("wave_slider", label = h3("Wave energy"), min = -100, max = 100, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             actionButton("reset_input", label = "Reset inputs") # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
                                                ),
                                                mainPanel(leafletOutput("mymap4"))

                                       ),
                                       tabPanel("White syndromes",
                                                sidebarPanel(
                                                  
                                                  pickerInput("region_select", "Region:",   
                                                              choices = c("Australia", "American Samoa", "Hawaii", "Marianas", "Pacific Remote Island Areas"), 
                                                              selected = c("Hawaii"),
                                                              multiple = FALSE),
                                                  
                                                  sliderInput("chl_slider", label = h3("Chlorophyll-a"), min = -100, max = 100, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             sliderInput("coral_slider", label = h3("Mean coral size"), min = 0, max = 100, step=10, width='200px', post  = " cm", value = 20),
                                                             hr(),
                                                             actionButton("reset_input", label = "Reset inputs") # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
                                                ),
                                                mainPanel(leafletOutput("mymap5"))
                                       ))
                           ),
                  # Historical data page
                  tabPanel("Historical data", 
                           div(class="outer",
                               tags$head(includeCSS("styles.css")),
                               leafletOutput("historical_data_map", width="100%", height="100%"))),
                  
                  # About the project page
                  tabPanel("About")
  ),
  
  server = function(input, output, session) { 
    
    output$disease_risk_map <- renderLeaflet({
      # if(input$nowcast_radio == 1){
      #       x <- total_nowcast
      #     }
      #     if(input$nowcast_radio == 2){
      #       x <- bbd_nowcast
      #     }
      #     if(input$nowcast_radio == 3){
      #       x <- ga_nowcast
      #     }
      #     if(input$nowcast_radio == 4){
      #       x <- ws_nowcast
      #     }
      basemap #%>%
      #     addRasterImage(disease_risk_map, colors = "Spectral", opacity = 0.6) %>%
      #     setView(lng = 180, lat = 16.4502 , zoom = 2)
    })
    
    # scenarios outputs
    # update region selections
    observeEvent(input$region_select, {
      if (input$region_select == "Hawaii") {
        updatePickerInput(session = session, inputId = "vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Hawaii"], 
                          selected = vs$Virtual_Stations[vs$Region == "Hawaii"])
      }
      
      if (input$region_select == "Pacific Remote Island Areas") {
        updatePickerInput(session = session, inputId = "vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Pacific Remote Island Areas"], 
                          selected = vs$Virtual_Stations[vs$Region == "Pacific Remote Island Areas"])
      }
      
      if (input$region_select == "Australia") {
        updatePickerInput(session = session, inputId = "vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Australia"], 
                          selected = vs$Virtual_Stations[vs$Region == "Australia"])
      }
      
      if (input$region_select == "Marianas") {
        updatePickerInput(session = session, inputId = "vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Marianas"], 
                          selected = vs$Virtual_Stations[vs$Region == "Marianas"])
      }
      
      if (input$region_select == "American Samoa") {
        updatePickerInput(session = session, inputId = "vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "American Samoa"], 
                          selected = vs$Virtual_Stations[vs$Region == "American Samoa"])
      }
      
    }, ignoreInit = TRUE)
    
    # # create dataframe with selected virtual stations
    # vs_reactive_db = reactive({
    #   simdata %>% filter(Virtual_Station %in% input$vs_select)
    # })
    # 
    # # VS-specific plots
    # output$vs_risk_plot <- renderPlot({
    #   # vs_plot_fun(vs_reactive_db())
    #   ggplot(vs_reactive_db(), aes(Date, Disease, col = Virtual_Station)) +
    #     geom_line() + 
    #     theme_classic()
    # })
    
    # create dataframe with selected countries
    # vs_reactive_db = reactive({
    #   if (input$region_select == "Hawaii") { 
    #     db = subset(simdata, Region == "Hawaii")
    #   }
    # })
    # regionSelection <- reactive({
    #   subset(vs, Region == input$region_select)
    # })

    # update <- renderText({vs$Virtual_Stations[vs$Region == input$region_select]})
    # update region selections
    # vs_selected <- vs$Virtual_Stations[vs$Region == input$region_select]
    # 
    # observeEvent(input$vs_select, {
      # update <- vs[vs$Region == input$region_select, "Virtual_Stations"]
      # x <- input$region_select
      # if (input$region_select == x) {
      #   updatePickerInput(session = session, inputId = "vs_select",
      #                     choices = vs$Virtual_Stations[vs$Region == x],
      #                     selected = vs$Virtual_Stations[vs$Region == x])
      # }

      # updatePickerInput(session = session, inputId = "vs_select",
      #                   choices = vs$Virtual_Stations[vs$Region == x],
      #                   selected = vs$Virtual_Stations[vs$Region == x])
    # }, ignoreInit = TRUE)
  
    observeEvent(input$vs_select, ignoreInit = TRUE, {
        updateSelectInput(session, "vs_select",
                          selected = vs[vs$Region == input$region_select, "Virtual_Stations", drop = TRUE])
    })
    

    output$historical_data_map <- renderLeaflet({
      historicalMap
    })
  }
)

# shinyApp(ui, server)