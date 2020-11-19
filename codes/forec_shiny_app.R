# Fore-C interactive explorer ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# Useful hyperlinks:
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
# https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html # interactive polygons
# to use the css I need to download docs: https://shiny.rstudio.com/articles/css.html

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
library(rgdal) # needed if using clickable polygons
library(sf) # needed if using clickable polygons

# load data
load("Compiled_data/historical_surveys.RData")
vs <- read.csv("Data/virtual_stations.csv", stringsAsFactors = F)
load("Compiled_data/vs_scenarios_long.RData")
source("codes/addScaleBar.R")

# load rasters
lastUpdate <- substr(max(list.files("Compiled_data/nowcasts/raster/")), 1, 10)
ga_nowcast <- raster(paste0("Compiled_data/forecasts_4wk/raster/", lastUpdate, "_ga.tif"))
ga_4wkcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_ga.tif"))
ga_8wkcast <- raster(paste0("Compiled_data/forecasts_8wk/raster/", lastUpdate, "_ga.tif"))
ga_12wkcast <- raster(paste0("Compiled_data/forecasts_12wk/raster/", lastUpdate, "_ga.tif"))

# map settings (commented out polygon mapping) 
# p <- as(ga_nowcast, "SpatialPolygonsDataFrame")
# nByTown_latlon <- spTransform(p, CRS("+proj=longlat +datum=WGS84"))
bins <- seq(0, 0.3, 0.05)
# pal <- colorBin("YlOrRd", domain = nByTown_latlon$X2020.11.18_ga, bins = bins)
pal <- colorBin(colorRampPalette(c("blue", "yellow", "red"))(30), domain = ga_nowcast$X2020.11.18_ga, bins = bins, na.color = "transparent")

# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(ga_12wkcast),
#                     na.color = "transparent") 

# testplot <- list(plot(seq(1,10,1), seq(1,10,1), xlab = "x", ylab = "y", pch = 16))
# 
# labelx <- paste0("Estimate disease risk = ", round(nByTown_latlon$X2020.11.18_ga, 2)*100, "%") %>% 
#   lapply(htmltools::HTML)
# 
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = nByTown_latlon,
#               fillColor = ~pal(nByTown_latlon$X2020.11.18_ga),
#               weight = 2,
#               opacity = 1,
#               color = ~pal(nByTown_latlon$X2020.11.18_ga),
#               fillOpacity = 0.7,
#               labels = labelx,
#               labelOptions = labelOptions(style = list("font-weight" = "normal", 
#                                                        padding = "3px 8px"),
#                                           textsize = "15px",
#                                           direction = "auto")) 
#   
#   popupGraph(testplot,
#              width = 300,
#              height = 300)



legendLabels <- c("0-5", "6-10", "9-15", "16-20", "21-25", "26-30", "NA")

# create maps
basemap <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite") %>%
  addScaleBar() %>%
  addRasterImage(ga_nowcast, colors = pal, opacity = 0.6, group = "Nowcast") %>%
  addRasterImage(ga_4wkcast, colors = pal, opacity = 0.6, group = "4-week forecast") %>%
  addRasterImage(ga_8wkcast, colors = pal, opacity = 0.6, group = "8-week forecast") %>%
  addRasterImage(ga_12wkcast, colors = pal, opacity = 0.6, group = "12-week forecast") %>%
  # setView(lng = 180, lat = 0 , zoom = 3)  %>% # global
  setView(lng = -157, lat = 20 , zoom = 7)  %>% # Hawaii
  addLayersControl(
    overlayGroups = c("Nowcast", "4-week forecast", "8-week forecast", "12-week forecast"),
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE), # icon versus buttons with text
    position = c("bottomright")
    ) %>%
  hideGroup(c("4-week forecast", "8-week forecast", "12-week forecast")) %>%
  addLegend("bottomleft", pal = pal, values = values(ga_12wkcast),
            title = "Disease risk (%)",
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(legendLabels)
            }
  )

# historical map
historicalMap = leaflet() %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircleMarkers(data=historical_data, lat = ~Latitude, lng = ~Longitude, radius = ~sqrt(N)
                   , color = ~'white', popup = ~survey_text
                   , clusterOptions = markerClusterOptions()) %>%
  addScaleBar() %>%
  setView(lng = -180, lat = 16.4502 , zoom = 3)

# plotting function to visualize disease by virtual station
vs_plot_fun <- function(df){
  ggplot(data = df, aes(x = Date, y = Estimate*100, group = VS, color = VS)) + 
    geom_point(size = 2) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = Q2.5*100, 
                    ymax = Q97.5*(2/3)*100, # just a placeholder to limit se upper bound, will use 75%ile later when not using brms
                    group = VS,
                    fill = VS), 
                alpha = 0.1) +  
    ylim(0,100) +
    xlab("") +
    ylab("Predicted disease prevalence (%)") +
    theme_classic(base_size = 16) + 
    theme(legend.title=element_blank())
}
  
# run shiny app
shinyApp(
  ui =  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,"", id="nav",
            
                   # Nowcasts and forecasts page
                   tabPanel("Coral disease predictions",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("disease_risk_map", width = "100%", height = "100%")),
                      ),

                   # Scenarios page
                   tabPanel("Scenarios",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Growth anomalies",
                                                sidebarPanel(
                                                  
                                                  pickerInput("ga_region_select", "Region:",   
                                                              choices = unique(vs[order(vs$Region),]$Region), 
                                                              selected = c("Hawaii"),
                                                              options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                              multiple = FALSE),
                                                  
                                                  pickerInput("ga_vs_select", "Virtual station(s):",
                                                              choices = vs$Virtual_Stations[vs$Region == "Hawaii"],
                                                              options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                              selected = vs$Virtual_Stations[vs$Region == "Hawaii"],
                                                              multiple = TRUE),
                                                  
                                                  "Scenarios:",
                                                  sliderInput("colsize_slider", label = ("Mean colony size"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                  sliderInput("cover_slider", label = ("Host coral cover"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                  sliderInput("fish_slider", label = ("Fish abundance"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                  sliderInput("depth_slider", label = ("Depth (m)"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                  sliderInput("temp_slider", label = ("Temperature"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                  actionButton("reset_input", label = "Reset inputs") # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
                                                ),
                                                mainPanel(
                                                  tabPanel("Disease risk", plotOutput("ga_vs_risk_plot"))
                                                )

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
    
    # Main map
    output$disease_risk_map <- renderLeaflet({
      basemap
    })
    
    # scenarios outputs
    # update region selections
    observeEvent(input$ga_region_select, {
      if (input$ga_region_select == "Hawaii") {
        updatePickerInput(session = session, inputId = "ga_vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Hawaii"], 
                          selected = vs$Virtual_Stations[vs$Region == "Hawaii"])
      }
      
      if (input$ga_region_select == "Pacific Remote Island Areas") {
        updatePickerInput(session = session, inputId = "ga_vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Pacific Remote Island Areas"], 
                          selected = vs$Virtual_Stations[vs$Region == "Pacific Remote Island Areas"])
      }
      
      if (input$ga_region_select == "Australia") {
        updatePickerInput(session = session, inputId = "ga_vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Australia"], 
                          selected = vs$Virtual_Stations[vs$Region == "Australia"])
      }
      
      if (input$ga_region_select == "Marianas") {
        updatePickerInput(session = session, inputId = "ga_vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "Marianas"], 
                          selected = vs$Virtual_Stations[vs$Region == "Marianas"])
      }
      
      if (input$ga_region_select == "American Samoa") {
        updatePickerInput(session = session, inputId = "ga_vs_select", 
                          choices = vs$Virtual_Stations[vs$Region == "American Samoa"], 
                          selected = vs$Virtual_Stations[vs$Region == "American Samoa"])
      }
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$vs_select, ignoreInit = TRUE, {
      updateSelectInput(session, "vs_select",
                        selected = vs[vs$Region == input$region_select, "Virtual_Stations", drop = TRUE])
    })
    
    # plot disease risk by virtual station and adjust by scenario settings
    output$ga_vs_risk_plot <- renderPlot({
      reactive_ga_db <- reactive({
        vs_long %>% 
          filter(VS %in% input$ga_vs_select 
                 & meanColSize_condition == input$colsize_slider
                 & HostCover_condition == input$cover_slider
                 & FishAbundance_condition == input$fish_slider
                 & Depth_condition == input$depth_slider
                 & HotSnap_condition == input$temp_slider
          )
      })
      vs_plot_fun(reactive_ga_db())
    })
    
    # map historical data
    output$historical_data_map <- renderLeaflet({
      historicalMap
    })
  }
)

# shinyApp(ui, server)