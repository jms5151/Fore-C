# Fore-C interactive explorer ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# Useful hyperlinks:
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
# https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html # interactive polygons
# to use the css I need to download docs: https://shiny.rstudio.com/articles/css.html
# https://stackoverflow.com/questions/62544187/popupgraph-r-leaflet-why-are-my-popup-graphs-blank OR
# https://stackoverflow.com/questions/62642615/adding-reactive-popup-graphs-plots-to-a-leaflet-map-with-shiny-r

#### click id with multiple polygons:
# https://stackoverflow.com/questions/41104576/changing-styles-when-selecting-and-deselecting-multiple-polygons-with-leaflet-sh
# newer response
# https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in/65935636#65935636
##########

# create bounding box to select multiple pixels: https://redoakstrategic.com/geoshaper/

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
library(dygraphs)
library(xts)
library(RColorBrewer)
library(plotly)
library(shinydashboard)
library(shinycssloaders)
library(leaflet.extras)

# load data
load("Compiled_data/historical_surveys.RData")
# vs <- read.csv("Data/virtual_stations.csv", stringsAsFactors = F)
# load("Compiled_data/vs_scenarios_long.RData")
source("codes/addScaleBar.R")
load("Compiled_data/grid.RData")
load("Compiled_data/spatial_grid.Rds")
# load("Compiled_data/simulated_data_for_dygraphs.RData")
load("Compiled_data/simulated_data_for_plotlygraphs.RData")
load("Compiled_data/regional_polygons.Rds")
load("Compiled_data/mitigation.RData")
load("Compiled_data/baseline.RData")

# create maps
bins <- c(0, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 1.0)
pal <- colorBin(brewer.pal(length(bins), "YlOrRd"), domain = reefs2$drisk, bins = bins, na.color = "transparent")
legendLabels <- c("0-5", "6-10", "11-15", "16-25", "26-50", "51-75", "76-100", "NA")

# poly_colors <- brewer.pal(length(region_poly), "Dark2")

leaf_reefs <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite") %>%
  addScaleBar("topright") %>% # bottomleft
  addPolygons(data = reefs2, 
              layerId = ~ID,
              fillColor = ~pal(reefs2$drisk),
              weight = 2,
              opacity = 1,
              color = ~pal(reefs2$drisk),
              fillOpacity = 0.7,
              group = "Local forecasts",
              highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)
              ) %>%
  addPolygons(data = region_poly,
              group = "Regional forecasts",
              color = poly_colors,
              highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
              ) %>%
  addLayersControl(
    overlayGroups = c("Local forecasts", "Regional forecasts"),
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE), # icon versus buttons with text
    position = c("topleft")) %>%
  hideGroup(c("Regional forecasts")) %>% 
  leaflet::addLegend("topleft", pal = pal, values = reefs2$drisk,
            title = "Disease risk (%)",
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(legendLabels) }) 

# scenario map
# leaf_scenarios <- leaflet() %>%
#   addTiles(group = "OpenStreetMap") %>%
#   addScaleBar("bottomleft") %>%
#   addPolygons(data = region_poly,
#               color = poly_colors,
#               highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
#   )

leaf_scenarios <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addScaleBar("bottomleft") %>%
  addPolygons(data = reefs2, 
              layerId = ~ID,
              fillColor = ~pal(reefs2$drisk),
              weight = 2,
              opacity = 1,
              color = ~pal(reefs2$drisk),
              fillOpacity = 0.7,
              group = "Local forecasts",
              highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)
  )

# historical map
historicalMap = leaflet() %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircleMarkers(data=historical_data, lat = ~Latitude, lng = ~Longitude, radius = ~sqrt(N)
                   , color = ~'white', popup = ~survey_text
                   , clusterOptions = markerClusterOptions()) %>%
  addScaleBar() %>%
  setView(lng = -180, lat = 16.4502 , zoom = 3)

button_info = list(
  list(
    active = -1, # not dropdown menu
    x = 0.2, # x location, if excluded, plots outside graph
    y = 1.0, # y location
    type = 'buttons',
    buttons = list(
      
      list(method = "restyle",
           args = list("visible", rep(list(TRUE), 29*2)), # first 29 = lines, second 29 = ribbons
           label = "Show CI"),
      
      list(method = "restyle",
           args = list("visible", c(rep(list(TRUE), 29), rep(list(FALSE), 29))),
           label = "Hide CI"))
  )
  
)
  
plotly_margins <- list(
  l = 50,
  r = 20,
  b = 20,
  t = 60
)

diseaseRisk_plotly <- function(df, titleName){
  plot_ly() %>%
    add_trace(data = df, 
              x = ~Date, 
              y = ~round(value*100), 
              split = ~cast, 
              type = 'scatter', 
              mode = 'lines',
              color = ~type,
              colors = c("Salmon", "Midnight blue"),
              text = ~paste("Date:", Date,
                            "<br>Disease risk:",
                            "<br>90th percentile", round(Upr*100), "%", # IQR90
                            "<br>75th percentile", round(value*100), "%", #IQR75
                            "<br>50th percentile", round(Lwr*100), "%" #IQR50
              ),
              hoverinfo = "text") %>%
    add_ribbons(data = df, 
                x= ~Date, 
                split = ~cast, 
                ymin = ~round(Lwr*100), 
                ymax = ~round(Upr*100),
                color = ~type,
                colors = c("Salmon", "Midnight blue"),
                opacity=0.3,
                hoverinfo='skip') %>%
    layout(title = titleName,
           xaxis = list(showgrid = F, 
                        title = ""), 
           yaxis = list(showline = T, 
                        showgrid = F, 
                        range = c(0, 100),
                        title = "Disease risk"),
           # hovermode = 'compare',
           font = list(size = 14),
           showlegend = FALSE,
           updatemenus = button_info, 
           margin = plotly_margins)      
}

diseaseRisk_placeholder_plot <- function(titleName) {
  plot_ly() %>%
    add_trace(x = ~range(p$Date), 
              y = 0, 
              type = 'scatter', 
              mode = 'lines') %>%
    layout(title = titleName,
           xaxis = list(showgrid = F, 
                        title = ""), 
           yaxis = list(showline = T, 
                        showgrid = F, 
                        range = c(0, 100),
                        title = "Disease risk"),
           font = list(size = 14),
           showlegend = FALSE,
           margin = plotly_margins)
}


mitigation_plot_fun <- function(w, f, c, p){
  plot_ly(data = w , x = ~Response, y = ~round(estimate*100), error_y = list(value = ~round(sd*100)), type = "bar", color = I("deepskyblue4")) %>%
    add_trace(data = f , y = ~round(estimate*100), color = I("darkred")) %>%
    add_trace(data = c , y = ~round(estimate*100), color = I("black")) %>%
    add_trace(x = "Combined", y = ~round(w$estimate*100 + f$estimate*100 + c$estimate*100), error_y = list(value = ~round(w$sd*100 + f$sd*100 + c$sd*100)), color = I("goldenrod1")) %>%
    layout(
      xaxis = list(showgrid = F, 
                   title = "",
                   categoryorder = "array",
                   categoryarray = ~c("Water quality",
                                      "Fish abundance",
                                      "Coral",
                                      "Combined"
                   )
      ), 
      yaxis = list(showline = T, 
                   showgrid = F, 
                   range = c(-100, 100),
                   title = paste0("Change in disease risk<br>(baseline = ", p, "%)")
                   ),
      font = list(size = 14),
      showlegend = FALSE) 
}

scenarios_placeholder_plot <- plot_ly(x = "Water quality", y = 0, type = "bar", color = I("deepskyblue4")) %>%
  add_trace(x = "Fish abundance", y = 0, color = I("darkred")) %>%
  add_trace(x = "Coral" , y = 0, color = I("black")) %>%
  add_trace(x = "Combined", y = 0, color = I("goldenrod1")) %>%
  layout(
    xaxis = list(showgrid = F, 
                 title = "",
                 categoryorder = "array",
                 categoryarray = ~c("Water quality",
                                    "Fish abundance",
                                    "Coral",
                                    "Combined"
                 )
    ), 
    yaxis = list(showline = T, 
                 showgrid = F, 
                 range = c(-100, 100),
                 title = "Change in disease risk<br>(from current conditions)"),
    font = list(size = 14),
    showlegend = FALSE) 

# run shiny app
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,"", id="nav",
                 # Nowcasts and forecasts page
                 tabPanel("Coral disease predictions",
                          leafletOutput("mymap") %>% withSpinner(color="#D3D3D3"),
                          hr(),
                          fluidRow(
                            column(6, plotlyOutput("plotlyGA") %>% withSpinner(color="#D3D3D3")),
                            column(6, plotlyOutput("plotlyWS",) %>% withSpinner(color="#D3D3D3"))
                          ),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 450, left = 30, fixed = TRUE,
                                        draggable = FALSE, height = "auto",
                                        class = "dropdown",
                                        dropMenu(
                                          dropdownButton(icon = icon('info'), size = "xs"),
                                          h3(strong('Information')),
                                          h5('Click on a pixel to explore near-real time coral disease forecasts for a given location')
                                        )
                          )
                 ),
                 
                 # Management scenarios page
                 tabPanel("Mitigation potential", #HTML("Long-term mitigation<br/>potential"),
                          leafletOutput("management_map", height = "300px") %>% withSpinner(color="#D3D3D3"),
                          
                          hr(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Growth anomalies",
                                               fluidRow(
                                                 column(4, wellPanel(class = "dropdown",
                                                                     dropMenu(
                                                                       dropdownButton("Info2", icon = icon('info'), size = "xs"),
                                                                       h3(strong('Information')),
                                                                       h5('Click on a pixel and adjust sliders to explore coral disease mitigation potential.')
                                                                     ),
                                                                     h4("Mitigation targets:"),                        
                                                                     # colors = deepskyblue4, dark red, black
                                                                     setSliderColor(c("#00688B", "#8B0000", "#000000"), c(1, 2, 3)),
                                                                     sliderInput("wq_slider_ga", 
                                                                                 label = span(h5(strong("Water quality")), 
                                                                                              tags$i(h6(textOutput("chlA_value"))),
                                                                                              tags$i(h6(textOutput("kd_value"))),
                                                                                              style="color:#00688B"),
                                                                                 min = -100, max = 100, step = 20, post  = " %", value = 0),
                                                                     sliderInput("fish_slider_ga", 
                                                                                 label = span(h5(strong("Fish abundance")), 
                                                                                              tags$i(h6(textOutput("fish_value"))),
                                                                                              style="color:#8B0000"),
                                                                                 min = -100, max = 100, step = 20, post  = " %", value = 0),
                                                                     sliderInput("coral_slider_ga", 
                                                                                 label = span(h5(strong("Coral")), 
                                                                                              tags$i(h6(textOutput("corsize_value"))),
                                                                                              tags$i(h6(textOutput("corcov_value"))),
                                                                                              style="color:#000000"),
                                                                                 min = -100, max = 100, step = 20, post  = " %", value = 0), #HTML("Coral<br/>Colony size<br/>Host coral cover")
                                                                     style = "background: white"
                                                 )
                                                 ),
                                                 column(8, wellPanel(plotlyOutput("barplot"),
                                                                     style = "background: white",))
                                               )
                                               
                                      ),                                
                                      tabPanel("White syndromes")
                          )
                 ),
                 # Historical data page
                 tabPanel("Historical data", 
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("historical_data_map", width="100%", height="100%"))
                          ),
                 
                 # About the project page
                 tabPanel("About")
                 )

server <- function(input, output, session) { 
    
  output$map1 <- renderLeaflet({
    leaf_reefs
  })
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())

  # output$plotlyGA <- DT::renderDT(NULL)
  # output$plotlyWS <- DT::renderDT(NULL)
  
  output$plotlyGA <- renderPlotly({diseaseRisk_placeholder_plot("Growth anomalies")})
  output$plotlyWS <- renderPlotly({diseaseRisk_placeholder_plot("White syndromes")})
  
  observeEvent(input$map1_shape_click, {

    # if(is.numeric(input$map1_shape_click$id) == TRUE){
    if(input$map1_shape_click$group == "Local forecasts"){
      # selected$groups <- c(selected$groups, input$map_shape_click$id)
      # proxy %>% showGroup(group = input$map_shape_click$id)
      
      z <- subset(p, ID == input$map1_shape_click$id)
      z2  <- subset(p, ID == input$map1_shape_click$id + 1)
      # https://rstudio.github.io/dygraphs/gallery-upper-lower-bars.html
      output$plotlyGA <- renderPlotly({
        diseaseRisk_plotly(z, "Growth anomalies")
      })
      
      output$plotlyWS <- renderPlotly({
        diseaseRisk_plotly(z2, "White syndromes")
      })

    }

  })
  
  # management scenarios outputs
  # management scenarios outputs
  output$management_map <- renderLeaflet({
    # leaf_reefs
    leaf_scenarios
  })
  
  output$barplot <- renderPlotly({scenarios_placeholder_plot})
  
  #create empty vector to hold all click ids
  selected2 <- reactiveValues(groups = vector())
  
  observeEvent(input$management_map_shape_click, {
    
    
    if(input$management_map_shape_click$group == "Local forecasts"){
      
      mitigate <- subset(mitigation_df, ID == input$management_map_shape_click$id)
      baseVals <- subset(baseline_vals, ID == input$management_map_shape_click$id)
      
      reactive_w <- reactive({
        subset(mitigate, Response == "Water quality" & Response_level == input$wq_slider_ga)
      })
      
      reactive_f <- reactive({
        subset(mitigate, Response == "Fish abundance" & Response_level == input$fish_slider_ga)
      })
      
      reactive_c <- reactive({
        subset(mitigate, Response == "Coral" & Response_level == input$coral_slider_ga)
      })
      
      output$barplot <- renderPlotly({
        mitigation_plot_fun(reactive_w(), reactive_f(), reactive_c(), round(baseVals$p*100))
      })
      
      output$chlA_value <- renderText({ paste0("chl-a: baseline = ", round(baseVals$chla, 2), 
                                               "; adjusted = ", 
                                               round(baseVals$chla + baseVals$chla * (input$wq_slider_ga/100), 2)) })
      
      output$kd_value <- renderText({ paste0("kd(490): baseline = ", round(baseVals$kd490, 2),
                                             "; adjusted = ", 
                                             round(baseVals$kd490 + baseVals$kd490 * (input$wq_slider_ga/100), 2)) })
      
      output$fish_value <- renderText({ paste0("fish: baseline = ", round(baseVals$fish),
                                               "; adjusted = ", 
                                               round(baseVals$fish + baseVals$fish * (input$fish_slider_ga/100), 2)) })
      
      output$corsize_value <- renderText({ paste0("Median colony size: baseline = ", round(baseVals$coral_size), 
                                                  "cm; adjusted = ",
                                                  round(baseVals$coral_size + baseVals$coral_size * (input$coral_slider_ga/100), 2),
                                                  "cm") })
      
      output$corcov_value <- renderText({ paste0("Host cover: baseline = ", round(baseVals$host_cover), 
                                                 "%; adjusted = ",
                                                 round(baseVals$host_cover + baseVals$host_cover * (input$coral_slider_ga/100), 2),
                                                 "%") })
    }
  })  
    # map historical data
    output$historical_data_map <- renderLeaflet({
      historicalMap
    })
  }

shinyApp(ui, server)

