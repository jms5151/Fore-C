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

# load data
load("Compiled_data/historical_surveys.RData")
vs <- read.csv("Data/virtual_stations.csv", stringsAsFactors = F)
load("Compiled_data/vs_scenarios_long.RData")
source("codes/addScaleBar.R")
load("Compiled_data/grid.RData")
load("Compiled_data/spatial_grid.Rds")
load("Compiled_data/simulated_data_for_dygraphs.RData")
load("Compiled_data/simulated_data_for_plotlygraphs.RData")
load("Compiled_data/regional_polygons.Rds")

# create maps
bins <- c(0, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 1.0)
pal <- colorBin(brewer.pal(length(bins), "YlOrRd"), domain = reefs2$drisk, bins = bins, na.color = "transparent")
legendLabels <- c("0-5", "6-10", "11-15", "16-25", "26-50", "51-75", "76-100", "NA")

poly_colors <- brewer.pal(length(region_poly), "Dark2")

leaf_reefs <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", group = "Satellite") %>%
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
leaf_scenarios <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addScaleBar("bottomleft") %>%
  addPolygons(data = region_poly,
              color = poly_colors,
              highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
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

button_info = list(
  list(
    active = -1, # not dropdown menu
    x = 0.2, # x location, if excluded, plots outside graph
    y = 1.0, # y location
    type = 'buttons',
    buttons = list(
      
      list(method = "restyle",
           args = list("visible", list(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)), # first 4 = lines, second 4 = ribbons
           label = "Show CI"),
      
      list(method = "restyle",
           args = list("visible", list(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
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
              color = ~as.character(cast),
              colors = "BrBG",
              text = ~paste("Date:", Date,
                            "<br>Risk:", round(value*100), "% (", round(Lwr*100), ",", round(Upr*100), ")"),
              hoverinfo = "text") %>%
    add_ribbons(data = df, 
                x= ~Date, 
                split = ~cast, 
                ymin = ~round(Lwr*100), 
                ymax = ~round(Upr*100),
                color = ~as.character(cast),
                colors = "BrBG", 
                opacity=0.3,
                hoverinfo='skip') %>%
    layout(title = titleName,
           xaxis = list(showgrid = F, 
                        title = ""), 
           yaxis = list(showline = T, 
                        showgrid = F, 
                        range = c(0, 100),
                        title = "Disease risk"),
           hovermode = 'compare',
           font = list(size = 14),
           showlegend = FALSE,
           updatemenus = button_info, 
           margin = plotly_margins)      
  
}

# made up data for scenarios page
fig <- plot_ly(y = ~rnorm(50), type = "box") # change y's to x's if you want flipped
fig <- fig %>% 
  add_trace(y = ~rnorm(50, 1)) %>% 
  add_trace(y = ~rnorm(50, 1)) %>%
  layout(showlegend = FALSE)

# run shiny app
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,"", id="nav",
                 # Nowcasts and forecasts page
                 tabPanel("Coral disease predictions",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("map1", width = "100%", height = "100%")),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        bottom = 0, right = 0, width = 600, fixed = TRUE,
                                        draggable = FALSE, height = "auto",
                                        plotlyOutput("plotlyGA",height = 300),
                                        plotlyOutput("plotlyWS", height = 300),
                                        style = "opacity: 0.92",
                                        span(tags$i(h6("Disease forecasts, select a pixel")), style="color:#045a8d")
                          )
                 ),
                 
                 # Management scenarios page
                 tabPanel("Management scenarios",
                          leafletOutput("management_map"),
                          
                          hr(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Growth anomalies",
                                               # h3("Growth anomalies"),
                                               fluidRow(
                                                 column(4, wellPanel(title = "Adjust scenarios", background = "maroon", solidHeader = TRUE,
                                                                     sliderInput("colsize_slider", label = ("Mean colony size"), min = -100, max = 100, step = 20, post  = " %", value = 0),
                                                                     sliderInput("cover_slider", label = ("Host coral cover"), min = -100, max = 100, step = 20, post  = " %", value = 0),
                                                                     sliderInput("fish_slider", label = ("Fish abundance"), min = -100, max = 100, step = 20, post  = " %", value = 0),
                                                                     style = "background: white",
                                                 )
                                                 ),
                                                 column(8, wellPanel(plotlyOutput("barplot"),
                                                                     style = "background: white",))
                                               )
                                               
                                      ),
                                      # h2("White syndromes"),
                                      tabPanel("White syndromes")
                          )
                 ),
                 
                 # sensitivity page
                 tabPanel("Sensitivity analysis",
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
                 )
  
server <- function(input, output, session) { 
    
  output$map1 <- renderLeaflet({
    leaf_reefs
  })
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
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
  output$management_map <- renderLeaflet({
    leaf_scenarios
  })
  
  output$barplot <- renderPlotly({
    fig
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

shinyApp(ui, server)

