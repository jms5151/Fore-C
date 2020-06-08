# Fore-C interactive explorer ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(shiny)
library(shinythemes)
library(ggridges)
library(ggplot2)
library(leaflet)
library(raster)
library(flexdashboard)
library(tidyverse)

# load data
load("Compiled_data/observational_data.RData")
load("Compiled_data/historical_surveys.RData")
load("Compiled_data/number_surveys.RData")
source("codes/addScaleBar.R")

# load nowcasts
lastUpdate <- substr(max(list.files("Compiled_data/nowcasts/raster/")), 1, 10)
total_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_total.tif"))
bbd_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_bbd.tif"))
ga_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_ga.tif"))
ws_nowcast <- raster(paste0("Compiled_data/nowcasts/raster/", lastUpdate, "_ws.tif"))

# run shiny app
shinyApp(
  ui = navbarPage(theme = shinytheme("superhero"), title="Fore-C", collapsible = TRUE, id="nav",
                  # Nowcasts and forecasts page
                  tabPanel("Coral disease predictions",
                           sidebarPanel(radioButtons("nowcast_radio", label = h3("Disease type"),
                                                     choices = list("Weighted total" = 1,
                                                                    "Black band disease" = 2,
                                                                    "Growth anomalies" = 3,
                                                                    "White syndromes" = 4),
                           selected = 1),
                           hr(),
                           selectInput("select_forecast", label = h3("Select forecast"),
                                       choices = c("Nowcast",
                                                   "4-week forecast",
                                                   "8-week forecast",
                                                   "12-week forecast"),
                                       selected = "Nowcast"),
                           hr(),
                           textOutput("update")
                           ),
                           mainPanel(
                            leafletOutput("mymap")
                           )

                  ),
                  # Scenarios page
                  tabPanel("Scenarios",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Black band disease",
                                                sidebarPanel(sliderInput("temp_slider", label = h3("Temperature"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             sliderInput("light_slider", label = h3("Irradiance"), min = -100, max = 100, step=10, width='200px', post  = " %", value = 0),
                                                             hr(),
                                                             actionButton("reset_input", label = "Reset inputs") # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
                                                ),
                                                mainPanel(leafletOutput("mymap3"))
                                       ),
                                       tabPanel("Growth anomalies",
                                                sidebarPanel(sliderInput("temp_slider", label = h3("Temperature"), min = -50, max = 50, step=10, width='200px', post  = " %", value = 0),
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
                                                sidebarPanel(sliderInput("chl_slider", label = h3("Chlorophyll-a"), min = -100, max = 100, step=10, width='200px', post  = " %", value = 0),
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
                           leafletOutput("mymap6")
                           ),
                  # Model descriptions page
                  tabPanel("Models",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Black band disease",
                                                textOutput("Model equation"),
                                                hr(),
                                                textOutput("Coefficients"),
                                                hr(),
                                                textOutput("Sensitivity"),
                                                hr(),
                                                textOutput("Accuracy")
                                                ),
                                       tabPanel("Growth anomalies",
                                                textOutput("Model equation"),
                                                (withMathJax("$$Y \\sim\\ Binomial(C,\\mu)$$")),
                                                br(),
                                                (withMathJax("$$\\mu=\\beta_0+(\\beta_{Temp}*X_{Temp})
                                                             +(\\beta_{Chl}*X_{Chl})
                                                             +(\\beta_{Dev}*X_{Dev})$$")),
                                                hr(),
                                                textOutput("Coefficients"),
                                                hr(),
                                                textOutput("Sensitivity"),
                                                hr(),
                                                textOutput("Accuracy")
                                       ),
                                       tabPanel("White syndromes",
                                                textOutput("Model equation"),
                                                hr(),
                                                textOutput("Coefficients"),
                                                hr(),
                                                textOutput("Sensitivity"),
                                                hr(),
                                                textOutput("Accuracy")
                                       )
                           )
                  ),
                  # Interventions page
                  tabPanel("Interventions/Montitoring Tools"),
                  # About the project page
                  tabPanel("About")
  ),
  
  server = function(input, output) { 
  
    basemap = leaflet() %>%
      addTiles() %>%
      addScaleBar()

    output$mymap <- renderLeaflet({
      if(input$nowcast_radio == 1){
            x <- total_nowcast
          }
          if(input$nowcast_radio == 2){
            x <- bbd_nowcast
          }
          if(input$nowcast_radio == 3){
            x <- ga_nowcast
          }
          if(input$nowcast_radio == 4){
            x <- ws_nowcast
          }
      basemap %>%
        addRasterImage(x, colors = "Spectral", opacity = 0.6) %>%
        setView(lng = 180, lat = 16.4502 , zoom = 2)
    })

    output$update <- renderText({paste0("Last update: ", lastUpdate)})

    output$mymap2 <- renderLeaflet({
      basemap %>%
        setView(lng = 144.7875, lat = 13.4502 , zoom = 8)
    })

    output$mymap3 <- renderLeaflet({
      basemap %>%
        setView(lng = 144.7875, lat = 13.4502 , zoom = 8)
    })

    output$mymap4 <- renderLeaflet({
      basemap %>%
        setView(lng = 144.7875, lat = 13.4502 , zoom = 8)
    })

    output$mymap5 <- renderLeaflet({
      basemap %>%
        setView(lng = 144.7875, lat = 13.4502 , zoom = 8)
    })

    output$mymap6 <- renderLeaflet({
      leaflet() %>%
        addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
        addCircleMarkers(data=historical_data, lat = ~Latitude, lng = ~Longitude, radius = ~sqrt(N)
                         , color = ~'white', popup = ~survey_text
                         , clusterOptions = markerClusterOptions()) %>%
        addScaleBar() %>%
        setView(lng = -180, lat = 16.4502 , zoom = 3)
    })

  }
)
