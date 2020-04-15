# Fore-C interactive explorer ------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(shiny)
library(shinythemes)
library(ggridges)
library(ggplot2)
library(leaflet)
library(raster)
# library(shinydashboard)
library(flexdashboard)
library(tidyverse)

### addPopupGraphs

# load data
load("Compiled_data/observational_data.RData")
load("Compiled_data/historical_surveys.RData")
load("Compiled_data/number_surveys.RData")
load("Compiled_data/long_term_risk.RData")
source("C:/Users/Jamie/Box/R_functions/addScaleBar.R")

# load nowcasts
fileName <- paste0("Compiled_data/nowcasts/raster/", max(list.files("Compiled_data/nowcasts/raster/")))
ga_nowcast <- raster(fileName)
fileName2 <- paste0("Compiled_data/nowcasts/csv/", max(list.files("Compiled_data/nowcasts/csv/")))
load(fileName2)

nowcast_median_risk <- ga_nowcast_risk %>% group_by(Disease_type) %>% summarize(median_risk = round(median(Prevalence)*100,1))

risk <- rbind(observations[,c("Region", "Prevalence", "Disease_type", "Risk")], ga_nowcast_risk)
# long_term_risk <- long_term_risk[,c(1,3:5)]
# risk <- rbind(long_term_risk, ga_nowcast_risk)

lastUpdate <- substr(max(list.files("Compiled_data/nowcasts/csv/")), 17, 26)

shinyApp(
  ui = navbarPage(theme = shinytheme("superhero"), title="Fore-C", collapsible = TRUE, id="nav",
                  tabPanel("Coral disease nowcast",
                           sidebarPanel(radioButtons("nowcast_radio", label = h3("Disease type"),
                                                     choices = list("Growth anomalies" = 1,
                                                                    "White syndrome" = 2,
                                                                    "Black band disease" = 3),
                           selected = 1),
                           hr(),
                           h3("Current risk:"),
                           gaugeOutput("ga_gauge"),
                           hr(),
                           textOutput("update")
                           ),
                           mainPanel(
                            leafletOutput("mymap"),
                            br(),
                            plotOutput("densityplot")
                           )

                  ),
                  tabPanel("Four-month forecast",
                           sidebarPanel(radioButtons("forecast_radio", label = h3("Disease type"),
                                                     choices = list("Growth anomalies" = 1,
                                                                    "White syndrome" = 2,
                                                                    "Black band disease" = 3),
                                                     selected = 1)),
                           mainPanel(leafletOutput("mymap2"))
                  ),
                  tabPanel("Scenarios",
                           sidebarPanel(radioButtons("snearios_radio", label = h3("Disease type"),
                                                     choices = list("Growth anomalies" = 1,
                                                                    "White syndrome" = 2,
                                                                    "Black band disease" = 3),
                                                     selected = 1)),
                           mainPanel(leafletOutput("mymap3"))
                  ),
                  tabPanel("Historical data",
                           leafletOutput("mymap4")),
                  tabPanel("Download report"),
                  tabPanel("Models",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Equations",
                                                # (textOutput("Growth anomalies:")),
                                                # br(),
                                                (withMathJax("$$Y \\sim\\ Binomial(C,\\mu)$$")),
                                                br(),
                                                # P{n \\choose k}
                                                (withMathJax("$$\\mu=\\beta_0+(\\beta_{Temp}*X_{Temp})
                                                             +(\\beta_{Chl}*X_{Chl})
                                                             +(\\beta_{Dev}*X_{Dev})$$"))),
                                       tabPanel("Coefficients"),
                                       tabPanel("Sensitivity"),
                                       tabPanel("Accuracy"),
                                       tabPanel("Forecasts")
                           )),
                  tabPanel("About")
  ),
  server = function(input, output) { 
  
    basemap = leaflet() %>%
      addTiles() %>%
      addScaleBar()
    
    diseaseType <- reactive({
      subset(models, County == input$countyInput[1] & Category == "Normal")
    })  
    
    output$ga_gauge <- renderGauge({
       gauge(nowcast_median_risk$median_risk[1], min = 0, max = 100, symbol = '%', gaugeSectors(
        success = c(0, 20), warning = c(21, 50), danger = c(51, 100)
      ))
    })
    
    output$mymap <- renderLeaflet({
      basemap %>%
        addRasterImage(ga_nowcast, colors = "Spectral", opacity = 0.6) %>%
        setView(lng=180, lat=16.4502 , zoom=2)
    })
    
    output$update <- renderText({paste0("Last update: ", lastUpdate)})
    
    output$densityplot <- renderPlot({
      if(input$historical_radio==1){
        x<-"GA"
      }
      if(input$historical_radio==2){
        x <- "TLS"
      }
      if(input$historical_radio==3){
        x <- "BBD"
      }
      hists <- reactive({
        subset(risk, Disease_type == x)
    })
      ggplot(hists(), aes(y = Region)) +
        geom_density_ridges(aes(x = Prevalence, fill = Risk), alpha = .3) +
        theme_ridges() +
        ylab('') +
        theme(axis.title.x = element_text(hjust=0.5)) +
        scale_fill_manual(values = c("black", "deepskyblue3")) +
        xlim(-0.01,0.15)
    })
    
    output$mymap2 <- renderLeaflet({
      basemap %>%
        setView(lng=144.7875, lat=13.4502 , zoom=8)
    })
    
    output$mymap3 <- renderLeaflet({
      basemap %>%
        setView(lng=144.7875, lat=13.4502 , zoom=8)
    })

    output$mymap4 <- renderLeaflet({
      leaflet() %>% 
        addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
        addCircleMarkers(data=historical_data, lat = ~Latitude, lng = ~Longitude, radius = ~sqrt(N)
                         , color = ~'white', popup = ~survey_text
                         , clusterOptions = markerClusterOptions()) %>%
        addScaleBar() %>%
        setView(lng=-180, lat=16.4502 , zoom=3)
    })
    
    output$numSurveysplot <- renderPlot({    
      if(input$historical_radio==1){
        x<-"GA"
      }
      if(input$historical_radio==2){
        x <- "TLS"
      }
      if(input$historical_radio==3){
        x <- "BBD"
      }
      surveys2 <- reactive({ 
        subset(surveys, Disease_type == x)
      })

      ggplot(data=surveys2(), aes(x=Region, y=N)) +
        geom_bar(stat="identity") +
        theme_classic() +
        xlab('') +
        ylab('Number of surveys') +
        ylim(0, 750)
    })
  }
)

# https://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny
# https://stackoverflow.com/questions/48781380/shiny-how-to-highlight-an-object-on-a-leaflet-map-when-selecting-a-record-in-a
# https://rstudio.github.io/shinydashboard/structure.html
# https://www.r-bloggers.com/4-tricks-for-working-with-r-leaflet-and-shiny/
# https://vac-lshtm.shinyapps.io/ncov_tracker/
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
# https://github.com/HeatherWelch/ETBF_bycatch_NatCon/blob/master/ETBF_bycatch_app/app.R
# https://coronavirus.jhu.edu/map.html
# https://mapspbgjam1.env.duke.edu/communities/


# https://rstudio.github.io/leaflet/shiny.html

# leafletOutput("mymap", width="100%", height="100%"),
# absolutePanel(id = "controls", class = "panel panel-default",
#               top = 80, left = 20, width = 250, fixed=TRUE,
#               draggable = TRUE, height = "auto",
#               radioButtons("radio", label = h3("Disease type"),
#                            choices = list("Growth anomalies" = 1,
#                                           "White syndrome" = 2,
#                                           "Black band disease" = 3),
#                            selected = 1),


# N <- 10000
# x1 <- rnbinom(N, 10, .5)
# x2 <- rnorm(N, 6, 3)
# x3 <- rnorm(N, 12, 6)
# x4 <- rnorm(N, 11, 4)
# x5 <- rnorm(N, 5, 2)
# x6 <- rnbinom(N, 14, .8)
# x7 <- rnorm(N, 3, 2)
# x8 <- rnorm(N, 10, 4)
# x9 <- rnorm(N, 12, 3)
# x10 <- rnorm(N, 25, 5)
# 
# hists <- data.frame("Group"=c(rep("GBR",N)
#                             , rep("Hawaii",N)
#                             , rep("CNMI",N)
#                             , rep("Samoa",N)
#                             , rep("PRIAs",N)
#                             , rep("GBR",N)
#                             , rep("Hawaii",N)
#                             , rep("CNMI",N)
#                             , rep("Samoa",N)
#                             , rep("PRIAs",N))
#                     , "Prevalence"=c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
#                     , "Risk" = c(rep('Long term average', N*5), rep('Current', N*5)))
