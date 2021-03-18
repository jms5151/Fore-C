library(shiny)
library(leaflet)
library(rgdal)
library(dygraphs)
library(xts)
library(tidyverse)

load("Compiled_data/grid.RData")
load("Compiled_data/spatial_grid.Rds")

# replace with fore-c data
bins <- seq(0, 0.3, 0.05)
pal <- colorBin(colorRampPalette(c("blue", "orange", "maroon"))(30), domain = reefs2$drisk, bins = bins, na.color = "transparent")

leaf_reefs <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addPolygons(data = reefs2, 
              layerId = ~ID,
              fillColor = ~pal(reefs2$drisk),
                            weight = 2,
                            opacity = 1,
                            color = ~pal(reefs2$drisk),
                            fillOpacity = 0.7) %>%
  setView(lng = -156, lat = 20 , zoom = 9)

# generate some random time series data
# reefsDF$Date <- as.Date("2021-03-01", "%Y-%m-%d")
# 
# reefsDF2 <- reefsDF %>% 
#   group_by(ID) %>%
#   complete(Date = seq.Date(Date - 16*7, Date, by="week"))
# 
# reefsDF2$Prev <- ifelse(reefsDF2$Date < "2021-01-20", rbeta(nrow(reefsDF2), 0.3, 1), NA)
# reefsDF2$Forecast1 <- ifelse(reefsDF2$Date > "2021-01-20", rbeta(nrow(reefsDF2), 0.3, 1), NA)
# reefsDF2$Forecast2 <- ifelse(reefsDF2$Date > "2021-01-20", rbeta(nrow(reefsDF2), 0.3, 1), NA)
# reefsDF2$Forecast3 <- ifelse(reefsDF2$Date > "2021-01-20", rbeta(nrow(reefsDF2), 0.3, 1), NA)
# # reefsDF2$Forecast4 <- ifelse(reefsDF2$Date > "2021-01-20", rbeta(nrow(reefsDF2), 0.3, 1), NA)
# 
# reefsDF2$PrevUpr <- reefsDF2$Prev + 0.1
# reefsDF2$PrevLwr <- reefsDF2$Prev - 0.1
# 
# reefsDF2$F1Upr <- reefsDF2$Forecast1 + 0.1
# reefsDF2$F1Lwr <- reefsDF2$Forecast1 - 0.1
# 
# reefsDF2$F2Upr <- reefsDF2$Forecast2 + 0.1
# reefsDF2$F2Lwr <- reefsDF2$Forecast2 - 0.1
# 
# reefsDF2$F3Upr <- reefsDF2$Forecast3 + 0.1
# reefsDF2$F3Lwr <- reefsDF2$Forecast3 - 0.1
# 
# save(reefsDF2, file = "Compiled_data/simulated_data_for_dygraphs.RData")
load("Compiled_data/simulated_data_for_dygraphs.RData")

ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,"", id="nav",
             tabPanel("Coral disease predictions",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          
                          leafletOutput("map1", width = "100%", height = "100%")),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        bottom = 10, left = 10, width = 550, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        dygraphOutput("dygraph1",height = 200),
                                        dygraphOutput("dygraph2", height = 200),
                                        style = "opacity: 0.92",
                                        span(tags$i(h6("Disease forecasts, select a pixel")), style="color:#045a8d")
                                        )
                      )
             )


server <- function(input, output, session) {
  # v <- reactiveValues(msg = "")
  
  output$map1 <- renderLeaflet({
    leaf_reefs
  })
  observeEvent(input$map1_shape_click, {
    
    reef_pixels_data <- reefsDF2[reefsDF2$ID == input$map1_shape_click$id,]
    reef_pixels2 <- reef_pixels_data[,c("Prev", "PrevUpr", "PrevLwr", 
                                        "Forecast1", "F1Lwr", "F1Upr", 
                                        "Forecast2", "F2Lwr", "F2Upr",
                                        "Forecast3", "F3Lwr", "F3Upr")]
    x <- xts(x = reef_pixels2, order.by = reef_pixels_data$Date)
    # https://rstudio.github.io/dygraphs/gallery-upper-lower-bars.html
    output$dygraph1 <- renderDygraph({
      dygraph(x, "Growth anomalies") %>%
        dySeries(c("PrevLwr", "Prev", "PrevUpr"), label = "Nowcast") %>%
        dySeries(c("F1Lwr", "Forecast1", "F1Upr"), label = "Forecast1") %>%
        dySeries(c("F2Lwr", "Forecast2", "F2Upr"), label = "Forecast2") %>%
        dySeries(c("F3Lwr", "Forecast3", "F3Upr"), label = "Forecast3") %>%
        dyAxis("y", label = "Disease risk", valueRange = c(0, 1.05)) %>%
        dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
      
    })
    output$dygraph2 <- renderDygraph({
      dygraph(x, "White syndromes") %>%
        dySeries(c("PrevLwr", "Prev", "PrevUpr"), label = "Nowcast") %>%
        dySeries(c("F1Lwr", "Forecast1", "F1Upr"), label = "Forecast1") %>%
        dySeries(c("F2Lwr", "Forecast2", "F2Upr"), label = "Forecast2") %>%
        dySeries(c("F3Lwr", "Forecast3", "F3Upr"), label = "Forecast3") %>%
        dyAxis("y", label = "Disease risk", valueRange = c(0, 1.05)) %>%
        dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
      
    })
  })

}

shinyApp(ui, server)

# time series plot
# reef_pixels_data <- reefsDF2[, c("Date", "Prev", "Upr", "Lwr")]

# https://rstudio.github.io/dygraphs/gallery-upper-lower-bars.html
reefs <- reefsDF2[reefsDF2$ID == 500, ]
reefs2 <- reefs[,c("Prev", "PrevUpr", "PrevLwr", 
                   "Forecast1", "F1Upr", "F1Lwr",
                   "Forecast2", "F2Upr", "F2Lwr")]
reefs_ts <- xts(reefs2, order.by = reefs$Date)

dygraph(reefs_ts, "White syndromes") %>%
  dySeries(c("PrevLwr", "Prev", "PrevUpr"), label = "Nowcast") %>%
  dySeries(c("F1Lwr", "Forecast1", "F1Upr"), label = "Forecast1") %>%
  dySeries(c("F2Lwr", "Forecast2", "F2Upr"), label = "Forecast2") %>%
  dyAxis("y", label = "Disease risk", valueRange = c(0, 1.05)) %>%
  dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
  dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
  dyRangeSelector(height = 20)

d1 <- "2021-01-30"
d2 <- "2021-02-22"
difftime(as.Date(d2, "%Y-%m-%d"), as.Date(d1, "%Y-%m-%d"))  
