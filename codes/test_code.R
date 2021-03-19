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
  setView(lng = 147.5, lat = 0 , zoom = 3)

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
                                        # bottom = 10, left = 10, width = 550, fixed=TRUE,
                                        bottom = 0, right = 0, width = 550, fixed=FALSE,
                                        draggable = FALSE, height = "auto",
                                        dygraphOutput("dygraph1",height = 200),
                                        # dygraphOutput("dygraph2", height = 200),
                                        plotlyOutput("plotlytest", height = 200),
                                        plotOutput("ggtest", height = 200),
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
    
    z <- subset(p, ID == input$map1_shape_click$id)
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
    # output$dygraph2 <- renderDygraph({
    #   dygraph(x, "White syndromes") %>%
    #     dySeries(c("PrevLwr", "Prev", "PrevUpr"), label = "Nowcast") %>%
    #     dySeries(c("F1Lwr", "Forecast1", "F1Upr"), label = "Forecast1") %>%
    #     dySeries(c("F2Lwr", "Forecast2", "F2Upr"), label = "Forecast2") %>%
    #     dySeries(c("F3Lwr", "Forecast3", "F3Upr"), label = "Forecast3") %>%
    #     dyAxis("y", label = "Disease risk", valueRange = c(0, 1.05)) %>%
    #     dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
    #     dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
    #   
    # })
    
    output$plotlytest <- renderPlotly({
      plot_ly() %>%
        add_trace(data = z, x = ~Date, y = ~value, split = ~cast, type = 'scatter', mode = 'lines+markers') %>%
        add_ribbons(data = z, x= ~Date, split = ~cast, ymin = ~Lwr, ymax = ~Upr, opacity=0.2)
      })
      
    output$ggtest <- renderPlot({
      ggplot(z, aes(x = Date, y = value, fill = type, group = cast)) + 
        geom_line() +
        geom_ribbon(aes(ymin = Lwr, ymax = Upr, x = Date), alpha = 0.3, fill="#3366FF") +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
        theme_classic() +
        theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5)) +
        ylim(0, 1) +
        ggtitle("White syndromes") +
        xlab("") +
        ylab("Disease risk")

      })

  })

}

shinyApp(ui, server)

# time series plot
# reef_pixels_data <- reefsDF2[, c("Date", "Prev", "Upr", "Lwr")]

# https://rstudio.github.io/dygraphs/gallery-upper-lower-bars.html
reefs <- reefsDF2[reefsDF2$ID == 500, ]
r2 <- reefs[,c("Prev", "PrevUpr", "PrevLwr", 
                   "Forecast1", "F1Upr", "F1Lwr",
                   "Forecast2", "F2Upr", "F2Lwr")]
reefs_ts <- xts(r2, order.by = reefs$Date)

dygraph(reefs_ts, "White syndromes") %>%
  dySeries(c("PrevLwr", "Prev", "PrevUpr"), label = "Nowcast") %>%
  dySeries(c("F1Lwr", "Forecast1", "F1Upr"), label = "Forecast1") %>%
  dySeries(c("F2Lwr", "Forecast2", "F2Upr"), label = "Forecast2") %>%
  dyAxis("y", label = "Disease risk", valueRange = c(0, 1.05)) %>%
  dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
  dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, colors = c("grey", "blue", "blue")) #%>%
  # dyRangeSelector(height = 20)

d1 <- "2021-01-30"
d2 <- "2021-02-22"
difftime(as.Date(d2, "%Y-%m-%d"), as.Date(d1, "%Y-%m-%d"))  

reefsDF$Date <- as.Date("2021-03-01", "%Y-%m-%d")

reefsDF2 <- reefsDF %>%
  group_by(ID) %>%
  complete(Date = seq.Date(Date - 16*7, Date, by="week"))

p <- data.frame()

for(i in 1:4){
  x <- reefsDF2
  x$type <- ifelse(x$Date < "2021-01-20", "Nowcast", "Forecast")
  if(i == 1){
    x$value <- rbeta(nrow(reefsDF2), 0.3, 1)
  } else {
    x$value <- ifelse(x$Date > "2021-01-20", rbeta(nrow(reefsDF2), 0.3, 1), NA)
  }
  x$Upr <- x$value + 0.1
  x$Upr <- ifelse(x$Upr > 1, 1, x$Upr)
  x$Lwr <- x$value - 0.1
  x$Lwr <- ifelse(x$Lwr < 0, 0, x$Lwr)
  x$cast <- i
  p <- rbind(p, x)
}

test <- subset(p, ID == 100)
# test <- subset(test, !is.na(test$value))
# library(ggplot2)

ggplot(test, aes(x = Date, y = value, fill = type, group = cast)) + 
  geom_line() +
  geom_ribbon(aes(ymin = Lwr, ymax = Upr, x = Date), alpha = 0.3, fill="#3366FF") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_classic() +
  theme(text = element_text(size = 14), plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1) +
  ggtitle("White syndromes") +
  xlab("") +
  ylab("Disease risk")

# library(plotly)
plot_ly() %>%
  add_trace(data = test, x = ~Date, y = ~value, split = ~cast, type = 'scatter', mode = 'lines+markers') %>%
  add_ribbons(data = test, x= ~Date, split = ~cast, ymin = ~Lwr, ymax = ~Upr, opacity=0.2)#, color = I("gray5"), line = list(color = 'rgba(7, 164, 181, 0.05)')) 
# line=list(color='#000000'), showlegend = FALSE#,
        add_ribbon(aes(ymin = Lwr, ymax = Upr, x = Date), alpha = 0.3, fill="#3366FF"))
