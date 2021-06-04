# create maps for Fore-C shiny app --------------------------------------------------------

# load libraries (and install if not already installed) 
if (!require("leaflet")) install.packages("leaflet"); library(leaflet)
# if (!require("leaflet.extras")) install.packages("leaflet.extras"); library(leaflet.extras) # need if using drawable polygons
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("viridis")) install.packages("viridis"); library(viridis)

# load polygon and data layers ------------------------------------------------------------
load("Compiled_data/historical_surveys.RData")
load("Compiled_data/spatial_grid.Rds")
load("Compiled_data/local_polygons.Rds")
load("Compiled_data/regional_polygons.Rds")

# function to add scale bar that adjusts with zoom ---------------------------------------
addScaleBar = function(map,
                       position = c('topright', 'bottomright', 'bottomleft', 'topleft'),
                       options = scaleBarOptions()) {
  
  options = c(options, list(position = match.arg(position)))
  invokeMethod(map, getMapData(map), 'addScaleBar', options)
}

scaleBarOptions = function(maxWidth = 100, metric = TRUE, imperial = TRUE,
                           updateWhenIdle = TRUE) {
  list(maxWidth=maxWidth, metric=metric, imperial=imperial,
       updateWhenIdle=updateWhenIdle)
}

# set colors for different polygon layers -------------------------------------------------
# bins <- c(0, 0.05, 0.10, 0.15, 0.25, 0.50, 0.75, 1.0)
bins <- c(0, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0)
cols <- c("white", "#FFFABF", "#FFDBA4", "#FF9E6E", "#FF603B", "red")
pal <- colorBin(cols, domain = reefs2$drisk, bins = bins, na.color = "transparent")

legendLabels <- c("0-5", "6-10", "11-15", "16-25", "26-50", "51-75", "76-100", "NA")
region_colors <- brewer.pal(length(region_poly), "Dark2")
mpa_colors <- viridis_pal(option = "A")(length(mpa_poly$Type))

# create landing page map of near-term forecasts ------------------------------------------
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
  addPolygons(data = mpa_poly,
              group = "Local management zones forecasts",
              layerId = ~ID,
              color = mpa_colors,
              highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
  ) %>%
  addPolygons(data = region_poly,
              group = "Regional (management zone) forecasts",
              layerId = ~ID,
              color = region_colors,
              highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
  ) %>%
  addLayersControl(
    overlayGroups = c("Local forecasts", "Local management zones forecasts", "Regional (management zone) forecasts"),
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE), # icon versus buttons with text
    position = c("bottomright")) %>%
  hideGroup(c("Local management zones forecasts", "Regional (management zone) forecasts")) %>%
  leaflet::addLegend("bottomright", pal = pal, values = reefs2$drisk,
                     title = "Disease risk (%)",
                     labFormat = function(type, cuts, p) {  # Here's the trick
                       paste0(legendLabels) }) 

# create map for scenarios page ------------------------------------------------------------
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
              highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)
  ) %>%
  leaflet::addLegend("bottomright", pal = pal, values = reefs2$drisk,
                     title = "Disease risk (%)",
                     labFormat = function(type, cuts, p) {  # Here's the trick
                       paste0(legendLabels) }) 

# create map of historical disease surveys -------------------------------------------------
historicalMap = leaflet() %>%
  addTiles(urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
  addCircleMarkers(data = historical_data, 
                   lat = ~Latitude, 
                   lng = ~Longitude, 
                   radius = ~sqrt(N), 
                   color = ~'white', 
                   popup = ~survey_text, 
                   clusterOptions = markerClusterOptions()) %>%
  addScaleBar() %>%
  setView(lng = -180, lat = 16.4502 , zoom = 3)
