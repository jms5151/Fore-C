# Fore-C interactive explorer -----------------------------------------------------
rm(list=ls()) #remove previous variable assignments

# Useful hyperlinks: --------------------------------------------------------------
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
# https://rstudio-pubs-static.s3.amazonaws.com/307862_b8c8460272dc4a2a9023d033d5f3ec34.html # interactive polygons
# to use the css I need to download docs: https://shiny.rstudio.com/articles/css.html
# https://stackoverflow.com/questions/62544187/popupgraph-r-leaflet-why-are-my-popup-graphs-blank OR
# https://stackoverflow.com/questions/62642615/adding-reactive-popup-graphs-plots-to-a-leaflet-map-with-shiny-r
# create bounding box to select multiple pixels: https://redoakstrategic.com/geoshaper/
#### click id with multiple polygons:
# https://stackoverflow.com/questions/41104576/changing-styles-when-selecting-and-deselecting-multiple-polygons-with-leaflet-sh
# newer response
# https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in/65935636#65935636
##########

# load libraries (and install if not already installed) 
if (!require("shiny")) install.packages("shiny"); library(shiny)
if (!require("shinythemes")) install.packages("shinythemes"); library(shinythemes)
if (!require("flexdashboard")) install.packages("flexdashboard"); library(flexdashboard)
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
if (!require("shinyWidgets")) install.packages("shinyWidgets"); library(shinyWidgets)
if (!require("xts")) install.packages("xts"); library(xts)
if (!require("shinydashboard")) install.packages("shinydashboard"); library(plotly)
if (!require("shinycssloaders")) install.packages("shinycssloaders"); library(shinycssloaders)
if (!require("shinyBS")) install.packages("shinyBS"); library(shinyBS) # for hover text

# library(ggplot2)
# library(raster)
# library(rgdal)
# library(sf) 

# load data -----------------------------------------------------------------------
load("Compiled_data/grid.RData")
load("Compiled_data/simulated_data_for_plotlygraphs.RData")
load("Compiled_data/mitigation.RData")
load("Compiled_data/baseline.RData")
load("Compiled_data/pixels_in_regional_polygons.RData")
load("Compiled_data/simulated_data_for_regional_plotlygraphs.RData")
load("Compiled_data/simulated_data_for_local_plotlygraphs.RData")

# load functions, maps, plots, user interface and server for shiny app ------------
# load maps & mapping functions
source("codes/forec_shiny_app_maps.R")

# load plots and plotting functions
source("codes/forec_shiny_app_plots.R")

# load information text blocks and settings, must be loaded before sourcing UI and server
source("codes/forec_shiny_app_info_text_and_settings.R")

# load user interface
source("codes/forec_shiny_app_user_interface.R")

# load server
source("codes/forec_shiny_app_server.R")

# run shiny app -------------------------------------------------------------------
shinyApp(ui, server)
