# Extract wave energy data ---------------------------------------------------------------

# load libraries
library(ncdf4)
library(raster)

# load wave energy data
wave_mean_nc <- brick("Data/sesync_wave_energy_data/msec_wave_mean.nc")

# survey data ----------------------------------------------------------------------------
# load survey data
load("Compiled_data/Survey_points.RData")

# extract wave energy data by survey points
survey_wave_mean <- extract(wave_mean_nc, cbind(surveys$Longitude, surveys$Latitude))

# add wave energy data to surveys
surveys$wave_mean <- survey_wave_mean

# rename and save data
wave_energy <- surveys
save(wave_energy, file = "Compiled_data/surveys_with_wave_energy.RData")

# grid data ----------------------------------------------------------------------------
# load survey data
load("Compiled_data/grid.RData")

# extract wave energy data by survey points
survey_wave_mean <- extract(wave_mean_nc, cbind(reefsDF$Longitude, reefsDF$Latitude))

# add wave energy data to surveys
reefsDF$wave_mean <- survey_wave_mean

# rename and save data
wave_energy <- reefsDF
save(wave_energy, file = "Compiled_data/grid_with_wave_energy.RData")
