# create grid for fore-c map based on CRW virtual station pixels ---------------------
rm(list=ls()) #remove previous variable assignments

# Great Barrier Reef, Australia
gbr <- expand.grid("Longitude"=seq(140.5,155.5,1), "Latitude"=seq(-26.5,-9.5,1))

# Hawaii
hawaii <- expand.grid("Longitude"=seq(-178.5,-153.5,1), "Latitude"=seq(16.5,37.5,1))

# Howland and Baker Atolls
howland_baker <- expand.grid("Longitude"=seq(-178.5,-174.5,1), "Latitude"=seq(-1.5,2.5,1))

# Johnston Atoll
johnston <- expand.grid("Longitude"=seq(-170.5,-168.5,1), "Latitude"=seq(15.5,17.5,1))

# Marianas
marianas <- expand.grid("Longitude"=seq(140.5,150.5,1), "Latitude"=seq(11.5,21.5,1))

# Samoas
samoas <- expand.grid("Longitude"=seq(-174.5,-166.5,1), "Latitude"=seq(-16.5,-8.5,1))

# Wake Atoll
wake <- expand.grid("Longitude"=seq(165.5,168.5,1), "Latitude"=seq(17.5,21.5,1))

# combine grids
grid <- do.call("rbind", list(gbr, hawaii, howland_baker, johnston, marianas, samoas, wake))

# save grid
save(grid, file="Compiled_data/grid.RData")
