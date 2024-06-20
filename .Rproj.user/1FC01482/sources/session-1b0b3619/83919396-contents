################################################################################
## Script name: 00_classification_units.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

# Set seed
##important as the SOM results change each time i.e. number of groups/clusters
set.seed(153)

################################################################################
## Terrain----
# Dimension reduction and estimation of k
runtime <- system.time(terr_som <- som_gap(terr_stack_sc,
                    xdim = 9,
                    ydim = 9,
                    K.max = 8))
#~30mins

# Rasterisation of terrain SOM grid and terrain PAM clustering
terr_pam <- som_pam(ref.rast = terr_stack_sc,
                    kohsom = terr_som$SOM,
                    k = 6)#terr_som$Kopt

writeRaster(terr_pam$sompam.rast[[1]], here(pth, "terrain_som.tif"))
writeRaster(terr_pam$sompam.rast[[2]], here(pth, "terrain.tif"))
################################################################################

################################################################################
## Climate----
# Dimension reduction and estimation of k
runtime <- system.time(clim_som <- som_gap(clim_stack_sc,
                                           xdim = 9,
                                           ydim = 9,
                                           K.max = 6))
plot(clim_som$SOMgap)
abline(v = clim_som$Kopt)

# Rasterisation of terrain SOM grid and terrain PAM clustering
clim_pam <- som_pam(ref.rast = clim_stack_sc,
                    kohsom = clim_som$SOM,
                    k = clim_som$Kopt)

writeRaster(clim_pam$sompam.rast[[1]], here(pth, "climate_som.tif"))
writeRaster(clim_pam$sompam.rast[[2]], here(pth, "climate.tif"))
################################################################################

################################################################################
## Soil----
# Dimension reduction and estimation of k
runtime <- system.time(soil_som <- som_gap(soil_stack_sc,
                                           xdim = 9,
                                           ydim = 9,
                                           K.max = 6))

plot(soil_som$SOMgap)
abline(v = soil_som$Kopt)

# Rasterisation of terrain SOM grid and terrain PAM clustering
soil_pam <- som_pam(ref.rast = soil_stack_sc,
                    kohsom = soil_som$SOM,
                    k = soil_som$Kopt)

writeRaster(soil_pam$sompam.rast[[1]], here(pth, "soil_som.tif"))
writeRaster(soil_pam$sompam.rast[[2]], here(pth, "soil.tif"))
################################################################################

################################################################################
## Space----
runtime <- system.time(space_som <- som_gap(space_stack_sc,
                                           xdim = 9,
                                           ydim = 9,
                                           K.max = 6))

plot(space_som$SOMgap)
abline(v = space_som$Kopt)

# Rasterisation of terrain SOM grid and terrain PAM clustering
space_pam <- som_pam(ref.rast = space_stack_sc,
                    kohsom = space_som$SOM,
                    k = 6) #was 4
writeRaster(space_pam$sompam.rast[[1]], here(pth, "space_som.tif"))
writeRaster(space_pam$sompam.rast[[2]], here(pth, "space.tif"))
