################################################################################
## Script name: 00_stratification_units.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

# Load classification unit rasters
terr_cu <- rast(here(pth, "terrain.tif"))
clim_cu <- rast(here(pth, "climate.tif"))
soil_cu <- rast(here(pth, "soil.tif"))
geo_cu <- rast(here(pth, "geology.tif"))
space_cu <- rast(here(pth, "space.tif"))
clim_res <- resample(clim_cu, terr_cu, method = "near")
soil_res <- resample(soil_cu, terr_cu, method = "near")
space_res <- resample(space_cu, terr_cu, method = "near")
all_cu <- c(terr_cu, clim_res, soil_res, space_res)
names(all_cu) <- c("Terrain",
                   "Climate",
                   "Soil",
                   "Space")

# Create stratification units
su <- strata(cu.rast = all_cu)
su_rast <- su$su.rast

plot(su_rast,
     type = "classes",
     col = hcl.colors(length(unique(su_rast)[,1]), "spectral"),
     plg = list(ncol = 4), mar = c(1.5, 1.5, 1.5, 12))

writeRaster(su_rast,
            here(pth, "stratification_units_incspac.tif"),
            overwrite = T)

strat <- rast(here("data", "sampling_design", "stratification_units_incspac.tif"))

