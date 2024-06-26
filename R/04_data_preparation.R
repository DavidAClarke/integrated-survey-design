################################################################################
## Script name: 01_data_preparation.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Load Bunger Hills coastline
ant_coast_bh <- st_read(here(shared_data,
                        "environmental",
                        "rock_outcrop_bunger_hills",
                        "Bunger_Hills_Polygon_Burton_Johnson2.shp"))

################################################################################
## Terrain variables----
# Load terrain variables
terr_var <- c("BH_REMA_8M.tif", "BH_slope.tif", "BH_aspect.tif", "BH_rough.tif",
              "BH_flow.tif", "BH_TPI.tif", "BH_TRI.tif")
varpath <- here(shared_data, "environmental", "rema")
vardir <- paste(varpath, terr_var, sep = "/")
terr_stack <- rast(vardir)
names(terr_stack) <- c("Elevation", "Slope", "Aspect", "Roughness",
                       "Flow direction", "TPI", "TRI")

# Remove undesirable slope values
slope <- terr_stack[[2]]
slope[slope > 30] <- NA
terr_stack[[2]] <- slope
rm(slope)

# Crop, scale and mask
terr_stack <- crop(terr_stack, ant_coast_bh)
#terr_stack_rs <- raster::stack(terr_stack)
terr_stack_rs <- ENMTools::check.env(terr_stack_rs)
#terr_stack_rs <- rast(terr_stack_rs)
terr_stack_sc <- terra::scale(terr_stack_rs)
terr_stack_sc <- resample(terr_stack_sc, terr_stack)
terr_stack_sc <- mask(terr_stack_sc, ant_coast_bh)
rm(terr_stack_rs)
################################################################################

################################################################################
## Climate variables----
clim_var <- c("mean_w.speed_100.tif",
              "mean_w.dir_100.tif",
              "mean_snowcov_100.tif",
              "mean_soil_100.tif",
              "mean_solrad_100.tif",
              "mean_snowmlt_100.tif",
              "mean_temp_100.tif")
varpath <- here(shared_data, "environmental", "era5")
vardir <- paste(varpath, clim_var, sep = "/")
clim_stack <- rast(vardir)[[c(1,3,5,7,9,11,13)]] #values not se's
names(clim_stack) <- c("Wind speed",
                       "Wind direction",
                       "Snowcover",
                       "Soil temperature",
                       "Solar radiation",
                       "Snowmelt",
                       "Temperature")

# Crop, scale and mask
clim_stack <- crop(clim_stack, ant_coast_bh)
#clim_stack_rs <- raster::stack(clim_stack)
clim_stack_rs <- ENMTools::check.env(clim_stack_rs)
#clim_stack_rs <- rast(clim_stack_rs)
clim_stack_sc <- terra::scale(clim_stack)
clim_stack_sc <- resample(clim_stack_sc, clim_stack)
clim_stack_sc <- mask(clim_stack_sc, ant_coast_bh)
rm(clim_stack_rs)
################################################################################

################################################################################
## Geology/Soil variables----
soil_var <- c("BH_cal_inter_back.tif",
              "BH_chl_inter_back.tif",
              "BH_grain_inter_pred.tif",
              "BH_mag_inter_back.tif",
              "BH_sedcon_inter_back.tif",
              "BH_sod_inter_back.tif")
varpath <- here(shared_data, "environmental", "soil")
vardir <- paste(varpath, soil_var, sep = "/")
soil_stack <- rast(vardir)
names(soil_stack) <- c("Calcium",
                       "Chloride",
                       "Grain size",
                       "Magnesium",
                       "Conductivity",
                       "Sodium")
pot <- rast(here(varpath, "BH_pot_inter_back.tif"))
pot <- resample(pot, soil_stack)
soil_stack <- c(soil_stack, pot)
names(soil_stack)[7] <- "Potassium"
soil_stack <- project(soil_stack, y = crs(ant_coast_bh), res = 100)
#soil_stack_rs <- raster::stack(soil_stack)
soil_stack_rs <- ENMTools::check.env(soil_stack_rs)
#soil_stack_rs <- rast(soil_stack_rs)
soil_stack_sc <- terra::scale(soil_stack)
soil_stack_sc <- mask(soil_stack_sc, ant_coast_bh)
soil_stack_sc <- crop(soil_stack_sc, clim_stack_sc)
rm(soil_stack_rs, pot)
################################################################################

################################################################################
## Spatial variables----
longitude <- init(soil_stack_sc, "x")
latitude <- init(soil_stack_sc, "y")
space_stack <- c(longitude, latitude)
names(space_stack) <- c("Longitude", "Latitude")
space_stack_sc <- terra::scale(space_stack)
space_stack_sc <- mask(space_stack_sc, ant_coast_bh)
rm(longitude, latitude)
################################################################################

ext(clim_stack_sc) <- ext(terr_stack_sc) <- ext(soil_stack_sc)
gc()
cat("\014")
