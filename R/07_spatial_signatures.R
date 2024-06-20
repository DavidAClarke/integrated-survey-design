################################################################################
## Script name: 00_spatial_signatures.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Terrain----
if(!exists("terr_stack")){
  terr_var <- c("BH_REMA_8M.tif",
              "slope_bh.tif",
              "aspect_bh.tif",
              "roughness_bh.tif",
              "flowdir_bh.tif",
              "tpi_bh.tif",
              "tri_bh.tif")
  varpath <- here(shared_data, "environmental", "rema")
  vardir <- paste(varpath, terr_var, sep = "/")
  terr_stack <- rast(vardir)
  names(terr_stack) <- c("Elevation",
                       "Slope",
                       "Aspect",
                       "Roughness",
                       "Flow direction",
                       "TPI",
                       "TRI")
  slope <- terr_stack[[2]]
  slope[slope > 30] <- NA
  terr_stack[[2]] <- slope
  rm(slope)
  terr_stack <- crop(terr_stack, ant_coast_bh)
  terr_stack <- ENMTools::check.env(terr_stack)
  terr_stack <- mask(terr_stack, ant_coast_bh)
  cat("\014")
} else {
    print("terr_stack already loaded")
}

# Single-layer SpatRaster with terrain classification units
terr_cu <- rast(here(pth, "terrain.tif"))
terr_cu <- resample(terr_cu, terr_stack, method = "near")

# Automatic selection of statistical distribution functions
terr_difun <- select_functions(cu.rast = terr_cu,
                               var.rast = terr_stack,
                               mode = "auto")

# Multi-layer SpatRaster of terrain variables and classification units
terr_all <- c(terr_stack, terr_cu)
# Ouput table from select_functions()
df <- terr_difun$distfun
# Predicted distribution functions for climatic variables
pred_time <- system.time(terr_pdif <- predict_functions(cuvar.rast = terr_all,
                                                        cu.ind = 8,
                                                        cu = df$Class.Unit,
                                                        vars = df$Variable,
                                                        dif = df$Dist.Func))

# Spatial signatures from predicted distribution functions
terr_sig <- signature(pdif.rast = terr_pdif,
                      inprex = paste(seq(1, 6), "_", sep = ""),
                      outname = paste("terrain_", seq(1, 6), sep = ""))

writeRaster(terr_sig, here(pth, "terr_sig.tif"))
################################################################################

################################################################################
## Climate----
# Multi-layer SpatRaster with climatic variables
if(!exists("clim_stack")){
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
  clim_stack <- crop(clim_stack, ant_coast_bh)
  clim_stack <- ENMTools::check.env(clim_stack_rs)
  clim_stack <- mask(clim_stack, ant_coast_bh)
  cat("\014")
} else {
  print("clim_stack already loaded")
}

# Single-layer SpatRaster with climatic classification units
clim_cu <- rast(here(pth, "climate.tif"))
clim_cu <- resample(clim_cu, clim_stack, method = "near")

# Automatic selection of statistical distribution functions
system.time(clim_difun <- select_functions(cu.rast = clim_cu,
                               var.rast = clim_stack,
                               mode = "auto"))
# ~3 seconds

# plot(c(clim_cu, clim_stack), col = hcl.colors(100, "Spectral"), nc = 4,
#      mar = c(1.5, 1.5, 1.5, 5)
# )
# knitr::kable(clim_difun$distfun, filter = "none", selection = "none")

# Multi-layer SpatRaster of climatic variables and classification units
clim_all <- c(clim_stack, clim_cu)
# Ouput table from select_functions()
df <- clim_difun$distfun
# Predicted distribution functions for climatic variables
pred_time <- system.time(clim_pdif <- predict_functions(cuvar.rast = clim_all,
                                                        cu.ind = 8,
                               cu = df$Class.Unit,
                               vars = df$Variable,
                               dif = df$Dist.Func))

plot(clim_pdif, col = hcl.colors(100, "Oslo", rev = TRUE), nc = 4,
     mar = c(1.5, 1.5, 1.5, 3.5)
)

# Spatial signatures from predicted distribution functions
clim_sig <- signature(pdif.rast = clim_pdif,
                      inprex = paste(seq(1, 5), "_", sep = ""),
                      outname = paste("climate_", seq(1, 5), sep = ""))

writeRaster(clim_sig, here(pth, "clim_sig.tif"))
################################################################################

################################################################################
## Soil----
if(!exists("soil_stack")){
  soil_var <- c("BH_cal_inter_back.tif",
                "BH_chl_inter_back.tif",
                "BH_grain_inter_pred.tif",
                "BH_mag_inter_back.tif",
                "BH_sedcon_inter_back.tif",
                "BH_sod_inter_back.tif")
  varpath <- here(shared_data, "environmental", "soil_interpolated")
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
  soil_stack <- crop(soil_stack, ant_coast_bh)
  soil_stack <- ENMTools::check.env(soil_stack)
  soil_stack <- mask(soil_stack, ant_coast_bh)
  cat("\014")
} else {
  print("soil_stack already loaded")
}

soil_cu <- rast(here(pth, "soil.tif"))
soil_cu <- resample(soil_cu, soil_stack, method = "near")

# Automatic selection of statistical distribution functions
system.time(soil_difun <- select_functions(cu.rast = soil_cu,
                                           var.rast = soil_stack,
                                           mode = "auto"))

# Multi-layer SpatRaster of soil variables and classification units
soil_all <- c(soil_stack, soil_cu)
# Ouput table from select_functions()
df <- soil_difun$distfun
# Predicted distribution functions for soil variables
pred_time <- system.time(soil_pdif <- predict_functions(cuvar.rast = soil_all,
                                                        cu.ind = 8,
                                                        cu = df$Class.Unit,
                                                        vars = df$Variable,
                                                        dif = df$Dist.Func))

# Spatial signatures from predicted distribution functions
soil_sig <- signature(pdif.rast = soil_pdif,
                      inprex = paste(seq(1, 6), "_", sep = ""),
                      outname = paste("soil_", seq(1, 6), sep = ""))
writeRaster(soil_sig, here(pth, "soil_sig.tif"))

################################################################################

################################################################################
## Space----
space_cu <- resample(space_cu, space_stack, method = "near")

# Automatic selection of statistical distribution functions
system.time(space_difun <- select_functions(cu.rast = space_cu,
                                           var.rast = space_stack,
                                           mode = "auto"))
# Multi-layer SpatRaster of climatic variables and classification units
space_all <- c(space_stack, space_cu)
# Ouput table from select_functions()
df <- space_difun$distfun
# Predicted distribution functions for climatic variables
pred_time <- system.time(space_pdif <- predict_functions(cuvar.rast = space_all,
                                                        cu.ind = 3,
                                                        cu = df$Class.Unit,
                                                        vars = df$Variable,
                                                        dif = df$Dist.Func))

# Spatial signatures from predicted distribution functions
space_sig <- signature(pdif.rast = space_pdif,
                      inprex = paste(seq(1, 4), "_", sep = ""),
                      outname = paste("space_", seq(1, 6), sep = ""))

writeRaster(space_sig, here(pth, "space_sig.tif"))
