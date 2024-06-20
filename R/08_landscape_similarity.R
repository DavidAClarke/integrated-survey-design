################################################################################
## Script name: 00_landscape_similarity.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

# Multi-layer spatraster with spatial signatures of classification units
terr_sig <- rast(here(pth, "terr_sig.tif"))
clim_sig <- rast(here(pth, "clim_sig.tif"))
soil_sig <- rast(here(pth, "soil_sig.tif"))

# Single layer
su <- rast(here(pth, "stratification_units_incspac.tif"))
su <- resample(su, terr_sig, method = "near")

clim_sig <- resample(clim_sig, terr_sig)
soil_sig <- resample(soil_sig, terr_sig)
space_sig <- resample(space_sig, terr_sig)

sig.rast <- c(terr_sig, clim_sig, soil_sig, space_sig)
su.code <- list(terrain = c(1,1),
                climate = c(2,2),
                soil = c(3,3),
                space = c(4,4))

source("R/similarity.R")
# Landscape similarity to stratification units
su_ls <- similarity(su.rast = su,
                    sig.rast = sig.rast,
                    su.code = su.code,
                    fun = mean,
                    to.disk = T,
                    outdir = pth,
                    overwrite = T)

plot(c(su_ls$landsim[[1]], sig.rast[[1]], sig.rast[[7]], sig.rast[[12]]),
     col = hcl.colors(100, "Oslo", rev = T), nc = 2, mar = c(1.5, 1.5, 1.5, 3.5))
