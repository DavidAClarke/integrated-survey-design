################################################################################
## Script name: 00_interpolate_soil_data.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Transform coast to UTM
ant_coast_bh_utm <- st_transform(ant_coast_bh, 32747)

## Load and tidy soil data from Gore and Leishman 2020
#source("R/00_soil_data.R")

## Load tidied and spatially explicit soil data
salts <- st_read(here(pth, "environmental", "bh_salts.shp"))
sedex <- st_read(here(pth, "environmental", "bh_sedex.shp"))
weather <- st_read(here(pth, "environmental", "bh_weather.shp"))
grain <- st_read(here(pth, "environmental", "bh_grain.shp"))

bh_coast <- ant_coast_bh_utm %>%

  st_crop(st_bbox(st_buffer(salts, 4000)))

## Make a grid for use in interpolation
grid <- st_make_grid(bh_coast, cellsize = 100, what = "centers") %>%

  st_intersection(bh_coast) %>%

  as_Spatial()

## Following is required for universal kriging
grid.df <- as(grid, "SpatialPointsDataFrame")
# length is number of features
grid.df@data <- data.frame(SampleID = c(seq(1:length(grid.df@coords[,1]))),
                           mineral = c(seq(1:length(grid.df@coords[,1]))),
                           amount = c(seq(1:length(grid.df@coords[,1]))),
                           units = c(seq(1:length(grid.df@coords[,1]))),
                           logamount = c(seq(1:length(grid.df@coords[,1]))))

########################### Sediment conductivity ##############################
SedEx.sp <- as_Spatial(sedex)
SedEx.sp[297,3] <- 0 #change NA to zero
SedEx.sp@data <- data.frame(SedEx.sp@data,
                            logamount = log(SedEx.sp@data$amount),
                            asinhamount = asinh((SedEx.sp@data$amount)),
                            sqrtamount = sqrt(SedEx.sp@data$amount))

con.sp <- SedEx.sp[SedEx.sp$mineral == "sedimentConductivity",]
# con.sp[is.na(con.sp$distance)] <- 0
con.sp <- subset(con.sp, amount > 0)

# below is required if not using st_jitter()
zd <- zerodist(con.sp)
con.sp <- con.sp[-zd[,2], ]

# Check anisotropy
intamap::estimateAnisotropy(con.sp,
                            depVar = "logamount",
                            formula = "logamount ~ coords.x1+coords.x2")

# no need to account for anisotropy
vario.fit.1 <- automap::autofitVariogram(logamount~coords.x1+coords.x2,
                                         con.sp)

g1 <- gstat(formula = logamount~coords.x1+coords.x2,
            model = vario.fit.1$var_model,
            data = con.sp)

z1 <- predict(g1, grid.df)
z1@data[,3] <- exp(z1@data[,1])
colnames(z1@data)[3] <- "back.trans"

# Cross-validation
cv1 <- gstat.cv(g1)

# Root Mean Square Error (RMSE)
rmse1 <- sqrt(sum((cv1$var1.pred - cv1$observed)^2) / nrow(cv1))
b1 <- bubble(cv1[, "residual"],
             maxsize = 2,
             main = paste0("Sediment conductivity residuals"," (RMSE = ",
                           round(rmse1,2),")"))
hist(cv1[,"residual"]@data$residual)

## now to rasterize
ref_rast <- rast(ext = ext(vect(grid)),
                 resolution = c(100,100),
                 crs = "epsg:32747")

con_krig_rs_z <- terra::rasterize(vect(z1),
                                  ref_rast,
                                  field = "var1.pred")

con_var_krig_rs_z <- terra::rasterize(vect(z1),
                                      ref_rast,
                                      field = "var1.var")

con_back_krig_rs_z <- terra::rasterize(vect(z1),
                                       ref_rast,
                                       field = "back.trans")

# writeRaster(con_krig_rs_z,
#             filename = file.path(pth,
#                                  "environmental",
#                                  "BH_sedcon_inter_pred.tif"),
#             overwrite = T)
#
# writeRaster(con_var_krig_rs_z,
#             filename = file.path(pth,
#                                  "environmental",
#                                  "BH_sedcon_inter_var.tif"),
#             overwrite = T)
#
# writeRaster(con_back_krig_rs_z,
#             filename = file.path(pth,
#                                  "environmental",
#                                  "BH_sedcon_inter_back.tif"),
#             overwrite = T)

############################### Chloride #######################################
