################################################################################
## Script name: 00_plot_locations.R
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Stratification units
su <- rast(here("data", "sampling_design", "stratification_units.tif"))

## Polygonize stratification
su_vec <- st_as_sf(as.polygons(su, trunc = F, dissolve = T))
st_crs(su_vec) <- 3031
new_site_pts <- st_as_sf(st_sample(su_vec, 168, by_polygon = T))

## Loop over sites
site.list <- list()

for(i in 1:1){

  plots_samples <- find_samples(new_site_pts[i,], ant_coast_bh, nsim = 3,
                                snum = i,crs = 4326, buff = 100)
  site.list[[i]] <- plots_samples
}
