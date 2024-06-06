################################################################################
## Script name: sample_design_functions.R
## Author: David A. Clarke
################################################################################

################################################################################
## Function name: dist2coast----
#layer1 needs to be raster
#layer2 is for cropping (raster or vector)
dist2coast <- function(layer1, layer2){

  tmp <- crop(layer1, layer2)
  tmp <- terra::mask(tmp, terra::vect(layer2))

  if(length(tmp[is.na(tmp)]) == 0){

    tmp[tmp < 0] <- 9999

  } else

    if(length(tmp[is.na(tmp)]) != 0) {

      tmp[is.na(tmp)] <- 9999

    }

  tmp[tmp < 9999] <- NA

  dist <- distance(tmp)

}
################################################################################

################################################################################
## Function name: downscale----
downscale <- function(mod_list, fine_grid, to_disk){

  pred_list <- list()

  for(i in 1:length(mod_list)){

    print(paste("Downscaling", names(mod_list[i])))

    predtime <- system.time(pred <- predict(mod_list[[i]], fine_grid, se.fit = T))

    fine_field <- data.frame(long = fine_grid$long,
                             lat = fine_grid$lat,
                             zz = pred$fit,
                             se = pred$se.fit)

    pred_list[[i]] <- fine_field



    if(to_disk == TRUE){

      print(paste("writing", names(mod_list[i]), "raster to disk"))

      ras <- rast(pred_list[[i]],
                  type = "xyz",
                  crs = crs(pred_stack_fine))

      writeRaster(ras,
                  here("Data",
                       "Environmental",
                       "ERA5",
                       paste0("mean_",names(mod_list)[i],"_100.tif")),
                  overwrite = T)
    }
  }

  return(pred_list)
}
################################################################################

################################################################################
## Function name:shrinkIfPossible----
# Inward buffer
shrinkIfPossible <- function(shp, size){

  sg <- st_buffer(st_geometry(shp), -size)

  st_geometry(shp)[!st_is_empty(sg)] = sg[!st_is_empty(sg)]

  return(shp)

}
################################################################################

################################################################################
## Function name: spatially_explicit (Make general)----
spatially_explicit <- function(df, x, y, to_epsg, jit = 0){

  df_sf <- df %>%

    dplyr::mutate(dplyr::across(c(x, y), as.numeric)) %>% # coordinates must be numeric

    dplyr::filter(!is.na(!!rlang::sym(x))) %>%

    dplyr::filter(!is.na(!!rlang::sym(y))) %>%

    sf::st_as_sf(
      coords = c(x, y),
      agr = "constant",
      crs = to_epsg,
      stringsAsFactors = FALSE,
      remove = TRUE) %>%

    sf::st_jitter(amount = jit)

  return(df_sf)
}
################################################################################
