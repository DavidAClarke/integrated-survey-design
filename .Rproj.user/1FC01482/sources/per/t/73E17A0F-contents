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

################################################################################
## Function name: find_samples----
# function args: x = site points, shp = topographic shapefile for intersection,
# nsim = number of simulations, snum = site number, crs (if not given sf object
# for sites), long/lat = coordinate variable names (if not given sf object for
# sites, ncrs = new crs for transforming sites, buff = buffer size around site)

find_samples <- function(x, shp, nsim = 3, snum, long, lat, crs, buff){

  if(nrow(x) != length(snum)) {stop("Number sites and site numbers must be equal")}

  if(class(x)[1] != "sf"){

    x <- st_as_sf(x, coords = c(long,lat), remove = F, crs = crs)

  }

  x <- st_transform(x, st_crs(shp))
  x.buff <- st_intersection(st_buffer(x, buff,nQuadSegs=1,
                                      endCapStyle = "SQUARE"),st_as_sf(shp))

  #set.seed(seed)

  # Plot locations
  sampled_points <- st_sample(
    x = x.buff,
    type = "SSI", #Simple Sequential Inhibition process from spatstat.random
    r = 100, # threshold distance (in metres)
    n = 3, # number of points
    nsim = nsim #number of simulated realisations
  )

  st_crs(sampled_points) <- st_crs(shp)

  if(nsim > 1){

    p <- ggplot() +
      #geom_sf(data = st_crop(st_as_sf(shp), st_buffer(x.buff, 500))) +
      geom_sf(data = x.buff) +
      geom_sf(data = x, size = 2, colour = "black") +
      geom_sf(data = sampled_points, size = 2, mapping = aes(colour = label)) +
      rcartocolor::scale_color_carto_d(name = "Simulation: ", palette = "Safe") +
      theme_void() +
      theme(plot.margin = margin(1,1,2,1, "cm"))

    p2 <- cowplot::ggdraw(p) +
      cowplot::draw_plot(
        {p +
            geom_sf(data = shp) +
            geom_sf(data = x, colour = "black") +
            theme(legend.position = "none")
        },
        x = 0.62,
        y = 0.55,
        # The width and height of the plot expressed as proportion of the entire ggdraw object
        width = 0.4,
        height = 0.4)


    plot(p2)

  }

  if(nsim > 1){

    sim <- readline("Which simulation do you want to use? Use simulation number ")

    plot.ctd <- sampled_points %>%
      filter(label == paste("Simulation",sim)) %>%
      st_cast("POINT") %>%
      mutate(label = paste0(paste0("S",snum),c("P1", "P2", "P3")))

  } else {

    plot.ctd <- sampled_points %>%
      mutate(label = paste0(paste0("S",snum),c("P1", "P2", "P3")))
  }

  # Sample locations
  plots <- st_buffer(plot.ctd, dist = 5, nQuadSegs=1,
                     endCapStyle = "SQUARE")

  plot.pts <- st_as_sf(st_sample(plots, c(3,3,3), by_polygon = T)) %>%
    mutate(site = paste0("S",snum)) %>%
    mutate(plot = c(rep("P1",3), rep("P2", 3), rep("P3",3))) %>%
    mutate(sample = rep(c("s1", "s2", "s3"), 3))

  return(list(x, plots, plot.pts))

}
################################################################################
