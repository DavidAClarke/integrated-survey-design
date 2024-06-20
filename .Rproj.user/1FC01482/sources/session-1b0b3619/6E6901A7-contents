similarity <- function(su.rast, su.code, sig.rast, fun = mean, to.disk = FALSE,
                       outdir = ".", prefix = "su_", extension = ".tif",
                       overwrite = FALSE, ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- j <- rw <- NULL
  #--------------------------------------------------------------#

  # Variable = landscape factor or factor scale

  # Split numeric codes according to user-define code structure
  ## List of numeric codes for stratification units
  su.list <- base::unlist(base::as.list(terra::unique(su.rast)))
  ## Recursive splitting
  `%do%` <- foreach::`%do%`
  `%:%` <- foreach::`%:%`
  x <- foreach::foreach(i = su.list) %:%
    foreach::foreach(j = 1:base::length(su.code)) %do% {

      # Split digits according to beginning/end of digits for single variable
      x <- stringi::stri_sub(i, su.code[[j]][[1]], su.code[[j]][[2]])
      x <- base::as.numeric(x)
      base::list(stats::setNames(x, base::names(su.code)[j]))

    }

  # Format numeric codes into data frame
  ## flatten nested list into named vector
  x <- base::unlist(x)
  ## Stacking named vector into data frame
  x <- utils::stack(x)
  ## Renaming columns
  base::colnames(x) <- c("value", "variable")
  ## Creation of ID column for stratification units
  x$id <- base::rep(
    1:(base::nrow(x)/base::length(su.code)),
    each = base::length(su.code)
  )
  ## Reshaping data frame with variables as new columns
  x <- stats::reshape(x, idvar = "id", timevar = "variable", direction = "wide")
  ## Renaming variable columns according user-defined code structure
  base::colnames(x)[-1] <- base::names(su.code)
  ## Resetting row numbers
  base::rownames(x) = base::seq(length = base::nrow(x))
  ## Adding column of codes for stratification unit
  x$SU <- su.list
  ## Removing ID
  x <- x[,-1]

  # Define processing scheme (disk [parallel] VS memory [sequential])
  `%ps%` <- if(to.disk == TRUE) { foreach::`%dopar%` } else { foreach::`%do%` }

  # Aggregation of spatial signatures into landscape similarity
  landsim <- foreach::foreach(rw = 1:base::nrow(x)) %ps% {

    # Base raster to serve as reference for multi-layer SpatRaster
    spatsign <- terra::rast()

    # Sequential construction of multi-layer SpatRaster of spatial signatures
    # ... for each stratification unit
    `%do%` <- foreach::`%do%`
    foreach::foreach(c = 1:base::length(su.code)) %do% {

      # Find spatial signature for classification unit in current iteration
      ## Search pattern
      sptsgn <- base::paste(
        "^", base::colnames(x[c]), "_", x[rw,c], "$", sep = ""
      )
      ## Find spatial signature
      sptsgn <- base::grep(
        pattern = sptsgn, base::names(sig.rast), value = TRUE
      )

      ##### I ADDED THE IF STATEMENT SO THAT IT WORKED FOR ME ######

      # Add spatial signature for classification unit in current iteration
      if(!purrr::is_empty(sptsgn)){
      terra::add(spatsign) <- sig.rast[[sptsgn]]}

    }

    # Name for raster layer of landscape similarity
    ## layer name (from column with numeric codes of stratification units SU)
    lname <- base::as.character(x[rw, -c(1:base::length(su.code))])
    ## File name
    rname <- base::paste(prefix, lname, extension, sep = "")

    # > Disk-based writing < #
    if(to.disk == TRUE) {

      # Aggregation of spatial signatures into landscape similarity
      landsim <- terra::app(
        spatsign,
        fun = fun,
        na.rm = TRUE,
        filename = base::file.path(outdir, rname),
        overwrite = overwrite,
        wopt = base::list(names = lname, ...)
      )
      gc()

      # Retrieve file name for raster layer of landscape similarity
      landsim <- rname

      # > Memory-based writing < #
    } else {

      # Aggregation of spatial signatures into landscape similarity
      landsim <- terra::app(spatsign, fun = fun, na.rm = TRUE)
      gc()

      # Rename and retrieve raster layer of landscape similarity
      base::names(landsim) <- lname
      terra::varnames(landsim) <- lname
      landsim
    }

  }

  if(to.disk == TRUE) {

    # Retrieve raster layers of landscape similarity from disk
    landsim <- base::unlist(landsim)
    landsim <- base::paste("^", landsim, "$", sep = "")
    landsim <- base::lapply(
      landsim, list.files, path = outdir, full.names = TRUE
    )
    landsim <- terra::rast(base::unlist(landsim))

    # Final objects
    list(landsim = landsim, codes = x)

  } else {

    # Retrieve raster layers of landscape similarity from memory
    landsim <- terra::rast(landsim)

    # Final objects
    list(landsim = landsim, codes = x)

  }

}
