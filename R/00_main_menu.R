################################################################################
## Script name: 00_main_menu
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Clear environment
rm(list = ls())

## Load required libraries
pkgs <- c("sf","tidyverse","here","terra","rassta", "rgbif", "vroom", "readxl",
          "terrainr", "ENMTools","gstat","automap","intamap","RColorBrewer",
          "janitor")
lapply(pkgs, require, character.only = T)

############################### File paths #####################################
shared_data <- "C:/Users/dcla0021/Documents/postdoc/projects/shared_data"
pth <- "C:/Users/dcla0021/Documents/postdoc/projects/sampling_plan/data"

## User defined functions
source("R/01_functions.R")

## Prepare soil data
source("R/02_soil_data.R")

## Interpolate soil data
source("R/03_interpolate_soil_data.R")

## Data preparation for stratification
source("R/04_data_preparation.R")

## Create classification units
source("R/05_classification_units.R")

## Create stratification units
source("R/06_stratification_units.R")

## Create spatial signatures
source("R/07_spatial_signatures.R")

## Create landscape similarity
source("R/08_landscape_similarity.R")

## Find plot locations and samples within, following constraints
source("R/09_plot_locations.R")

