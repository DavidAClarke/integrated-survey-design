################################################################################
## Script name: 00_main_menu
## Author: David Clarke
## Copyright (c) David Clarke, 2024
## Email: david_anthony_clarke@hotmail.com
################################################################################

## Clear environment
rm(list = ls())

## Load required libraries
pkgs <- c("sf","tidyverse","here","terra","rassta", "rgbif", "vroom",
          "RColorBrewer", "readxl", "terrainr", "ENMTools")
lapply(pkgs, require, character.only = T)

############################### File paths #####################################
shared_data <- "C:/Users/dcla0021/Documents/postdoc/projects/shared_data"

## User defined functions
source("R/01_functions.R")
