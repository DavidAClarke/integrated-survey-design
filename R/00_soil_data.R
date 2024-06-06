#Sediment data
GoreLeishmanSalt <- read_excel(here(pth, "environmental",
                                    "BH_Gore_Leishman_SaltSeds_28Dec2019.xlsx"),
                               sheet = "Salt")

GoreLeishmanSedEx <- read_excel(here(pth, "environmental",
                                    "BH_Gore_Leishman_SaltSeds_28Dec2019.xlsx"),
                                sheet = "Sediment extracts")

GoreLeishmanWeather <- read_excel(here(pth, "environmental",
                                    "BH_Gore_Leishman_SaltSeds_28Dec2019.xlsx"),
                                  sheet = "Weathering criteria")

GoreLeishmanGrain <- read_excel(here(pth, "environmental",
                                    "BH_Gore_Leishman_SaltSeds_28Dec2019.xlsx"),
                                sheet = "Sediment grain size statistics")

###Tidy data up
##Salt
BH_Salts <- GoreLeishmanSalt %>%

  dplyr::filter(!row_number() %in% 1) %>%

  dplyr::select(!c(...8, ...9, Northings...7)) %>%

  rename(Halite = "Northings...3",
         Calcite = "Northings...4",
         Aragonite = "Northings...5",
         Gypsum = "Northings...6") %>%

  pivot_longer(!c(Eastings, Sample), names_to = "mineral",
               values_to = "Northings") %>%

  drop_na(Northings) %>%

  mutate(mineral = as.factor(mineral)) %>%

  relocate(mineral, .after = Sample) %>%

  mutate(Northings = as.numeric(Northings)) %>%

  mutate(Eastings = (Eastings + 5000) * 100) %>%

  mutate(Northings = (Northings + 26000) * 100) %>%

  mutate(mineralID = case_when(mineral == "Aragonite" ~ 1,
                               mineral == "Calcite" ~ 2,
                               mineral == "Gypsum" ~ 3,
                               mineral == "Halite" ~ 4))
rm(GoreLeishmanSalt)

#Convert to spatial
BH_Salts_sf <- BH_Salts %>%

  mutate_at(vars(Eastings, Northings), as.numeric) %>%

  st_as_sf(
    coords = c("Eastings", "Northings"),
    agr = "constant",
    crs = 32747,        # BH UTM EPSG
    stringsAsFactors = FALSE,
    remove = TRUE
  ) #%>%
  #st_jitter()
st_write(BH_Salts_sf, here(pth, "environmental", "bh_salts.shp"))
rm(BH_Salts)

##Sediment extracts
BH_SedEx <- GoreLeishmanSedEx %>%

  dplyr::filter(!row_number() %in% 1) %>%

  dplyr::select(...1, ...2, ...3, Sediment...5, Sediment...7, Sediment...9,
                Sediment...11, Sediment...13, Solution...14) %>%

  rename(SampleID = "...1", Eastings = "...2", Northings = "...3",
         Sodium = "Sediment...5", Magnesium = "Sediment...7",
         Chloride = "Sediment...9", Potassium = "Sediment...11",
         Calcium = "Sediment...13", sedimentConductivity = "Solution...14") %>%

  dplyr::filter(!SampleID == "Average") %>%

  mutate(across(everything(), ~str_replace_all(., "No data", "0"))) %>%

  pivot_longer(!c(Eastings, SampleID, Northings), names_to = "mineral",
               values_to = "amount") %>%

  mutate(units = case_when(mineral == "sedimentConductivity" ~ "µS/cm",
                           mineral != "sedimentConductivity" ~ "mg/kg")) %>%

  mutate(across(where(is.character) & !c(SampleID, mineral, units),
                as.numeric)) %>%

  mutate(Eastings = (Eastings + 5000) * 100) %>%

  mutate(Northings = (Northings + 26000) * 100)

rm(GoreLeishmanSedEx)

#Convert to spatial
BH_SedEx_sf <- BH_SedEx %>%

  mutate_at(vars(Eastings, Northings), as.numeric) %>%

  st_as_sf(
    coords = c("Eastings", "Northings"),
    agr = "constant",
    crs = 32747,        # BH UTM EPSG
    stringsAsFactors = FALSE,
    remove = TRUE
  ) #%>%
#st_jitter()
st_write(BH_SedEx_sf, here(pth, "environmental", "bh_sedex.shp"))
rm(BH_SedEx)

##Weathering criteria
BH_Weather <- GoreLeishmanWeather %>%

  dplyr::filter(!row_number() %in% c(1:2)) %>%

  janitor::row_to_names(1) %>%

  #janitor::clean_names() %>%
  dplyr::select(!c(`Grid cell`, `Sum of all weathering criteria`)) %>%

  mutate(across(where(is.character) & c(Easting, Northing), as.numeric)) %>%

  mutate(across(where(is.character), as.factor)) %>%

  pivot_longer(!c(Easting, Northing), names_to = "weatherType",
               values_to = "status") %>%

  mutate(weatherType = as.factor(weatherType)) %>%

  mutate(Easting = (Easting + 5000) * 100) %>%

  mutate(Northing = (Northing + 26000) * 100) %>%

  mutate(weatherTypeID = case_when(weatherType == "Frost cracks" ~ 1,
                                   weatherType == "Glacial polish" ~ 2,
                                   weatherType == "Orientated wind pits" ~ 3,
                                   weatherType == "Sand accumulations" ~ 4,
                                   weatherType == "Sea salt" ~ 5,
                                   weatherType == "Tafoni" ~ 6))
rm(GoreLeishmanWeather)

#Convert to spatial
BH_Weather_sf <- BH_Weather %>%

  mutate_at(vars(Easting, Northing), as.numeric) %>%

  st_as_sf(
    coords = c("Easting", "Northing"),
    agr = "constant",
    crs = 32747,        # BH UTM EPSG
    stringsAsFactors = FALSE,
    remove = TRUE
  ) #%>%
  #st_jitter()
st_write(BH_Weather_sf, here(pth, "environmental", "bh_weather.shp"))
rm(BH_Weather)

##Sediment grain size
BH_Grain <- GoreLeishmanGrain %>%

  dplyr::filter(!row_number() %in% 1) %>%

  janitor::row_to_names(1) %>%

  janitor::clean_names() %>%

  dplyr::select(c(3:4, 10:16)) %>%

  mutate(across(everything(), as.numeric)) %>%

  mutate(d16_mm = d16_mm/1000) %>%

  mutate(d5_mm = d5_mm/1000) %>%

  mutate(eastings = (eastings + 5000) * 100) %>%

  mutate(northings = (northings + 26000) * 100) %>%

  relocate(d5_mm, .after = d25_mm_2) %>%

  dplyr::mutate(across(!c(eastings, northings), ~replace_na(.x, 0))) %>%

  rowwise() %>%

  mutate(medGrainSize = median(c(d16_mm:d5_mm), na.rm = T)) %>%

  dplyr::select(eastings, northings, medGrainSize) %>%

  #pivot_longer(!c(eastings, northings), names_to = "grainType", values_to = "grainSize") %>%
  drop_na(eastings)

rm(GoreLeishmanGrain)

#Convert to spatial
BH_Grain_sf <- BH_Grain %>%

  mutate_at(vars(eastings, northings), as.numeric) %>%

  st_as_sf(
    coords = c("eastings", "northings"),
    agr = "constant",
    crs = 32747,        # BH UTM EPSG
    stringsAsFactors = FALSE,
    remove = TRUE
  ) #%>%
  #st_jitter()
st_write(BH_Grain_sf, here(pth, "environmental", "bh_grain.shp"))
rm(BH_Grain)

gc()
cat("\014") #clears the console screen
