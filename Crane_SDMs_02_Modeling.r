# This script generates pseudoabsences and runs models (preliminary)

# Torch needs to be installed and loaded seperately because we have to carefully specify the cuda version
# that's compatible with the version of Nvidia toolkit I installed (12.2; should work with my 4060ti)
# Thankfully, we can set an environment variable for that
# Edit: I give up on getting the GPU version of this to work; final issue came down to working with predict()
# Resolved version compatibility issues by install the github version
#TODO redo torch stuff python someday?
#Sys.setenv(CUDA = "12.2")

# libraries
# Install torch from github (commit 3f1bb59)
#remotes::install_github("mlverse/torch")
library(torch)

# automatically download uninstalled packages from the registry
libNames <- c('dtplyr', 'data.table', 'tidyverse', 'ggthemes', 'devtools', 'terra', 'basemaps', 'geodata', 'tidyterra', 'ggspatial', 'cowplot', 'usdm', 'ENMeval', 'tabnet', 'tidymodels', 'finetune', 'vip', 'magick')
for(i in 1:length(libNames)){
    if (!libNames[i] %in% rownames(installed.packages())){
        install.packages(libNames[i], dependencies = TRUE)
    }
    library(libNames[i], character.only=TRUE)
}
# Install predicts ("dismo but uses terra") from rspatial
#install.packages('predicts', repos='https://rspatial.r-universe.dev')
library(predicts)

# Set working directory
setwd("H:/Crane_SDMs")

# SET JAVA PARAMS *~*~*~*~*~*~*~*~*~*~*~*~*~
# The predicts package calls MaxEnt using rjava; can modify JVM params before it gets started
# It's unclear when the JVM initializes when calling MaxEnt, so set these at the beginning of the script
options(java.parameters = "-Xmx16g")

# Read occs
GruidaeAllOcc_before2000_spacRare10km <- fread("GruidaeAllOcc_before2000_spacRare10km.csv")
GruidaeAllOcc_2000_2018_spacRare10km <- fread("GruidaeAllOcc_2000_2018_spacRare10km.csv")
GruidaeAllOcc_2019_2023_spacRare10km <- fread("GruidaeAllOcc_2019_2023_spacRare10km.csv")
GruidaeAllOcc_spacRare10km <- rbind(GruidaeAllOcc_before2000_spacRare10km, 
                                    GruidaeAllOcc_2000_2018_spacRare10km, 
                                    GruidaeAllOcc_2019_2023_spacRare10km)


# Visualize Occs per year for each species
GruidaeAllOcc_spacRare10km %>% 
    mutate(myYear = year(myEventDate)) %>% 
    filter(myYear >= 1980) %>%
    group_by(species, myYear) %>% 
    dplyr::summarise(occs = n()) %>%
    filter(species %in% c("Grus grus", "Grus canadensis")) %>%
    ggplot() +
    geom_col(mapping = aes(x = myYear, y = occs )) +
    facet_grid(rows = vars(species)) +
    theme_calc()
ggsave("occs_spacRare10km_byYear1980-2023_set1.png", height = 4, width = 6, units = "in", dpi = 300)

GruidaeAllOcc_spacRare10km %>% 
    mutate(myYear = year(myEventDate)) %>% 
    filter(myYear >= 1980) %>%
    group_by(species, myYear) %>% 
    dplyr::summarise(occs = n()) %>%
    filter(species %in% c("Anthropoides paradiseus", "Anthropoides virgo", "Balearica regulorum", "Grus antigone", "Grus rubicunda")) %>%
    ggplot() +
    geom_col(mapping = aes(x = myYear, y = occs )) +
    facet_grid(rows = vars(species)) +
    theme_calc()
ggsave("occs_spacRare10km_byYear1980-2023_set2.png", height = 8, width = 6, units = "in", dpi = 300)

GruidaeAllOcc_spacRare10km %>% 
    mutate(myYear = year(myEventDate)) %>% 
    filter(myYear >= 1980) %>%
    group_by(species, myYear) %>% 
    dplyr::summarise(occs = n()) %>%
    filter(!(species %in% c("Grus grus", "Grus canadensis", "Anthropoides paradiseus", "Anthropoides virgo", "Balearica regulorum", "Grus antigone", "Grus rubicunda"))) %>%
    ggplot() +
    geom_col(mapping = aes(x = myYear, y = occs )) +
    facet_grid(rows = vars(species)) +
    theme_calc()
ggsave("occs_spacRare10km_byYear1980-2023_set3.png", height = 11, width = 6, units = "in", dpi = 300)

# Get All Species and move Sarus, Common, and Sandhill Cranes to the end (most likely to cause issues)
allSpecies <- unique(GruidaeAllOcc_spacRare10km$species)
allSpecies <- allSpecies[!(allSpecies %in% c("Grus antigone", "Grus grus", "Grus canadensis"))]
allSpecies <- c(allSpecies, c("Grus antigone", "Grus grus", "Grus canadensis"))
allSpecies <- allSpecies[!(allSpecies %in% c("Anthropoides paradiseus", 
                                             "Anthropoides virgo", 
                                             "Balearica pavonina",
                                             "Balearica regulorum",
                                             "Bugeranus carunculatus", 
                                             "Grus americana",
                                             "Grus japonensis",
                                             "Grus leucogeranus",
                                             "Grus monacha",
                                             "Grus nigricollis",
                                             "Grus rubicunda",
                                             "Grus vipio",
                                             "Grus antigone",
                                             "Grus grus",
                                             "Grus canadensis"))] # Already ran these species

# BEGIN MODELING LOOP
for(speciesForModel in allSpecies){
################################################
# Filter occs to species and set seasons *~*~*~*
################################################

message(paste0("\nWorking on ", speciesForModel, "...\n"))

# Set breeding season and wintering by species ~!~!~!~!~!~!~!~!~!~!~!
switch(speciesForModel,
       "Anthropoides paradiseus" = { # Johnsgard, Miranda 2019
           breedingSeason <- c(11, 12, 1)
           winteringSeason <- c(5, 6, 7)
       },
       "Anthropoides virgo" = { # Johnsgard, Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Balearica pavonina" = { # Model as resident; Johnsgard, Miranda 2019
           breedingSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
           winteringSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
       },
       "Balearica regulorum" = { # Model as resident; Johnsgard, Miranda 2019
           breedingSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
           winteringSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
       },
       "Bugeranus carunculatus" = { # Johnsgard, Miranda 2019 - Seaonsal movements following floodplain cycles?
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus americana" = { # Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus antigone" = { # Modeled as resident until able to split off the Australian pop; apparently breeds at different times
           breedingSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
           winteringSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
       },
       "Grus canadensis" = { # Miranda 2019 - Florida pop breeds at different time?
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus grus" = { # Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus japonensis" = { # Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus leucogeranus" = { # Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus monacha" = { # Johnsgard and Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus nigricollis" = { # Johnsgard and Miranda 2019; short distance migratory: high-altitude to low
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       },
       "Grus rubicunda" = { # Modeled as resident; different pops breed at different times?
           breedingSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
           winteringSeason <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
       },
       "Grus vipio" = { # Johnsgard and Miranda 2019
           breedingSeason <- c(5,6,7)
           winteringSeason <- c(11,12,1)
       }
       )

# Set path for output for current species
pathForOutput <- file.path("H:", "Crane_SDMs", gsub(" ", "_", speciesForModel))

# filter occs before 2018 (retain for evaluation)
GruidaeAllOcc_spacRare10km_currentSpecies <- GruidaeAllOcc_spacRare10km %>% 
    mutate(myYear = year(myEventDate)) %>%
    filter(species == speciesForModel, myYear >= 2018) # , myMonth %in% c(11, 12, 1))
# Isolated occurrences before 2018 for testing
GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData <- GruidaeAllOcc_spacRare10km %>% 
    mutate(myYear = year(myEventDate)) %>%
    filter(species == speciesForModel, myYear < 2018, myYear >= 1980)

# Grus grus and Grus canadensis have have nearly 100x more occurrences 2018 - 2023 than other species
# Cull them randomly to get 10% of those datasets to reduce computational burden
set.seed(33)
if(speciesForModel %in% c("Grus grus", "Grus canadensis")){
    GruidaeAllOcc_spacRare10km_currentSpecies <- GruidaeAllOcc_spacRare10km_currentSpecies %>%
        slice_sample(prop = 0.1)
    fwrite(GruidaeAllOcc_spacRare10km_currentSpecies, file = file.path(pathForOutput, "final_occurences_used_after_culling_to_10percent.csv"))
}


# Convert for predicts (needs spatVector points)
occs <- GruidaeAllOcc_spacRare10km_currentSpecies %>% 
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>% 
    select(lon, lat) %>%
    as_spatvector(crs = "EPSG:4326")

# get extent to trim rasters (500km buffer); note the raster trimming is independent of the 500 meter 
# buffer for background selection later
occs_extent <- terra::ext(terra::buffer(occs, width = 500000))

# For G. canadensis, the buffers wrap the antimeridian. This causes problems, so let's just filter that to coords x < 0 (western hemisphere)...
# This *does* cause some areas in northern asia to be excluded from background sampling that should 
# be included, but it's a relatively small area
# Same thing happens for the Siberian Crane, so need a similar correction!
if(speciesForModel == "Grus canadensis"){
    occs_extent <- terra::ext(terra::vect(as.matrix(terra::geom(terra::buffer(occs, width = 500000), df = TRUE) %>% filter(x < 0)), type = "polygon"))
}
if(speciesForModel == "Grus leucogeranus"){
    occs_extent <- terra::ext(terra::vect(as.matrix(terra::geom(terra::buffer(occs, width = 500000), df = TRUE) %>% filter(x > 0)), type = "polygon"))
}

# Read geodata info for quick and dirty analysis ~*~*~*~*~*~*~*~ REPLACE WITH CHELSA BIOCLIM+
# get bioclim and crop it; use this raster to crop other rasters
#bioclim_rasters <- geodata::worldclim_global(var = "bio", res = 0.5, path = "H:/geodata/")
bioclim_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_bio[0-9]{1,2}_.+"))
cmi_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_cmi_.+"))
if(file.exists("H:/Chelsa-related/CHELSA_clim/ndd5_NA0.geotif")){
    gdd5_rasters <- terra::rast("H:/Chelsa-related/CHELSA_clim/ndd5_NA0.geotif")
}else{
    gdd5_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_gdd5_.+"))
    gdd5_rasters[is.na(gdd5_rasters)] <- 0 # Set NA to 0
    writeRaster(x = gdd5_rasters, filename = "H:/Chelsa-related/CHELSA_clim/ndd5_NA0.geotif", filetype = "GTiff")
}
if(file.exists("H:/Chelsa-related/CHELSA_clim/gsl_NA0.geotif")){
    gsl_rasters <- terra::rast("H:/Chelsa-related/CHELSA_clim/gsl_NA0.geotif")
}else{
    gsl_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_gsl_.+"))
    gsl_rasters[is.na(gsl_rasters)] <- 0 # Set NA to 0
    writeRaster(x = gsl_rasters, filename = "H:/Chelsa-related/CHELSA_clim/gsl_NA0.geotif", filetype = "GTiff")
}
#TREELIM associated rasters are full of annoying holes; I fixed the gsl raster above, but others are hard to fix; maybe terra::near()?
#gsp_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_gsp_.+"))
#gst_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_gst_.+"))
hurs_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_hurs_.+"))
if(file.exists("H:/Chelsa-related/CHELSA_clim/ngd5_NA0.geotif")){
    ngd5_rasters <- terra::rast("H:/Chelsa-related/CHELSA_clim/ngd5_NA0.geotif")
}else{
    ngd5_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_ngd5_.+"))
    ngd5_rasters[is.na(ngd5_rasters)] <- 0 # Set NA to 0
    writeRaster(x = ngd5_rasters, filename = "H:/Chelsa-related/CHELSA_clim/ngd5_NA0.geotif", filetype = "GTiff")
}
scd_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_scd_.+"))

bioclim_rasters <- c(bioclim_rasters, cmi_rasters, gdd5_rasters, gsl_rasters, hurs_rasters, ngd5_rasters, scd_rasters)
rm(cmi_rasters, gdd5_rasters, gsl_rasters, hurs_rasters, ngd5_rasters, scd_rasters)

bioclim_rasters <- terra::crop(bioclim_rasters, occs_extent)

# elevation raster and cropping
elev_raster <- geodata::elevation_global(res = 0.5, path = "H:/geodata/")
#elev_raster <- terra::crop(x = elev_raster, y = bioclim_rasters$wc2.1_30s_bio_1)
elev_raster <- terra::crop(x = elev_raster, y = bioclim_rasters$`CHELSA_bio1_1981-2010_V.2.1`)

# get world boundaries; trim to extent that we need for these projections
world_boundaries <- world(res = 1, path = "H:/geodata/")
#world_boundaries <- terra::crop(world_boundaries, bioclim_rasters$wc2.1_30s_bio_1)
world_boundaries <- terra::crop(world_boundaries, bioclim_rasters$`CHELSA_bio1_1981-2010_V.2.1`)

# Get admin areas for countries still present
state_boundaries <- gadm(country = world_boundaries$NAME_0, level = 1, resolution = 1, path = "H:/geodata/")

# all vars
explanatory_vars <- c(bioclim_rasters, elev_raster)

# Sample background
# Filter to months of interest prior to sampling background
occs_summer <- GruidaeAllOcc_spacRare10km_currentSpecies %>% 
    filter(myMonth %in% breedingSeason) %>% 
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>% 
    select(lon, lat) %>%
    as_spatvector(crs = "EPSG:4326")
occs_winter <- GruidaeAllOcc_spacRare10km_currentSpecies %>% 
    filter(myMonth %in% winteringSeason) %>%
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>% 
    select(lon, lat) %>%
    as_spatvector(crs = "EPSG:4326")

# Temporal test data
temporalTest_summer <- GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData %>% 
    filter(myMonth %in% breedingSeason) %>% 
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>% 
    as_spatvector(crs = "EPSG:4326")

temporalTest_winter <- GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData %>% 
    filter(myMonth %in% winteringSeason) %>% 
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>% 
    as_spatvector(crs = "EPSG:4326")


# 500km buffers - summer
occs_summer_buffer500 <- terra::buffer(occs_summer, width = 500000)
# Again, special case for G. canadensis to prevent antimeridian wrap; same with Siberian crane
if(speciesForModel == "Grus canadensis"){
    occs_summer_buffer500 <- terra::vect(as.matrix(terra::geom(terra::buffer(occs_summer, width = 500000), df = TRUE) %>% filter(x < 0)), type = "polygon")
}
if(speciesForModel == "Grus leucogeranus"){
    occs_summer_buffer500 <- terra::vect(as.matrix(terra::geom(terra::buffer(occs_summer, width = 500000), df = TRUE) %>% filter(x > 0)), type = "polygon")
}
temp_rast <- rast(occs_summer_buffer500, ncols = 10000, nrows = 10000) # rows / cols here determine resolution of raster
pseudo_abs_summer_buffer500_mask <- rasterize(occs_summer_buffer500, temp_rast)
rm(temp_rast)
pseudo_abs_summer_buffer500_mask <- terra::resample(pseudo_abs_summer_buffer500_mask, explanatory_vars$wc2.1_30s_elev) # Have to resample to run next step
pseudo_abs_summer_buffer500_mask <- terra::intersect(pseudo_abs_summer_buffer500_mask, explanatory_vars$wc2.1_30s_elev) # again, soil raster has holes; avoid those
pseudo_abs_summer_buffer500_mask[pseudo_abs_summer_buffer500_mask == 0] <- NA # Sets False/0 to NA

# 500km buffers - winter
occs_winter_buffer500 <- terra::buffer(occs_winter, width = 500000)
# Again, special case for G. canadensis to prevent antimeridian wrap
if(speciesForModel == "Grus canadensis"){
    occs_winter_buffer500 <- terra::vect(as.matrix(terra::geom(terra::buffer(occs_winter, width = 500000), df = TRUE) %>% filter(x < 0)), type = "polygon")
}
if(speciesForModel == "Grus leucogeranus"){
    occs_winter_buffer500 <- terra::vect(as.matrix(terra::geom(terra::buffer(occs_summer, width = 500000), df = TRUE) %>% filter(x > 0)), type = "polygon")
}
temp_rast <- rast(occs_winter_buffer500, ncols = 1000, nrows = 1000) # rows / cols here determine resolution of raster
pseudo_abs_winter_buffer500_mask <- rasterize(occs_winter_buffer500, temp_rast)
rm(temp_rast)
pseudo_abs_winter_buffer500_mask <- terra::resample(pseudo_abs_winter_buffer500_mask, explanatory_vars$wc2.1_30s_elev) # Have to resample to run next step
pseudo_abs_winter_buffer500_mask <- terra::intersect(pseudo_abs_winter_buffer500_mask, explanatory_vars$wc2.1_30s_elev) # again, soil raster has holes; avoid those
pseudo_abs_winter_buffer500_mask[pseudo_abs_winter_buffer500_mask == 0] <- NA # Sets False/0 to NA

# set RNG seed for consistency
set.seed(33)

# get pseudo abs
# How many background points to generate????? n = nrow(occs_summer)*2
pseudo_abs_summer_buffer500 <- predicts::backgroundSample(mask = pseudo_abs_summer_buffer500_mask, n = 10000, tryf = 100)
pseudo_abs_summer_buffer500 <- as.data.frame(pseudo_abs_summer_buffer500) %>% rename(lon = x, lat = y)

pseudo_abs_winter_buffer500 <- predicts::backgroundSample(mask = pseudo_abs_winter_buffer500_mask, n = 10000, tryf = 100)
pseudo_abs_winter_buffer500 <- as.data.frame(pseudo_abs_winter_buffer500) %>% rename(lon = x, lat = y)


# sample rasters
# Extract values from rasters
# Summer
occ_data_summer <- terra::extract(explanatory_vars, occs_summer, ID = FALSE, bind = TRUE) %>% 
    as.data.frame(geom = "XY") %>% 
    mutate(pb = 1)
pseudo_data_summer_buffer500 <- terra::extract(explanatory_vars, pseudo_abs_summer_buffer500, ID = FALSE, bind = TRUE) %>% 
    as.data.frame(geom = "XY") %>% 
    mutate(pb=0)

# Winter
occ_data_winter <- terra::extract(explanatory_vars, occs_winter, ID = FALSE, bind = TRUE) %>% 
    as.data.frame(geom = "XY") %>% 
    mutate(pb = 1)
pseudo_data_winter_buffer500 <- terra::extract(explanatory_vars, pseudo_abs_winter_buffer500, ID = FALSE, bind = TRUE) %>% 
    as.data.frame(geom = "XY") %>% 
    mutate(pb=0)

# Checkerboard groups for k-fold eval
# had to limit this to the number of rows in the original data since it generated too many rows (?) in 
# the rbind call below
checkerboard_summer <- get.checkerboard(occs = occ_data_summer %>% select(x,y),
                                        bg = pseudo_data_summer_buffer500 %>% select(x,y),
                                        envs = explanatory_vars,
                                        aggregation.factor = c(10,10))

# Eval plots for k groups in checkerboard
# png(file.path(pathForOutput, "evalplot_grps_occs.png"), width = 800, height = 500)
# evalplot.grps(pts = occ_data_summer %>% select(x,y),
#               pts.grp = checkerboard_summer$occs.grp[1:nrow(occ_data_summer)],
#               envs = explanatory_vars$`CHELSA_bio5_1981-2010_V.2.1`,
#               pts.size = 1)
# dev.off()
# png(file.path(pathForOutput, "evalplot_grps_bg.png"), width = 800, height = 500)
# evalplot.grps(pts = pseudo_data_summer_buffer500 %>% select(x,y),
#               pts.grp = checkerboard_summer$bg.grp[1:nrow(pseudo_data_summer_buffer500)],
#               envs = explanatory_vars$`CHELSA_bio5_1981-2010_V.2.1`,
#               pts.size = 1)
# dev.off()

checkerboard_winter <- get.checkerboard(occs = occ_data_winter %>% select(x,y),
                                        bg = pseudo_data_winter_buffer500 %>% select(x,y),
                                        envs = explanatory_vars,
                                        aggregation.factor = c(10,10))

# Temporal test data extraction
temporalTest_data_summer <- terra::extract(explanatory_vars, temporalTest_summer, ID = FALSE, bind = TRUE) %>% as.data.frame()
temporalTest_data_summer <- temporalTest_data_summer %>% mutate(pb = 1)

temporalTest_data_winter <- terra::extract(explanatory_vars, temporalTest_winter, ID = FALSE, bind = TRUE) %>% as.data.frame()
temporalTest_data_winter <- temporalTest_data_winter %>% mutate(pb = 1)

# bind
sdm_data_summer <- rbind(occ_data_summer %>% mutate(kGroup = checkerboard_summer$occs.grp[1:nrow(occ_data_summer)]), 
                         pseudo_data_summer_buffer500 %>% mutate(kGroup = checkerboard_summer$bg.grp))
sdm_data_winter <- rbind(occ_data_winter %>% mutate(kGroup = checkerboard_winter$occs.grp[1:nrow(occ_data_winter)]), 
                         pseudo_data_winter_buffer500 %>% mutate(kGroup = checkerboard_winter$bg.grp))

# Fix var names that terra::extract() breaks prior to modeling
# Terra extract replaced "-" with ".", so need to fix that on the test set (it should throw a warning when doing that...)
colnames(sdm_data_summer) <- sub("([0-9]{4})\\.", "\\1-", colnames(sdm_data_summer))
colnames(sdm_data_winter) <- sub("([0-9]{4})\\.", "\\1-", colnames(sdm_data_winter))
colnames(temporalTest_data_summer) <- sub("([0-9]{4})\\.", "\\1-", colnames(temporalTest_data_summer))
colnames(temporalTest_data_winter) <- sub("([0-9]{4})\\.", "\\1-", colnames(temporalTest_data_winter))

# Write extracted data to file ~*~*~*~*~*~*~*~*~*~*~*~*~*
fwrite(sdm_data_summer, file = file.path(".", gsub(" ", "_", speciesForModel), paste0(gsub(" ", "_", speciesForModel), "_sdm_data_summer.csv")))
fwrite(sdm_data_winter, file = file.path(".", gsub(" ", "_", speciesForModel), paste0(gsub(" ", "_", speciesForModel), "_sdm_data_winter.csv")))

# Use variance inflation factors to select which variables to use in the model
# we'll use the sampled background and occurences for this to avoid calling this on 
# the high res rasters. The patterns below get pasted together into a regex used with grep to the 
# exact column names to keep (CHELSA var names are kind of annoying)
keeper_patterns_summer <- c("_bio5_", "_bio13_")
keeper_patterns_winter <- c("_bio6_", "_bio13_")
sdm_data_summer_VIF <- usdm::vifstep(sdm_data_summer %>% select(-x, -y, -pb, -kGroup), size = 10000, keep = unique(grep(paste(keeper_patterns_summer, collapse="|"), names(sdm_data_summer), value=TRUE)))
sdm_data_winter_VIF <- usdm::vifstep(sdm_data_winter %>% select(-x, -y, -pb, -kGroup), size = 10000, keep = unique(grep(paste(keeper_patterns_summer, collapse="|"), names(sdm_data_summer), value=TRUE)))

# ... and drop the excluded vars
sdm_data_summer <- sdm_data_summer %>% select(-sdm_data_summer_VIF@excluded)
sdm_data_winter <- sdm_data_winter %>% select(-sdm_data_winter_VIF@excluded)

# # For pseudo absences, we need to generate them WITH associated dates for the monthly data
# # Respects Dplyr groups
backgroundSampleWithDates <- function(.data, latCol = "decimalLatitude", lonCol = "decimalLongitude", dateCol = NULL, distanceKM = 500, clippingRaster = NULL, tries = 100, relativeBackgroundMult = 3, useSetBackgroundPerMonth = FALSE, setBackgroundPerMonth = 100, warn = 2){
    # Check .data is valid
    if((length(.data) == 0) | (!inherits(.data, "data.frame"))){
        stop("'data' must be set and be of class 'data.frame'")
    }
    if((is.null(dateCol)) | (!inherits(dateCol, "character"))){
        stop("'dateCol' must be set and be of class 'character'")
    }
    message("Beginning background point selection...\n")
    # spacial rarefaction while accounting for dplyr groups
    # vars with . appended to the end to avoid a weird recursive reference thing
    .data %>% group_modify(function(df, y, latCol. = latCol, lonCol. = lonCol, dateCol. = dateCol, distanceKM. = distanceKM, clippingRaster. = clippingRaster, tries. = tries, .warn = warn){
        startTime <- Sys.time()
        groupVarNames <- names(y)
        for(name in groupVarNames){
            message(paste0("Grouping variable: ", name, ", Value: ", y[name]))
        }
        message(paste0("Size Of occs for group: ", nrow(df)))
        message(paste0("Low date: ", min(df[[dateCol.]])))
        message(paste0("High date: ", max(df[[dateCol.]])))
        # Convert to spatVector Points
        occs_spatVector <- df %>%
            mutate(lon = !!sym(lonCol.), lat = !!sym(latCol.)) %>%
            select(lat, lon) %>%
            as_spatvector(crs = "EPSG:4326")
        # Need to create a mask for generating backaground sampling; distanceKM buffer
        occs_buffer <- terra::buffer(occs_spatVector, width = distanceKM. * 1000)
        temp_rast <- rast(occs_buffer, ncols = 1000, nrows = 1000) # rows / cols here determine resolution of raster
        background_buffer_mask <- rasterize(occs_buffer, temp_rast)
        rm(temp_rast)
        if(!is.null(clippingRaster.)){
            background_buffer_mask <- terra::resample(background_buffer_mask, clippingRaster.) # Have to resample to run next step
            background_buffer_mask <- terra::intersect(background_buffer_mask, clippingRaster.) # again, soil raster has holes; avoid those
        }
        background_buffer_mask[background_buffer_mask == 0] <- NA # Sets False/0 to NA

        # get pseudo abs
        if(useSetBackgroundPerMonth){
            nBackground <- setBackgroundPerMonth
        }else{
            nBackground <- nrow(df) * relativeBackgroundMult
        }
        background_buffer <- predicts::backgroundSample(mask = background_buffer_mask, n = nBackground, tryf = tries., warn = .warn)
        background_buffer <- as.data.frame(background_buffer) %>%
            rename(!!lonCol. := x, !!latCol. := y) %>%
            mutate(!!dateCol := sample(df[[dateCol.]], size = nrow(background_buffer), replace = TRUE))

        # message about what was done
        message(paste0("Background points generated within ", distanceKM, " km from occs: ", nrow(background_buffer)))
        message(paste0("Run time for group: ", format(difftime(Sys.time(), startTime)), "\n"))

        # bind input with new background; pb = 1 is an occ, pb = 0 is background; plyr dependency...
        return(plyr::rbind.fill(df %>% mutate(pb = 1), background_buffer %>% mutate(pb = 0)))
    }
    )
}

# Custom eval function: maximum test sensitivity plus specificity 
# TODO add this eval function to the below after writing it
# maxTestSensPlusSpec <- function(vars){
#     testThresholds <- seq(0, 100, 0.5)
# }

################################
## Maxent - summer #############
################################

# ENMEval (using hierarchical checkerboards for cross-validation)
ENMEval_summer <- ENMevaluate(occs = sdm_data_summer %>% filter(pb == 1) %>% relocate(x, y) %>% select (-pb, -kGroup),
                              bg = sdm_data_summer %>% filter(pb == 0) %>% relocate(x, y) %>% select (-pb, -kGroup),
                              algorithm = "maxent.jar",
                              tune.args = list(fc = c("L","LQ","LQT"), rm = seq(1, 5, 1)),
                              partitions = "user",
                              user.grp = list(occs.grp = unlist(sdm_data_summer %>% filter(pb == 1) %>% select(kGroup)),
                                              bg.grp = unlist(sdm_data_summer %>% filter(pb == 0) %>% select(kGroup))),
                              taxon.name = speciesForModel,
                              parallel = FALSE, # If TRUE: Error in unserialize(node$con) : error reading from connection
                              other.settings = list(path = file.path(pathForOutput, "summer"),
                                                    other.args = c("jackknife=TRUE", "responsecurves=TRUE", "threads=16"))
                              )
# Get results dataframe and filter to best model; criteria are lowest omission rate and continuous boyce index (CBI) to break ties
ENMEval_summer_res <- eval.results(ENMEval_summer)

bestModelSelector_summer <- ENMEval_summer_res %>%
    filter(!is.na(or.10p.avg)) %>%
    filter(or.10p.avg == min(or.10p.avg))
if((nrow(bestModelSelector_summer) > 1) && !is.na(max(bestModelSelector_summer$cbi.val.avg))){ # If FNR tie, use cbi on validation to break if it exists, else use auc on validation
    bestModelSelector_summer <- bestModelSelector_summer %>% 
        filter(cbi.val.avg == max(cbi.val.avg))
}else if((nrow(bestModelSelector_summer) > 1) && !is.na(max(bestModelSelector_summer$auc.val.avg))){
    bestModelSelector_summer <- bestModelSelector_summer %>% 
        filter(auc.val.avg == max(auc.val.avg))
}
if(nrow(bestModelSelector_summer) > 1){ # If there is STILL a tie, just take the first model; these are equivalent
    bestModelSelector_summer <- bestModelSelector_summer %>% head(n = 1)
}
bestModelSelector_summer <- bestModelSelector_summer %>% 
    .$tune.args

maxent_data_summer <- eval.models(ENMEval_summer)[[bestModelSelector_summer]]

# K-fold cross validation and test isolation
# Does background actually need to be divided into folds? I think maybe yes since it does inform the model...
# Consider BlockCV package? EDIT: There was a functionf for this in the ENMEval package; stored in kGroup
summer_background <- sdm_data_summer %>% filter(pb == 0) %>% select(-x, -y)
summer_present <- sdm_data_summer %>% filter(pb == 1) %>% select (-x, -y)


# Modeling with MaxEnt - K-folds
summer_eval_kfold <- list() # gets summary stats for each value
# Let's also fish out the FNR at the reported thresholds for max Kappa, max sum of spec and sens, and equal spec and sens
summer_eval_kfold_FNR <-data.frame(FNR_max_kappa = numeric(), 
                                   FNR_max_spec_sens = numeric(), 
                                   FNR_equal_sens_spec = numeric(),
                                   threshold_max_kappa = numeric(), 
                                   threshold_max_spec_sens = numeric(), 
                                   threshold_equal_sens_spec = numeric())
## THIS FUNCTION WAS ONE OF THE BROKEN ONES DURING DISSERTATION WRITING
# The FNR pulled for the dissertation figure was just the first fold eval repeated k times rather than pulling each individually
# Fix was changing 1 to i in all the "summer_eval_kfold[[1]]@trstats" below
for (i in 1:max(summer_present$kGroup)) {
    train <- rbind(summer_present %>% filter(kGroup != i),
                   summer_background %>% filter(kGroup != i))
    test <- summer_present %>% filter(kGroup == i)
    p <- predict(object = maxent_data_summer, test)
    # Evaluate on all background points or.... ?
    a <- predict(object = maxent_data_summer, summer_background)
    summer_eval_kfold[[i]] <- pa_evaluate(p, a)
    summer_eval_kfold_FNR[nrow(summer_eval_kfold_FNR) + 1, ] <- c(summer_eval_kfold[[i]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - summer_eval_kfold[[i]]@thresholds[['max_kappa']]))) %>% 
                                                                      .$FNR,
                                                                  summer_eval_kfold[[i]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - summer_eval_kfold[[i]]@thresholds[['max_spec_sens']]))) %>% 
                                                                      .$FNR,
                                                                  summer_eval_kfold[[i]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - summer_eval_kfold[[i]]@thresholds[['equal_sens_spec']]))) %>% 
                                                                      .$FNR,
                                                                  summer_eval_kfold[[i]]@thresholds[['max_kappa']],
                                                                  summer_eval_kfold[[i]]@thresholds[['max_spec_sens']],
                                                                  summer_eval_kfold[[i]]@thresholds[['equal_sens_spec']]
                                                                  )
}
# This gets the stats for each model and binds it to the False Negative Rate info
summer_eval_kfold_stats <- lapply(summer_eval_kfold, function(x){x@stats}) %>% 
    do.call(what = rbind) %>%
    as.data.frame() %>%
    cbind(run.number = 1:length(summer_eval_kfold)) %>%
    cbind(summer_eval_kfold_FNR)

kfolds_plot <- summer_eval_kfold_stats %>%
    select(run.number, auc, FNR_max_spec_sens) %>% # Use FNR based on max sum of specificity and sensitivity
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = !run.number, names_to = "stat") %>%
    ggplot(mapping = aes(x = run.number, y = value, group = stat, label = round(value, 2),  fill = stat)) +
    geom_col() + 
    geom_label(fill = "white") +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0,1)) +
    facet_grid(rows = vars(stat)) + 
    theme_light(base_size = 10) + 
    ggtitle("K-fold Performance", paste0("FNR; threshold at max(specificity + sensitivity)=", round(summer_eval_kfold[[1]]@thresholds[['max_spec_sens']], 3)))
kfolds_plot
    
# Evaluate Temporal Test Data with MaxEnt Model
# (should probably make fair background points for this rather than reusing training...)
summer_eval_temporal <- data.frame(year =  numeric(), 
                                   testN = numeric(), 
                                   backgroundN = numeric(), 
                                   auc = numeric(),
                                   FNR_max_kappa = numeric(), 
                                   FNR_max_spec_sens = numeric(), 
                                   FNR_equal_sens_spec = numeric(),
                                   threshold_max_kappa = numeric(), 
                                   threshold_max_spec_sens = numeric(), 
                                   threshold_equal_sens_spec = numeric())


for(ye in sort(unique(temporalTest_data_summer$myYear))){
    test <- temporalTest_data_summer %>% 
        filter(myYear == ye) %>% 
        select(-sdm_data_summer_VIF@excluded, -species, -decimalLatitude, -decimalLongitude, -gbifID, -myEventDate, -myMonth, -myYear, -pb)
    if(nrow(test %>% drop_na()) == 0){
        next # Protection against instances where there are only a few occurrences and that may drop to zero when some vars are missing data
    }
    bg <- sdm_data_summer %>% filter(pb == 0) %>% select(-pb)
    p <- predict(object = maxent_data_summer, test)
    # Evaluate on all background points or.... ?
    a <- predict(object = maxent_data_summer, bg)
    # Bind results to next row of dataframe
    res <- pa_evaluate(p, a)
    summer_eval_temporal[nrow(summer_eval_temporal) + 1, ] <- c(ye, 
                                                                nrow(test), 
                                                                nrow(bg), 
                                                                res@stats$auc[[1]],
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['max_kappa']]))) %>% 
                                                                    .$FNR,
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['max_spec_sens']]))) %>% 
                                                                    .$FNR,
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['equal_sens_spec']]))) %>% 
                                                                    .$FNR,
                                                                res@thresholds[['max_kappa']],
                                                                res@thresholds[['max_spec_sens']],
                                                                res@thresholds[['equal_sens_spec']])
}

# Temporal performance plot and combo plot
temporal_plot <- summer_eval_temporal %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>%
    ggplot(mapping = aes(x = year, y = value, label = testN)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    ggrepel::geom_label_repel(size = 1.5, direction = "both", fill = "white", box.padding = 0.25, max.overlaps = Inf, min.segment.length = 0, segment.linetype = 1, segment.size = 0.2, segment.color = "grey", label.size = 0.1, label.padding = 0.1) +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("Past years; Background = ", summer_eval_temporal$backgroundN[[1]]))
temporal_plot

p_combo <- plot_grid(kfolds_plot, temporal_plot, labels = c("A", "B"), label_size = 20, rel_heights = c(1,1))
p_combo <- p_combo + theme(plot.background = element_rect(fill = "white", color = "white")) #get rid of transparency
p_combo
ggsave(file.path(pathForOutput, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_summer_kfolds_and_temporal.png")), plot = p_combo, , width = 8, height = 4, units = "in", dpi = 600)

# Plot MaxEnt on current rasters
# First, make downscaled version of input rasters to make rendering less insane
if(!file.exists(paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))){
    targetForDownscaling <- rast("H:/wc2.1_cruts4.09_2.5m_prec_2020-2024_TargetForDownscaling/wc2.1_cruts4.09_2.5m_prec_2020-01.tif")
    targetForDownscaling <- terra::crop(targetForDownscaling, occs_extent)
    explanatory_vars_2.5min <- resample(explanatory_vars, targetForDownscaling, filetype = "GTiff", filename = paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))
    rm(targetForDownscaling)
}else{
    explanatory_vars_2.5min <- rast(paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))
}

# Run model predictions on rasters and plot
maxent_data_summer_predictions <- predict(explanatory_vars_2.5min, maxent_data_summer, na.rm = TRUE, args = c("threads=32"))

ggplot() +
    geom_spatraster(data = maxent_data_summer_predictions, na.rm = TRUE) + 
    scale_y_continuous(limits = c(ext(maxent_data_summer_predictions)$ymin, ext(maxent_data_summer_predictions)$ymax), expand = c(0,0)) + 
    scale_x_continuous(limits = c(ext(maxent_data_summer_predictions)$xmin, ext(maxent_data_summer_predictions)$xmax), expand = c(0,0)) + 
    scale_fill_whitebox_c(palette = "muted", name = "Suitability") +
    geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
    geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
    #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
    #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
    ggtitle("MaxEnt Model, abs within 500 km buffer, >2018", paste0(speciesForModel, " Summer Niche")) + 
    theme_clean() + 
    theme(plot.background = element_rect(color = "white")) #get rid of plot outline

ggsave(file.path(pathForOutput, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_summer_absBuffer500_after2015.png")), width = 9, height = 5.75, units = "in", dpi = 600)

################################
## Maxent - winter #############
################################

# ENMEval (using hierarchical checkerboards for cross-validation)
ENMEval_winter <- ENMevaluate(occs = sdm_data_winter %>% filter(pb == 1) %>% relocate(x, y) %>% select (-pb, -kGroup),
                              bg = sdm_data_winter %>% filter(pb == 0) %>% relocate(x, y) %>% select (-pb, -kGroup),
                              algorithm = "maxent.jar",
                              tune.args = list(fc = c("L","LQ","LQT"), rm = seq(1, 5, 1)),
                              partitions = "user",
                              user.grp = list(occs.grp = unlist(sdm_data_winter %>% filter(pb == 1) %>% select(kGroup)),
                                              bg.grp = unlist(sdm_data_winter %>% filter(pb == 0) %>% select(kGroup))),
                              taxon.name = speciesForModel,
                              parallel = FALSE, # If TRUE: Error in unserialize(node$con) : error reading from connection
                              other.settings = list(path = file.path(pathForOutput, "winter"),
                                                    other.args = c("jackknife=TRUE", "responsecurves=TRUE", "threads=16"))
)
# Get results dataframe and filter to best model; criteria are lowest omission rate and continuous boyce index (CBI) to break ties
ENMEval_winter_res <- eval.results(ENMEval_winter)

bestModelSelector_winter <- ENMEval_winter_res %>%
    filter(!is.na(or.10p.avg)) %>%
    filter(or.10p.avg == min(or.10p.avg))
if((nrow(bestModelSelector_winter) > 1) && !is.na(max(bestModelSelector_winter$cbi.val.avg))){ # If FNR tie, use cbi on validation to break if it exists, else use auc on validation
    bestModelSelector_winter <- bestModelSelector_winter %>% 
        filter(cbi.val.avg == max(cbi.val.avg))
}else if((nrow(bestModelSelector_winter) > 1) && !is.na(max(bestModelSelector_winter$auc.val.avg))){
    bestModelSelector_winter <- bestModelSelector_winter %>% 
        filter(auc.val.avg == max(auc.val.avg))
}
if(nrow(bestModelSelector_winter) > 1){ # If there is STILL a tie, just take the first model; these are equivalent
    bestModelSelector_winter <- bestModelSelector_winter %>% head(n = 1)
}
bestModelSelector_winter <- bestModelSelector_winter %>% 
    .$tune.args

maxent_data_winter <- eval.models(ENMEval_winter)[[bestModelSelector_winter]]

# K-fold cross validation and test isolation
# Does background actually need to be divided into folds? I think maybe yes since it does inform the model...
# Consider BlockCV package? EDIT: There was a functionf for this in the ENMEval package; stored in kGroup
winter_background <- sdm_data_winter %>% filter(pb == 0) %>% select(-x, -y)
winter_present <- sdm_data_winter %>% filter(pb == 1) %>% select (-x, -y)


# Modeling with MaxEnt - K-folds
winter_eval_kfold <- list() # gets summary stats for each value
# Let's also fish out the FNR at the reported thresholds for max Kappa, max sum of spec and sens, and equal spec and sens
winter_eval_kfold_FNR <-data.frame(FNR_max_kappa = numeric(), 
                                   FNR_max_spec_sens = numeric(), 
                                   FNR_equal_sens_spec = numeric(),
                                   threshold_max_kappa = numeric(), 
                                   threshold_max_spec_sens = numeric(), 
                                   threshold_equal_sens_spec = numeric())
for (i in 1:max(winter_present$kGroup)) {
    train <- rbind(winter_present %>% filter(kGroup != i),
                   winter_background %>% filter(kGroup != i))
    test <- winter_present %>% filter(kGroup == i)
    p <- predict(object = maxent_data_winter, test)
    # Evaluate on all background points or.... ?
    a <- predict(object = maxent_data_winter, winter_background)
    winter_eval_kfold[[i]] <- pa_evaluate(p, a)
    winter_eval_kfold_FNR[nrow(winter_eval_kfold_FNR) + 1, ] <- c(winter_eval_kfold[[1]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - winter_eval_kfold[[1]]@thresholds[['max_kappa']]))) %>% 
                                                                      .$FNR,
                                                                  winter_eval_kfold[[1]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - winter_eval_kfold[[1]]@thresholds[['max_spec_sens']]))) %>% 
                                                                      .$FNR,
                                                                  winter_eval_kfold[[1]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - winter_eval_kfold[[1]]@thresholds[['equal_sens_spec']]))) %>% 
                                                                      .$FNR,
                                                                  winter_eval_kfold[[1]]@thresholds[['max_kappa']],
                                                                  winter_eval_kfold[[1]]@thresholds[['max_spec_sens']],
                                                                  winter_eval_kfold[[1]]@thresholds[['equal_sens_spec']]
    )
}
# This gets the stats for each model and binds it to the False Negative Rate info
winter_eval_kfold_stats <- lapply(winter_eval_kfold, function(x){x@stats}) %>% 
    do.call(what = rbind) %>%
    as.data.frame() %>%
    cbind(run.number = 1:length(winter_eval_kfold)) %>%
    cbind(winter_eval_kfold_FNR)

kfolds_plot <- winter_eval_kfold_stats %>%
    select(run.number, auc, FNR_max_spec_sens) %>% # Use FNR based on max sum of specificity and sensitivity
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = !run.number, names_to = "stat") %>%
    ggplot(mapping = aes(x = run.number, y = value, group = stat, label = round(value, 2),  fill = stat)) +
    geom_col() + 
    geom_label(fill = "white") +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0,1)) +
    facet_grid(rows = vars(stat)) + 
    theme_light(base_size = 10) + 
    ggtitle("K-fold Performance", paste0("FNR; threshold at max(specificity + sensitivity)=", round(winter_eval_kfold[[1]]@thresholds[['max_spec_sens']], 3)))
kfolds_plot

# Evaluate Temporal Test Data with MaxEnt Model
# (should probably make fair background points for this rather than reusing training...)
winter_eval_temporal <- data.frame(year =  numeric(), 
                                   testN = numeric(), 
                                   backgroundN = numeric(), 
                                   auc = numeric(),
                                   FNR_max_kappa = numeric(), 
                                   FNR_max_spec_sens = numeric(), 
                                   FNR_equal_sens_spec = numeric(),
                                   threshold_max_kappa = numeric(), 
                                   threshold_max_spec_sens = numeric(), 
                                   threshold_equal_sens_spec = numeric())


for(ye in sort(unique(temporalTest_data_winter$myYear))){
    test <- temporalTest_data_winter %>% 
        filter(myYear == ye) %>% 
        select(-sdm_data_winter_VIF@excluded, -species, -decimalLatitude, -decimalLongitude, -gbifID, -myEventDate, -myMonth, -myYear, -pb)
    if(nrow(test %>% drop_na()) == 0){
        next # Protection against instances where there are only a few occurrences and that may drop to zero when some vars are missing data
    }
    bg <- sdm_data_winter %>% filter(pb == 0) %>% select(-pb)
    p <- predict(object = maxent_data_winter, test)
    # Evaluate on all background points or.... ?
    a <- predict(object = maxent_data_winter, bg)
    # Bind results to next row of dataframe
    res <- pa_evaluate(p, a)
    winter_eval_temporal[nrow(winter_eval_temporal) + 1, ] <- c(ye, 
                                                                nrow(test), 
                                                                nrow(bg), 
                                                                res@stats$auc[[1]],
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['max_kappa']]))) %>% 
                                                                    .$FNR,
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['max_spec_sens']]))) %>% 
                                                                    .$FNR,
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['equal_sens_spec']]))) %>% 
                                                                    .$FNR,
                                                                res@thresholds[['max_kappa']],
                                                                res@thresholds[['max_spec_sens']],
                                                                res@thresholds[['equal_sens_spec']])
}

# Temporal performance plot and combo plot
temporal_plot <- winter_eval_temporal %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>%
    ggplot(mapping = aes(x = year, y = value, label = testN)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    ggrepel::geom_label_repel(size = 1.5, direction = "both", fill = "white", box.padding = 0.25, max.overlaps = Inf, min.segment.length = 0, segment.linetype = 1, segment.size = 0.2, segment.color = "grey", label.size = 0.1, label.padding = 0.1) +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("Past years; Background = ", winter_eval_temporal$backgroundN[[1]]))
temporal_plot

p_combo <- plot_grid(kfolds_plot, temporal_plot, labels = c("A", "B"), label_size = 20, rel_heights = c(1,1))
p_combo <- p_combo + theme(plot.background = element_rect(fill = "white", color = "white")) #get rid of transparency
p_combo
ggsave(file.path(pathForOutput, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_winter_kfolds_and_temporal.png")), plot = p_combo, , width = 8, height = 4, units = "in", dpi = 600)

# Plot MaxEnt on current rasters
# First, make downscaled version of input rasters to make rendering less insane
if(!file.exists(paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))){
    targetForDownscaling <- rast("H:/wc2.1_cruts4.09_2.5m_prec_2020-2024_TargetForDownscaling/wc2.1_cruts4.09_2.5m_prec_2020-01.tif")
    targetForDownscaling <- terra::crop(targetForDownscaling, occs_extent)
    explanatory_vars_2.5min <- resample(explanatory_vars, targetForDownscaling, filetype = "GTiff", filename = paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))
    rm(targetForDownscaling)
}else{
    explanatory_vars_2.5min <- rast(paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))
}

# Run model predictions on rasters and plot
maxent_data_winter_predictions <- predict(explanatory_vars_2.5min, maxent_data_winter, na.rm = TRUE, args = c("threads=32"))

ggplot() +
    geom_spatraster(data = maxent_data_winter_predictions, na.rm = TRUE) + 
    scale_y_continuous(limits = c(ext(maxent_data_winter_predictions)$ymin, ext(maxent_data_winter_predictions)$ymax), expand = c(0,0)) + 
    scale_x_continuous(limits = c(ext(maxent_data_winter_predictions)$xmin, ext(maxent_data_winter_predictions)$xmax), expand = c(0,0)) + 
    scale_fill_whitebox_c(palette = "muted", name = "Suitability") +
    geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
    geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
    #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
    #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
    ggtitle("MaxEnt Model, abs within 500 km buffer, >2018", paste0(speciesForModel, " winter Niche")) + 
    theme_clean() + 
    theme(plot.background = element_rect(color = "white")) #get rid of plot outline

ggsave(file.path(pathForOutput, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_winter_absBuffer500_after2015.png")), width = 9, height = 5.75, units = "in", dpi = 600)

#################################
## Using monthly data - summer ##
#################################
# Extract data from Chelsa monthly vars
pathToChelsa <- file.path("H:", "Chelsa-related", "CHELSA_monthly")
chelsa_vars <- list.dirs(pathToChelsa, full.names = FALSE)
# Remove CLT (cloud area fraction) b/c irrelevant + weird extent that differs from the other rasters
chelsa_vars <- chelsa_vars[!(chelsa_vars %in% c("", "clt"))]

# Generate Background Points; can use the backgroundSampleWithDates() function from the Torch Experiment section
# filter to summer for testing; also combine with temporal eval data for later filtering
# Chelsa only has data from 1980 - 2018 and before, so have to filter
occ_data_currentSpecies <-  rbind(GruidaeAllOcc_spacRare10km_currentSpecies, GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData) %>%
    mutate(gbifID = as.character(gbifID)) %>%
    filter(myMonth %in% c(5,6,7), myYear %in% 1980:2018) %>%
    group_by(myYear, myMonth) %>%
    backgroundSampleWithDates(dateCol = "myEventDate", clippingRaster = explanatory_vars$wc2.1_30s_elev, tries = 500, useSetBackgroundPerMonth = TRUE, setBackgroundPerMonth = 1000)

# Convert to spatVector and drop decimalLat and decimalLon
occs_currentSpecies <- occ_data_currentSpecies %>%
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>%
    select(-decimalLatitude, -decimalLongitude) %>% # ~*~*~*~*~*~ Drop these before training, but need for background point generation
    as_spatvector(crs = "EPSG:4326")

# Function to get data and sample background from montly data (also calc some new monthy "psuedo-bioclim" vars?)
# Expects spatVector of Points with a dataframe with month and year cols
extractMonthlyCHELSA <- function(.data, CHELSApath = NULL, CHELSAvars = NULL, monthCol = NULL, yearCol = NULL){
    # Check .data is valid; .data should be spatVector of Points
    if((length(.data) == 0) | (!inherits(.data, "SpatVector"))){
        stop("'.data' must be set and be of class 'SpatVector'")
    }
    if(geomtype(.data) != "points"){
        stop("'.data' is a SpatVector, but not of geomtype() 'points'. '.data.' must be geomtype() 'points'.")
    }
    if((is.null(monthCol)) | (!inherits(monthCol, "character"))){
        stop("'monthCol' must be set and be of class 'character'")
    }
    if((is.null(yearCol)) | (!inherits(yearCol, "character"))){
        stop("'yearCol' must be set and be of class 'character'")
    }
    message("Beginning data extraction...\n")
    # Extract from monthly (Dplyr groups designate months and years)
    occs_WithData <- terra::vect()
    
    # Measure runtime to report later
    startTime <- Sys.time()
    
    # For every variable, year (in the data), and month (in the data) extract data from rasters stored in CHELSApath
    years <- unique(.data[yearCol] %>% as.data.frame()) %>% unlist()
    for(ye in years){
        # First, filter data to the current year
        # Horribly ugly way of getting the subset, but I'm tired of fighting terra on this one
        thisYearData <- subset(.data, subset = (unlist(.data %>% as.data.frame() %>% select(all_of(yearCol))) == ye))
        # get months in that subset
        months <- unique(thisYearData[monthCol] %>% as.data.frame()) %>% unlist()
        for(mon in months){
            # Filter to current month; this gives us a subset that we can actually run the extraction on
            thisMonthData <- subset(thisYearData, subset = (unlist(thisYearData %>% as.data.frame() %>% select(all_of(monthCol))) == mon))
            message(paste0("\nExtracting data for year="), ye, ", month=", mon, " for CHELSA vars: ", paste(unlist(CHELSAvars), collapse = ", "), "...")
            # Fixed a problem with the pet var path by changing the name folder
            rastPaths <- file.path(pathToChelsa, chelsa_vars,paste0("CHELSA_", chelsa_vars, "_", sprintf("%02d", mon), "_", ye, "_", "v.2.1.tif"))
            # Problem with rsds path: the year and month are switched in the file name compared to the other vars (how annoying!)
            # Can fix that with a gsub call
            rastPaths <- gsub(pattern = "CHELSA_rsds_([0-9]{2})_([0-9]{4})_v.2.1.tif", replacement = "CHELSA_rsds_\\2_\\1_v.2.1.tif", rastPaths)
            # We're just extracting data, so no need to trim (but will have to for project later, at least of for years/months we want to project into)
            temp <- terra::extract(terra::rast(rastPaths), thisMonthData, ID = FALSE, bind = TRUE)
            # vars need to be renamed so rbind() can work properly (and not create 300+ columns...)
            # The 'or's ( | ) in the regex are necessary because of month and year in rsds filenames being a different order
            names(temp) <- gsub(pattern = "CHELSA_([a-zA-Z_]+)_([0-9]{2}|[0-9]{4})_([0-9]{4}|[0-9]{2})_v.2.1", replacement = "\\1", names(temp))
            message(paste0("Got data for ", length(temp), " points."))
            # Bind to occs_WithData
            occs_WithData <- rbind(occs_WithData, temp)
        }
    }
    message(paste0("\nDone. Run time: ", format(difftime(Sys.time(), startTime)), "\n"))
    return(occs_WithData)
}

# Run it
occs_currentSpecies_withData <- occs_currentSpecies %>% 
    extractMonthlyCHELSA(CHELSApath = pathToChelsa, CHELSAvars = chelsa_vars, monthCol = "myMonth", yearCol = "myYear")
# Elevation raster isn't temporal, so we can just sample and bind; might have to resample this raster during projection, though
occs_currentSpecies_withData <- terra::extract(elev_raster, occs_currentSpecies_withData, ID = FALSE, bind = TRUE)

#Quick plot for yearly datapoints
occs_currentSpecies_withData %>% 
    as.data.frame() %>% 
    group_by(myYear) %>% 
    summarize(counts = n()) %>% ggplot(mapping = aes(x = myYear, y = counts)) + 
    geom_col() + 
    scale_x_continuous(breaks = seq(min(occs_currentSpecies_withData$myYear),max(occs_currentSpecies_withData$myYear))) + 
    theme_clean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Split into train and temporal test; also call as.data.frame() since this doens't need to be spatVector right now
# Also, drop columns we don't want in the model (except pb since that IDs occ and background)
occs_currentSpecies_withData_trainTest <- occs_currentSpecies_withData %>% 
    as.data.frame(geom = "XY") %>% 
    filter(myYear > 2015) %>%
    select(-myYear, -myMonth, -species, -gbifID, -myEventDate)
occs_currentSpecies_withData_temporalTest <- occs_currentSpecies_withData %>% 
    as.data.frame(geom = "XY") %>% 
    filter(myYear <= 2015) %>% # Can't drop myYear yet for temporal!
    select(-myMonth, -species, -gbifID, -myEventDate)

# Use variance inflation factors to select vars
occs_currentSpecies_withData_trainTest_VIF <- usdm::vifstep(occs_currentSpecies_withData_trainTest %>% select(-pb, -x, -y), size = 10000)
# ... and drop the excluded vars
occs_currentSpecies_withData_trainTest <- occs_currentSpecies_withData_trainTest %>% select(-occs_currentSpecies_withData_trainTest_VIF@excluded)

# K-folds (take last K-folds model to eval for temporal for now)
# K-fold cross validation and test isolation
# Just use random folds here
monthly_background <- occs_currentSpecies_withData_trainTest %>% filter(pb == 0)
monthly_background <- cbind(monthly_background, kGroup = predicts::folds(monthly_background, k = 5))

monthly_present <- occs_currentSpecies_withData_trainTest %>% filter(pb == 1)
monthly_present <- cbind(monthly_present, kGroup = predicts::folds(monthly_present, k = 5))

# ENMEval (using hierarchical checkerboards for cross-validation)
ENMEval_summer_monthly <- ENMevaluate(occs = monthly_present %>% filter(pb == 1) %>% relocate(x, y) %>% select (-pb, -kGroup),
                                      bg = monthly_background %>% filter(pb == 0) %>% relocate(x, y) %>% select (-pb, -kGroup),
                                      algorithm = "maxent.jar",
                                      tune.args = list(fc = c("L","LQ","LQT"), rm = seq(1, 3, 0.5)),
                                      partitions = "user",
                                      user.grp = list(occs.grp = unlist(monthly_present %>% filter(pb == 1) %>% select(kGroup)),
                                                      bg.grp = unlist(monthly_background %>% filter(pb == 0) %>% select(kGroup))),
                                      taxon.name = speciesForModel,
                                      parallel = FALSE, # If TRUE: Error in unserialize(node$con) : error reading from connection
                                      other.settings = list(path = file.path(pathForOutput,"monthly"),
                                                            other.args = c("jackknife=TRUE", "responsecurves=TRUE", "threads=16"))
                                      )

# Get results dataframe and filter to best model; criteria are lowest omission rate and continuous boyce index (CBI) to break ties
ENMEval_summer_monthly_res <- eval.results(ENMEval_summer_monthly)

bestModelSelector_summer_monthly <- ENMEval_summer_monthly_res %>%
    filter(!is.na(or.10p.avg)) %>%
    filter(or.10p.avg == min(or.10p.avg))
if((nrow(bestModelSelector_summer_monthly) > 1) && !is.na(max(bestModelSelector_summer_monthly$cbi.val.avg))){ # If FNR tie, use cbi on validation to break if it exists, else use auc on validation
    bestModelSelector_summer_monthly <- bestModelSelector_summer_monthly %>% 
        filter(cbi.val.avg == max(cbi.val.avg))
}else if((nrow(bestModelSelector_summer_monthly) > 1) && !is.na(max(bestModelSelector_summer_monthly$auc.val.avg))){
    bestModelSelector_summer_monthly <- bestModelSelector_summer_monthly %>% 
        filter(auc.val.avg == max(auc.val.avg))
}
if(nrow(bestModelSelector_summer_monthly) > 1){ # If there is STILL a tie, just take the first model; these are equivalent
    bestModelSelector_summer_monthly <- bestModelSelector_summer_monthly %>% head(n = 1)
}
bestModelSelector_summer_monthly <- bestModelSelector_summer_monthly %>% 
    .$tune.args

maxent_data_monthly <- eval.models(ENMEval_summer_monthly)[[bestModelSelector_summer_monthly]]

# Modeling with MaxEnt - K-fold performance
monthly_eval_kfold <- list() # gets summary stats for each value
# Let's also fish out the FNR at the reported thresholds for max Kappa, max sum of spec and sens, and equal spec and sens
monthly_eval_kfold_FNR <-data.frame(FNR_max_kappa = numeric(), 
                                   FNR_max_spec_sens = numeric(), 
                                   FNR_equal_sens_spec = numeric(),
                                   threshold_max_kappa = numeric(), 
                                   threshold_max_spec_sens = numeric(), 
                                   threshold_equal_sens_spec = numeric())
for (i in 1:max(monthly_present$kGroup)) {
    train <- rbind(monthly_present %>% filter(kGroup!= i),
                   monthly_background %>% filter(kGroup != i))
    test <- monthly_present %>% filter(kGroup == i)
    p <- predict(object = maxent_data_monthly, test)
    # Evaluate on all background points or.... ?
    a <- predict(object = maxent_data_monthly, monthly_background)
    monthly_eval_kfold[[i]] <- pa_evaluate(p, a)
    monthly_eval_kfold_FNR[nrow(monthly_eval_kfold_FNR) + 1, ] <- c(monthly_eval_kfold[[1]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - monthly_eval_kfold[[1]]@thresholds[['max_kappa']]))) %>% 
                                                                      .$FNR,
                                                                  monthly_eval_kfold[[1]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - monthly_eval_kfold[[1]]@thresholds[['max_spec_sens']]))) %>% 
                                                                      .$FNR,
                                                                  monthly_eval_kfold[[1]]@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - monthly_eval_kfold[[1]]@thresholds[['equal_sens_spec']]))) %>% 
                                                                      .$FNR,
                                                                  monthly_eval_kfold[[1]]@thresholds[['max_kappa']],
                                                                  monthly_eval_kfold[[1]]@thresholds[['max_spec_sens']],
                                                                  monthly_eval_kfold[[1]]@thresholds[['equal_sens_spec']]
    )
}
# This gets the stats for each model and binds it to the False Negative Rate info
monthly_eval_kfold_stats <- lapply(monthly_eval_kfold, function(x){x@stats}) %>% 
    do.call(what = rbind) %>%
    as.data.frame() %>%
    cbind(run.number = 1:length(monthly_eval_kfold)) %>%
    cbind(monthly_eval_kfold_FNR)

kfolds_plot <- monthly_eval_kfold_stats %>%
    select(run.number, auc, FNR_max_spec_sens) %>% # Use FNR based on max sum of specificity and sensitivity
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = !run.number, names_to = "stat") %>%
    ggplot(mapping = aes(x = run.number, y = value, group = stat, label = round(value, 2),  fill = stat)) +
    geom_col() + 
    geom_label(fill = "white") +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0,1)) +
    facet_grid(rows = vars(stat)) + 
    theme_light() + 
    ggtitle("K-fold Performance On Monthly Data", paste0("FNR; threshold at max(specificity + sensitivity)=", round(monthly_eval_kfold[[1]]@thresholds[['max_spec_sens']], 3)))
kfolds_plot

# Last MaxEnt Model from Kfolds is stored in maxent_data_monthly. Let's evaluate it on the temporal test set
# Evaluate Temporal Test Data with MaxEnt Model
# TESTING - on this small dataset, performance on Kfolds is inconsistent; test train on full
monthly_eval_temporal <- data.frame(year =  numeric(), 
                                   testN = numeric(), 
                                   backgroundN = numeric(), 
                                   auc = numeric(),
                                   FNR_max_kappa = numeric(), 
                                   FNR_max_spec_sens = numeric(), 
                                   FNR_equal_sens_spec = numeric(),
                                   threshold_max_kappa = numeric(), 
                                   threshold_max_spec_sens = numeric(), 
                                   threshold_equal_sens_spec = numeric())
for(ye in sort(unique(occs_currentSpecies_withData_temporalTest$myYear))){
    test <- occs_currentSpecies_withData_temporalTest %>% 
        filter(myYear == ye, pb == 1) %>% select(-pb, -myYear)
    bg <- occs_currentSpecies_withData_temporalTest %>% 
        filter(myYear == ye, pb == 0) %>% select(-pb, -myYear)
    if(nrow(test %>% drop_na()) == 0){
        next # Protection against instances where there are only a few occurrences and that may drop to zero when some vars are missing data
    }
    p <- predict(object = maxent_data_monthly, test)
    # Evaluate on all background points or.... ?
    a <- predict(object = maxent_data_monthly, bg)
    # Bind results to next row of dataframe
    res <- pa_evaluate(p, a)
    monthly_eval_temporal[nrow(monthly_eval_temporal) + 1, ] <- c(ye, 
                                                                nrow(test), 
                                                                nrow(bg), 
                                                                res@stats$auc[[1]],
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['max_kappa']]))) %>% 
                                                                    .$FNR,
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['max_spec_sens']]))) %>% 
                                                                    .$FNR,
                                                                res@tr_stats %>% 
                                                                    slice(which.min(abs(treshold - res@thresholds[['equal_sens_spec']]))) %>% 
                                                                    .$FNR,
                                                                res@thresholds[['max_kappa']],
                                                                res@thresholds[['max_spec_sens']],
                                                                res@thresholds[['equal_sens_spec']])
}

# Temporal performance plot and combo plot
temporal_plot <- monthly_eval_temporal %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>%
    ggplot(mapping = aes(x = year, y = value, label = testN)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    ggrepel::geom_label_repel(size = 1.5, direction = "both", fill = "white", box.padding = 0.25, max.overlaps = Inf, min.segment.length = 0, segment.linetype = 1, segment.size = 0.2, segment.color = "grey", label.size = 0.1, label.padding = 0.1) +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("Past years; Trained Monthly"))
temporal_plot

p_combo <- plot_grid(kfolds_plot, temporal_plot, labels = c("A", "B"), label_size = 20, rel_heights = c(1,1))
p_combo <- p_combo + theme(plot.background = element_rect(fill = "white", color = "white")) #get rid of transparency
p_combo
ggsave(filename = file.path(pathForOutput, paste0("MaxEnt_Model_trainedOnMonthly_", gsub(" ", "_", speciesForModel), "_monthly_kfolds_and_temporal.png")), plot = p_combo, , width = 8, height = 4, units = "in", dpi = 600)

#########################
## /Using monthly data ##
#########################

#########################
## Torch Experiment #####
#########################
# Drop the one sample in Balearica pavonina that's causing issues
if (speciesForModel %in% c("Balearica pavonina")){
    bad_row <- GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData %>% 
        filter(myYear == 2005, myMonth == 8)
    fwrite(bad_row, file = file.path(pathForOutput, "bad_row_removed_from_nnTemporalTestData.csv"))
    GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData <- GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData %>%
        filter(gbifID != 1579859651) # Only hits the one row
}

# Drop the one sample in Grus monacha that's causing issues
if (speciesForModel %in% c("Grus monacha")){
    bad_row <- GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData %>% 
        filter(myYear == 2010, myMonth == 3)
    fwrite(bad_row, file = file.path(pathForOutput, "bad_row_removed_from_nnTemporalTestData.csv"))
    GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData <- GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData %>%
        filter(gbifID != 2790072070) # Only hits the one row
}

# Test run of julian-date-aware neural network
# Get background points; shouldn't have split these into before 2018 and after to begin with, so recombine then resplit...
occ_data_all <-  rbind(GruidaeAllOcc_spacRare10km_currentSpecies, 
                       GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData) %>%
    mutate(gbifID = as.character(gbifID)) %>%
    group_by(myYear, myMonth) %>%
    backgroundSampleWithDates(dateCol = "myEventDate", clippingRaster = explanatory_vars$wc2.1_30s_elev, tries = 500, useSetBackgroundPerMonth = TRUE, setBackgroundPerMonth = 1000, warn = 2)

# Convert to spatVector and drop decimalLat and decimalLon
occs_all <- occ_data_all %>%
    mutate(lon = decimalLongitude, lat = decimalLatitude) %>%
    select(-decimalLatitude, -decimalLongitude) %>% # ~*~*~*~*~*~ Drop these before training, but need for background point generation
    as_spatvector(crs = "EPSG:4326")

# Extract data from explanatory vars
occ_data_all <- terra::extract(explanatory_vars, occs_all, ID = FALSE, bind = TRUE) %>% as.data.frame()

# sine and cosine transformed Julian day; this gives the model a way of learning that
# dates in December are "near" dates in January
occ_data_all <- occ_data_all %>%
    mutate(sine.julian = sin(lubridate::yday(myEventDate)/365 * 2 * pi),
           cosine.julian = cos(lubridate::yday(myEventDate)/365 * 2 * pi))

# dataset without metadata (except myYear; need that for filtering)
occ_data_all_Torch <- occ_data_all %>% select(-species, -gbifID, -myEventDate)

# TODO add kfold (random) here; will need x, y if using enmEval, so conversion to dataframe above will need geom="XY"; select below will also need to be fixed

# Use variance inflation factors to select which variables to use in the model
keeper_patterns_Torch <- c("sine.julian", "cosine.julian", "wc2.1_30s_elev")
occ_data_all_VIF <- usdm::vifstep(occ_data_all_Torch %>% select(-pb, -myYear, -myMonth), size = 50000, keep = unique(grep(paste(keeper_patterns_Torch, collapse="|"), names(occ_data_all_Torch), value=TRUE)))
# ... and drop the excluded vars
occ_data_all_Torch <-occ_data_all_Torch %>% select(-occ_data_all_VIF@excluded)

# Scale sine and cosine julian dates by 100 and round to zero decimals (increase variance + slight smoothing, effectively)
# Also, make elevation a 'numeric' (Torch doesn't like mixing floats and ints)
occ_data_all_Torch <- occ_data_all_Torch %>% 
    mutate(sine.julian = round(sine.julian * 100, 0),
           cosine.julian = round(cosine.julian * 100, 0)) %>%
    mutate(wc2.1_30s_elev = as.numeric(wc2.1_30s_elev))

# Isolate data prior to 2018 entirely, and drop myYear
occ_data_all_Torch_after2018 <- occ_data_all_Torch %>% filter(myYear >= 2018) %>% select(-myYear,-myMonth) %>% mutate(pb = as.factor(pb))
occ_data_all_Torch_before2018 <- occ_data_all_Torch %>% filter(myYear < 2018) %>% mutate(pb = as.factor(pb))

# Okay, Torch time...
# After playing with torch for a while, found the tidymodels and tabnet which cut out a lot of boilerplate (VERY nice)
# Function to create datasets for both training and validation (validation is just 20% subset)
# This also puts the tensors on the GPU, so this needs to run on my PC (or something with an NVIDIA GPU, at least)

# split training and validation (need to add temp row id for quick anti_join)
occ_data_all_Torch_after2018 <- occ_data_all_Torch_after2018 %>% mutate(rowID = row_number())

# Checked the below split with setdiff(occ_data_all_Torch_after2018, rbind(occ_data_all_Torch_after2018_train, occ_data_all_Torch_after2018_valid))
# So, the two subsets do union to the original dataset exactly
occ_data_all_Torch_after2018_train <- occ_data_all_Torch_after2018 %>%
    slice_sample(prop = 0.8, by = pb)

occ_data_all_Torch_after2018_valid <- occ_data_all_Torch_after2018 %>% 
    anti_join(occ_data_all_Torch_after2018_train, by = "rowID")

occ_data_all_Torch_after2018_train <- occ_data_all_Torch_after2018_train %>% select(-rowID)
occ_data_all_Torch_after2018_valid <- occ_data_all_Torch_after2018_valid %>% select(-rowID)

# make tidymodels recipe: pb is predicted by all other vars
# TODO roles in recipe() can define a case_weight. Maybe useable with 
cranes_model_recipe <- recipe(pb ~ ., occ_data_all_Torch_after2018_train)

# unfortunately, I cannot figure out how to make R's predict() work with a tabnet model on the GPU
# Everything works until the predict() call which fails due to the input data not being on the GPU
# SO, need to move it to the cpu using device= in the set_engine() call below
# TODO Running this on cpu rather than cuda is a loss; figure out how to not have to do this

# hyperparameter settings and tuning
# some are tuned; others follow example from https://blogs.rstudio.com/ai/posts/2021-02-11-tabnet/
# TODO figure out more of how these parameters and the above grid of possibilities impact the model
# Epochs here are just for hyperparameter tuning; increase to larger value on final train
cranes_tabnet_model <- tabnet(epochs = 50, 
                              decision_width = tune(), 
                              attention_width = tune(), 
                              num_steps = tune(), 
                              penalty = tune(), #0.000001, 
                              momentum = tune(), 
                              feature_reusage = 1.5, 
                              checkpoint_epochs = 20,
                              learn_rate = tune()) %>%
    set_engine("torch", device = "cpu") %>%
    set_mode("classification")

# Set number of threads and garbage collection threshold (remove thread limits for GPU run? I don't even know if these affect that)
#torch::torch_set_num_threads(16)
#torch::torch_set_num_interop_threads(16)
#options(torch.threshold_call_gc = 2000)

# Bundle into tabnet workflow
cranes_tabnet_wf <- workflow() %>% 
    add_model(cranes_tabnet_model) %>% 
    add_recipe(cranes_model_recipe)

# Hyperparameter tuning grid (combinations of parameters)
hp_tuning_grid <- cranes_tabnet_wf %>% 
    extract_parameter_set_dials() %>% 
    update(decision_width = decision_width(range = c(10, 40)), 
           attention_width = attention_width(range = c(8, 30)), 
           momentum = momentum(range = c(0.01,0.06)),
           penalty = penalty(range = c(-3,-7)),
           num_steps = num_steps(range = c(2, 6)), 
           learn_rate = learn_rate(range = c(-2.5, -1))) %>% 
    grid_space_filling(size = 20)


# The tuning and results
tuning_ctrl <- control_race(verbose_elim = TRUE, event_level = "second")
tuning_folds <- vfold_cv(occ_data_all_Torch_after2018_train, v = 4, strata = pb)
set.seed(33)

tuning_res <- cranes_tabnet_wf %>% 
    tune_race_anova(
        resamples = tuning_folds,
        grid = hp_tuning_grid,
        control = tuning_ctrl
    )

tuning_res_top <- tuning_res %>% 
    show_best(metric = "roc_auc", n = 10) %>% 
    select(- c(.estimator, .config))
tuning_res_top
tuning_res_best <- tuning_res %>% select_best(metric = "roc_auc")

# define final model
cranes_tabnet_model_final <- tabnet(epochs = 1000, 
                              batch_size = 100000, # prevents memory blowing up during prediction? Maybe helping?
                              decision_width = tuning_res_best$decision_width[[1]], 
                              attention_width = tuning_res_best$attention_width[[1]], 
                              num_steps = tuning_res_best$num_steps[[1]], 
                              penalty = tuning_res_best$penalty[[1]], 
                              #virtual_batch_size = 500, # Just defer to default here?
                              momentum = tuning_res_best$momentum[[1]], 
                              checkpoint_epochs = 50,
                              feature_reusage = 1.5, 
                              #loss = torch::nn_aum_loss, # Experimental loss function; good for imbalance?
                              learn_rate = tuning_res_best$learn_rate[[1]],
                              verbose = TRUE) %>%
    set_engine("torch", device = "cpu") %>%
    set_mode("classification")

# TESTING ~*~*~ Remove / comment out
# cranes_tabnet_model_final <- tabnet(epochs = 100, 
#                                     decision_width = 8, 
#                                     attention_width = 8, 
#                                     num_steps = 3, 
#                                     penalty = 0.000001, 
#                                     momentum = 0.02, 
#                                     feature_reusage = 1.5, 
#                                     learn_rate = 0.1,
#                                     verbose = TRUE,
#                                     optimizer = "adam") %>%
#     set_engine("torch", device = "cpu") %>%
#     set_mode("classification")

# ... and put it in a workflow
cranes_tabnet_wf_final <- workflow() %>% 
    add_model(cranes_tabnet_model_final) %>% 
    add_recipe(cranes_model_recipe)

# Fit the model
cranes_fitted_nn <- cranes_tabnet_wf_final %>%
    fit(occ_data_all_Torch_after2018_train)

# Calc accuracy on test data
preds <- occ_data_all_Torch_after2018_valid %>% 
    bind_cols(predict(cranes_fitted_nn, occ_data_all_Torch_after2018_valid, type = "prob"))

nn_AUC_on_Validation <- yardstick::roc_auc(data = preds, truth = pb, event_level = "second", .pred_1)
nn_AUC_on_Validation

# Pred_wrapper function
myPredWrapper <- function(object, newdata){
        unlist(predict(object, newdata, type = "prob")$.pred_1)
}

# Make file path for output for TabNet related things
filePath_nn <- file.path(pathForOutput, "neural_network")

if(!dir.exists(filePath_nn)){
    dir.create(filePath_nn)
}

# Visualize importance using reduction in model AUC when data permuted
fit_for_vip <- extract_fit_parsnip(cranes_fitted_nn)
var_import <- vi(fit_for_vip, method = "permute",
                 train = occ_data_all_Torch_after2018_train,
                 target = "pb",
                 event_level = "second",
                 metric = yardstick::roc_auc_vec,
                 pred_wrapper = myPredWrapper,
                 smaller_is_better = FALSE,
                 keep = TRUE,
                 nsim = 30)

vip(var_import, 
    geom = "boxplot",
    aesthetics = list(fatten = 2, outlier.size = 0.2, linewidth = 0.3),
    num_features = 99) + 
    scale_y_reverse() +
    scale_x_discrete(limits = rev) +
    ylab("Reduction in ROC-AUC") +
    ggtitle("Predictor Importance", "(n=30 Permutations)") + 
    theme_light()
ggsave(file.path(filePath_nn, paste0("TabNet_Model_", gsub(" ", "_", speciesForModel), "_variable_importance.png")), width = 7, height = 4.25, units = "in", dpi = 600)

# Temporal eval on data from past years
# We can use yardstick to fetch a lot of the same data as we were using to evaluate the other models
nn_eval_temporal_year <- data.frame(year =  numeric(), 
                                    testN = numeric(), 
                                    backgroundN = numeric(), 
                                    auc = numeric(),
                                    FNR_max_kappa = numeric(), 
                                    FNR_max_spec_sens = numeric(), 
                                    FNR_equal_sens_spec = numeric(),
                                    threshold_max_kappa = numeric(), 
                                    threshold_max_spec_sens = numeric(), 
                                    threshold_equal_sens_spec = numeric())
for(ye in sort(unique(occ_data_all_Torch_before2018$myYear))){
    test <- occ_data_all_Torch_before2018 %>% 
        filter(myYear == ye, pb == 1) %>% select(-pb, -myYear, -myMonth)
    if(nrow(test)==0){next} # skip if nothing to eval
    bg <- occ_data_all_Torch_before2018 %>% 
        filter(myYear == ye, pb == 0) %>% select(-pb, -myYear, -myMonth)
    p <- predict(object = cranes_fitted_nn, test, type = "prob")$.pred_1
    a <- predict(object = cranes_fitted_nn, bg, type = "prob")$.pred_1
    # Bind results to next row of dataframe
    res <- pa_evaluate(p, a)
    nn_eval_temporal_year[nrow(nn_eval_temporal_year) + 1, ] <- c(ye, 
                                                                  nrow(test), 
                                                                  nrow(bg), 
                                                                  res@stats$auc[[1]],
                                                                  res@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - res@thresholds[['max_kappa']]))) %>% 
                                                                      .$FNR,
                                                                  res@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - res@thresholds[['max_spec_sens']]))) %>% 
                                                                      .$FNR,
                                                                  res@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - res@thresholds[['equal_sens_spec']]))) %>% 
                                                                      .$FNR,
                                                                  res@thresholds[['max_kappa']],
                                                                  res@thresholds[['max_spec_sens']],
                                                                  res@thresholds[['equal_sens_spec']])
}

# Temporal performance plot (year round data from past years)
temporal_plot_nn <- nn_eval_temporal_year %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>%
    ggplot(mapping = aes(x = year, y = value, label = testN)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    ggrepel::geom_label_repel(size = 1.5, direction = "both", fill = "white", box.padding = 0.25, max.overlaps = Inf, min.segment.length = 0, segment.linetype = 1, segment.size = 0.2, segment.color = "grey", label.size = 0.1, label.padding = 0.1) +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("TabNet Model Trained / Evaluated on Year-Round Data (Past years)"))
temporal_plot_nn
ggsave(file.path(filePath_nn, paste0("TabNet_Model_", gsub(" ", "_", speciesForModel), "_temporal_plot_yearRoundEval.png")), width = 7, height = 4.25, units = "in", dpi = 600)

# Past years eval, but only using breeding season data
nn_eval_temporal_summerOnly <- data.frame(year =  numeric(), 
                                    testN = numeric(), 
                                    backgroundN = numeric(), 
                                    auc = numeric(),
                                    FNR_max_kappa = numeric(), 
                                    FNR_max_spec_sens = numeric(), 
                                    FNR_equal_sens_spec = numeric(),
                                    threshold_max_kappa = numeric(), 
                                    threshold_max_spec_sens = numeric(), 
                                    threshold_equal_sens_spec = numeric())
for(ye in sort(unique(occ_data_all_Torch_before2018$myYear))){
    test <- occ_data_all_Torch_before2018 %>%
        filter(myYear == ye, pb == 1, myMonth %in% breedingSeason) %>% select(-pb, -myYear, -myMonth)
    if(nrow(test)==0){next} # skip if nothing to eval
    bg <- occ_data_all_Torch_before2018 %>% 
        filter(myYear == ye, pb == 0, myMonth %in% breedingSeason) %>% select(-pb, -myYear, -myMonth)
    p <- predict(object = cranes_fitted_nn, test, type = "prob")$.pred_1
    a <- predict(object = cranes_fitted_nn, bg, type = "prob")$.pred_1
    # Bind results to next row of dataframe
    res <- pa_evaluate(p, a)
    nn_eval_temporal_summerOnly [nrow(nn_eval_temporal_summerOnly ) + 1, ] <- c(ye, 
                                                                  nrow(test), 
                                                                  nrow(bg), 
                                                                  res@stats$auc[[1]],
                                                                  res@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - res@thresholds[['max_kappa']]))) %>% 
                                                                      .$FNR,
                                                                  res@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - res@thresholds[['max_spec_sens']]))) %>% 
                                                                      .$FNR,
                                                                  res@tr_stats %>% 
                                                                      slice(which.min(abs(treshold - res@thresholds[['equal_sens_spec']]))) %>% 
                                                                      .$FNR,
                                                                  res@thresholds[['max_kappa']],
                                                                  res@thresholds[['max_spec_sens']],
                                                                  res@thresholds[['equal_sens_spec']])
}

# Temporal performance plot (limited to breeding season)
temporal_plot_nn_breeding <- nn_eval_temporal_summerOnly %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>%
    ggplot(mapping = aes(x = year, y = value, label = testN)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    ggrepel::geom_label_repel(size = 1.5, direction = "both", fill = "white", box.padding = 0.25, max.overlaps = Inf, min.segment.length = 0, segment.linetype = 1, segment.size = 0.2, segment.color = "grey", label.size = 0.1, label.padding = 0.1) +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("TabNet Model (Past years, breeding season only)"))
temporal_plot_nn_breeding
ggsave(file.path(filePath_nn, paste0("TabNet_Model_", gsub(" ", "_", speciesForModel), "_temporal_plot_breedingOnlyEval.png")), width = 7, height = 4.25, units = "in", dpi = 600)


# Past years eval, but only using breeding season data
nn_eval_temporal_winterOnly <- data.frame(year =  numeric(), 
                                          testN = numeric(), 
                                          backgroundN = numeric(), 
                                          auc = numeric(),
                                          FNR_max_kappa = numeric(), 
                                          FNR_max_spec_sens = numeric(), 
                                          FNR_equal_sens_spec = numeric(),
                                          threshold_max_kappa = numeric(), 
                                          threshold_max_spec_sens = numeric(), 
                                          threshold_equal_sens_spec = numeric())
for(ye in sort(unique(occ_data_all_Torch_before2018$myYear))){
    test <- occ_data_all_Torch_before2018 %>%
        filter(myYear == ye, pb == 1, myMonth %in% winteringSeason) %>% select(-pb, -myYear, -myMonth)
    if(nrow(test)==0){next} # skip if nothing to eval
    bg <- occ_data_all_Torch_before2018 %>% 
        filter(myYear == ye, pb == 0, myMonth %in% winteringSeason) %>% select(-pb, -myYear, -myMonth)
    p <- predict(object = cranes_fitted_nn, test, type = "prob")$.pred_1
    a <- predict(object = cranes_fitted_nn, bg, type = "prob")$.pred_1
    # Bind results to next row of dataframe
    res <- pa_evaluate(p, a)
    nn_eval_temporal_winterOnly [nrow(nn_eval_temporal_winterOnly ) + 1, ] <- c(ye, 
                                                                                nrow(test), 
                                                                                nrow(bg), 
                                                                                res@stats$auc[[1]],
                                                                                res@tr_stats %>% 
                                                                                    slice(which.min(abs(treshold - res@thresholds[['max_kappa']]))) %>% 
                                                                                    .$FNR,
                                                                                res@tr_stats %>% 
                                                                                    slice(which.min(abs(treshold - res@thresholds[['max_spec_sens']]))) %>% 
                                                                                    .$FNR,
                                                                                res@tr_stats %>% 
                                                                                    slice(which.min(abs(treshold - res@thresholds[['equal_sens_spec']]))) %>% 
                                                                                    .$FNR,
                                                                                res@thresholds[['max_kappa']],
                                                                                res@thresholds[['max_spec_sens']],
                                                                                res@thresholds[['equal_sens_spec']])
}

# Temporal performance plot (limited to wintering season)
temporal_plot_nn_wintering <- nn_eval_temporal_winterOnly %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>%
    ggplot(mapping = aes(x = year, y = value, label = testN)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    ggrepel::geom_label_repel(size = 1.5, direction = "both", fill = "white", box.padding = 0.25, max.overlaps = Inf, min.segment.length = 0, segment.linetype = 1, segment.size = 0.2, segment.color = "grey", label.size = 0.1, label.padding = 0.1) +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("TabNet Model (Past years, wintering season only)"))
temporal_plot_nn_wintering
ggsave(file.path(filePath_nn, paste0("TabNet_Model_", gsub(" ", "_", speciesForModel), "_temporal_plot_winteringOnlyEval.png")), width = 7, height = 4.25, units = "in", dpi = 600)

# Fix the names for projection rasters (why do R packages silently change names so often? Maddening!)
names(explanatory_vars_2.5min) <- gsub(pattern = "([0-9]{4})-([0-9]{4})", replacement = "\\1.\\2", names(explanatory_vars_2.5min))

# Remove unused variables and rows containing NAs
# explanatory_vars_2.5min_withJulian_slim <- explanatory_vars_2.5min_withJulian %>% 
#     select(names(occ_data_all_Torch_after2018_train %>% select(-pb))) %>%
#     drop_na()

explanatory_vars_2.5min_slim <- explanatory_vars_2.5min %>% 
    select(names(occ_data_all_Torch_after2018_train %>% select(-pb, -sine.julian, -cosine.julian))) %>%
    drop_na()

gc() # gc() call here because *weird* memory issues during prediction the first time I ran this

# set filepath for frames
filePath_nn_frames <- file.path(filePath_nn, "frames")

if(!dir.exists(filePath_nn_frames)){
    dir.create(filePath_nn_frames)
}

# Loop for creating prediction rasters for each week of the year
weekly_prediction_rasters <- list()
week_seq <- seq(0,51)
# index weekly_prediction_rasters is week + 1
for(week in week_seq){
    nn_data_currWeek_predictions <- terra::predict(explanatory_vars_2.5min_slim, 
                                                   cranes_fitted_nn, type = "prob", 
                                                   na.rm = TRUE,
                                                   const = data.frame(sine.julian = round(sin((week * 7 + 1) / 365 * 2 * pi) * 100, 0),
                                                                      cosine.julian = round(cos((week * 7 + 1) / 365 * 2 * pi) * 100, 0)),
                                                   filename = file.path(filePath_nn_frames, paste0("nn_model_pred_week", sprintf("%02d", week), ".geotif")),
                                                   overwrite = TRUE,
                                                   wopt = list(filetype = "GTiff",
                                                               memmax = 20,
                                                               steps = 50))
    
    weekly_prediction_rasters[[week+1]] <- nn_data_currWeek_predictions$.pred_1
}

# save some fancy plots
for(week in week_seq){
    week_plot <- ggplot() +
        geom_spatraster(data = weekly_prediction_rasters[[week + 1]], na.rm = TRUE) + 
        scale_y_continuous(limits = c(ext(weekly_prediction_rasters[[week + 1]])$ymin, ext(weekly_prediction_rasters[[week + 1]])$ymax), expand = c(0,0)) + 
        scale_x_continuous(limits = c(ext(weekly_prediction_rasters[[week + 1]])$xmin, ext(weekly_prediction_rasters[[week + 1]])$xmax), expand = c(0,0)) + 
        scale_fill_whitebox_c(palette = "muted", name = "Suitability") +
        geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
        geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
        #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
        #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
        ggtitle("TabNet Model, abs within 500 km buffer, >2018", paste0(speciesForModel, " Week ", week)) + 
        theme_clean() + 
        theme(plot.background = element_rect(color = "white"))
    ggsave(plot = week_plot, file.path(filePath_nn_frames, paste0("TabNet_Model_", gsub(" ", "_", speciesForModel), "_week", sprintf("%02d", week), ".png")), width = 7, height = 4.25, units = "in", dpi = 600)
}

# Make a gif (uses Magick package)
week_plots_png_list <- list.files(filePath_nn_frames, pattern = "\\.png$", full.names = TRUE)
week_plots_png_joined <- image_join(lapply(week_plots_png_list, image_read))
week_plots_png_animated <- image_animate(week_plots_png_joined, fps = 4)
image_write(image = week_plots_png_animated, path = file.path(filePath_nn_frames, "weekly_plots_animated.gif"))
#image_write_video(image = week_plots_png_animated, path = file.path(filePath_nn_frames, "weekly_plots_animated.mp4"))

rm(week_plots_png_animated, week_plots_png_joined) #dump these before writing the .RData file (they're huge)

#########################
## /Torch Experiment ####
#########################

# Final extra plots
# Maxent summer vs nn summer
combined_maxent_nn_summer_temporal_eval <- rbind(summer_eval_temporal %>% mutate(model = "Maxent"), 
                                                 nn_eval_temporal_summerOnly %>% mutate(model = "TabNet"))
temporal_plot_summerEval_combo <- combined_maxent_nn_summer_temporal_eval %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens, model) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>% group_by(model) %>%
    ggplot(mapping = aes(x = year, y = value, group = model, colour = model)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("MaxEnt and TabNet Models (Past years, breeding season only)"))
temporal_plot_summerEval_combo
ggsave(file.path(filePath_nn, paste0("TabNet_and_Maxent_Model_", gsub(" ", "_", speciesForModel), "_temporal_plot_breedingOnlyEval.png")), width = 7, height = 4.25, units = "in", dpi = 600)

# Maxent winter vs nn winter
combined_maxent_nn_winter_temporal_eval <- rbind(winter_eval_temporal %>% mutate(model = "Maxent"), 
                                                 nn_eval_temporal_winterOnly %>% mutate(model = "TabNet"))
temporal_plot_winterEval_combo <- combined_maxent_nn_winter_temporal_eval %>%
    select(year, testN, backgroundN, auc, FNR_max_spec_sens, model) %>% 
    rename(FNR = FNR_max_spec_sens) %>%
    pivot_longer(cols = c(auc, FNR), names_to = "stat") %>% group_by(model) %>%
    ggplot(mapping = aes(x = year, y = value, group = model, colour = model)) +
    geom_line(linewidth = 0.25) + 
    geom_point(size = 1) +
    facet_grid(rows = vars(stat), scales = "free_y") +
    theme_clean() +
    scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0,1)) +
    theme(plot.background = element_rect(fill = "white", color = "white")) + #get rid of transparency
    ggtitle("ROC-AUC", paste0("MaxEnt and TabNet Models (Past years, wintering season only)"))
temporal_plot_winterEval_combo
ggsave(file.path(filePath_nn, paste0("TabNet_and_Maxent_Model_", gsub(" ", "_", speciesForModel), "_temporal_plot_winteringOnlyEval.png")), width = 7, height = 4.25, units = "in", dpi = 600)


##########################
### SAVE THE WORKSPACE ###
##########################
# Only save things that could be useful (and that aren't just references, like the rasters)
save(cranes_fitted_nn,
     cranes_tabnet_model_final,
     ENMEval_summer_monthly_res,
     ENMEval_summer_res,
     ENMEval_winter_res,
     GruidaeAllOcc_spacRare10km_currentSpecies,
     GruidaeAllOcc_spacRare10km_currentSpecies_temporalTestData,
     hp_tuning_grid,
     maxent_data_monthly,
     maxent_data_summer,
     maxent_data_winter,
     nn_eval_temporal_summerOnly,
     nn_eval_temporal_winterOnly,
     nn_eval_temporal_year,
     occs_extent,
     sdm_data_summer,
     sdm_data_winter,
     summer_eval_kfold_FNR,
     summer_eval_kfold_stats,
     summer_eval_temporal,
     var_import,
     winter_eval_kfold_FNR,
     winter_eval_kfold_stats,
     winter_eval_temporal,
     breedingSeason,
     winteringSeason,
     nn_AUC_on_Validation,
     file = file.path(pathForOutput, paste0(gsub(" ", "_", speciesForModel), ".RData")))

# TabNet explain - Random sample of 1000 occurences from training
explain_obj <- tabnet_explain(extract_fit_engine(cranes_fitted_nn), 
                              occ_data_all_Torch_after2018_train %>% 
                                  filter(pb == 1) %>% 
                                  slice_sample(n = 500))
explain_plot_steps <- autoplot(explain_obj, type = "steps") +
    ggtitle(paste0(speciesForModel, " Feature Importance Masks"), "(n=500 random samples)") + 
    xlab("Sample") + 
    ylab("") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
explain_plot_steps
ggsave(file.path(filePath_nn, paste0("TabNet_Explain_", gsub(" ", "_", speciesForModel), "_trainingData_withSteps.png")), width = 7.5, height = 4.25, units = "in", dpi = 300)

# Tabnet training loss by epoch
fit_for_vip_No_Checkpoints <- fit_for_vip
# Remove the annoying checkpoint markers...
temp <- fit_for_vip$fit$fit$metrics
for(i in 1:length(temp)){
    temp[[i]]$checkpoint <- NULL
}
fit_for_vip_No_Checkpoints$fit$fit$metrics <- temp
rm(temp)
#plot it
autoplot(fit_for_vip_No_Checkpoints) + 
    theme_clean()
ggsave(file.path(filePath_nn, paste0("TabNet_Explain_", gsub(" ", "_", speciesForModel), "_trainingDataLossOverEpochs.png")), width = 6, height = 4.25, units = "in", dpi = 300)


# END OF MODELING LOOP
}

#####################################################################################
## Below is unused google earth engine + dynamicSDM approach that didn't work out
## Keeping this to remind me how I got rgee working...
## This was mainly abandoned due to having to mix the terra and raster libraries which was very confusing
## rgee is also a bit broken...
#
# Raster library has to be loaded for conversion from SpatVector to SpatialPoints
# Annoying! See: https://github.com/rspatial/terra/issues/600
#temp_spatialPoints <- as(temp, "Spatial")
#Ggrus_buffer <- dismo::circles(p = temp_spatialPoints, d = 300000, lonlat = TRUE, dissolve = TRUE)

# My own terra version
#Ggrus_buffer_terra <- terra::buffer(temp, width = 300000)

# Extract explanatory data (Google Earth Engine)
# This didn't work at all!
# I'm leaving this code here to remind myself how I fixed rgee (after setting up a conda environment for it)
# Had to downgrade the earthengine-api in my conda environment to 0.1.370 to make any of this work
# Solution was via this github thread https://github.com/r-spatial/rgee/issues/353#issuecomment-1983765552
#
# rgee::ee_install_set_pyenv(
#     py_path = file.path("C:", "Users", "Conrad", "miniconda3", "envs", "rgee_env"),
#     py_env = "rgee_env"
# )
# # had to rewrite part of this rgee function make rgee work...
# ee_check_python <- function (quiet = FALSE) 
# {
#     python_test <- reticulate::py_available(initialize = TRUE)
#     if (python_test) {
#         py_version <- as.character(reticulate::py_discover_config()[["version"]])
#         if (!quiet) {
#             cli::cat_line("  Python version\n", 
#                      "[Ok]", "", reticulate::py_discover_config()[["python"]], 
#                       sprintf("v%s", py_version))
#         }
#     }
#     else {
#         stop("Unable to find a Python version, you will need to fix before run ", 
#              "rgee::ee_Initialize(). For more details run reticulate::py_available()")
#     }
#     if (utils::compareVersion(py_version, "3.5") == -1) {
#         stop("rgee needs Python 3.5 >=")
#     }
#     return(invisible(TRUE))
# }
# assignInNamespace("ee_check_python", ee_check_python, ns = "rgee")
# 
# ee_Initialize(user="radwil@gmail.com")
# 
# temp_occData_extracted <- extract_dynamic_coords(occ.data = Ggrus, datasetname = "UCSB-CHG/CHIRPS/DAILY", bandname = "precipitation", temporal.res = 30, temporal.direction = "prior", GEE.math.fun = "sum", save.directory = file.path("H:", "Crane_SDMs", "EE_Experiments"), spatial.res.metres = 10000, save.method = "combined")
# temp_pseudoData_extracted <- extract_dynamic_coords()
