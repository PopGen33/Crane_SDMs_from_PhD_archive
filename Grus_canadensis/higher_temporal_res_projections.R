# Projection into future under
# Runs from the directory with the species' .RData file

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

# set working directory
setwd("H:/Crane_SDMs")

# set species name
speciesForModel <- "Grus canadensis"

# set output path
pathForOutput <- file.path("H:", "Crane_SDMs", gsub(" ", "_", speciesForModel))

# Load models
load(file.path(pathForOutput,"Grus_canadensis.RData"))

# Make file path for output for TabNet related things
filePath_proj <- file.path(pathForOutput, "higher_res_projection")

if(!dir.exists(filePath_proj)){
    dir.create(filePath_proj)
}

# occs_extent was apparently a pointer rather than an R object, so have to get the extent again
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

# Load environmental data for GFDL-ESM4 for SSP1-RCP2.6 and SSP5-RCP8.5 for time period 2071-2100
#  https://www.gfdl.noaa.gov/earth-system-esm4/

if(!file.exists(paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP1.tif")) | !file.exists(paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP5.tif"))){
    # SSP 1
    bioclim_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1", full.names = TRUE, pattern = ".+_bio[0-9]{1,2}_.+"))
    # Climate Moisture Index NOT available for projection!
    cmi_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_cmi_.+"))
    if(file.exists("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/ndd5_NA0.geotif")){
        gdd5_rasters <- terra::rast("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/ndd5_NA0.geotif")
    }else{
        gdd5_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1", full.names = TRUE, pattern = ".+_gdd5_.+"))
        gdd5_rasters[is.na(gdd5_rasters)] <- 0 # Set NA to 0
        writeRaster(x = gdd5_rasters, filename = "H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/ndd5_NA0.geotif", filetype = "GTiff")
    }
    if(file.exists("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/gsl_NA0.geotif")){
        gsl_rasters <- terra::rast("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/gsl_NA0.geotif")
    }else{
        gsl_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1", full.names = TRUE, pattern = ".+_gsl_.+"))
        gsl_rasters[is.na(gsl_rasters)] <- 0 # Set NA to 0
        writeRaster(x = gsl_rasters, filename = "H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/gsl_NA0.geotif", filetype = "GTiff")
    }
    # Relative humidity NOT available for projection!
    hurs_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_hurs_.+"))
    if(file.exists("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/ngd5_NA0.geotif")){
        ngd5_rasters <- terra::rast("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/ngd5_NA0.geotif")
    }else{
        ngd5_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1", full.names = TRUE, pattern = ".+_ngd5_.+"))
        ngd5_rasters[is.na(ngd5_rasters)] <- 0 # Set NA to 0
        writeRaster(x = ngd5_rasters, filename = "H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1/ngd5_NA0.geotif", filetype = "GTiff")
    }
    scd_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP1", full.names = TRUE, pattern = ".+_scd_.+"))
    
    bioclim_rasters <- c(bioclim_rasters, cmi_rasters, gdd5_rasters, gsl_rasters, hurs_rasters, ngd5_rasters, scd_rasters)
    rm(cmi_rasters, gdd5_rasters, gsl_rasters, hurs_rasters, ngd5_rasters, scd_rasters)
    
    bioclim_rasters_SSP1 <- terra::crop(bioclim_rasters, occs_extent)
    
    #SSP5
    bioclim_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5", full.names = TRUE, pattern = ".+_bio[0-9]{1,2}_.+"))
    # Climate Moisture Index NOT available for projection!
    cmi_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_cmi_.+"))
    if(file.exists("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/ndd5_NA0.geotif")){
        gdd5_rasters <- terra::rast("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/ndd5_NA0.geotif")
    }else{
        gdd5_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5", full.names = TRUE, pattern = ".+_gdd5_.+"))
        gdd5_rasters[is.na(gdd5_rasters)] <- 0 # Set NA to 0
        writeRaster(x = gdd5_rasters, filename = "H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/ndd5_NA0.geotif", filetype = "GTiff")
    }
    if(file.exists("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/gsl_NA0.geotif")){
        gsl_rasters <- terra::rast("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/gsl_NA0.geotif")
    }else{
        gsl_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5", full.names = TRUE, pattern = ".+_gsl_.+"))
        gsl_rasters[is.na(gsl_rasters)] <- 0 # Set NA to 0
        writeRaster(x = gsl_rasters, filename = "H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/gsl_NA0.geotif", filetype = "GTiff")
    }
    # Relative humidity NOT available for projection!
    hurs_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_clim/1981-2010/bio", full.names = TRUE, pattern = ".+_hurs_.+"))
    if(file.exists("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/ngd5_NA0.geotif")){
        ngd5_rasters <- terra::rast("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/ngd5_NA0.geotif")
    }else{
        ngd5_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5", full.names = TRUE, pattern = ".+_ngd5_.+"))
        ngd5_rasters[is.na(ngd5_rasters)] <- 0 # Set NA to 0
        writeRaster(x = ngd5_rasters, filename = "H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5/ngd5_NA0.geotif", filetype = "GTiff")
    }
    scd_rasters <- terra::rast(list.files("H:/Chelsa-related/CHELSA_proj/GFDL-ESM4_SSP5", full.names = TRUE, pattern = ".+_scd_.+"))
    
    bioclim_rasters <- c(bioclim_rasters, cmi_rasters, gdd5_rasters, gsl_rasters, hurs_rasters, ngd5_rasters, scd_rasters)
    rm(cmi_rasters, gdd5_rasters, gsl_rasters, hurs_rasters, ngd5_rasters, scd_rasters)
    
    bioclim_rasters_SSP5 <- terra::crop(bioclim_rasters, occs_extent)
    
    # remove 'bioclim_rasters' so I don't get confused later
    rm(bioclim_rasters)
    
    # elevation raster and cropping
    elev_raster <- geodata::elevation_global(res = 0.5, path = "H:/geodata/")
    elev_raster <- terra::crop(x = elev_raster, y = bioclim_rasters_SSP1$`CHELSA_bio1_2071-2100_gfdl-esm4_ssp126_V.2.1`)
    
    # all vars
    explanatory_vars_SSP1 <- c(bioclim_rasters_SSP1, elev_raster)
    explanatory_vars_SSP5 <- c(bioclim_rasters_SSP5, elev_raster)
}

# First, make downscaled version of rasters to make rendering less insane
# SSP1
if(!file.exists(paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP1.tif"))){
    targetForDownscaling <- rast("H:/wc2.1_cruts4.09_2.5m_prec_2020-2024_TargetForDownscaling/wc2.1_cruts4.09_2.5m_prec_2020-01.tif")
    targetForDownscaling <- terra::crop(targetForDownscaling, occs_extent)
    explanatory_vars_2.5min_SSP1 <- resample(explanatory_vars_SSP1, targetForDownscaling, filetype = "GTiff", filename = paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP1.tif"))
    rm(targetForDownscaling)
}else{
    explanatory_vars_2.5min_SSP1 <- rast(paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP1.tif"))
}

# SSP5
if(!file.exists(paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP5.tif"))){
    targetForDownscaling <- rast("H:/wc2.1_cruts4.09_2.5m_prec_2020-2024_TargetForDownscaling/wc2.1_cruts4.09_2.5m_prec_2020-01.tif")
    targetForDownscaling <- terra::crop(targetForDownscaling, occs_extent)
    explanatory_vars_2.5min_SSP5 <- resample(explanatory_vars_SSP5, targetForDownscaling, filetype = "GTiff", filename = paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP5.tif"))
    rm(targetForDownscaling)
}else{
    explanatory_vars_2.5min_SSP5 <- rast(paste0("H:/tempRenderingRasters2.5min_proj/", gsub(" ", "_", speciesForModel), "_SSP5.tif"))
}

# get world boundaries; trim to extent that we need for these projections
world_boundaries <- world(res = 1, path = "H:/geodata/")
#world_boundaries <- terra::crop(world_boundaries, bioclim_rasters$wc2.1_30s_bio_1)
world_boundaries <- terra::crop(world_boundaries, explanatory_vars_2.5min_SSP1$`CHELSA_bio1_2071-2100_gfdl-esm4_ssp126_V.2.1`)

# Get admin areas for countries still present
state_boundaries <- gadm(country = world_boundaries$NAME_0, level = 1, resolution = 1, path = "H:/geodata/")

# We're only concerned with projection, so can rm() the higher res rasters
# rm(bioclim_rasters_SSP1, bioclim_rasters_SSP5, explanatory_vars_SSP1, explanatory_vars_SSP5)

# Load old, current projection rasters for reprojecting at higher res (and fix the broken cropping!)
explanatory_vars_2.5min <- rast(paste0("H:/tempRenderingRasters2.5min/", gsub(" ", "_", speciesForModel), ".tif"))
explanatory_vars_2.5min <- terra::crop(explanatory_vars_2.5min, occs_extent)

# Fix variable names for the bioclim rasters (must be same as during model training)
names(explanatory_vars_2.5min_SSP1) <- gsub(pattern = "2071-2100", replacement = "1981-2010", names(explanatory_vars_2.5min_SSP1))
names(explanatory_vars_2.5min_SSP1) <- gsub(pattern = "_gfdl-esm4_ssp[0-9]{3}", replacement = "", names(explanatory_vars_2.5min_SSP1))

names(explanatory_vars_2.5min_SSP5) <- gsub(pattern = "2071-2100", replacement = "1981-2010", names(explanatory_vars_2.5min_SSP5))
names(explanatory_vars_2.5min_SSP5) <- gsub(pattern = "_gfdl-esm4_ssp[0-9]{3}", replacement = "", names(explanatory_vars_2.5min_SSP5))

# Maxent Summer Projection
# Run model predictions on rasters and plot
maxent_data_summer_predictions_SSP1 <- predict(explanatory_vars_2.5min_SSP1, maxent_data_summer, na.rm = TRUE, args = c("threads=32"))
maxent_data_summer_predictions_SSP5 <- predict(explanatory_vars_2.5min_SSP5, maxent_data_summer, na.rm = TRUE, args = c("threads=32"))
maxent_data_summer_predictions_current <- predict(explanatory_vars_2.5min, maxent_data_summer, na.rm = TRUE, args = c("threads=32"))

# Maxent Winter Projection
maxent_data_winter_predictions_current <- predict(explanatory_vars_2.5min, maxent_data_winter, na.rm = TRUE, args = c("threads=32"))

# Render and Save Maxent Projections
ggplot() +
    geom_spatraster(data = maxent_data_summer_predictions_SSP1, na.rm = TRUE) + 
    scale_y_continuous(limits = c(ext(maxent_data_summer_predictions_SSP1)$ymin, ext(maxent_data_summer_predictions_SSP1)$ymax), expand = c(0,0)) + 
    scale_x_continuous(limits = c(ext(maxent_data_summer_predictions_SSP1)$xmin, ext(maxent_data_summer_predictions_SSP1)$xmax), expand = c(0,0)) + 
    scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
    geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
    geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
    #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
    #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
    theme_clean() + 
    theme(plot.background = element_rect(color = "white"),
          panel.grid.major = element_line(linewidth = 0))

ggsave(file.path(filePath_proj, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_summer_2070-2100_SSP1.png")), width = 9, height = 5.75, units = "in", dpi = 400)


ggplot() +
    geom_spatraster(data = maxent_data_summer_predictions_SSP5, na.rm = TRUE) + 
    scale_y_continuous(limits = c(ext(maxent_data_summer_predictions_SSP5)$ymin, ext(maxent_data_summer_predictions_SSP5)$ymax), expand = c(0,0)) + 
    scale_x_continuous(limits = c(ext(maxent_data_summer_predictions_SSP5)$xmin, ext(maxent_data_summer_predictions_SSP5)$xmax), expand = c(0,0)) + 
    scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
    geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
    geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
    #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
    #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
    theme_clean() + 
    theme(plot.background = element_rect(color = "white"),
          panel.grid.major = element_line(linewidth = 0))

ggsave(file.path(filePath_proj, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_summer_2070-2100_SSP5.png")), width = 9, height = 5.75, units = "in", dpi = 400)

ggplot() +
    geom_spatraster(data = maxent_data_summer_predictions_current, na.rm = TRUE) + 
    scale_y_continuous(limits = c(ext(maxent_data_summer_predictions_current)$ymin, ext(maxent_data_summer_predictions_current)$ymax), expand = c(0,0)) + 
    scale_x_continuous(limits = c(ext(maxent_data_summer_predictions_current)$xmin, ext(maxent_data_summer_predictions_current)$xmax), expand = c(0,0)) + 
    scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
    geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
    geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
    #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
    #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
    theme_clean() + 
    theme(plot.background = element_rect(color = "white"),
          panel.grid.major = element_line(linewidth = 0))

ggsave(file.path(filePath_proj, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_summer_current.png")), width = 9, height = 5.75, units = "in", dpi = 400)

ggplot() +
    geom_spatraster(data = maxent_data_winter_predictions_current, na.rm = TRUE) + 
    scale_y_continuous(limits = c(ext(maxent_data_winter_predictions_current)$ymin, ext(maxent_data_winter_predictions_current)$ymax), expand = c(0,0)) + 
    scale_x_continuous(limits = c(ext(maxent_data_winter_predictions_current)$xmin, ext(maxent_data_winter_predictions_current)$xmax), expand = c(0,0)) + 
    scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
    geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
    geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.5) +
    #geom_spatvector(data = as_spatvector(occs, crs = "EPSG:4326"), color = "black", size = 0.2) +
    #annotation_scale() + # Removed this scale bar because it's somewhat misleading in this projection 
    theme_clean() + 
    theme(plot.background = element_rect(color = "white"),
          panel.grid.major = element_line(linewidth = 0))

ggsave(file.path(filePath_proj, paste0("MaxEnt_Model_", gsub(" ", "_", speciesForModel), "_winter_current.png")), width = 9, height = 5.75, units = "in", dpi = 400)


# Neural Network Projection
# Fix variable names AGAIN, but this time it's just changing the hyphen to a period (must be same as during model training)
names(explanatory_vars_2.5min_SSP1) <- gsub(pattern = "([0-9]{4})-([0-9]{4})", replacement = "\\1.\\2", names(explanatory_vars_2.5min_SSP1))
names(explanatory_vars_2.5min_SSP5) <- gsub(pattern = "([0-9]{4})-([0-9]{4})", replacement = "\\1.\\2", names(explanatory_vars_2.5min_SSP5))

# Make new path for nn output for projection
filePath_nn <- file.path(filePath_proj, "neural_network")

if(!dir.exists(filePath_nn)){
    dir.create(filePath_nn)
}

# Running an empty tabnet() call prior to the below loop fixes an error parsnip was throwing
# A real and true mystery why this does anything...
tabnet()

# SSP1
# set filepath for frames
filePath_nn_frames <- file.path(filePath_nn, "frames_SSP1")

if(!dir.exists(filePath_nn_frames)){
    dir.create(filePath_nn_frames)
}

# Loop for creating prediction rasters for each day of the year
daily_prediction_rasters <- list()
day_seq <- seq(0,364)
# index daily_prediction_rasters is day + 1
for(day in day_seq){
    nn_data_currDay_predictions <- terra::predict(explanatory_vars_2.5min_SSP1, 
                                                   cranes_fitted_nn, type = "prob", 
                                                   na.rm = TRUE,
                                                   const = data.frame(sine.julian = round(sin((day + 1) / 365 * 2 * pi) * 100, 0),
                                                                      cosine.julian = round(cos((day + 1) / 365 * 2 * pi) * 100, 0)),
                                                   filename = file.path(filePath_nn_frames, paste0("nn_model_pred_day", sprintf("%02d", day), ".geotif")),
                                                   overwrite = TRUE,
                                                   wopt = list(filetype = "GTiff",
                                                               memmax = 20,
                                                               steps = 50))
    
    daily_prediction_rasters[[day+1]] <- nn_data_currDay_predictions$.pred_1
}

# Calculate 15 day averages (Range: Day x - 7 : Day x + 7; e.g. if Day 14, range is 7:21)
# I know it might look weird using day_seq here, but it's much easier for me to think with something that starts at 0
daily_prediction_rasters_AVERAGE_15Day <- list()
for(day in day_seq){
    lowbound <- day - 7
    highbound <- day + 7
    # 3 cases
    if(lowbound < 0){
        set1_lowIndex <- length(daily_prediction_rasters) + lowbound + 1
        set1_highIndex <- length(daily_prediction_rasters)
        set2_lowIndex <- 1
        set2_highIndex <- highbound + 1
    }else if(highbound > (length(daily_prediction_rasters) - 1)){
        set1_lowIndex <- lowbound + 1 
        set1_highIndex <- length(daily_prediction_rasters)
        set2_lowIndex <- 1
        set2_highIndex <- highbound - length(daily_prediction_rasters) + 1
    }else{
        set1_lowIndex <- lowbound + 1 
        set1_highIndex <- highbound + 1
        set2_lowIndex <- 0
        set2_highIndex <- 0
    }
    average_set <- c(daily_prediction_rasters[set1_lowIndex : set1_highIndex], 
                     daily_prediction_rasters[set2_lowIndex : set2_highIndex])
    # Debug messaging for checking index (got, I miss python's negative indices so much right now)
    # message(paste0("Original lowbound: ", lowbound, "\n",
    #                "Original highbound: ", highbound, "\n",
    #                "set 1 low index: ", set1_lowIndex, "\n",
    #                "set 1 high index: ", set1_highIndex, "\n",
    #                "set 2 low index: ", set2_lowIndex, "\n",
    #                "set 2 high index: ", set2_highIndex, "\n\n"))
    daily_prediction_rasters_AVERAGE_15Day[[day+1]] <- app(sds(average_set), mean)
}

# save some fancy plots for the AVERAGES
for(day in day_seq){
    day_plot <- ggplot() +
        geom_spatraster(data = daily_prediction_rasters_AVERAGE_15Day[[day + 1]], na.rm = TRUE) + 
        scale_y_continuous(limits = c(ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$ymin, ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$ymax), expand = c(0,0)) + 
        scale_x_continuous(limits = c(ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$xmin, ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$xmax), expand = c(0,0)) + 
        scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
        geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
        geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.4) +
        annotate("text", label = sprintf("%03d", day), x = -173, y = 20) + 
        theme_clean() + 
        theme(plot.background = element_rect(color = "white"),
              panel.grid.major = element_line(linewidth = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) # geom_annotate() call adds y axis label for some reason?
    ggsave(plot = day_plot, file.path(filePath_nn_frames, paste0("TabNet_Model_15DayAverage_", gsub(" ", "_", speciesForModel), "_day", sprintf("%03d", day), ".png")), width = 7, height = 4.25, units = "in", dpi = 400)
}

# Make a gif (uses Magick package)
day_plots_png_list <- list.files(filePath_nn_frames, pattern = "15DayAverage_.+\\.png$", full.names = TRUE)
day_plots_png_joined <- image_join(lapply(day_plots_png_list, image_read))
day_plots_png_animated <- image_animate(day_plots_png_joined, fps = 20)
image_write(image = day_plots_png_animated, path = file.path(filePath_nn_frames, "dailyAverages_plots_animated.gif"))
#image_write_video(image = day_plots_png_animated, path = file.path(filePath_nn_frames, "dailyAverages_plots_animated.mp4"))

# SS5
# set filepath for frames
filePath_nn_frames <- file.path(filePath_nn, "frames_SSP5")

if(!dir.exists(filePath_nn_frames)){
    dir.create(filePath_nn_frames)
}

# Loop for creating prediction rasters for each day of the year
daily_prediction_rasters <- list()
day_seq <- seq(0,364)
# index daily_prediction_rasters is day + 1
for(day in day_seq){
    nn_data_currDay_predictions <- terra::predict(explanatory_vars_2.5min_SSP5, 
                                                  cranes_fitted_nn, type = "prob", 
                                                  na.rm = TRUE,
                                                  const = data.frame(sine.julian = round(sin((day + 1) / 365 * 2 * pi) * 100, 0),
                                                                     cosine.julian = round(cos((day + 1) / 365 * 2 * pi) * 100, 0)),
                                                  filename = file.path(filePath_nn_frames, paste0("nn_model_pred_day", sprintf("%02d", day), ".geotif")),
                                                  overwrite = TRUE,
                                                  wopt = list(filetype = "GTiff",
                                                              memmax = 20,
                                                              steps = 50))
    
    daily_prediction_rasters[[day+1]] <- nn_data_currDay_predictions$.pred_1
}

# Calculate 15 day averages (Range: Day x - 7 : Day x + 7; e.g. if Day 14, range is 7:21)
# I know it might look weird using day_seq here, but it's much easier for me to think with something that starts at 0
daily_prediction_rasters_AVERAGE_15Day <- list()
for(day in day_seq){
    lowbound <- day - 7
    highbound <- day + 7
    # 3 cases
    if(lowbound < 0){
        set1_lowIndex <- length(daily_prediction_rasters) + lowbound + 1
        set1_highIndex <- length(daily_prediction_rasters)
        set2_lowIndex <- 1
        set2_highIndex <- highbound + 1
    }else if(highbound > (length(daily_prediction_rasters) - 1)){
        set1_lowIndex <- lowbound + 1 
        set1_highIndex <- length(daily_prediction_rasters)
        set2_lowIndex <- 1
        set2_highIndex <- highbound - length(daily_prediction_rasters) + 1
    }else{
        set1_lowIndex <- lowbound + 1 
        set1_highIndex <- highbound + 1
        set2_lowIndex <- 0
        set2_highIndex <- 0
    }
    average_set <- c(daily_prediction_rasters[set1_lowIndex : set1_highIndex], 
                     daily_prediction_rasters[set2_lowIndex : set2_highIndex])
    # Debug messaging for checking index (god, I miss python's negative indices so much right now)
    # message(paste0("Original lowbound: ", lowbound, "\n",
    #                "Original highbound: ", highbound, "\n",
    #                "set 1 low index: ", set1_lowIndex, "\n",
    #                "set 1 high index: ", set1_highIndex, "\n",
    #                "set 2 low index: ", set2_lowIndex, "\n",
    #                "set 2 high index: ", set2_highIndex, "\n\n"))
    daily_prediction_rasters_AVERAGE_15Day[[day+1]] <- app(sds(average_set), mean)
}

# save some fancy plots for the AVERAGES
for(day in day_seq){
    day_plot <- ggplot() +
        geom_spatraster(data = daily_prediction_rasters_AVERAGE_15Day[[day + 1]], na.rm = TRUE) + 
        scale_y_continuous(limits = c(ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$ymin, ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$ymax), expand = c(0,0)) + 
        scale_x_continuous(limits = c(ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$xmin, ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$xmax), expand = c(0,0)) + 
        scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
        geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
        geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.4) +
        annotate("text", label = sprintf("%03d", day), x = -173, y = 20) + 
        theme_clean() + 
        theme(plot.background = element_rect(color = "white"),
              panel.grid.major = element_line(linewidth = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) # geom_annotate() call adds y axis label for some reason?
    ggsave(plot = day_plot, file.path(filePath_nn_frames, paste0("TabNet_Model_15DayAverage_", gsub(" ", "_", speciesForModel), "_day", sprintf("%03d", day), ".png")), width = 7, height = 4.25, units = "in", dpi = 400)
}

# Make a gif (uses Magick package)
day_plots_png_list <- list.files(filePath_nn_frames, pattern = "15DayAverage_.+\\.png$", full.names = TRUE)
day_plots_png_joined <- image_join(lapply(day_plots_png_list, image_read))
day_plots_png_animated <- image_animate(day_plots_png_joined, fps = 20)
image_write(image = day_plots_png_animated, path = file.path(filePath_nn_frames, "dailyAverages_plots_animated.gif"))
#image_write_video(image = day_plots_png_animated, path = file.path(filePath_nn_frames, "dailyAverages_plots_animated.mp4"))

## Reproject Tabnet to current time using the new, higher res rendering

# first, fix names
names(explanatory_vars_2.5min) <- gsub("(.+1981)-(2010.+)", "\\1.\\2", names(explanatory_vars_2.5min))

# set filepath for frames
filePath_nn_frames <- file.path(filePath_nn, "frames_current")

if(!dir.exists(filePath_nn_frames)){
    dir.create(filePath_nn_frames)
}

# Loop for creating prediction rasters for each day of the year
daily_prediction_rasters <- list()
day_seq <- seq(0,364)
# index daily_prediction_rasters is day + 1
for(day in day_seq){
    nn_data_currDay_predictions <- terra::predict(explanatory_vars_2.5min, 
                                                  cranes_fitted_nn, type = "prob", 
                                                  na.rm = TRUE,
                                                  const = data.frame(sine.julian = round(sin((day + 1) / 365 * 2 * pi) * 100, 0),
                                                                     cosine.julian = round(cos((day + 1) / 365 * 2 * pi) * 100, 0)),
                                                  filename = file.path(filePath_nn_frames, paste0("nn_model_pred_day", sprintf("%02d", day), ".geotif")),
                                                  overwrite = TRUE,
                                                  wopt = list(filetype = "GTiff",
                                                              memmax = 20,
                                                              steps = 50))
    
    daily_prediction_rasters[[day+1]] <- nn_data_currDay_predictions$.pred_1
}

# Calculate 15 day averages (Range: Day x - 7 : Day x + 7; e.g. if Day 14, range is 7:21)
# I know it might look weird using day_seq here, but it's much easier for me to think with something that starts at 0
daily_prediction_rasters_AVERAGE_15Day <- list()
for(day in day_seq){
    lowbound <- day - 7
    highbound <- day + 7
    # 3 cases
    if(lowbound < 0){
        set1_lowIndex <- length(daily_prediction_rasters) + lowbound + 1
        set1_highIndex <- length(daily_prediction_rasters)
        set2_lowIndex <- 1
        set2_highIndex <- highbound + 1
    }else if(highbound > (length(daily_prediction_rasters) - 1)){
        set1_lowIndex <- lowbound + 1 
        set1_highIndex <- length(daily_prediction_rasters)
        set2_lowIndex <- 1
        set2_highIndex <- highbound - length(daily_prediction_rasters) + 1
    }else{
        set1_lowIndex <- lowbound + 1 
        set1_highIndex <- highbound + 1
        set2_lowIndex <- 0
        set2_highIndex <- 0
    }
    average_set <- c(daily_prediction_rasters[set1_lowIndex : set1_highIndex], 
                     daily_prediction_rasters[set2_lowIndex : set2_highIndex])
    # Debug messaging for checking index (god, I miss python's negative indices so much right now)
    # message(paste0("Original lowbound: ", lowbound, "\n",
    #                "Original highbound: ", highbound, "\n",
    #                "set 1 low index: ", set1_lowIndex, "\n",
    #                "set 1 high index: ", set1_highIndex, "\n",
    #                "set 2 low index: ", set2_lowIndex, "\n",
    #                "set 2 high index: ", set2_highIndex, "\n\n"))
    daily_prediction_rasters_AVERAGE_15Day[[day+1]] <- app(sds(average_set), mean)
}

# save some fancy plots for the AVERAGES
for(day in day_seq){
    day_plot <- ggplot() +
        geom_spatraster(data = daily_prediction_rasters_AVERAGE_15Day[[day + 1]], na.rm = TRUE) + 
        scale_y_continuous(limits = c(ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$ymin, ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$ymax), expand = c(0,0)) + 
        scale_x_continuous(limits = c(ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$xmin, ext(daily_prediction_rasters_AVERAGE_15Day[[day + 1]])$xmax), expand = c(0,0)) + 
        scale_fill_whitebox_c(palette = "viridi", name = "Suitability", limits = c(0,1)) +
        geom_spatvector(data = state_boundaries, fill = "transparent", color = "grey", linewidth = 0.1) +
        geom_spatvector(data = world_boundaries, fill = "transparent", color = "black", linewidth = 0.4) +
        annotate("text", label = sprintf("%03d", day), x = -173, y = 20) + 
        theme_clean() + 
        theme(plot.background = element_rect(color = "white"),
              panel.grid.major = element_line(linewidth = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank()) # geom_annotate() call adds y axis label for some reason?
    ggsave(plot = day_plot, file.path(filePath_nn_frames, paste0("TabNet_Model_15DayAverage_", gsub(" ", "_", speciesForModel), "_day", sprintf("%03d", day), ".png")), width = 7, height = 4.25, units = "in", dpi = 400)
}

# Make a gif (uses Magick package)
day_plots_png_list <- list.files(filePath_nn_frames, pattern = "15DayAverage_.+\\.png$", full.names = TRUE)
day_plots_png_joined <- image_join(lapply(day_plots_png_list, image_read))
day_plots_png_animated <- image_animate(day_plots_png_joined, fps = 20)
image_write(image = day_plots_png_animated, path = file.path(filePath_nn_frames, "dailyAverages_plots_animated.gif"))
#image_write_video(image = day_plots_png_animated, path = file.path(filePath_nn_frames, "dailyAverages_plots_animated.mp4"))

