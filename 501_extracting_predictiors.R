# Extract predictor values at plot locations


#replace 'Data' with 'Data', and remove 'cleaned_data'



##### LOAD OBJECTS

sb_year <- readRDS("R_objects/sb_year.RDS") %>%
  select(-Upland, -Lowland, -Habitat_Quality_Code)

all_coords <- readRDS("R_objects/all_coords_2022_03_08.RDS")
all_coords <- st_transform(all_coords, LCC)
all_coords_sf <- all_coords
all_coords <- vect(all_coords)

lc_cavm <- rast("R_objects/lc_cavm.tif")
lc_future <- rast("R_objects/lc_future.tif")

bioclim_present <- rast("R_objects/bioclim_present.tif")
bioclim_ssp245_can <- rast("R_objects/bioclim_ssp245_can.tif")
bioclim_ssp585_can <- rast("R_objects/bioclim_ssp585_can.tif")
bioclim_ssp245_had <- rast("R_objects/bioclim_ssp245_had.tif")
bioclim_ssp585_had <- rast("R_objects/bioclim_ssp585_had.tif")
bioclim_ssp245_acc <- rast("R_objects/bioclim_ssp245_acc.tif")
bioclim_ssp585_acc <- rast("R_objects/bioclim_ssp585_acc.tif")

substrate <- rast("R_objects/substrate.tif")
elevation <- rast("R_objects/elevation.tif")
stdev_elevation <- rast("R_objects/stdev_elevation.tif")
d2coast <- rast("R_objects/distance2coast.tif")


lc_2000 <- readRDS("R_objects/lc_2000_resampled.RDS") 

landcover_classes_both <- readRDS("R_objects/landcover_classes_both.RDS")

remove_area <- vect("R_objects/remove_from_study_area_ch3/remove_from_study_area_ch3.shp")
remove_area <- terra::project(remove_area, LCC)





##### PREPARE RASTER STACKS

# rename all layers of the future datasets and merge together


lc_cavm <- resample(lc_cavm, lc_2000)
lc_future <- resample(lc_future, lc_2000)
bioclim_present <- resample(bioclim_present, lc_2000)
bioclim_ssp245_can <- resample(bioclim_ssp245_can, lc_2000)
bioclim_ssp585_can <- resample(bioclim_ssp585_can, lc_2000)
bioclim_ssp245_had <- resample(bioclim_ssp245_had, lc_2000)
bioclim_ssp585_had <- resample(bioclim_ssp585_had, lc_2000)
bioclim_ssp245_acc <- resample(bioclim_ssp245_acc, lc_2000)
bioclim_ssp585_acc <- resample(bioclim_ssp585_acc, lc_2000)
substrate <- resample(substrate, lc_2000)
elevation <- resample(elevation, lc_2000)
stdev_elevation <- resample(stdev_elevation, lc_2000)
d2coast <- resample(d2coast, lc_2000)


 
# creating squared rasters for quadratic terms in models


square_raster <- function(x){
  
  x2 <- x ^ 2
  names(x2) <- paste(names(x2), 2, sep = "")
  x3 <- c(x, x2) 
  return(x3)
  
}

bioclim_present <- square_raster(bioclim_present)
bioclim_ssp245_can <- square_raster(bioclim_ssp245_can)
bioclim_ssp585_can <- square_raster(bioclim_ssp585_can)
bioclim_ssp245_had <- square_raster(bioclim_ssp245_had)
bioclim_ssp585_had <- square_raster(bioclim_ssp585_had)
bioclim_ssp245_acc <- square_raster(bioclim_ssp245_acc)
bioclim_ssp585_acc <- square_raster(bioclim_ssp585_acc)
elevation <- square_raster(elevation)
stdev_elevation <- square_raster(stdev_elevation)
d2coast <- square_raster(d2coast)




# raster stacks

#ssp 585 goes with SRES a2
#ssp 245 goes with SRES b2
#had goes with ha
#can goes with cc
#acc goes with cs


stack_present_cont <- c(bioclim_present, elevation, stdev_elevation, d2coast)
stack_present_cat <- c(lc_cavm, substrate)
stack_present <- c(lc_cavm, substrate, bioclim_present, elevation, stdev_elevation, d2coast)
stack_future245_can <- c(lc_future$rf_cc_b2_5k, lc_future$rf_cc_b2_20k, lc_future$rf_cc_b2_e, substrate, bioclim_ssp245_can, elevation, stdev_elevation, d2coast)
stack_future585_can <- c(lc_future$rf_cc_a2_5k, lc_future$rf_cc_a2_20k, lc_future$rf_cc_a2_e, substrate, bioclim_ssp585_can, elevation, stdev_elevation, d2coast)
stack_future245_had <- c(lc_future$rf_ha_b2_5k, lc_future$rf_ha_b2_20k, lc_future$rf_ha_b2_e, substrate, bioclim_ssp245_had, elevation, stdev_elevation, d2coast)
stack_future585_had <- c(lc_future$rf_ha_a2_5k, lc_future$rf_ha_a2_20k, lc_future$rf_ha_a2_e, substrate, bioclim_ssp585_had, elevation, stdev_elevation, d2coast)
stack_future245_acc <- c(lc_future$rf_cs_b2_5k, lc_future$rf_cs_b2_20k, lc_future$rf_cs_b2_e, substrate, bioclim_ssp245_acc, elevation, stdev_elevation, d2coast)
stack_future585_acc <- c(lc_future$rf_cs_a2_5k, lc_future$rf_cs_a2_20k, lc_future$rf_cs_a2_e, substrate, bioclim_ssp585_acc, elevation, stdev_elevation, d2coast)


# create polygon for removing Labrador from study area

provinces <- vect("R_objects/Canada provincial boundaries/lpr_000b21a_e.shp")
nfld <- provinces[provinces$PREABBR == "N.L."]
nfld <- terra::project(nfld, LCC)
nfld <- try(st_as_sf(nfld), silent = TRUE)
nfld <- ms_filter_islands(nfld, min_area = 100000000)
nfld <- vect(nfld)
nfld <- terra::simplifyGeom(nfld, 10000)
nfld <- terra::buffer(nfld, 75000)


# remove the regions that aren't part of our study area 

stack_present <- terra::mask(stack_present, d2coast) # remove Greenland and USA
stack_present <- terra::mask(stack_present, substrate) # remove boreal Canada 
stack_present <-  terra::mask(stack_present, nfld , inverse = TRUE) # remove Labrador
stack_present <- terra::mask(stack_present, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_present_cont <- terra::mask(stack_present_cont, d2coast) # remove Greenland and USA
stack_present_cont <- terra::mask(stack_present_cont, substrate) # remove boreal Canada 
stack_present_cont <-  terra::mask(stack_present_cont, nfld , inverse = TRUE) # remove Labrador
stack_present_cont <- terra::mask(stack_present_cont, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_present_cat <- terra::mask(stack_present_cat, d2coast) # remove Greenland and USA
stack_present_cat <- terra::mask(stack_present_cat, substrate) # remove boreal Canada 
stack_present_cat <-  terra::mask(stack_present_cat, nfld , inverse = TRUE) # remove Labrador
stack_present_cat <- terra::mask(stack_present_cat, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_future245_can <- terra::mask(stack_future245_can, d2coast) # remove Greenland and USA
stack_future245_can <- terra::mask(stack_future245_can, substrate) # remove boreal Canada
stack_future245_can <-  terra::mask(stack_future245_can, nfld , inverse = TRUE) #remove Labrador
stack_future245_can <- terra::mask(stack_future245_can, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_future585_can <- terra::mask(stack_future585_can, d2coast) # remove Greenland and USA
stack_future585_can <- terra::mask(stack_future585_can, substrate) # remove boreal Canada
stack_future585_can <-  terra::mask(stack_future585_can, nfld , inverse = TRUE) #remove Labrador
stack_future585_can <- terra::mask(stack_future585_can, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_future245_had <- terra::mask(stack_future245_had, d2coast) # remove Greenland and USA
stack_future245_had <- terra::mask(stack_future245_had, substrate) # remove boreal Canada
stack_future245_had <-  terra::mask(stack_future245_had, nfld , inverse = TRUE) #remove Labrador
stack_future245_had <- terra::mask(stack_future245_had, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_future585_had <- terra::mask(stack_future585_had, d2coast) # remove Greenland and USA
stack_future585_had <- terra::mask(stack_future585_had, substrate) # remove boreal Canada
stack_future585_had <-  terra::mask(stack_future585_had, nfld , inverse = TRUE) #remove Labrador
stack_future585_had <- terra::mask(stack_future585_had, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_future245_acc <- terra::mask(stack_future245_acc, d2coast) # remove Greenland and USA
stack_future245_acc <- terra::mask(stack_future245_acc, substrate) # remove boreal Canada
stack_future245_acc <-  terra::mask(stack_future245_acc, nfld , inverse = TRUE) #remove Labrador
stack_future245_acc <- terra::mask(stack_future245_acc,  remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island

stack_future585_acc <- terra::mask(stack_future585_acc, d2coast) # remove Greenland and USA
stack_future585_acc <- terra::mask(stack_future585_acc, substrate) # remove boreal Canada
stack_future585_acc <-  terra::mask(stack_future585_acc, nfld , inverse = TRUE) #remove Labrador
stack_future585_acc <- terra::mask(stack_future585_acc, remove_area, inverse = TRUE) # remove Arctic Cordillera and eastern Baffin Island





##### EXTRACT PREDICTOR VALUES

# extract the value of predictors within each PRISM plot during the present time period

# continuous predictors

present_values_cont <- try(as.data.frame(terra::extract(stack_present_cont, all_coords, mean, na.rm=TRUE)), silent = TRUE)
present_values_cont$Plot <- all_coords$Plot

# categorical predictors

# extract the area of each raster cell covered by the plot and summarize

present_values_cat <- exact_extract(stack_present_cat, all_coords_sf, coverage_area = TRUE, include_area = TRUE)

# add plot names to the elements of the output list

names(present_values_cat) <-all_coords$Plot

# merge the list elements into a df

present_values_cat <- bind_rows(present_values_cat, .id = "Plot") %>%
  group_by(Plot) %>%
  mutate(count = n()) %>%
  group_by(Plot) %>%
  slice_max(coverage_area) %>%
  rename(new_value = lc_class)

present_values_cat <- merge(present_values_cat, landcover_classes_both, all = TRUE) %>%
  select(Plot, lc_class, substrate) %>%
  mutate(lc_class = as.factor(lc_class)) %>%
  filter(!is.na(Plot))

# merge continuous and categorical

present_values <- merge(present_values_cat, present_values_cont) %>%
  mutate(across(.cols = where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# version with no NAs

present_values_noNA <- present_values %>%
  na.omit()



##### SAVE OBJECTS

saveRDS(stack_present, "R_objects/stack_present.RDS")
saveRDS(stack_future245_can, "R_objects/stack_future245_can.RDS")
saveRDS(stack_future585_can, "R_objects/stack_future585_can.RDS")
saveRDS(stack_future245_had, "R_objects/stack_future245_had.RDS")
saveRDS(stack_future585_had, "R_objects/stack_future585_had.RDS")
saveRDS(stack_future245_acc, "R_objects/stack_future245_acc.RDS")
saveRDS(stack_future585_acc, "R_objects/stack_future585_acc.RDS")
saveRDS(present_values, "R_objects/present_values.RDS")
saveRDS(present_values_noNA, "R_objects/present_values_noNA.RDS")

