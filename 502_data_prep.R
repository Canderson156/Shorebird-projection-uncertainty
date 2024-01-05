



##### LOAD OBJECTS

landcover_classes_lc2000 <- readRDS("R_objects/landcover_classes_lc2000.RDS")
present_values <- readRDS("R_objects/present_values.RDS")
present_values_noNA <- readRDS("R_objects/present_values_noNA.RDS")


stack_present <- readRDS("R_objects/stack_present.RDS")
stack_future245_can <- readRDS("R_objects/stack_future245_can.RDS")
stack_future585_can <- readRDS("R_objects/stack_future585_can.RDS")
stack_future245_had <- readRDS("R_objects/stack_future245_had.RDS")
stack_future585_had <- readRDS("R_objects/stack_future585_had.RDS")
stack_future245_acc <- readRDS("R_objects/stack_future245_acc.RDS")
stack_future585_acc <- readRDS("R_objects/stack_future585_acc.RDS")


sb_year <- readRDS("R_objects/sb_year.RDS") %>%
  select(-Upland, -Lowland, -Habitat_Quality_Code)



##### Prepare shorebird data


# remove species with small sample sizes
# remove subarctic species

sb_year <- sb_year %>%
  filter(Species %notin% c("WHIM", "HUGO", "WISN", "LESA", "SEPL", "RNPH", "REKN", "RUTU", "SAND"))

n_unique(sb_year$Plot)



# remove second round of surveys in 2019
# remove flood year data from 1996
# randomly select one observation when there are multiple years 

set.seed(58305)

sb_year <- sb_year %>%
  mutate(Plot_year = paste(Plot, Year, sep = "_")) %>%
  filter(Year != 2019,
         Year != 1996) %>%
  group_by(Plot) %>%
  mutate(n_visits = n()/n_unique(Species)) %>%
  ungroup() %>%
  group_by(Plot, Species) %>%
  slice_sample(n = 1) %>%
  ungroup()                 

# merge shorebird observations with predictor values

present_values <- merge(sb_year, present_values) %>%
  na.omit() %>%
  filter(lc_class %notin% c("Glaciers", "Carbonate mountain complex", "Noncarbonate mountain complex", "Water")) %>%
  filter(substrate != 10) %>%
  mutate(presence = ifelse(presence == TRUE, "yes", "no")) %>%
  filter(Region_Code != 0)





##### Prepare predictor data for present and future scenarios


# combine all future scenarios into one list

scenarios2 <- list(future245_can = stack_future245_can,
                  future245_had = stack_future245_had,
                  future245_acc = stack_future245_acc,
                  future585_can = stack_future585_can,
                  future585_had = stack_future585_had,
                  future585_acc = stack_future585_acc)


# re-organize list 
scenarios <- rep(list(rast()), length(scenarios2)*3)

for(i in 1:length(scenarios2)){
  
  scenarios[[i*3-2]] <- scenarios2[[i]]  %>%
    select(-contains("_20k"), -contains("2_e")) %>%
    rename(lc_class = starts_with("rf"))
  
  scenarios[[i*3-1]] <- scenarios2[[i]]  %>%
    select(-contains("_5k"), -contains("2_e")) %>%
    rename(lc_class = starts_with("rf"))
  
  scenarios[[i*3]] <- scenarios2[[i]]  %>%
    select(-contains("_5k"), -contains("_20k")) %>%
    rename(lc_class = starts_with("rf"))
}


# give appropriate names

dispersal <- c("5k", "20k", "e")

names_scenarios <- expand.grid(names(scenarios2), dispersal) %>%
  arrange(Var1) %>%
  unite(scenarios, Var1:Var2)

names(scenarios) <- names_scenarios$scenarios


# add present day scenario

scenarios[[19]] <- stack_present
names(scenarios) <- c(names(scenarios[1:length(scenarios)-1]), "present")





###### EDITS TO MAKE PREDICT FUNCTION WORK


# edit substrate class to work with predict function

edit_substrate <- function(x){
  
  x$substrate <- as.numeric(x$substrate)
  x$substrate[x$substrate == 10] <- NA
  
  return(x)
}

scenarios <- lapply(scenarios, edit_substrate)

# remove unused levels for land cover

# remove the levels that aren't being used in present_values
present_values <- present_values %>%
  mutate(lc_class = as.character(lc_class)) %>%
  na.omit()

# which classes to I need to remove?

remove <- levels(scenarios$present$lc_class)[[1]]$lc_class[levels(scenarios$present$lc_class)[[1]]$lc_class %notin% unique(present_values$lc_class)]


# set all the removed classes to NAs

edit_lc_class <- function(x){
  
  x$lc_class[x$lc_class %in% remove] <- NA
  
  return(x)
}

scenarios <- lapply(scenarios, edit_lc_class)


# what are the classes that remain?

new_levels <- levels(scenarios$present$lc_class)[[1]] %>%
  filter(lc_class %in% unique(present_values$lc_class))


# change the levels of to reflect the new classes

edit_new_classes <- function(x){
  
  levels(x$lc_class) <- new_levels
  
  return(x)
  
  }

scenarios <- lapply(scenarios, edit_new_classes)



# split present_values by species

present_values$lc_class <- as.factor(present_values$lc_class)
species_data <- group_split(present_values, Species)
names(species_data) <- sort(unique(present_values$Species))








# Apply elevation mask to restrict study area to areas at sampled elevations

elevation_mask <- classify(scenarios$present$elevation,
                           cbind(0,max(present_values$elevation),1))

elevation_mask <- classify(elevation_mask,
                           cbind(max(present_values$elevation),2000,NA))


scenarios <- lapply(scenarios, mask, elevation_mask)







##### SAVE OBJECTS

scenarios <- lapply(scenarios, wrap)
saveRDS(scenarios, "R_objects/scenarios_ch3.RDS")
saveRDS(species_data, "R_objects/species_data_ch3.RDS")
saveRDS(present_values, "R_objects/species_data_ch3_long.RDS")





