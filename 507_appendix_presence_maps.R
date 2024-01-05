# Map for each species with presences and absences



all_coords <- readRDS("R_objects/all_coords_2022_03_08.RDS")
all_coords <- st_transform(all_coords, LCC)
all_coords <- st_centroid(all_coords)


species_data <- readRDS("R_objects/species_data_ch3_long.RDS")



##### REMOVE THE PLOTS THAT ARENT INCLUDED IN MY STUDY

all_coords_CH3 <- all_coords %>%
  filter(Plot %in% species_data$Plot)





##### SAVE OBJECTS

write_sf(all_coords_CH3, "Data/Cleaned_data/all_coords/all_coords_CH3_2023_06_13.shp", overwrite = TRUE)

