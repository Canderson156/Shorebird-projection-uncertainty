# Load packages, functions, objects etc necessary for all scripts to run

# Time to run: <30 seconds, max a few mins if there are packages to install


##### LOAD PACKAGES




# check for updates
options(pkgType = "binary")
options(timeout = 3600)
options(scipen = 999) # disable scientific notation
options(stringsAsFactors = FALSE) # disable strings as factors

#update.packages(checkBuilt = TRUE, ask = FALSE, lib.loc = .libPaths()[1])


library(sensitivity)
library(rgl)
library(doParallel)
library(randomForest)
library(ggplot2)
library(caret)
library(CAST)
library(rockchalk)
library(janitor)
library(MuMIn)
library(semEff)
library(vcdExtra)
library(RColorBrewer)
library(dichromat)
library(lmtest)
library(nlme)
library(ggfortify)
library(TTR)
library(changepoint)
library(kableExtra)
library(lmerTest)
library(tictoc)
library(terra)
library(Rmisc)
library(miceadds)
library(arm)
library(lme4)
library(sf)
library(gdalUtilities)
library(tmaptools)
library(pack)
library(tictoc)
library(DHARMa)
library(AMR)
library(formatR)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rmapshaper)
library(gridExtra)
library(cowplot)
library(ggtext)
library(exactextractr)
library(multcomp)
library(multcompView)
library(piecewiseSEM)
library(fastDummies)
library(forcats)
library(pROC)
library(cvAUC)
library(PresenceAbsence)
library(stringr)
library(dismo)
library(tidyverse)
library(tidyterra)
library(ggbeeswarm)
library(ggforce)
library(MLeval)
library(ggpmisc)
library(rsample)
library(ranger)
library(e1071)
library(furrr)
library(parallel)
library(future)
library(stringr.tools)
library(car)
library(gghighlight)
library(corrplot)
library(patchwork)
library(ggpubr)



##### CUSTOM FUNCTIONS

source.all("R_functions/")



##### CRS OBJECTS USED THROUGHOUT PROJECT

LCC <- "+init=EPSG:3347"
WGS84 <- "+init=EPSG:4326"
NAD83 <- "+init=EPSG:4269"
WClim_CRS <- "GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4326]]"




