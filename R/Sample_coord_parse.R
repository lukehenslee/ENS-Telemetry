#==================================================================================================
# Convert sample site coordinates
# Date: May 3, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#
#
#==================================================================================================
#NOTES: 
#==================================================================================================

library(ggpubr)
library(forcats)
library(ggforce)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(tidyverse)
library(ggplot2)
library(bbmle)
library(dplyr)
library(manipulate)
library(visreg) 
library(ggridges)
library(reshape2)
library(cowplot)
library(ggmap)
library(ggspatial)
library(sf)
library(spData)
library(ggsn)
library(extrafont)
library(knitr)
loadfonts(device = "win")
library(parzer)
library(readxl)

# Set wd
setwd("Q:/RESEARCH/Tagging")

# Import data
latLongUNK <- read_xlsx("Tagging data/Sample_locations_UNK.xlsx")

# Parse lat lon
latLongUNK$lat <- parse_lat(latLongUNK$lat)

latLongUNK$long <- parse_lon(latLongUNK$long)

# Save file
write.csv(latLongUNK, "Tagging data/Converted_sample_sites_UNK.csv")

# Parse SHK lat lon

# Import data
LatLongSHK <- read_xlsx("Tagging data/Sample_locations_SHK.xlsx")

# Parse Lat lon
LatLongSHK$Lat <- parse_lat(LatLongSHK$Lat)

LatLongSHK$Long <- parse_lon(LatLongSHK$Long)

# Save file
write.csv(LatLongSHK, "Tagging data/Converted_sample_sites_SHK.csv")
