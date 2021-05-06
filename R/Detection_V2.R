#==================================================================================================
# Estimate receiver detection probability 
# Date: May 4, 2021
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
mcode <- read_csv("M-code list/M_code_list_V12.csv")

arrays <- read_csv("R/output/Previous versions/Marine passage/Array_all_passage_final.csv")

# Array 2
fish2 <- mcode %>% 
  filter(`FF Loc` == "Koyuk" | `FF Loc` == "Ungalik" | `FF Loc` == "Inglutalik")  

array2 <- filter(arrays, arrays$array_ID == 2)

notdetected2 <- anti_join(fish2, array2, by = c("M-code" = "Tag.ID")) 

r2 <- nrow(fish2) - nrow(notdetected2)
z2 <- nrow(notdetected2)

p2 <- r2/(r2 + z2)
p2 # 1.0

# Array 3
fish3 <- mcode %>% 
  filter(`FF Loc` == "Koyuk" | `FF Loc` == "Ungalik" | `FF Loc` == "Inglutalik")  

array3 <- filter(arrays, arrays$array_ID == 3)

notdetected3 <- anti_join(fish3, array3, by = c("M-code" = "Tag.ID"))

r3 <- nrow(fish3) - nrow(notdetected3)
z3 <- nrow(notdetected3)

p3 <- r3/(r3 + z3)
p3 # 1.0

# Array 4

# First we subset all fish tagged north of the array with final fates south of the array 
fish4 <- mcode %>% 
  filter(`FF Loc` == "Egavik" | `FF Loc` == "Unalakleet" | `FF Loc` == "Golsovia" | `FF Loc` == "S") %>% 
  filter(`Lat` >= 64.131518)

# Then all fish tagged south of the array with final fates north
fish4 <- data.frame(rbind(fish4, mcode %>% 
                               filter(`FF Loc` == "Tagoomenik" | `FF Loc` == "Shaktoolik" | `FF Loc` == "Ungalik"
                                      | `FF Loc` == "Inglutalik" | `FF Loc` == "Koyuk" | `FF Loc` == "N") %>% 
                               filter(`Lat` <= 64.131518)))

# Import Junction array detections
array4 <- filter(arrays, arrays$array_ID == 4)

# Filter mcodes which were not detected by array 4
notdetected4 <- anti_join(fish4, array4, by = c("M.code" = "Tag.ID"))

r4 <- nrow(fish4) - nrow(notdetected4)
z4 <- nrow(notdetected4)

p4 <- r4/(r4 + z4)
p4 # 0.922

# Array 5
# First we subset all fish tagged north of the array with final fates south of the array 
fish5 <- mcode %>% 
  filter(`FF Loc` == "Unalakleet" | `FF Loc` == "Golsovia" | `FF Loc` == "S") %>% 
  filter(`Lat` >=  63.955056)

fish5 <- data.frame(rbind(fish5, mcode %>% 
                               filter(`FF Loc` == "Egavik" |`FF Loc` == "Tagoomenik" | `FF Loc` == "Shaktoolik" | `FF Loc` == "Ungalik"
                                      | `FF Loc` == "Inglutalik" | `FF Loc` == "Koyuk" | `FF Loc` == "N") %>% 
                               filter(`Lat` <= 63.955056)))

# Okay, now we have to figure out which of those fish were detected at the Blueberry creek array
  # Import Blueberry creek detections
array5 <- filter(arrays, arrays$array_ID == 5)

# Filter mcodes which were not detected by array 5
notdetected5 <- anti_join(fish5, array5, by = c("M.code" = "Tag.ID"))

r5 <- nrow(fish5) - nrow(notdetected5)
z5 <- nrow(notdetected5)

p5 <- r5/(r5 + z5)
p5 # 0.919

# Array 6
fish6 <- mcode %>% 
  filter(`FF Loc` == "Golsovia" | `FF Loc` == "S")  

array6 <- filter(arrays, arrays$array_ID == 6)

notdetected6 <- anti_join(fish6, array6, by = c("M-code" = "Tag.ID"))

r6 <- nrow(fish6) - nrow(notdetected6) 
z6 <- nrow(notdetected6)

p6 <- r6/(r6 + z6)
p6 # 0.93

# Array 7
  # Calculate lambda, the joint probability of survival and detection
  # r(k-1)/m(k-1)

# r(k-1) were fish detected at 6 and 7
array7 <- filter(arrays, arrays$array_ID == 7) 

rk <- semi_join(array6, array7, by = "Tag.ID")


# m(k-1) were all fish detected at site 6, minus those who went to Golsovia
gol <- filter(mcode, mcode$`FF Loc` == "Golsovia")

mk <- anti_join(array6, gol, by = c("Tag.ID" = "M-code"))  

# One fish hit array 6, then 7, then went to Golsovia, so we need to take it out of rk
rk <- anti_join(rk, gol, by = c("Tag.ID" = "M-code"))
rk1 <- nrow(rk) # 30

mk1 <- nrow(mk) # 33

lambdak <- rk1/mk1

lambdak # 0.909 
