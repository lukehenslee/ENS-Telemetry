#==================================================================================================
# Binomial stock model for SD5 stocks
# Date: April 30, 2021
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


# Import data ####
setwd("Q:/RESEARCH/Tagging/Github/data")

mcode <- read.csv("M_code_list.csv")

# Create binomial response for fish bound for SD5 spawning streams ####
  # Subset fish assigned to stocks

stocks <- filter(mcode, mcode$Spawning.group %in% c("4", "5", "6", "S"))

# Creat bionmial response column 
bi <- vector(length = nrow(stocks))

for(i in 1:length(bi)) {
  if(stocks[i,16] == 5) {
    bi[i] <- 1
  } else {
    bi[i] <- 0
  }
}

stocks$binomial <- bi

# Build model ####

bi_model <- glm(binomial ~ Lat*Long, data = stocks,
                family = binomial(link = "logit"))

summary(bi_model)

#### 2) Build plot

ggplot(stocks, aes(Lat, binomial)) +
  geom_point(shape = 3, size = 5) +
  geom_smooth(size = 0.5, color = "black", method = "glm", se = T, 
              method.args = list(family = "binomial")) +
  xlim(63.5,64.5) +
  scale_x_continuous(name = "Latitude (N)", expand = c(0,0), 
                     breaks = seq(63.6, 64.4, .1)) +
  scale_y_continuous(name = "Likelihood", 
                     expand = c(0, 0), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, .2)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         text = element_text(family = "Times New Roman"),
         legend.position = c(0.75,0.75),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.key.size = unit(.5, "cm"),
         plot.margin = unit(c(0.2, 0.1, 0.1, 0.1), "cm"),
         axis.ticks.y = element_line(size = 0.5),
         axis.ticks.x = element_line(size = 0.5),
         axis.ticks.length = unit(0.2,"cm"),
         axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         axis.text.x = element_text(colour = "black", size = 14, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.border = element_rect(color = "black", fill = "transparent", size = 0.5))
```

### Figure 5

#### 1) Generate predictions

# Import coordinates from shoreline of SD5

pred.coords <- c(-160.968230276312,63.6354930312494,-160.8875394706995,63.69567240416612,-160.7922424981599,63.77118796678232,-160.8085943752948,63.8685679216954,-160.8785764978863,63.95552257435548,-160.9119737005435,63.98098033666164,-160.972252701095,64.09264988416524,-160.9675032919054,64.13119121164306,-160.970324875245,64.18723530046518,-160.9704429425267,64.20300403740617,-161.0056096958181,64.23510899250674,-161.1483281899604,64.31066929894213,-161.2570090466422,64.37142768198946,-161.4104320993511,64.41158377117317,-161.4553268040264,64.40868651199482,-161.5131916294441,64.36891175561681)
long <- pred.coords[c(T, F)]
lat <- pred.coords[c(F,T)]


# Create sequence of lats along path
lat1  <- vector()
for(i in 1:14) {
  lat1 <- append(x = lat1, values = c(seq(lat[i], 
                                          lat[i+1], length.out = 100)))
}

# Create sequence of longs along path
long1  <- vector()
for(i in 1:14) {
  long1 <- append(x = long1, values = c(seq(long[i], 
                                            long[i+1], length.out = 100)))
}

# Predict model output values and create dataframe
pred <- predict(bi_model, data.frame(Lat = lat1, Long = long1),
                type = "response")

pred.data <- data.frame(cbind(lat1, long1, pred))
```

#### 2) Create basemap

ENSmap <- c(left = -161.6, bottom = 63.5, right = -160.5, top = 64.5)

ENS <- ggmap::get_stamenmap (bbox = ENSmap, zoom = 10, maptype = "terrain-background", where = "cache")

ggmap(ENS)

#### 3) Create stat map
```{r message=FALSE, warning=FALSE}
# Creat basemap
ENSmap <- c(left = -161.6, bottom = 64.1, right = -160.8, top = 64.5)
ENS <- ggmap::get_stamenmap (bbox = ENSmap, maptype = "toner-lite", 
                                     where = "cache")


# Create GLM map with predictive points
glm <- ggmap(ENS) +
  geom_point(data = pred.data, aes(x=long1, 
                                   y=lat1, color = pred), size = 2.5) +
  scale_color_viridis(discrete = F, option = "C", name = "Likelihood", 
                      breaks = c(0.1, 0.2, 0.3, 0.4)) +
  scale_x_continuous(breaks = seq(-161.4, -160.6, .4), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(63.6, 64.4, .1), expand = c(0,0)) + 
  xlab(NULL) + ylab(NULL) +
  annotation_north_arrow(style = north_arrow_nautical()) + 
  coord_sf(crs = 4326) + 
  annotation_scale(style = "ticks", location = "br", height = unit(2, "mm"), 
                   tick_height = 1, text_cex = 1, line_width = 2) +
  theme(axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 0, r = 0, b = 0, l = 0), colour = "black"),
        text = element_text(family = "Times New Roman")) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = 1)) +
  theme (plot.margin = unit(c(0,0,.5,0), "cm")) + 
  theme(legend.position = c(0.8, 0.7), legend.text = element_text(size = 9),
        legend.title = element_text(size = 9), 
        legend.spacing.x = unit(0.5, 'cm'))

glm

#### 4) Create inset map
alaska.box2 <- c(left = -168.5, bottom = 62, right = -155, top = 69)
alaska.base2 <- ggmap::get_stamenmap (bbox = alaska.box2, zoom = 5, maptype = "toner-background", 
                                      where = "cache")
alaska <- ggmap(alaska.base2) +
  geom_rect(xmin = -161.6, ymin = 63.5, xmax = -160.5, ymax = 64.5, fill = "transparent", 
            color = "red", size = .5) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", size = .5)) +
  theme (plot.margin = unit(c(0,0,0,0), "cm"))
```

#### 5) Combine maps
```{r, fig.cap = "FIGURE 5: Graphical representation of general linear model showing relationship of latitude of capture to likelihood of capturing a terminal stock coho in Shaktoolik subdistrict.",  message=FALSE, warning=FALSE}
GLM_map <- ggdraw() +
  draw_plot(glm) +
  draw_plot(alaska, x = 0.35, y = 0.3, width = 0.2, height = 0.2)

GLM_map
```
dev.new (width = 3.53, height = 5.62, units = "in", noRStudioGD = T); last_plot()
ggsave ("GLM_ENS.tiff", width = dev.size()[1], height = dev.size()[2], dpi = 600); dev.off()
