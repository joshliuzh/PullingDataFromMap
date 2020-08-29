# This script shows how to pull map from Google servers and plots a heat map with contour. It also shows how to plot numerical values on the map.
# The data is property listings in DC Each row is a property. Important columns include:
# longitude
# latitude
# price

library(dplyr)
library(ggplot2)
library(ggmap)
library(plotly)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(plotly)
library(DT)
library(reshape2)

load('dataDC.Rdata')

register_google(key = '')
# User needs to obtain their own key at Google Map API website;
map_DC = get_map(location=c(lon = mean(dataDC$longitude, na.rm = T), lat =mean(dataDC$latitude, rm.na = T) ), zoom=12, maptype = "terrain", source='google', color='color')
ggmap(map_DC, extent = "device") + geom_density2d(data = dataDC, 
                                                   aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data =dataDC, 
                                                                                                                  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0.2, 0.5), guide = FALSE)+labs(title = "DC Density Heatmap")


DC_price = dataDC[, c("longitude", "latitude", "price")]
q90 = quantile(DC_price$price, prob= 0.90)
DC_price$price = ifelse(DC_price$price>q90, q90, DC_price$price)
map_DC = get_map(location=c(lon = mean(DC_price$longitude, na.rm = T), lat =mean(DC_price$latitude, rm.na = T) ), zoom=11, maptype = "terrain", source='google', color='color')
ggmap(map_DC, extent = "device") + geom_point(
  aes(x=longitude, y=latitude, colour = price),
  data=DC_price, alpha=.5, na.rm = T, size = 0.005)+scale_color_gradient(low="green", high="red")+
  labs(title = "DC Price Plot")
