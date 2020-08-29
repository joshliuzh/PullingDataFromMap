# This script shows how to pull map from Google servers and plots a heat map with contour. It also shows how to plot numerical values on the map.
# The data is property listings in New York. Each row is a property. Important columns include:
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
load('airbnbNYC.Rdata')

register_google(key = '')
# User needs to obtain their own key at Google Map API website;
map_NYC = get_map(location=c(lon = mean(airbnbNYC$longitude, na.rm = T), lat =mean(airbnbNYC$latitude, rm.na = T) ), zoom=11, maptype = "terrain", source='google', color='color')
ggmap(map_NYC, extent = "device") + geom_density2d(data = airbnbNYC, 
   aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data =airbnbNYC, 
   aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
   bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0.2, 0.5), guide = FALSE)+labs(title = "NYC Density Heatmap")


NYC_price = airbnbNYC[, c("longitude", "latitude", "price")]
q90 = quantile(NYC_price$price, prob= 0.90)
NYC_price$price = ifelse(NYC_price$price>q90, q90, NYC_price$price)
map_NYC = get_map(location=c(lon = mean(NYC_price$longitude, na.rm = T), lat =mean(NYC_price$latitude, rm.na = T) ), zoom=11, maptype = "terrain", source='google', color='color')
ggmap(map_NYC, extent = "device") + geom_point(
  aes(x=longitude, y=latitude, colour = price),
  data=NYC_price, alpha=.5, na.rm = T, size = 0.005)+scale_color_gradient(low="green", high="red")+
  labs(title = "NYC Price Plot")
