library(tidyverse)
library(osmdata) 
library(ggmap)
library(rvest)
library(sf)
library(sp)
library(rgeos)
#must download xQuartz
library(showtext)

title = "ISTANBUL"
subtitle = "TURKEY"

city_name = title + subtitle # need to concatenate

zoom = .1


#gets OMS-defined centroid by city_ name
dat <- getbb(city_name, format_out ="data.frame", limit = 1) 
dat <- dat[,c("lat","lon")]

#converts centroid into coordinates for bounding box
cols.num <- c("lat","lon")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)

n = (dat$lat + (zoom*1.05))
s = (dat$lat - (zoom*1.05)) 
w = (dat$lon + zoom) 
e = (dat$lon - zoom)


#make bounding box
my_box <- rgeos::bbox2SP(n, s, w, e,
                         proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
my_box_sf <- st_as_sf(my_box)


#pulling data
big_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
                  osmdata_sf()


med_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
                  osmdata_sf()


small_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
                   osmdata_sf()

railway <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

coastline <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "natural", value="coastline") %>%
  osmdata_sf()

#create map
map <- ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#cccccc",
          size = .25,
          alpha = .8) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#919191",
          size = .5,
          alpha = .8) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "#6e6e6e",
          size = .1,
          alpha = .7) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#3b3b3b",
          size = .8,
          alpha = .7) +
  geom_sf(data = coastline$osm_lines,
          inherit.aes = FALSE,
          color = "#6e6e6e",
          size = .25,
          alpha = .7) +
  coord_sf(xlim = c(e, w), 
           ylim = c(s + (zoom/6), n - (zoom/6)),
           expand = FALSE)+
  theme_void()+
  ggtitle(city_name, subtitle = subtitle)

#plot map and set theme
print <- map + theme(
  plot.title = element_text(family = 'Trebuchet MS', color = "black", size = 100, face = "bold", hjust = 0.5, margin = margin(t = 20)),
  plot.subtitle = element_text(family = 'Trebuchet MS', color = "black", size = 40, hjust = 0.5,margin = margin(t=5, b = 20)))


setwd('/Users/mac/Desktop/waywiser/OSM-Print-Map-Generator/prints')
ggsave(filename=(paste(city_name,".png", sep="", collapse=NULL)), plot=print, device="png",
       path="./", height=17, width=11, units="in", dpi=300)




