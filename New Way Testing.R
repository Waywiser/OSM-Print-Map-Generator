library(tidyverse)
library(osmdata) 
library(showtext)
library(ggmap)
library(rvest)
library(sf)
library(rgeos)


city_name = "Ithaca"
zoom = .15

#gets OMS-defined centroid by city_ name
dat <- getbb(city_name, format_out ="data.frame", limit = 1) 
dat <- dat[,c("lat","lon")]

#converts centroid into coordinates for bounding box
cols.num <- c("lat","lon")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)

n = (dat$lat + (zoom*1.2))
s = (dat$lat - (zoom*1.2)) 
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

river <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()


#plotting data
ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#cccccc",
          size = .4,
          alpha = .3) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#919191",
          size = .8,
          alpha = .3) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "#6e6e6e",
          size = .6,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#3b3b3b",
          size = 1,
          alpha = .3) +
  coord_sf(xlim = c(e, w), 
           ylim = c(s, n),
           expand = FALSE)+
  theme_void()


#quick test
ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "blue") +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "lightgrey") +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black") +
  coord_sf(xlim = c(e, w), 
           ylim = c(s, n),
           expand = FALSE)+
  theme_void()
