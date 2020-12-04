library(ggplot2)
library(ggpattern)
library(osmdata)
library(sp)
library(sf)
library(remotes)
library(rgeos)
library(rmapshaper)

#remotes::install_github("coolbutuseless/ggpattern")

#inputs
title = "PARIS"
subtitle = "FRANCE"

city_name <- (paste(title,",", subtitle, sep="", collapse=NULL))
zoom = 100

blue <- c("https://i.pinimg.com/originals/0d/c6/f0/0dc6f0f6af86d72b22fd7346010697e1.jpg")
orange<- c("https://www.achildsplacepa.org/wp-content/uploads/2018/12/GettyImages-986491786.jpg")
peach<- c("https://media1.thehungryjpeg.com/thumbs2/ori_3503830_c5c37d89f20dcbb582caab4663617214d8b1914a_watercolor-digital-paper-watercolor-background-watercolor-texture.jpg")
green <- c("https://i.pinimg.com/originals/82/68/7e/82687e742ac712c43faa592a4881d456.jpg")

color = orange
#adjust zoom
zoom = (1/zoom)


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
CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
my_box <- rgeos::bbox2SP(n, s, w, e,
                         proj4string = CRS(CRS))
my_box_sf <- st_as_sf(my_box)


#pull streets for blocks
streets <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "highway", value = c("motorway","primary", "secondary", "tertiary", "residential"))
streets <- osmdata_sp(streets, quiet = TRUE)
streets <- streets$osm_lines

#pull parks
parks <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "leisure", value = c("park","pitch", "nature_reserve", "garden", "residential"))
parks <- osmdata_sp(parks, quiet = TRUE)
parks_p <- parks$osm_polygons
parks_mp <- parks$osm_polygons

parks <- st_as_sf(parks)
parks <- st_crop(parks, my_box_sf)  
plot(parks$geometry)

#add cemetery
cemetery <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "landuse", value = c("cemetery"))
cemetery <- osmdata_sp(cemetery, quiet = TRUE)
cemetery_p <- cemetery$osm_polygons
cemetery_mp <- cemetery$osm_multipolygons
cemetery_p <- st_as_sf(cemetery_p)
cemetery_mp <- st_as_sf(cemetery_mp)


cemetery <- st_crop(cemetery, my_box_sf) 