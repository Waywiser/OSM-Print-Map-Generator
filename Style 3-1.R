library(ggplot2)
library(ggpattern)
library(osmdata)
library(sp)
library(sf)
library(remotes)
library(rgeos)
library(rmapshaper)
library(raster)

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
parks_mp <- parks$osm_multipolygons


parks_p <- st_as_sf(parks_p)
parks_mp <- st_as_sf(parks_mp)

plot(parks_p$geometry)
plot(parks_mp$geometry)

  #parks<- st_crop(parks, my_box_sf)  

#pull cemetery
cemetery <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "landuse", value = c("cemetery"))
cemetery <- osmdata_sp(cemetery, quiet = TRUE)
cemetery_p <- cemetery$osm_polygons
cemetery_mp <- cemetery$osm_multipolygons
cemetery_p <- st_as_sf(cemetery_p)
cemetery_mp <- st_as_sf(cemetery_mp)

plot(cemetery_p$geometry)
plot(cemetery_mp$geometry)

  #cemetery <- st_crop(cemetery, my_box_sf) 


#pull water
water <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "waterway", value = c("river", "canal", "riverbank"))
water <- osmdata_sp(water, quiet = TRUE)
water_p <- water$osm_polygons
water_mp <- water$osm_multipolygons
water_p <- st_as_sf(water_p)
water_mp <- st_as_sf(water_mp)

plot(water_p$geometry)
plot(water_mp$geometry)

mapview(water_p)

#buffer streets / get blocks
split <- gIntersection(my_box, streets)               # intersect your line with the polygon
streets_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, streets_buf)   


#import rasters
otif <- "C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator/orange.tif"
otif <- "/Users/mac/Desktop/waywiser/OSM-Print-Map-Generator/orange.tif"
gtif<- "/Users/mac/Desktop/waywiser/OSM-Print-Map-Generator/green.tif"

#make roads layer
tif <- raster(otif) 
tif=stack(tif)
bb <- extent(my_box_sf)
extent(tif) <- bb

streets_buf <- st_as_sf(streets_buf)
streets_tif <- mask(tif, streets_buf)

#make parks layer
tif <- raster(gtif) 
tif=stack(tif)
bb <- extent(my_box_sf)
extent(tif) <- bb

parks_tif <- mask(tif, parks_p)

plotRGB(tif)


plotRGB(parks_tif)
plotRGB(streets_tif, add=T)
plot(parks, border = "green", add=T)

#crop data
img_stack_crop <- crop(img_stack, parks)



plot(streets_buf)
plot(parks_p, col = 'forest green', add = T )

setwd('/Users/mac/Desktop/waywiser/OSM-Print-Map-Generator/prints')
setwd("C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator/prints")
dev.copy2pdf(file="test.pdf", width = 7, height = 5)

pdf("MyPlot.pdf", height=10, width=10)
plot(runif(100), runif(100))
dev.off()

library(mapview)
mapview(parks$osm_polygons)
