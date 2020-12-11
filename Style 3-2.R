library(ggplot2)
library(ggpattern)
library(osmdata)
library(sp)
library(sf)
library(remotes)
library(rgeos)
library(rmapshaper)
library(raster)
library(geojsonio)


#inputs
title = "PARIS"
subtitle = "FRANCE"

city_name <- (paste(title,",", subtitle, sep="", collapse=NULL))
zoom = 80

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

########################
########################
########################
#pulling data


#pull streets for blocks
streets <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "highway", value = c("motorway","primary", "secondary", "tertiary", "residential"))
streets <- osmdata_sp(streets, quiet = TRUE)
streets <- streets$osm_lines

#pull parks
parks<- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "leisure", 
                  value = c("parks", "pitch", "nature_reserve", "garden", "residential"
                  )) %>%
  osmdata_sf()

parks_p <- parks$osm_polygons
parks_mp <- parks$osm_multipolygons
reqd <- "geometry"
parks_p <- parks_p[,reqd]
parks_mp <- parks_mp[,reqd]
parks_combined <- rbind(parks_p, parks_mp)
st_is_valid(parks_combined, reason = TRUE)
parks_combined<- st_make_valid(parks_combined)
st_is_valid(parks_combined, reason = TRUE)
total_parks <- st_union(parks_combined)
total_parks <- as(total_parks, 'Spatial')
total_parks <- gIntersection(my_box, total_parks)
total_parks <- st_as_sf(total_parks)

#pull cemetery
cemetery <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "landuse", value = c("cemetery"))
cemetery <- osmdata_sp(cemetery, quiet = TRUE)
cemetery_p <- cemetery$osm_polygons
cemetery_mp <- cemetery$osm_multipolygons

cemetery<- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "landuse", 
                  value = c("cemetery"
                  )) %>%
  osmdata_sf()
cemetery_p <- cemetery$osm_polygons
cemetery_mp <- cemetery$osm_multipolygons
reqd <- "geometry"
cemetery_p <- cemetery_p[,reqd]
cemetery_mp <- cemetery_mp[,reqd]
cemetery_combined <- rbind(cemetery_p, cemetery_mp)
st_is_valid(cemetery_combined, reason = TRUE)
cemetery_combined<- st_make_valid(cemetery_combined)
st_is_valid(cemetery_combined, reason = TRUE)
total_cemetery <- st_union(cemetery_combined)
total_cemetery <- as(total_cemetery, 'Spatial')
total_cemetery <- gIntersection(my_box, total_cemetery)
total_cemetery <- st_as_sf(total_cemetery)


#pull water
#pull water for plot
water1 <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "waterway", 
                  value = c("riverbank"
                  )) %>%
  osmdata_sf()
water1_p <- water1$osm_polygons
water1_mp <- water1$osm_multipolygons

water2 <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "water", 
                  value = c("river", "lake"
                  )) %>%
  osmdata_sf()
water2_p <- water2$osm_polygons
water2_mp <- water2$osm_multipolygons


water3 <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = c("bay"
                  )) %>%
  osmdata_sf()
water3_p <- water3$osm_polygons
water3_mp <- water3$osm_multipolygons

#remove all columns except geometry
reqd <- "geometry"
water1_p <- water1_p[,reqd]
water1_mp <- water1_mp[,reqd]
water2_p <- water2_p[,reqd]
water2_mp <- water2_mp[,reqd]
water3_p <- water3_p[,reqd]
water3_mp <- water3_mp[,reqd]

#combine all water features
water_combined <- rbind(water1_p, water1_mp, water2_p, water2_mp, water3_p, water3_mp)

#check geometries and make valid
st_is_valid(water_combined, reason = TRUE)
water_combined<- st_make_valid(water_combined)
st_is_valid(water_combined, reason = TRUE)

#water2_mp %>% st_buffer(0)
total_water <- st_union(water_combined)
total_water <- as(total_water, 'Spatial')
total_water <- gIntersection(my_box, total_water)

#convert back to sf
total_water <- st_as_sf(total_water)

########################
########################
########################
#end pulling sequence

#buffer streets / get blocks
split <- gIntersection(my_box, streets)               # intersect your line with the polygon
streets_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, streets_buf)   

#build plot and export

#plot
plot(streets_buf, col = 'grey', border = NA)
plot(blocks, col = '#f7eb8b', border = NA, add = T)
plot(total_parks, col = 'light green', border = NA, add = T )
plot(total_water, col = 'light blue', border = NA, add = T)

setwd('/Users/mac/Desktop/waywiser/OSM-Print-Map-Generator/prints')
setwd("C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator/prints")
dev.copy2pdf(file="PARIS blank.pdf", width = 11, height = 17)

pdf("MyPlot.pdf", height=10, width=10)
plot(runif(100), runif(100))
dev.off()
