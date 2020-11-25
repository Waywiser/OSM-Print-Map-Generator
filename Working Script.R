library(osmdata)
library(mapview)
library(sp)
library(rgeos)
library(plotrix)

#inputs
city_name <- 'Ithaca'
zoom = .15

#gets OMS-defined centroid by city_ name
dat <- getbb(city_name, format_out ="data.frame", limit = 1) 
dat <- dat[,c("lat","lon")]

#converts centroid into coordinates for bounding box
cols.num <- c("lat","lon")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)
#dat.coord <- dat[,c(2,1)]

#makes centroid 
coordinates(dat.coord) <- ~lon+lat
proj4string(dat.coord) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"



#make bounding box spatial
#boundingbox <- raster::extent(((getbb (city_name))))
#boundingbox <- as(boundingbox,"SpatialPolygons")
#proj4string(boundingbox) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"  


#> Loading required package: sp
my_box <- rgeos::bbox2SP(n = (dat$lat + zoom*1.5),
                         s = (dat$lat - zoom*1.5), 
                         w = (dat$lon + zoom), 
                         e = (dat$lon - zoom),
                         proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#mapview::mapview(my_box)
plot(my_box)
#plot(dat.coord)


motorway <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.5), (dat$lon + zoom), (dat$lat + zoom*1.5)))%>%
  add_osm_feature(key = "highway", value= 'motorway')
  motorway <- osmdata_sp(motorway, quiet = TRUE)
  motorway <- motorway$osm_lines
  
trunk <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.5), (dat$lon + zoom), (dat$lat + zoom*1.5)))%>%
  add_osm_feature(key = "highway", value= 'trunk')
  trunk <- osmdata_sp(trunk, quiet = TRUE)
  trunk <- trunk$osm_lines
  
primary <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.5), (dat$lon + zoom), (dat$lat + zoom*1.5)))%>%
  add_osm_feature(key = "highway", value= 'primary')
  primary <- osmdata_sp(primary, quiet = TRUE)
  primary <- primary$osm_lines
  
secondary <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.5), (dat$lon + zoom), (dat$lat + zoom*1.5)))%>%
  add_osm_feature(key = "highway", value= 'secondary')
  secondary <- osmdata_sp(secondary, quiet = TRUE)
  secondary <- secondary$osm_lines
  
tertiary <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.5), (dat$lon + zoom), (dat$lat + zoom*1.5)))%>%
  add_osm_feature(key = "highway", value= 'tertiary')
  tertiary <- osmdata_sp(tertiary, quiet = TRUE)
  tertiary <- tertiary$osm_lines
  
roads <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.5), (dat$lon + zoom), (dat$lat + zoom*1.5)))%>%
  add_osm_feature(key = "highway")
  roads <- osmdata_sp(roads, quiet = TRUE)
  roads <- roads$osm_lines
  
#clip features to bounding box
  roads <- gIntersection(roads, my_box, byid = TRUE, drop_lower_td = TRUE)
  tertiary <- gIntersection(tertiary, my_box, byid = TRUE, drop_lower_td = TRUE)
  secondary <- gIntersection(secondary, my_box, byid = TRUE, drop_lower_td = TRUE)
  primary <- gIntersection(primary, my_box, byid = TRUE, drop_lower_td = TRUE)
  trunk <- gIntersection(trunk, my_box, byid = TRUE, drop_lower_td = TRUE)
  motorway <- gIntersection(motorway, my_box, byid = TRUE, drop_lower_td = TRUE)


#plot map
  plot(my_box,lwd=0.1)
  plot(roads,lwd=0.1, col= 'lightgray', add=TRUE)
  plot(tertiary,lwd=0.2, col= 'grey', add=TRUE)
  plot(secondary,lwd=0.7, col= 'dimgrey', add=TRUE)
  plot(primary,lwd=0.1, col= 'DARKSLATEGRAY', add=TRUE)
  plot(trunk,lwd=1.4, col= 'black', add=TRUE)
  plot(motorway,lwd=1.5, col= 'dimgrey', add=TRUE)

  
  
#title box take 1  
  n = (dat$lat + zoom*1.5)
  s = (dat$lat - zoom*1.5)
  w = (dat$lon + zoom)
  e = (dat$lon - zoom)

  rect((e + .002),
       (s*1.0034),
       (w -.002),
       (n*.9998),
        col = 'white',
       border = FALSE)
  
 text(dat$lon, dat$lat + (zoom *.78), city_name, cex =2)

 
  