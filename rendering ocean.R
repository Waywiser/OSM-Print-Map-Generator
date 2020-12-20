library (osmplotr)
library (osmdata)
library (magrittr)

plot(my_box)
plot(coastline$geometry, add = T)


streets <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "natural", value = c("coastline"))
streets <- osmdata_sp(streets, quiet = TRUE)
streets <- streets$osm_lines


split <- gIntersection(my_box, streets)               # intersect your line with the polygon
streets_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, streets_buf)   

mapview::mapview(blocks)

library(rmapshaper)
blocks<- ms_explode(blocks)


blocks <- st_as_sf(blocks)
blocks$area <- round(((st_area(blocks))*0.00000038610215855), 1)
max <- max(blocks$area)
ocean <- blocks[which(blocks$area < max),]
mapview::mapview(ocean$geometry)

plot(my_box)
plot(ocean$geometry, add = T, col = "black")

