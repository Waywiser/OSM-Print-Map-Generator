library (osmplotr)
library (osmdata)
library (magrittr)

plot(my_box)
plot(ocean, add = T)

ocean <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "natural",
                  value = c("coastline"
                  )) 
ocean <- osmdata_sp(ocean, quiet = TRUE)
ocean <- ocean$osm_lines
split <- gIntersection(my_box, ocean)               # intersect your line with the polygon
ocean_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, ocean_buf)   
blocks<- ms_explode(blocks)
blocks <- st_as_sf(blocks)
blocks$area <- round(((st_area(blocks))*0.00000038610215855), 1)
max <- max(blocks$area)
ocean <- blocks[which(blocks$area < max),]

library(mapvie)
mapview(blocks$geometry) + mapview(big_streets$osm_lines)

mapview::mapview(small_streets)
mapview::mapview(total_water)
