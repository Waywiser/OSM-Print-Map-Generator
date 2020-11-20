city_name = "Ithaca"
color=24

library('osmdata')
library('sp')
library('rgeos')
library('mapview')
library('tmap')


#get bounding box
((getbb (city_name)))
getbb(city_name, format_out ="data.frame", limit = 3)




#get roads
# Roads in 3 classes
  roads <- opq (city_name) %>%
    add_osm_feature(key = "highway") 
    roads <- osmdata_sp(roads, quiet = TRUE)
    roads <- roads$osm_lines
    
  motorway <- opq (city_name) %>%
    add_osm_feature(key = "highway", value="motorway")
    motorway <- osmdata_sp(motorway, quiet = TRUE)
    motorway <- motorway$osm_lines
  
  trunk <- opq (city_name) %>%
    add_osm_feature(key = "highway", value="trunk")
    trunk <- osmdata_sp(trunk, quiet = TRUE)
    trunk <- trunk$osm_lines
    
  primary <- opq (boundingbox) %>%
    add_osm_feature(key = "highway", value="primary")
    primary <- osmdata_sp(primary, quiet = TRUE)
    primary <- primary$osm_lines
  
#bounding box to spatial
    boundingbox <- raster::extent(((getbb (city_name))))
    boundingbox <- as(boundingbox,"SpatialPolygons")
    proj4string(boundingbox) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"  

#clip features to bounding box
    motorway <- gIntersection(motorway, boundingbox, byid = TRUE, drop_lower_td = TRUE)
    trunk <- gIntersection(trunk, boundingbox, byid = TRUE, drop_lower_td = TRUE)
    roads <- gIntersection(roads, boundingbox, byid = TRUE, drop_lower_td = TRUE)
   
#Plot All Features
  #plot(boundingbox,lwd=0.2)
    mapview(trunk) + mapview(boundingbox)
    mapview(boundingbox)

    plot(roads,lwd=0.1, col= color)
    plot(primary,lwd=1, col= 'black', add=TRUE)
    plot(trunk,lwd=0.4, col= 'black', add=TRUE)
    plot(coastline,lwd=0.1, col= 'light blue', add=TRUE)
    plot.margin=grid::unit(c(0,0,0,0), "mm")
    mtext(city_name, side=3, line=.1, col="black", font=2,cex=1.5, family="mono")
    
  
  