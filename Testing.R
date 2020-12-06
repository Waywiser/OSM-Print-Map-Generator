


#pull water for plot
  #Waterway pull
    water1 <- my_box_sf %>%
        opq()%>%
        add_osm_feature(key = "waterway", 
                        value = c("riverbank"
                        )) %>%
        osmdata_sf()
    water1_p <- water1$osm_polygons

  #water pull
    water2 <- my_box_sf %>%
      opq()%>%
      add_osm_feature(key = "water", 
                      value = c("river", "lake"
                      )) %>%
      osmdata_sf()
    water2_mp <- water2$osm_multipolygons
    water2_p <- water2$osm_polygons

  #natural - water pull
    water3 <- my_box_sf %>%
      opq()%>%
      add_osm_feature(key = "natural", 
                      value = c("water"
                      )) %>%
      osmdata_sf()
    water3_mp <- water3$osm_multipolygons
    water3_p <- water3$osm_polygons




plot(my_box_sf) 
plot(water1_p$geometry, col = 'blue', add = T)
plot(water2_mp$geometry, col = 'red', add = T)
plot(water2_p$geometry, col = 'red', add = T)
plot(water3_p$geometry, col = 'green', add = T)
plot(water3_mp$geometry, col = 'green', add = T)



mapview(water3_mp)



