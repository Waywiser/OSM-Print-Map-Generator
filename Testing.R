


#pull water for plot
water1 <- my_box_sf %>%
    opq()%>%
    add_osm_feature(key = "waterway", 
                    value = c("riverbank"
                    )) %>%
    osmdata_sf()
water_p <- water$osm_polygons

water2 <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "water", 
                  value = c("river"
                  )) %>%
  osmdata_sf()
water2_p <- water2$osm_multipolygons


plot(my_box_sf) 
plot(water_p$geometry, col = 'blue', add = T)
plot(water2_p$geometry, col = 'red', add = T)



