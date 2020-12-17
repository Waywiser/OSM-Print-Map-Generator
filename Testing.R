library(rgeos)
#pull water for plot
water1 <- my_box_sf %>%
    opq()%>%
    add_osm_feature(key = "waterway",
                    value = c("riverbank", "dock"
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
                  value = c("bay", "strait", "wetland", "water"
                  )) %>%

  osmdata_sf()
water3_p <- water3$osm_polygons
water3_mp <- water3$osm_multipolygons


water4 <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "landuse",
                  value = c("resevoir"
                  )) %>%
  
  osmdata_sf()
water4_p <- water4$osm_polygons
water4_mp <- water4$osm_multipolygons

plot(my_box_sf)
plot(coastline$geometry, add =T)

#remove all columns except geometry
reqd <- "geometry"
water1_p <- water1_p[,reqd]
water1_mp <- water1_mp[,reqd]
water2_p <- water2_p[,reqd]
water2_mp <- water2_mp[,reqd]
water3_p <- water3_p[,reqd]
water3_mp <- water3_mp[,reqd]
water4_p <- water4_p[,reqd]
water4_mp <- water4_mp[,reqd]

#combine all water features
water_combined <- rbind(water1_p, water1_mp, water2_p, water2_mp, water3_p, water3_mp,water4_p, water4_mp)

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

