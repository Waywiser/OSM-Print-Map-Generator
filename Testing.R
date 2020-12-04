library(raster)
library(ggplot2)

setwd('C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator')




#####################################
r <- raster('orange.png')
bb <- extent(my_box_sf)
extent(r) <- bb

#r <- setExtent(r, bb, keepres=TRUE)

plot(r)



ggplot() +
  geom_raster(data = r, aes(x = x, y = y, fill = orange)) + 
  coord_quickmap()


#####################################
library(png)

url <- "https://www.rstudio.com/wp-content/uploads/2014/06/RStudio-Ball.png"
destfile <- "myfile.png"
r_studio <- download.file(url,destfile, mode="wb")
r_studio <- readPNG("myfile.png")

img <- r_studio
plot(img)


#####################################
library(raster)
r <- ("C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator/orange.tif")
r_imp <- raster(r)
 
plot(r_imp)

ggplot() +
  geom_raster(data = r_imp, aes(x = x, y = y, fill = orange)) + 
  coord_quickmap()

#############################################
#############################################
#############################################
#############################################
library(viridis)


datafold <- "C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator/orange.tif"
test <- raster(datafold) 

bb <- extent(my_box_sf)
extent(test) <- bb

test_spdf <- as(test, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_sf(data = all_streets,
          inherit.aes = FALSE,
          color = "white",
          lwd = 3,
          alpha = 1)+ 
  scale_fill_gradientn(colours=c("#a81502", "#f7cf89"))+
  coord_sf() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))


##############################################
##############################################
##############################################
##############################################
rec <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "landuse", value = c("recreation_ground"))
parks <- osmdata_sp(rec, quiet = TRUE)
parks <- parks$osm_polygons

parks <- st_as_sf(parks)


img_stack_crop <- crop(img_stack, parks)

img_stack=stack(datafold)
bb <- extent(my_box_sf)
extent(img_stack) <- bb

plot(streets)
plotRGB(img_stack_crop, add=T)
plot(parks, border = "green", add=T)


#trying ggplot
img_spdf <- as(img_stack, "SpatialPixelsDataFrame")
img_df <- as.data.frame(img_spdf)
img_df$alpha <- 1
rgb(235, 124, 12, 1, names = NULL, maxColorValue = 1)

ggplot() +  
  geom_tile(data=img_df, aes(x=x, y=y, fill=rgb(img_df$orange.1/255, img_df$orange.2/255, img_df$orange.3/255, img_df$alpha)), alpha=0.8) + 
  geom_sf(data = all_streets,
          inherit.aes = FALSE,
          color = "white",
          lwd = 3,
          alpha = 1)+ 
  scale_fill_gradientn(colours=c("#a81502", "#f7cf89"))+
  coord_sf() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))

