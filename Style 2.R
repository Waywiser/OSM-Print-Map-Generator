library(osmdata)
library(mapview)
library(sp)
library(rgeos)
library(plotrix)
library(rgdal)
library(rmapshaper)
library(raster)
library(sf)
library(ggplot2)


#inputs
title = "GREAT BEND"
subtitle = "KANSAS"

city_name <- (paste(title,",", subtitle, sep="", collapse=NULL))
zoom = 100

blue = c("#537699","#D7D9DB", "#5B80A6","#97B1CC","#408BD6", "#387DC2", "#48596B")
multi = c("#ABE188","#F7EF99", "#F1BB87","#F78E69","#5D675B", "#042A2B", "#5EB1BF")
multi2 = c("#1082C4","#98D6FA", "#F2A274","#98D6FA","#E8C0A9", "#0474B5", "#A1623D")
multi3 = c("#4787D1","#184C87", "#A2C8F5","#E3B886","#BA956A", "#B89369", "#4487D4")
multi4 = c("#2E2D25","#736A05", "#D9D8D2","#3E469C","#B3AD6F", "#8389C9", "#6B74D6")
multi5 = c("870058", "A4303F", "F2D0A4", "FFECCC", "C8D6AF", "8DA1B9", "85BDBF")

color = blue

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

#pull streets for plot
small_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link",
                            "secondary", "tertiary", "secondary_link", "tertiary_link",
                            "residential", "living_street", "unclassified","service"
                  )) %>%
  osmdata_sf()
small_streets <- st_crop(all_streets$osm_lines, my_box_sf)

#check plots
plot(my_box)
plot(streets, add = T, col = 'black', lwd = 2)

projection(streets, asText=TRUE)
projection(my_box, asText=TRUE)

#clip features to bounding box
#streets <- gIntersection(streets, my_box, byid = TRUE, drop_lower_td = TRUE)

#convert streets to polygons and take difference
split <- gIntersection(my_box, streets)               # intersect your line with the polygon
split_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, split_buf)                 # split using gDifference

#convert to spdf and to single polygons
blocks <- SpatialPolygonsDataFrame(blocks,data=as.data.frame("blocks_df"))


#maybe convert to st here to speed things up
blocks<- ms_explode(blocks)

plot(blocks)

#random colors
blocks$color <- sample(c(color), size = nrow(blocks), replace = TRUE)

#check plots again
plot(blocks, col = blocks$color, border="NA")
plot(streets, lwd = .8, col = 'white', add = T)

#convert to ggplot readable 
blocks_sf <- st_as_sf(blocks)
crs(streets) <- "+proj=longlat +datum=WGS84 +no_defs"
mapview(streets)

#build map
map <- ggplot() +
  geom_sf(data = blocks_sf, fill = blocks_sf$color, alpha = .8)+
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .9,
          alpha = .9) +
  coord_sf(xlim = c(e, w), 
           ylim = c(s + (zoom/6), n - (zoom/6)),
           expand = FALSE)+
  theme_void()+
  ggtitle(title, subtitle = subtitle)


#plot map and set theme
print <- map + theme(
  plot.title = element_text(family = 'Trebuchet MS', color = "black", size = 100, face = "bold", hjust = 0.5, margin = margin(t = 20)),
  plot.subtitle = element_text(family = 'Trebuchet MS', color = "black", size = 40, hjust = 0.5,margin = margin(t=5, b = 20)))

setwd('/Users/mac/Desktop/waywiser/OSM-Print-Map-Generator/data')
ggsave(filename=(paste(title," Blocks",".png", sep="", collapse=NULL)), plot=print, device="png",
       path="./", height=17, width=11, units="in", dpi=300)

gc()

