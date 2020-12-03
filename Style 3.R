library(ggplot2)
library(ggpattern)
library(osmdata)
library(sp)
library(sf)

#inputs
title = "GREAT BEND"
subtitle = "KANSAS"

city_name <- (paste(title,",", subtitle, sep="", collapse=NULL))
zoom = 100

blue <- c("https://i.pinimg.com/originals/0d/c6/f0/0dc6f0f6af86d72b22fd7346010697e1.jpg")
orange<- c("https://www.achildsplacepa.org/wp-content/uploads/2018/12/GettyImages-986491786.jpg")
peach<- c("https://media1.thehungryjpeg.com/thumbs2/ori_3503830_c5c37d89f20dcbb582caab4663617214d8b1914a_watercolor-digital-paper-watercolor-background-watercolor-texture.jpg")

color = orange

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

parks <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "leisure", value = c("park","pitch", "nature_reserve", "garden", "residential"))
#add cemetery


streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()

#check plots
plot(my_box)
plot(streets, add = T, col = 'black', lwd = 2)

#convert streets to polygons and take difference
split <- gIntersection(my_box, streets)               # intersect your line with the polygon
split_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, split_buf)                 # split using gDifference


#make plot
p <- ggplot() +
  geom_sf_pattern(
    data = blocks,
    inherit.aes = FALSE,
    pattern = 'image',
    pattern_type = 'squish',
    pattern_filename = color
  ) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#cccccc",
          size = .25,
          alpha = .8) +
  theme_void()
  
#plot map
p


