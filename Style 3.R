library(ggplot2)
library(ggpattern)
library(osmdata)
library(sp)
library(sf)
library(remotes)
library(rgeos)
library(rmapshaper)

#remotes::install_github("coolbutuseless/ggpattern")

#inputs
title = "GREAT BEND"
subtitle = "KANSAS"

city_name <- (paste(title,",", subtitle, sep="", collapse=NULL))
zoom = 100

blue <- c("https://i.pinimg.com/originals/0d/c6/f0/0dc6f0f6af86d72b22fd7346010697e1.jpg")
orange<- c("https://www.achildsplacepa.org/wp-content/uploads/2018/12/GettyImages-986491786.jpg")
peach<- c("https://media1.thehungryjpeg.com/thumbs2/ori_3503830_c5c37d89f20dcbb582caab4663617214d8b1914a_watercolor-digital-paper-watercolor-background-watercolor-texture.jpg")
green <- c("https://i.pinimg.com/originals/82/68/7e/82687e742ac712c43faa592a4881d456.jpg")

color = orange
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

#pull parks
parks <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "leisure", value = c("park","pitch", "nature_reserve", "garden", "residential"))
parks <- osmdata_sp(parks, quiet = TRUE)
parks <- parks$osm_polygons
parks <- st_as_sf(parks)
parks <- st_crop(parks, my_box_sf)  
plot(parks$geometry)

#add cemetery
cemetery <- opq(bbox = c((dat$lon - zoom), (dat$lat - zoom*1.05), (dat$lon + zoom), (dat$lat + zoom*1.05)))%>%
  add_osm_feature(key = "landuse", value = c("cemetery"))
cemetery <- osmdata_sp(cemetery, quiet = TRUE)
cemetery_p <- cemetery$osm_polygons
cemetery_mp <- cemetery$osm_multipolygons
cemetery_p <- st_as_sf(cemetery_p)
cemetery_mp <- st_as_sf(cemetery_mp)


cemetery <- st_crop(cemetery, my_box_sf) 


all_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()
  all_streets <- st_crop(all_streets$osm_lines, my_box_sf)  



#convert streets to polygons and take difference
split <- gIntersection(my_box, streets)               # intersect your line with the polygon
split_buf <- gBuffer(split, width = 0.0001)        # create a very thin polygon buffer of the intersected line
blocks <- gDifference(my_box, split_buf)                 # split using gDifference
blocks <- st_as_sf(blocks)

#check plots
plot(my_box)
plot(blocks, add = T, col = 'black', lwd = 2)

#create shadow blocks
blocks_ex <- ms_explode(blocks)
alpha = c(.4, .2, .1, 0, .5, .35, .3)
blocks_ex$alpha <- sample(c(alpha), size = nrow(blocks_ex), replace = TRUE)

#make plot
p <- ggplot() +
  geom_sf_pattern(
    data = blocks,
    inherit.aes = FALSE,
    pattern = 'image',
    pattern_type = 'squish',
    pattern_filename = color,
    lwd = 0
  ) +
  geom_sf(data = blocks_ex,
          inherit.aes = FALSE,
          fill = "orange",
          lwd = 0,
          alpha = blocks_ex$alpha*1.1
          )+ 
  geom_sf_pattern(
    data = cemetery_p,
    inherit.aes = FALSE,
    pattern = 'image',
    pattern_type = 'squish',
    pattern_filename = green,
    lwd = 0,
    alpha = 1
  ) +
  geom_sf_pattern(
    data = parks,
    inherit.aes = FALSE,
    pattern = 'image',
    pattern_type = 'squish',
    pattern_filename = green,
    lwd = 0,
    alpha = 1
  ) +
  geom_sf(data = all_streets,
          inherit.aes = FALSE,
          color = "white",
          lwd = 3,
          alpha = 1)+ 
  geom_sf(data = my_box_sf,
          inherit.aes = FALSE,
          color = "white",
          size = 2.5,
          alpha = 0)+ 
  theme_void()

#plot map
p




m <- ggplot() +
  geom_sf_pattern(
    data = blocks,
    inherit.aes = FALSE,
    pattern = 'image',
    pattern_type = 'squish',
    pattern_filename = color
  ) 
m
