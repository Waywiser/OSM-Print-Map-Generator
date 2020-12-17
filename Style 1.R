library(osmdata) 
library(ggmap)
library(sf)
library(sp)



###############################
########### INPUTS ############
###############################


city = "DUBLIN"
country = "IRELAND"
zoom.level = 5


save.location <- 'C:/Users/Ari/Documents/GitHub/OSM-Print-Map-Generator/prints'



###############################
###############################
######### SCRIPT ##############
###############################
###############################

city_name <- (paste(city,",", country, sep="", collapse=NULL))
zoom= 1/zoom.level

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


#pulling data
big_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
                  osmdata_sf()


med_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
                  osmdata_sf()


small_streets <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service"
                  )) %>%
                   osmdata_sf()

small_streets <- small_streets$osm_lines
small_streets <- small_streets[c("name","geometry")]

railway <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()


coastline <- my_box_sf %>%
  opq()%>%
  add_osm_feature(key = "natural", 
                  value = c("coastline"
                  )) %>%
  osmdata_sf()
coastline <- coastline$osm_lines
plot(coastline$geometry)



#create map
map  <- 
ggplot() +
  geom_sf(data = total_water,
          inherit.aes = FALSE,
          fill = '#233036',
          color = "white",
          size = .1,
          alpha = 1) +
  geom_sf(data = small_streets,
          inherit.aes = FALSE,
          color = "#cccccc",
          size = .25,
          alpha = 1) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#919191",
          size = .65,
          alpha = 1) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "#cccccc",
          size = .05,
          alpha = 1) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#919191",
          size = .65,
          alpha = 1) +
  geom_sf(data = coastline,
          inherit.aes = FALSE,
          color = "#919191",
          size = .65,
          alpha = 1) +
  coord_sf(xlim = c(e, w), 
           ylim = c(s + (zoom/6), n - (zoom/6)),
           expand = FALSE)+
  theme_void()
  ggtitle(city, subtitle = country)
  
  map

#plot map and set theme
print <- map + theme(
  plot.title = element_text(family = 'Trebuchet MS', color = "black", size = 100, face = "bold", hjust = 0.5, margin = margin(t = 20)),
  plot.subtitle = element_text(family = 'Trebuchet MS', color = "black", size = 40, hjust = 0.5,margin = margin(t=5, b = 20)))

print



setwd(save.location)
ggsave(filename=(paste(city_name,".png", sep="", collapse=NULL)), plot=map, device="png",
       path="./", height=11, width=8.5, units="in", dpi=300)




