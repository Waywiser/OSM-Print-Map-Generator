library(sp)
library(rgeos)
library(ggplot2)
library(rgdal)

#inputs
city_name <- 'Paris'
zoom = .15

#gets OMS-defined centroid by city_ name
dat <- getbb(city_name, format_out ="data.frame", limit = 1) 
dat <- dat[,c("lat","lon")]
#converts centroid into coordinates for bounding box
cols.num <- c("lat","lon")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)

n = (dat$lat + (zoom*1.2))
s = (dat$lat - (zoom*1.2)) 
w = (dat$lon + zoom) 
e = (dat$lon - zoom)


#makes bounding box
my_box <- rgeos::bbox2SP(n, s, w, e,
                         proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#plots
plot(my_box,lwd=0.5)
temp <- legend(x = c(e, w), y = (c(n-zoom/4, (s+zoom*2.24))), legend = c(" ", " "), family="Arial",
               title = city_name)
