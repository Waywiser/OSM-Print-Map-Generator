#inputs
city_name <- 'New Orleans'
zoom = .5

#gets OMS-defined centroid by city_ name
dat <- getbb(city_name, format_out ="data.frame", limit = 1) 
dat <- dat[,c("lat","lon")]
#converts centroid into coordinates for bounding box
cols.num <- c("lat","lon")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)


my_box <- rgeos::bbox2SP(n = (dat$lat + zoom*1.5),
                         s = (dat$lat - zoom*1.5), 
                         w = (dat$lon + zoom), 
                         e = (dat$lon - zoom),
                         proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

plot(my_box,lwd=0.1)

gc()
