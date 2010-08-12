library(spatstat)
library(geoR)
library(maptools)


source('load_data.R')

afg.out <- readShapePoly(paste(shape.files,"boundary/admin1_poly_32.shp",sep=""))
spatstat.options(checkpolygons = FALSE) 
x = afg$Longitude
y = afg$Latitude
points <- as.ppp(ppp(x[!is.na(x)], y[!is.na(y)], window=as(afg.out, "owin")))
d <- density(points,0.3)



