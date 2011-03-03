library(ggplot2)
library(spatstat)
library(maptools)


to_ppp <- function(long,lat,win){
    as.ppp(ppp(long,lat,window=win))
}
load("afg.data")

afg <- afg.data
spatstat.options(checkpolygons = FALSE) 
afg$data <- afg$data[!is.na(afg$data$Latitude),]

######## Everything above run before presentation

data = subset(afg$data, Type=="Friendly Fire" & DateOccurred > as.Date("2009-01-01"))
win = as(afg$outline,"owin")
points = to_ppp(data$Longitude, data$Latitude, win)
d = density(points,0.2)

img = t(as.matrix(d))
df = expand.grid(x=d$xcol, y=d$yrow)
df$z = melt(img)$value

p = ggplot(df, aes(x=x,y=y))
p + geom_tile(aes(fill=z))+theme_bw()