library(spatstat)
library(geoR)
library(maptools)
library(mapproj)


source('load_data.R') #afg, year, other stuff...

afg.outline <- readShapePoly(
    paste(shape.files,"boundary/admin1_poly_32.shp",sep="")
)
spatstat.options(checkpolygons = FALSE) 
x = afg$Longitude
y = afg$Latitude
l = factor(apply(
    cbind(afg$AttackOn,afg$Type),
    1,
    function(row)paste(row[1], row[2], sep="_")
))
win = as(afg.outline, "owin")
# TODO - roads (thickness by road type?)
# TODO - boundaries (low alpha)
# TODO - outline (fat)
# roads = fortify.Lines(afg.road)
t = unclass(as.POSIXct.Date(afg$DateOccurred))
# 6 years
num_weeks = 6 * 52  # weeks
day = 60 * 60 * 24 # days (seconds)
now = t[1]
num_days = round(t[length(t)]-t[1]) / day
three_months = day * 31 * 3
for (day in seq(2)){
    time.flags = (t > now) & (t < now+three_months)
    xi = afg$Longitude[time.flags]
    yi = afg$Latitude[time.flags]
    afg.points = as.ppp(ppp(
        xi[!is.na(xi)], 
        yi[!is.na(yi)], 
        window = win
    ))
    d <- density(afg.points,0.2)
    img <- as.matrix(d)
    # I'm confused about these xcol and yrows
    df <- expand.grid(x=d$xcol, y=d$yrow)
    df$z <- melt(img)$value
    # look at the x and y things. awful
    p <- ggplot(df, aes(x=y,y=x)) + geom_tile(aes(fill=z)) + coord_map() 
    # TODO - outlines and boundaries
    # p <- p + geom_path(data=roads,aes(y=lat,x=long,group=group,alpha=0.4))
    # TODO - make this not square
    ggsave(filename=paste('afghanistan_',day,'.png',sep=""), plot = p)
}

