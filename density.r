library(spatstat)
library(geoR)
library(maptools)
library(mapproj)


source('load_data.R') #afg, year, other stuff...

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
cat("fortifying polygons")
roads = fortify.SpatialLinesDataFrame(ringroad)
outline = fortify.SpatialPolygons(afg.outline)
# build geoms



t = unclass(as.POSIXct.Date(afg$DateOccurred))
# 6 years
num_weeks = 6 * 52  # weeks
day_duration = 60 * 60 * 24 # seconds
now = t[1]
num_days = round(t[length(t)]-t[1]) / day_duration
three_months = day_duration * 31 * 3
for (day in seq(num_days)){
    time.flags = (t > now) & (t < now+three_months)
    long = afg$Longitude[time.flags]
    lat = afg$Latitude[time.flags]
    afg.points = as.ppp(ppp(
        long[!is.na(long)], 
        lat[!is.na(lat)], 
        window = win
    ))
    now <- now+day_duration
    d <- density(afg.points,0.2)
    img <- t(as.matrix(d)) # note transposition
    df <- expand.grid(x=d$xcol, y=d$yrow)
    df$z <- melt(img)$value
    
    cmin = 0
    cmax = 60
    # threshold
    df$z[df$z < cmin] = cmin
    df$z[df$z > cmax] = cmax
    # normalise
    df$z <- df$z / max(df$z[!is.na(df$z)])
    
    p <- ggplot(df, aes(x=x,y=y)) + geom_tile(aes(fill=z))
    p <- p + theme_bw() #+ coord_map() #coord_map only works on the paths
    p <- p + geom_path(data=afg.poly, aes(y=lat,x=long,group=group), alpha=0.1)
    p <- p + geom_path(data=outline,  aes(y=lat,x=long,group=group), size=0.5)
    p <- p + geom_path(data=roads,    aes(y=lat,x=long,group=group), size=1)
    p <- p + scale_fill_gradient(
        "Intensity",
        low="white",
        high="red",
        limits=c(0, 1)
    )
    # TODO - make this not square
    ggsave(filename=paste('/Users/mike/Data/frames/afghanistan_',day,'.png',sep=""), plot = p)
    cat(paste("saved frame",day))
}

