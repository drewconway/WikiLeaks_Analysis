library(spatstat)
library(geoR)
library(maptools)
library(mapproj)

# this script uses a kernel smoother to create an animated heat map of 
# activity in afghanistan over 2004 to 2009. Warning: this script takes a long
# time to run, due to the slow speed of plotting so much information

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
unix_start = as.Date("1970-01-01")
for (day in seq(3)){
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
    p <- p + geom_path(data=roads,    aes(y=lat,x=long,group=group), size=1.5)
    p <- p + geom_path(data=roads,    aes(y=lat,x=long,group=group), size=1, 
        colour="darkseagreen1")
    p <- p + scale_fill_gradient(
        "Intensity",
        low="cornsilk2",
        high="firebrick",
        limits=c(0, 1),
        legend=FALSE
    )
    # this bit doesn't quite work - need to sort out the origin.
    now.posix <- as.POSIXct(now,origin=unix_start)
    df.date = data.frame(x=70,y=30,t=format(now.posix,"%B %Y"))
    p <- p + geom_text(data = df.date, aes(x=x, y=y, label=t))
    p <- p + opts(panel.grid.major = theme_blank())
    p <- p + opts(panel.grid.minor = theme_blank())
    p <- p + opts(panel.background = theme_blank())
    p <- p + opts(axis.title.x = theme_blank())
    p <- p + opts(axis.title.y = theme_blank())
    p <- p + opts(axis.ticks = theme_blank())
    p <- p + opts(panel.border = theme_blank())
    p <- p + opts(axis.text.x = theme_blank())
    p <- p + opts(axis.text.y = theme_blank())
    # TODO - make this not square
    ggsave(filename=paste('/Users/mike/Data/frames/afghanistan_',day,'.png',sep=""), plot = p)
    cat(paste("saved frame",day))
}
# run this command to join the files
# ffmpeg -f image2 -r 10 -i ~/Data/frames/afghanistan_%d.png -b 600k afghanistan.mp4
