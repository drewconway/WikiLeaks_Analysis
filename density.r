library(spatstat)
library(geoR)
library(maptools)
library(mapproj)

# this script uses a kernel smoother to create an animated heat map of 
# activity in afghanistan over 2004 to 2009. 

# Warning: this script takes a long time to run, due to the slow speed of 
# plotting so much information. Ideas to speed it up are welcomed! 

# Warning: you need to set this path to point at a place where you can store
# a large number of frames
frames_dir = "/Users/mike/Data/frames/"

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
# extract time in a usable format
t = unclass(as.POSIXct.Date(afg$DateOccurred))
# 6 years
num_weeks = 6 * 52  # weeks
day_duration = 60 * 60 * 24 # seconds
now = t[1]
num_days = round(t[length(t)]-t[1]) / day_duration
# various periods for temporal smoothing
three_months = day_duration * 31 * 3 # three months is a bit too long
one_month = day_duration * 31
two_weeks = day_duration * 14 # two weeks is a bit too quick
unix_start = as.Date("1970-01-01")
for (day in seq(num_days)){
    time.flags = (t < now) & (t > now-one_month)
    long = afg$Longitude[time.flags]
    lat = afg$Latitude[time.flags]
    afg.points = as.ppp(ppp(
        long[!is.na(long)], 
        lat[!is.na(lat)], 
        window = win
    ))
    now <- now+day_duration
    # calculate kernel smoothing
    d <- density(afg.points,0.2)
    img <- t(as.matrix(d)) # note transposition
    # build grid points
    df <- expand.grid(x=d$xcol, y=d$yrow)
    df$z <- melt(img)$value
    # sort out a colour range, then threshold and normalise
    cmin = 0
    cmax = 60 # 60 events per window length
    # threshold
    df$z[df$z < cmin] = cmin
    df$z[df$z > cmax] = cmax
    # normalise
    df$z <- df$z / max(df$z[!is.na(df$z)])
    # build up plot
    p <- ggplot(df, aes(x=x,y=y)) + geom_tile(aes(fill=z))
    # remove theme
    p <- p + theme_bw()
    # add boundaries, roads
    p <- p + geom_path(data=afg.poly, aes(y=lat,x=long,group=group), alpha=0.1)
    p <- p + geom_path(data=outline,  aes(y=lat,x=long,group=group), size=0.5)
    p <- p + geom_path(data=roads,    aes(y=lat,x=long,group=group), size=1.5)
    p <- p + geom_path(data=roads,    aes(y=lat,x=long,group=group), size=1, 
        colour="darkgoldenrod1")
    # add fill
    p <- p + scale_fill_gradient(
        "Intensity",
        low="cornsilk2",
        high="firebrick",
        limits=c(0, 1),
        legend=FALSE
    )
    # add time
    now.posix <- as.POSIXct(now,origin=unix_start)
    df.date = data.frame(x=70,y=30,t=format(now.posix,"%B %Y"))
    p <- p + geom_text(data = df.date, aes(x=x, y=y, label=t), hjust=0)   
    # remove all grid stuff (thanks John!) 
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
    cat(paste("processing frame",day,"\n\t"))
    ggsave(filename=paste(frames_dir,'afghanistan_',day,'.png',sep=""), plot = p)
}
# run this command to join the files (replacing the path as necessary)
# ffmpeg -f image2 -r 20 -i ~/Data/frames/afghanistan_%d.png -b 600k afghanistan.mp4
