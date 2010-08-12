library(spatstat)
library(geoR)
library(maptools)


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
    function(row)paste(row[1],row[2],sep="_")
))
win = as(afg.outline, "owin")
df.list = list()
for (y in unique(year)){
    year.flags = year==y
    xi = afg$Longitude[year.flags]
    yi = afg$Latitude[year.flags]
    afg.points = as.ppp(ppp(
        xi[!is.na(xi)], 
        yi[!is.na(yi)], 
        window = win
    ))
    d <- density(afg.points,1)
    img <- as.matrix(d)
    # I'm confused about these xcol and yrows
    df <- expand.grid(x=d$xcol, y=d$yrow)
    df$z <- melt(img)$value
    df$year <- y
    df.list[[length(df.list)+1]] <- df
}

df_stacked <- do.call(rbind, df.list)


# note the x=y / y=x issue. Would be nice to get this clarified...
p <- ggplot(df_stacked,aes(x=y,y=x)) + geom_tile(aes(fill=z)) 
p <- p + facet_wrap(~year) + scale_colour_brewer()
p


