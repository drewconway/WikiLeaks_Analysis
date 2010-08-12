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
    d <- density(afg.points,0.3)
    img <- as.matrix(d)
    # I'm confused about these xcol and yrows
    df <- expand.grid(x=a$xcol, y=a$yrow)
    df$z <- melt(img)$value
    df.list <- c(df.list,df)
}

df_stacked <- do.call(rbind, df.list)


# note the x=y / y=x issue. Would be nice to get this clarified...
p <- ggplot(df,aes(x=y,y=x)) + geom_tile(aes(fill=z))



