library(spatstat)
library(geoR)
library(maptools)
library(mapproj)

source('load_data.R')

spatstat.options(checkpolygons = FALSE)
win <- as(afg.outline, "owin")
roads <- fortify.SpatialLinesDataFrame(ringroad)
outline <- fortify.SpatialPolygons(afg.outline)

summary.terms <- scan('summary_terms.txt', what = 'character')

for (term in summary.terms)
{
  # Set up a column in afg indicating whether each event summary mentions term.
  for (i in 1:nrow(afg))
  {
    if (grepl(term, strsplit(tolower(afg[i, 'Summary']), '\\s+')))
    {
      afg[i, term] <- 1
    }
    else
    {
      afg[i, term] <- 0
    }
  }
  
  # Visualize events using Mike's approach.
  longitude <- with(subset(afg, get(term) == 1), Longitude)
  latitude <- with(subset(afg, get(term) == 1), Latitude)
  afg.points <- as.ppp(ppp(longitude[!is.na(longitude)],
                           latitude[!is.na(latitude)],
                           window = win))
  d <- density(afg.points, 0.2)
  img <- t(as.matrix(d))
  df <- expand.grid(x = d$xcol, y = d$yrow)
  df$z <- melt(img)$value
  
  p <- ggplot(df, aes(x = x,y = y)) +
         geom_tile(aes(fill = z))
  p <- p + theme_bw()
  p <- p + geom_path(data = afg.poly,
                     aes(y = lat, x = long, group = group),
                     alpha = 0.1)
  p <- p + geom_path(data = outline,
                     aes(y = lat, x = long, group = group),
                     size = 0.5)
  p <- p + geom_path(data = roads,
                     aes(y = lat, x = long, group = group),
                     size = 1)
  p <- p + scale_fill_gradient("Intensity",
                               low = "white",
                               high = "red")
  df.date <- data.frame(X = 70, Y = 30, Term = term)
  p <- p + geom_text(data = df.date,
                     aes(x = X, y = Y, label = Term))
  p <- p + opts(panel.grid.major = theme_blank())
  p <- p + opts(panel.grid.minor = theme_blank())
  p <- p + opts(panel.background = theme_blank())
  p <- p + opts(axis.title.x = theme_blank())
  p <- p + opts(axis.title.y = theme_blank())
  p <- p + opts(axis.ticks = theme_blank())
  p <- p + opts(panel.border = theme_blank())
  p <- p + opts(axis.text.x = theme_blank())
  p <- p + opts(axis.text.y = theme_blank())
  ggsave(filename = paste('images/terms/', term, '.png', sep = ''),
         plot = p)
}
