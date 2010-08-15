# File-Name:       wikileaks_road_distance.R                 
# Date:            2010-08-14                      
# Author:          Harlan Harris
# Email:           harlan@harris.name                                      
# Purpose:         Computes distance of events to the major roads.
# Data Used:       afg.csv (http://leakmirror.wikileaks.org/file/straw-glass-and-bottle/afg-war-diary.csv.7z)
# Packages Used:   
# Output File:     
# Data Output:     
# Machine:         

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

source("load_data.R")

polyline2df <- function(m) {
  # the df will be rows(m)-1 rows long
  df.rows <- nrow(m)-1
  df <- data.frame(x1=numeric(df.rows), x2=numeric(df.rows), y1=numeric(df.rows), y2=numeric(df.rows))
  for (r in seq(df.rows)) {
    df[r, ] <- c(m[r,1], m[r+1,1], m[r,2], m[r+1,2])
  }
  df
}

# bleah, this is so horrible because everything is buried in objects and can't be
# iterated over
rr.segments <- data.frame()
for (rr.idx in 1:nrow(ringroad)) {
  rr <- coordinates(ringroad[rr.idx, ])[[1]][[1]]
  rr.segments <- rbind(rr.segments, polyline2df(rr))
}

# next step: find the distance from each event in afg to the segments in rr.segments,
# adding a column to afg

min.distance.lineseq <- function(from.lon, from.lat, lon1s, lon2s, lat1s, lat2s, 
      k.per.lat=111, k.per.lon=92) {
  # For a single "from" point, computes the minimum distance to any of the line
  # segments defined by (lat1, lon1)-(lat2,lon2).
  #
  # http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment/1501725#1501725
  #
  # k.per.lat, k.per.lon - approximate conversions between degrees and km
  ######

  # recast into km coordinates
  from.x <- from.lon * k.per.lon
  from.y <- from.lat * k.per.lat
  x1s <- lon1s * k.per.lon
  x2s <- lon2s * k.per.lon
  y1s <- lat1s * k.per.lat
  y2s <- lat2s * k.per.lat
  
  # squared length of the segments
  len.sq <- (x1s - x2s)^2 + (y1s - y2s)^2
  # what do we do if any of the segments are actually points???

  # Consider the line extending the segment, parameterized as (x1,y1) + t ((x2,y2) - (x1,y1)).
  # We find projection of point from onto the line. 
  # It falls where t = [(from-p1) . (p2-p1)] / |p2-p1|^2

  # we'll compute, for each line segment, three distances:
  # to the extended line and to each of the two endpoints
  # plus t will tell us which one to use
  t <- (((from.x - x1s) * (x2s - x1s)) + ((from.y - y1s) * (y2s - y1s))) / len.sq
  t[is.na(t)] <- 0
  
  d.1 <- sqrt((from.x - x1s)^2 + (from.y - y1s)^2)
  d.2 <- sqrt((from.x - x2s)^2 + (from.y - y2s)^2)
  proj.x <- x1s + t * (x2s - x1s)
  proj.y <- y1s + t * (y2s - y1s)
  d.proj <- sqrt((from.x - proj.x)^2 + (from.y - proj.y)^2)

  min(ifelse(t < 0, d.1, ifelse(t > 1, d.2, d.proj)))
}
stopifnot(all.equal(min.distance.lineseq(61, 61, c(60,60,60), c(63,63,57), c(60,62,61), c(60,61,58)),
		    68.65569,
		    tol=.001))

system.time(
distances <- aaply(afg, 1, function(event) {
  with(rr.segments,
    min.distance.lineseq(event$Longitude, event$Latitude, x1, x2, y1, y2))
}, .progress='text')
)

afg$distToRoad <- distances

plot.dist1 <- ggplot(afg, aes(log(distToRoad), ..density..)) + geom_histogram() + scale_x_log10() + 
  facet_wrap(~Year) + coord_cartesian(xlim = c(.01,100)) 


