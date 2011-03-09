# File-Name:       wikileaks_analysis.R                 
# Date:            2010-07-26                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Load and format WikiLeaks data, to be loaded in the header of all subsequent files
# Data Used:       afg.csv (http://leakmirror.wikileaks.org/file/straw-glass-and-bottle/afg-war-diary.csv.7z)
# Packages Used:   ggplot2,plyr,maptools,Zelig,
# Output File:     
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# For data manipulation and visualization
library(ggplot2)
library(plyr)
library(maptools)
library(RColorBrewer)

library(spatstat)
library(geoR)
library(maptools)
library(mapproj)

# For models
# library(geoR)
# library(topicmodels)

# Shapefile directory
shape.files<-"shapefiles/"

### DATA CLEAN ###

# This will take several seconds on most laptops
cat("reading data\n")
afg<-read.csv("/Users/mike/Dropbox/Projects/WikiLeaks_Analysis/Wikidata/afg.csv",stringsAsFactors=FALSE)

# Add header data leftout by WikiLeaks, label reference taken from http://wardiary.wikileaks.org/
colnames(afg)<-c("ReportKey","DateOccurred","Type","Category","TrackingNumber","Title","Summary","Region","AttackOn",
    "ComplexAttack","ReportingUnit","UnitName","TypeOfUnit","FriendlyWIA","FriendlyKIA","HostNationWIA","HostNationKIA",
    "CivilianWIA","CivilianKIA","EnemyWIA","EnemyKIA","EnemyDetained","MGRS","Latitude","Longitude","OriginatorGroup",
    "UpdatedByGroup","CCIR","Sigact","Affiliation","DColor","Classification")

cat("converting date\n")    
# Convert date to R format
afg$DateOccurred <- as.Date(afg$DateOccurred)
year <- format.Date(afg$DateOccurred,"%Y")

# Collapse bad region data
afg$Region[grep("RC ",afg$Region,fixed=T,invert=T)]<-"UNKNOWN"
afg$Region<-as.factor(afg$Region)

# Aggregate WIA and KIA data into new columns
all.wia<-afg$FriendlyWIA+afg$HostNationWIA+afg$CivilianWIA+afg$EnemyWIA
all.kia<-afg$FriendlyKIA+afg$HostNationKIA+afg$CivilianKIA+afg$EnemyKIA
all.cas<-all.wia+all.kia

# Create a new column to identify where Summary column includes the word 'contractor'
has.cntr<-rep(FALSE,nrow(afg))
has.cntr[grep("contractor",afg$Summary,fixed=FALSE,ignore.case=TRUE)]<-TRUE

# Add new columns to afg dataframe
afg<-transform(afg,AllKIA=all.kia,AllWIA=all.wia,AllCasualty=all.cas,Year=year,HasCntr=has.cntr)
    
# Create some useful unit subsets for the time-series analysis
cjtf82<-subset(afg,afg$ReportingUnit=="CJTF-82")
paladin<-subset(afg,afg$ReportingUnit=="TF PALADIN LNO")
cjsotf<-subset(afg,afg$ReportingUnit=="CJSOTF-A")

cat("reticulating splines\n")
# Load shapefiles
# Afghanistan adminstrative file

#afg.shp <- readShapePoly(paste(shape.files,"admin/admin3_poly_32.shp",sep=""))
#afg.poly <- fortify.SpatialPolygons(afg.shp)

#afg.outline <- readShapePoly(paste(shape.files,"boundary/admin1_poly_32.shp",sep=""))
#intl.poly<-fortify.SpatialPolygons(afg.outline)

# Road files
# OK, there's some bad data in these shape files that triggers a bug in
# maptools. This works around it.
#trace(".shp2LinesDF",
#  quote({good <- (df$LENGTH_ != 0); df <- df[good, ]; shapes <- shapes[good] }),
#  at=7,
#  print=FALSE,
#  where=readShapeLines)

#afg.road <- readShapeLines(paste(shape.files,"roads/roads-all.shp",sep=""))
#road.poly<-fortify.Lines(afg.road)
#ringroad <- afg.road[afg.road$CLASS==1, ]

# get the distances to the road, either by loading it from a file or running
# a (long!) script
#if (file.exists('distances.Rsave')) {
#  load('distances.Rsave')
#  afg$distToRoad <- distances
#} else {
#  source("wikileaks_road_distance.R")
#}

# Settlements
afg.sett <- readShapePoints("shapefiles/points/settlements/07_03_settlements.shp") 
# pick out capital and major cities
sett.flag = (afg.sett$TYPE==3) | (afg.sett$TYPE==2)
afg.sett <- afg.sett[sett.flag,]
# these are hand chosen offsets so we can see the settlement names better
offsets <- c('u','l','l','d','','d','','','l','','','','','l','','','','d',
'','','d','d','','','','','','','','d','','')
hjust = c()
vjust = c()
for (o in offsets){
    if (o=='l'){
        hjust = c(hjust,1.1)
        vjust = c(vjust,-0.1)
    } else if (o=='d'){
        hjust = c(hjust,-0.1)
        vjust = c(vjust,1.1)
    } else if (o=='u'){
        hjust = c(hjust,-0.1)
        vjust = c(vjust,-0.2)
    } else {
        hjust = c(hjust,-0.1)
        vjust = c(vjust,-0.1)
    }
}
afg.sett$hjust <- hjust
afg.sett$vjust <- vjust
# save files to speed up life
afg.data = list(
    data = afg,
    outline = afg.outline, 
    admin = afg.shp,
    road = afg.road,
    ringroad = ringroad,
    sett = afg.sett,
    distToRoad = afg$distToRoad
)
# convert Type to a factor so that ggplot behaves when animating
afg.data$data$Type[afg.data$data$Type==""] = "Other"
afg.data$data$Type[afg.data$data$Type=="Counter Insurgency"] = "Counter-Insurgency"
afg.data$data$Type[afg.data$data$Type=="Enemy"] = "Enemy Action"
afg.data$data$Type = factor(afg.data$data$Type)
save(afg.data, file="afg.data")



