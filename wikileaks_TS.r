library(ggplot2)
library(maptools)

afg <- read.csv("afg.csv",stringsAsFactors=FALSE)

# Add header data leftout by WikiLeaks, label reference taken from http://wardiary.wikileaks.org/
colnames(afg)<-c("ReportKey","DateOccurred","Type","Category","TrackingNumber","Title","Summary","Region","AttackOn",
    "ComplexAttack","ReportingUnit","UnitName","TypeOfUnit","FriendlyWIA","FriendlyKIA","HostNationWIA","HostNationKIA",
    "CivilianWIA","CivilianKIA","EnemyWIA","EnemyKIA","EnemyDetained","MGRS","Latitude","Longitude","OriginatorGroup",
    "UpdatedByGroup","CCIR","Sigact","Affiliation","DColor","Classification")

# pick out date, label, latitude and longitude
afg_ts <- data.frame(
    date = as.Date(afg$DateOccurred),
    year = format(as.Date(afg$DateOccurred),"%y"),
    lat = afg$Latitude,
    lon = afg$Longitude,
    label = factor(apply(cbind(afg$AttackOn,afg$Type),1,function(row)paste(row[1],row[2],sep="_")))
)

# Read Afghanistan shapefile
temp <- tempfile()
download.file("http://www.aims.org.af/services/mapping/shape_files/afghanistan/polygon/district_boundary.zip",temp)
untar(temp,compressed="gzip")
unlink(temp)
afg.shp  <- readShapePoly("admin3_poly_32.shp")
afg.poly <- fortify.SpatialPolygons(afg.shp)

# rough lat/lon limits of Afghanistan:
max_lat = max(afg.poly$lat)
min_lat = min(afg.poly$lat)
max_lon = max(afg.poly$lon)
min_lon = min(afg.poly$lon)

# filter out events not in Afghanistan (?!)
to_keep <- (afg_ts$lat > min_lat) & (afg_ts$lat < max_lat) & (afg_ts$lon > min_lon) & (afg_ts$lon < max_lon)
afg_ts <- afg_ts[to_keep,]

# filter out NA events
to_throw <- is.na(afg_ts$label)
afg_ts <- afg_ts[!to_throw,]
afg_ts$label = factor(as.character(afg_ts$label))

png("images/events_by_label.png",width=1200,height=750,res=100)
p <- ggplot(afg_ts,aes(y=lat,x=long)) + geom_point(aes(color=label,alpha=0.4))+facet_wrap(~year)
print(p)
dev.off()

# New plot with regional map of Afghanistan overlayed on attack data
png("images/events_by_label_map.png",width=1200,height=750,res=100)
p.map<-ggplot(afg_ts,aes(y=lat,x=lon))+geom_point(aes(colour=label,alpha=0.4))+facet_wrap(~year)
p.map<-p.map+geom_path(data=afg.poly,aes(y=lat,x=long,group=group,alpha=0.4))+
    scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+scale_alpha(legend=FALSE)+
    opts(title="Wikileaks Geospatial Attack Data by Year and Type (Afghanistan District Boundaries)",panel.grid.major=theme_blank())+
    theme_bw()+ylab("Latitude")+xlab("Longitude")+coord_map()
print(p.map)
dev.off()
