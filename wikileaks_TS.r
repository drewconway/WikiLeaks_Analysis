
library(ggplot2)

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

# rough lat/lon limits of Afghanistan:
max_lat = 37.3
min_lat = 29.4
max_lon = 75.0
min_lon = 60.6

# filter out events not in Afghanistan (?!)
to_keep <- (afg_ts$lat > min_lat) & (afg_ts$lat < max_lat) & (afg_ts$lon > min_lon) & (afg_ts$lon < max_lon)
afg_ts <- afg_ts[to_keep,]

# filter out NA events
to_throw <- is.na(afg_ts$label)
afg_ts <- afg_ts[!to_throw,]
afg_ts$label = factor(as.character(afg_ts$label))

png("images/events_by_label.png",width=1200,height=750,res=100)
p <- ggplot(afg_ts,aes(x=lat,y=lon))
p + geom_point(aes(color=label,alpha=0.4)) + facet_wrap(~ year)
dev.off()
