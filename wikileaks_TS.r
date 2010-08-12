source("load_data.R")
source("utils.R")

# pick out date, label, latitude and longitude
afg_ts <- data.frame(
    date = as.Date(afg$DateOccurred),
    year = format(as.Date(afg$DateOccurred),"%y"),
    lat = afg$Latitude,
    lon = afg$Longitude,
    type = factor(apply(cbind(afg$AttackOn,afg$Type),1,function(row)paste(row[1],row[2],sep="_")))
)

# rough lat/lon limits of Afghanistan:
max_lat = max(afg.poly$lat)
min_lat = min(afg.poly$lat)
max_lon = max(afg.poly$lon)
min_lon = min(afg.poly$lon)

# filter out events not in Afghanistan (?!)
to_keep <- (afg_ts$lat > min_lat) & (afg_ts$lat < max_lat) & (afg_ts$lon > min_lon) & (afg_ts$lon < max_lon)
afg_ts <- afg_ts[to_keep,]

# filter out NA events
to_throw <- is.na(afg_ts$type)
afg_ts <- afg_ts[!to_throw,]
afg_ts$type = factor(as.character(afg_ts$type))

png("images/events_by_type.png",width=1200,height=750,res=100)
p <- ggplot(afg_ts,aes(y=lat,x=lon)) + geom_point(aes(color=type,alpha=0.4))+facet_wrap(~year)
print(p)
dev.off()

# New plot with regional map of Afghanistan overlayed on attack data
png("images/events_by_year_map.png",width=1200,height=750,res=100)
year.map<-ggplot(afg_ts,aes(y=lat,x=lon))+geom_point(aes(colour=type,alpha=0.4))+facet_wrap(~year)
year.map<-year.map+geom_path(data=afg.poly,aes(y=lat,x=long,group=group,alpha=0.4))+
    scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+scale_alpha(legend=FALSE)+
    opts(title="Wikileaks Geospatial Attack Data by Year and Type (Afghanistan District Boundaries)",panel.grid.major=theme_blank())+
    theme_bw()+ylab("Latitude")+xlab("Longitude")+coord_map()
print(year.map)
dev.off()

setwd("images")
type.dir<-"by_type"
dir.create(type.dir)
attack.types<-levels(afg_ts$type)
point.cols<-c(brewer.pal(8,"Set1"),brewer.pal(8,"Set2"))
col.index<-1

for(t in attack.types) {
    type.subset<-subset(afg_ts,afg_ts$type==t)
    type.subset<-data.frame(type.subset,row.names=1:nrow(type.subset))
    png(paste(type.dir,"/",t,"_by_year.png",sep=""),width=1200,height=750,res=100)
    t.map<-ggplot(type.subset,aes(y=lat,x=lon))+geom_point(aes(colour="c",alpha=0.4))+facet_wrap(~year)
    t.map<-t.map+geom_path(data=afg.poly,aes(y=lat,x=long,group=group,alpha=0.4))
    t.map<-t.map+scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+scale_alpha(legend=FALSE)
    t.map<-t.map+scale_colour_manual(values=c("c"=point.cols[col.index]),name="",breaks="c",labels=t)
    t.map<-t.map+opts(title=paste("Wikileaks Geospatial Data by for ",t,"\nby Year with District Boundaries",sep=""),panel.grid.major=theme_blank())
    t.map<-t.map+ylab("Latitude")+xlab("Longitude")+theme_bw()+coord_map()
    print(t.map)
    dev.off()
    col.index<-col.index+1
}
setwd("..")