# analysis involving road distance

library(gam)

# assumes load_data.R has been run

# there are some odd points outside the boundaries. Let's subset by Lat/Long roughly:
afg.trimmed <- subset(afg, subset=Longitude > 60.5 & Longitude < 75.3 & Latitude > 29.4 & Latitude < 39.1)
# eh, does someone have a better way???

# sanity check: map of events with distance to road as color
plot.sanity<-ggplot(afg.trimmed,aes(x=Longitude,y=Latitude))+geom_point(aes(colour=distToRoad,alpha=.4,size=3))
plot.sanity<-plot.sanity+geom_path(data=afg.poly,aes(x=long,y=lat,group=group,alph=.4))+
    opts(title="Distance to Road: Sanity Check Graph", 
          panel.grid.major=theme_blank(),panel.grid.minor=theme_blank()) + 
    #scale_colour_manual(values=cntr.colours,name="Attack On")+
    scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+scale_alpha(legend=FALSE)+
    scale_size_continuous(legend=FALSE)+coord_map()+theme_bw()
# yep, looks fine

# some possible analyses...

# for events with casualties, how is proportion of KIA affected
# by distance to a highway and attack type?
afg.casualties <- subset(afg.trimmed, subset=AllCasualty > 0)
afg.casualties$KIARatio <- with(afg.casualties, AllKIA / AllCasualty)

# lump together types with < 20 events in this subset
afg.casualties$Type[afg.casualties$Type %in% names(which(table(afg.casualties$Type) < 20))] <- 'Other'

plot.kia.dist <- ggplot(afg.casualties, aes(distToRoad, KIARatio)) + 
	geom_jitter(size=3, alpha=.3) + stat_smooth() +
	coord_cartesian(ylim=c(-.05,1.05), xlim=c(-25, 200)) + facet_wrap(~ Type)

# density modeling based on year and various sorts of event types
