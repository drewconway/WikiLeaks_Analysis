# File-Name:       wikileaks_tm.R          
# Date:            2010-08-11                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Perform basic text mining operations on WL data
# Data Used:       see load_data.R
# Packages Used:          
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

source("load_data.R")
source("utils.R")

## LDA topic models
library(topicmodels)

# Create corpus from Summary column for all regions
create.lda<-function(afg.df, num.topics=3, num.words=10, stemming=FALSE, stopwords = TRUE, minWordLength = 3) {
    # Create nested list of Summary topic models for all Year/Region pairs
    ## WARNING: with above dtm.control settings this takes a VERY LONG TIME
    ## To speed the process set stemming=FALSE and stopwords=FALSE, however, 
    ## results will be different/worse.
    afg.terms<-list()
    regions<-levels(as.factor(afg.df$Region))
    years<-levels(as.factor(afg.df$Year))
    dtm.control<-list(stemming = stemming, stopwords = stopwords, minWordLength = minWordLength)
    for(y in years){
        y.sub<-subset(afg.df,afg.df$Year==y)
        for(r in regions){
            r.sub<-subset(y.sub,y.sub$Region==r)
            yr.corp<-Corpus(VectorSource(r.sub$Summary))
            dtm<-DocumentTermMatrix(yr.corp,control=dtm.control)
            dtm<-removeSparseTerms(dtm,0.95)
            afg.terms[[y]][[r]]<-get_terms(LDA(dtm,control = list(alpha = 0.05), k = num.words),num.topics)
        }
        names(afg.terms[[y]])<-regions
    }
    names(afg.terms)<-years
    return(afg.terms)
}

## Only run this portion if you have not yet generated LDA
#afg.terms<-create.lda(afg,stopwords=c(stopwords("english"),"report","reported","reports","report)","(delayed","(s//rel","rocket,","gctf)","(s)","(fouo)","(s//rel)"))

# Create proper topic dataframe
#topic.df<-melt(afg.terms)
#topic.df<-topic.df[,2:5]
# Strip ID column and rename
#topic.df[,1]<-as.numeric(gsub("Topic ","",topic.df[,1]))    # Remove 'Topic ' from leading column
#names(topic.df)<-c("TopicID","Word","Region","Year")
#write.csv(topic.df,"afg_topic.csv")

# Load saved LDA topic data
topic.df<-read.csv("afg_topic.csv",stringsAsFactors=FALSE)

# Create subsets for captial region and all others, create topic counts
topic.capital<-subset(topic.df,topic.df$Region=="RC CAPITAL")
capital.count<-ddply(topic.capital,.(Word,Year),nrow)

topic.other<-subset(topic.df,topic.df$Region!="RC CAPITAL" & topic.df$Region!="UNKNOWN")
other.count<-ddply(topic.other,.(Word,Year,Region),nrow)

all.count<-ddply(topic.other,.(Word,Region),nrow)

## Visualization

# Create regional boundaries using (min_lat,min_long,max_lat,max_long)
south.boundary<-c(32,61,32,70)
north.boundary<-c(36,64,36,74)
ew.boundary<-c(36,66,32,66)
# rough lat/lon limits of Afghanistan:
max_lat = max(afg.poly$lat)
min_lat = min(afg.poly$lat)
max_lon = max(afg.poly$long)
min_lon = min(afg.poly$long)

# Random point generators for regions
reast<-function() {
    x=runif(1,min=min_lon,max=south.boundary[4])
    y=runif(1,min=min_lat,max=south.boundary[3])
    c(x,y)
}

rnorth<-function() {
    x=runif(1,min=north.boundary[2],max=north.boundary[4])
    y=runif(1,min=north.boundary[1],max=max_lat)
    c(x,y)
}


rsouth<-function() {
    x=runif(1,min=ew.boundary[2],max=north.boundary[4])
    y=runif(1,min=ew.boundary[3],max=ew.boundary[1])
    c(x,y)
}

rwest<-function() {
    x=runif(1,min=min_lon,max=ew.boundary[4])
    y=runif(1,min=ew.boundary[3],max=ew.boundary[1])
    c(x,y)
}

# Switch for generating coordinates
rpoint.switch<-list("RC SOUTH"=rsouth,"RC NORTH"=rnorth,"RC EAST"=reast,"RC WEST"=rwest)  

# Alternate layout option: grid pattern within regions
grid.south<-function(n) {
    # n: number of points to layout
    s<-round(sqrt(n))
    x.itv<-(south.boundary[4]-min_lon)/s   # set x-axis interval
    x.s<-seq(min_lon,south.boundary[4],x.itv)
    # Repeat for y-axis
    y.itv<-(south.boundary[1]-min_lat)/s
    y.s<-seq(min_lat,south.boundary[1],y.itv)
    # Create axis points
    x.pts<-unlist(lapply(x.s,function(x) {rep(x,s)}))
    y.pts<-rep(y.s,s)
    # Add some noise
    x.pts<-x.pts+runif(length(x.pts),-.2,.2)
    y.pts<-y.pts+runif(length(y.pts),-.2,.2)
    return(cbind(x.pts[1:n],y.pts[1:n]))
}

grid.north<-function(n) {
    # n: number of points to layout
    s<-round(sqrt(n))
    x.itv<-(north.boundary[4]-north.boundary[2])/s   # set x-axis interval
    x.s<-seq(north.boundary[2],north.boundary[4],x.itv)
    # Repeat for y-axis
    y.itv<-(max_lat-north.boundary[1])/s
    y.s<-seq(north.boundary[1],max_lat,y.itv)
    # Create axis points
    x.pts<-unlist(lapply(x.s,function(x) {rep(x,s)}))
    y.pts<-rep(y.s,s)
    # Add some noise
    x.pts<-x.pts+runif(length(x.pts),-.2,.2)
    y.pts<-y.pts+runif(length(y.pts),-.2,.2)
    return(cbind(x.pts[1:n],y.pts[1:n]))
}

grid.east<-function(n) {
    # n: number of points to layout
    s<-round(sqrt(n))
    x.itv<-(72-ew.boundary[2])/s   # set x-axis interval
    x.s<-seq(ew.boundary[2],72,x.itv)
    # Repeat for y-axis
    y.itv<-(north.boundary[1]-south.boundary[1])/s
    y.s<-seq(south.boundary[1],north.boundary[1],y.itv)
    # Create axis points
    x.pts<-unlist(lapply(x.s,function(x) {rep(x,s)}))
    y.pts<-rep(y.s,s)
    # Add some noise
    x.pts<-x.pts+runif(length(x.pts),-.2,.2)
    y.pts<-y.pts+runif(length(y.pts),-.2,.2)
    return(cbind(x.pts[1:n],y.pts[1:n]))
}

grid.west<-function(n) {
    # n: number of points to layout
    s<-round(sqrt(n))
    x.itv<-(ew.boundary[2]-min_lon)/s   # set x-axis interval
    x.s<-seq(min_lon,ew.boundary[2],x.itv)
    # Repeat for y-axis
    y.itv<-(north.boundary[1]-south.boundary[1])/s
    y.s<-seq(south.boundary[1],north.boundary[1],y.itv)
    # Create axis points
    x.pts<-unlist(lapply(x.s,function(x) {rep(x,s)}))
    y.pts<-rep(y.s,s)
    # Add some noise
    x.pts<-x.pts+runif(length(x.pts),-.2,.2)
    y.pts<-y.pts+runif(length(y.pts),-.2,.2)
    return(cbind(x.pts[1:n],y.pts[1:n]))
}
# Switch for calling functions
grid.switch<-list("RC SOUTH"=grid.south,"RC NORTH"=grid.north,"RC EAST"=grid.east,"RC WEST"=grid.west)

# Create random word points in regions
rand.points<-lapply(other.count$Region,function(r) do.call(rpoint.switch[[r]],args=list()))
rand.points<-do.call(rbind,rand.points)

# Create grid word points in regions
years<-as.factor(unique(other.count$Year))
regions<-unique(other.count$Region)
grid.layout<-list()
for(y in years){
    for(r in regions){
        yr.rows<-nrow(subset(other.count,other.count$Year==y & other.count$Region==r))
        grid<-do.call(grid.switch[[r]],list(yr.rows))
        grid.layout[[paste(y,"-",r,sep="")]]<-cbind(grid,r,y)
    }
}

all.grid<-list()
for(r in regions){
    region.rows<-nrow(subset(all.count,all.count$Region==r))
    grid<-do.call(grid.switch[[r]],list(region.rows))
    all.grid[[r]]<-cbind(grid,r)
}

# Clean and organize grid layouts
grid.layout<-as.data.frame(do.call(rbind,grid.layout),stringsAsFactors=FALSE)
names(grid.layout)<-c("grid.long","grid.lat","Region","Year")
grid.layout$grid.long<-as.numeric(grid.layout$grid.long)
grid.layout$grid.lat<-as.numeric(grid.layout$grid.lat)
grid.layout$Year<-as.factor(grid.layout$Year)
grid.layout$Region<-as.factor(grid.layout$Region)
grid.layout<-grid.layout[with(grid.layout,order(Year,Region)),] # Sort by year and region

all.layout<-as.data.frame(do.call(rbind,all.grid),stringsAsFactors=FALSE)
names(all.layout)<-c("grid.long","grid.lat","Region")
all.layout$Region<-as.factor(all.layout$Region)
all.layout$grid.long<-as.numeric(all.layout$grid.long)
all.layout$grid.lat<-as.numeric(all.layout$grid.lat)

# Add to df
other.final<-transform(other.count,rand.long=rand.points[,1],rand.lat=rand.points[,2])
row.names(grid.layout)<-row.names(other.final[with(other.final,order(Year,Region)),]) # Add corresponding row names for merge
# Final merge
other.final<-merge(other.final,grid.layout,by=c("row.names","Year","Region"))

row.names(all.layout)<-row.names(all.count[with(all.count,order(Region)),])
all.final<-merge(all.count,all.layout,by=c("row.names","Region"))

# Plot topics by year in region 
region.colours<-c("RC SOUTH"="darkred","RC NORTH"="darkblue","RC EAST"="darkgreen","RC WEST"="darkviolet")

# Random word placement
topic.rand<-ggplot(other.final,aes(x=rand.long,y=rand.lat))+geom_text(aes(label=Word,size=as.factor(V1),colour=Region,alpha=0.7))+facet_wrap(~Year)
topic.rand<-topic.rand+geom_path(data=intl.poly,aes(x=long,y=lat,group=group,alpha=0.5))+coord_map()+theme_bw()+
    scale_alpha(to=c(0.4,0.6),legend=FALSE)+scale_size_manual(values=2:7,name="Word Frequency")+
    scale_colour_manual(value=region.colours)+
    xlab("")+ylab("")+opts(title="LDA Topic Models for WikiLeaks Report Summaries from Four\nRegions, by Year, Projected onto Map and Sized by Frequency")
ggsave(topic.rand,filename="images/topic_model_map_rand.png",width=12,height=7.5,dpi=300)

# Grid word placement
topic.grid<-ggplot(other.final,aes(x=grid.long,y=grid.lat))+geom_text(aes(label=Word,size=as.factor(V1),colour=Region,alpha=0.7))#+facet_wrap(~Year)
topic.grid<-topic.grid+geom_path(data=intl.poly,aes(x=long,y=lat,group=group,alpha=0.5))+coord_map()+theme_bw()+
    scale_alpha(to=c(0.4,0.6),legend=FALSE)+scale_size_manual(values=2:7,name="Word Frequency")+
    scale_colour_manual(value=region.colours)+scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+
    xlab("")+ylab("")+opts(title="LDA Topic Models for WikiLeaks Report Summaries from\nFour Regions, by Year, Projected onto Map and Sized by Frequency")
ggsave(topic.grid,filename="images/topic_model_map_grid.png",width=12,height=7.5,dpi=300)

all.grid<-ggplot(all.final,aes(x=grid.long,y=grid.lat))+geom_text(aes(label=Word,size=as.factor(V1),colour=Region,alpha=0.7))
all.grid<-all.grid+geom_path(data=intl.poly,aes(x=long,y=lat,group=group,alpha=0.5))+coord_map()+theme_bw()+
    scale_alpha(to=c(0.4,0.6),legend=FALSE)+scale_size_manual(values=2:7,name="Word Frequency")+
    scale_colour_manual(value=region.colours)+scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+
    xlab("")+ylab("")+opts(title="LDA Topic Models for WikiLeaks Report Summaries from\nFour Regions, by Year, Projected onto Map and Sized by Frequency")
ggsave(all.grid,filename="images/topic_model_map_grid_all.png",width=10,height=12,dpi=300)

