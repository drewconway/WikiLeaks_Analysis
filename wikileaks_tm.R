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
afg.terms<-create.lda(afg,stopwords=c(stopwords("english"),"report","reported","reports"))

# Create proper topic dataframe
topic.df<-melt(afg.terms)
topic.df<-topic.df[,2:5]
# Strip ID column and rename
topic.df[,1]<-as.numeric(gsub("Topic ","",topic.df[,1]))    # Remove 'Topic ' from leading column
names(topic.df)<-c("TopicID","Word","Region","Year")
write.csv(topic.df,"afg_topic.csv")

topic.df<-read.csv("afg_topic.csv",stringsAsFactors=FALSE)

# Create subsets for captial region and all others, create topic counts
topic.capital<-subset(topic.df,topic.df$Region=="RC CAPITAL")
capital.count<-ddply(topic.capital,.(Word,Year),nrow)

topic.other<-subset(topic.df,topic.df$Region!="RC CAPITAL" & topic.df$Region!="UNKNOWN")
other.count<-ddply(topic.other,.(Word,Year,Region),nrow)

## Visualization

# Create regional boundaries using lat/long
south.boundary=c(32,60,32,69)
north.boundary=c(36,64,36,72)
ew.boundary=c(36,66,32,66)
# rough lat/lon limits of Afghanistan:
max_lat = max(afg.poly$lat)
min_lat = min(afg.poly$lat)
max_lon = max(afg.poly$lon)
min_lon = min(afg.poly$lon)

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

# Create word points in regions
plot.points<-lapply(other.count$Region,function(r) do.call(rpoint.switch[[r]],args=list()))
plot.points<-do.call(rbind,plot.points)
# Add to df
other.count<-transform(other.count,long=plot.points[,1],lat=plot.points[,2])

# Plot topics by year in region
region.colours<-c("RC SOUTH"="darkred","RC NORTH"="darkblue","RC EAST"="darkgreen","RC WEST"="darkviolet")
topic.year<-ggplot(other.count,aes(x=long,y=lat))+geom_text(aes(label=Word,size=as.factor(V1),colour=Region,alpha=.6))+facet_wrap(~Year)
topic.year<-topic.year+geom_path(data=intl.poly,aes(x=long,y=lat,group=group))+coord_map()+theme_bw()+
    scale_alpha(legend=FALSE)+scale_size_manual(values=2:7,name="Frequency in Topic Models")+
    scale_colour_manual(value=region.colours)+scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+
    xlab("")+ylab("")+opts(title="LDA Topic Models for WikiLeaks Report Summaries\nfrom Four Regions, by Year, Projected onto Map and Sized by Frequency")
ggsave(topic.year,filename="images/topic_model_map.png",width=12,height=7.5,dpi=300)