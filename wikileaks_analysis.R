# File-Name:       wikileaks_analysis.R                 
# Date:            2010-07-26                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Produce initial plots and Benford analysis
# Data Used:       afg.csv (http://leakmirror.wikileaks.org/file/straw-glass-and-bottle/afg-war-diary.csv.7z)
# Packages Used:   see load_data.R
# Output File:     
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

source("load_data.R")
source("utils.R")

### BASE VISUALIZATION ###
dir.create("images")

# Create some basic descriptive plots of the full data set...
# 1. Report volume by region and attack target
png("images/report_region.png",width=1200,height=750,res=100)
report.region<-ggplot(afg,aes(x=DateOccurred))+stat_bin(aes(y=..count..,fill=AttackOn),binwidth=30)+facet_wrap(~Region)+
    opts(title="Report Volume at Monthly Intervals, by Region and Attack Target")+xlab("Date")+ylab("Report Counts")+
    scale_fill_manual(values=c("red","blue","green","orange"),name="Who Attacked")+scale_x_date(major="years",minor="months")
print(report.region)
dev.off()

# 2. Benford's test on number of total reports in data per week
week.count<-cbind(table(cbind(format.Date(afg$DateOccurred,"%Y %W"))))

# Function for pulling out leading digit from some integer stored as string
leading.dig<-function(x) {
    as.numeric(strsplit(as.character(x),"")[[1]][1])
}   

# Count digits and store as data frame
dig.count<-cbind(table(sapply(as.vector(week.count),leading.dig)))
dig.count<-as.data.frame(dig.count)
colnames(dig.count)<-"DigitCount"

# Benford's distribution
dbenford<-function(d,base=10) {
    return(log(1+(1/d),base=base))
}

# Plot observed probability of leading digit and theoretical
png("images/benford_all.png",width=1000,height=800,res=100)
ggplot(dig.count,aes(x=1:nrow(dig.count),y=DigitCount/sum(DigitCount)))+geom_path(aes(colour="Observed"))+
    geom_point(aes(colour="Observed"))+stat_function(fun=dbenford,args=list(base=10),aes(colour="Theoretical"))+
    scale_colour_manual(values=c("Observed"="darkblue","Theoretical"="darkred"),name="Leading Digits")+
    scale_x_continuous(breaks=1:nrow(dig.count))+ylab("Pr(Digit)")+xlab("Digits")+
    opts(title="Benford's Law Test for Wikileaks Data (Observed Reports/Week)")
dev.off()

# Chi-square test for goodness of fit
chisq.test(x=dig.count$DigitCount/sum(dig.count$DigitCount),y=sapply(1:9,dbenford))

# Next, do same analysis on weekly report counts, but but break down by region
# WARNING: This process can take a long time, depending on hardware
region.dcount<-ddply(afg,.(format.Date(DateOccurred, "%Y %W"),Region), nrow)
colnames(region.dcount)<-c("YearWeek","Region", "ReportCount")

# Now, chop and skew data to create regional subsets
region.subs<-lapply(unique(region.dcount$Region), function(r) cbind(table(sapply(subset(region.dcount,region.dcount$Region==r)$ReportCount,leading.dig)),as.character(r)))
region.subs[[6]]<-rbind(region.subs[[6]],c(0,"UNKNOWN"),c(0,"UNKNOWN")) # Add missing values
region.subs<-lapply(region.subs,function(x) {as.data.frame(x,row.names=1:9,stringsAsFactors=F)})

# Combine that into a single data frame
region.subs<-list_to_dataframe(region.subs)
region.subs<-transform(region.subs,V3=rep(1:9,6))
colnames(region.subs)<-c("DigitCount","Region","Digit")
region.subs$DigitCount<-as.numeric(region.subs$DigitCount)
region.subs$Digit<-as.numeric(region.subs$Digit)

# Finally, add sums for facet_wrap
dig.sums<-sapply(unique(region.subs$Region),function(r) sum(subset(region.subs,Region==r)$DigitCount),USE.NAMES=F)
sum.vec<-unlist(lapply(dig.sums,function(x) {rep(x,9)}))
region.subs<-transform(region.subs,DigitSum=sum.vec)


# Plot the results for each region in single pane
png("images/benford_region.png",width=1400,height=1000,res=100)
ggplot(region.subs,aes(x=1:9))+geom_line(aes(y=DigitCount/DigitSum, colour="Observed"))+
    geom_point(aes(y=DigitCount/DigitSum, colour="Observed"))+
    stat_function(fun=dbenford,args=list(base=10),aes(colour="Theoretical"))+
    scale_x_continuous(breaks=1:9)+ylab("Pr(Digit)")+xlab("Digits")+
    opts(title="Benford's Law Test for Wikileaks Data by Region (Observed Reports/Week)")+
    scale_colour_manual(values=c("Observed"="darkblue","Theoretical"="darkred"),name="Leading Digits")+
    facet_wrap(~Region)
dev.off()

# 3) Contractor analysis

# First, create a new column to identify where Summary column includes the word 'contractor'
has.cntr<-rep(FALSE,nrow(afg))
has.cntr[grep("contractor",afg$Summary,fixed=FALSE,ignore.case=TRUE)]<-TRUE
afg<-transform(afg,HasCntr=has.cntr)
# cntr.colours<-c("1"="red","0"="blue")

# First plot, kernel density plots of KIA and WIA involving contractors
png("images/contractor_timeline.png",width=2000,height=1000,res=100)
cntr.cas<-ggplot(afg,aes(x=DateOccurred))+stat_bin(aes(fill=as.factor(AttackOn),y=log(..count..)))+facet_wrap(~HasCntr,nrow=2,ncol=1)+
    opts(title="Wikileaks Data Where Report Summary Contains\nWord 'CONTRACTOR' - Kernel Density by 'Attack On'")+
    xlab("Date (30 Day Intervals)")+ylab("Kernel Density")+theme_bw()
print(cntr.cas)
dev.off()

png("images/contractor_geo.png",width=1400,height=1000,res=120)
cntr.geo<-ggplot(afg.cntr,aes(x=Longitude,y=Latitude))+geom_point(aes(colour=AttackOn,alpha=.4,size=3))
cntr.geo<-cntr.geo+geom_path(data=afg.poly,aes(x=long,y=lat,group=group,alph=.4))+
    opts(title="Wikileaks Data Where Report Summary Contains\nWord 'CONTRACTOR' - GEOSPATIAL With District Boundaries",
    panel.grid.major=theme_blank(),panel.grid.minor=theme_blank())+scale_colour_manual(values=cntr.colours,name="Attack On")+
    scale_x_continuous(breaks=NA)+scale_y_continuous(breaks=NA)+scale_alpha(legend=FALSE)+
    scale_size_continuous(legend=FALSE)+coord_map()+theme_bw()
print(cntr.geo)
dev.off()
