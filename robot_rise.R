# File-Name:       robot_rise.R           
# Date:            2010-10-28                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Kernetl density of the rise of various robot related words
# Data Used:       afg.csv
# Packages Used:   See load_data.R
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

source("load_data.R")
# source("utils.R")

# Create a series of new columns to ID robot words
robot.words<-c("robot","uav","predator","reaper","drone")   # Robot terms
new.columns<-lapply(robot.words, function(w) grepl(w, afg$Summary, ignore.case=TRUE))

# Merge into full afg df
words.columns<-do.call("cbind",new.columns)
words.df<-as.data.frame(words.columns)
names(words.df)<-robot.words
has.robot<-sapply(1:nrow(words.df), function(x) ifelse(any(words.df[x,]==TRUE),TRUE,FALSE))
row.names(words.df)<-1:nrow(afg)
words.df$has.robot<-has.robot
afg.robot<-merge(afg,words.df,by="row.names")
robot.only<-subset(afg.robot,has.robot==TRUE)

# Create visualization
robot.rise<-ggplot(subset(afg.robot,has.robot==TRUE),aes(x=DateOccurred))+stat_density(aes(fill="darkslateblue"))+
    stat_density(data=afg.robot,aes(x=DateOccurred,alpha=0.5,fill="dimgray"))+scale_alpha(legend=FALSE)+
    scale_fill_manual(values=c("darkslateblue"="darkslateblue","dimgray"="dimgray"),breaks=c("darkslateblue","dimgray"),labels=c("Involving drones","All (incl. drones)"),name="Distribution of Events")+
    xlab("Date")+ylab("")+theme_bw()+opts(panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),axis.ticks=theme_blank(),axis.title.x=theme_blank(),axis.title.y=theme_blank(),axis.text.y=theme_blank(),panel.border=theme_blank())
ggsave(plot=robot.rise,filename="images/robot_rise.pdf",width=10,height=7)