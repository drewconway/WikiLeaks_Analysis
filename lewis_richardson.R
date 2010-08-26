# File-Name:       lewis_richardson.R          
# Date:            2010-08-25                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Calculated the magnitude and frequency of casualty of events, 
#                  see "Variation of the Frequency of Fatal Quarrels with Magnitude" - Lewis Richardson
# Data Used:       see load_data.R
# Packages Used:   see load_data.R, igraph
# Output File:     
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

source("load_data.R")
source("utils.R")
library(igraph)

# Frequency of KIA events
kia.only<-afg$AllKIA[which(afg$AllKIA>0)]
kia.counts<-ddply(afg,.(AllKIA),nrow)
names(kia.counts)<-c("KIA","Freq")
kia.counts<-subset(kia.counts,kia.counts$KIA>0) # Ignore events with no KIA

# Frequency of all casualty
all.cas<-afg$AllCasualty[which(afg$AllCasualty>0)]
all.counts<-ddply(afg,.(AllCasualty),nrow)
names(all.counts)<-c("All","Freq")
all.counts<-subset(all.counts,all.counts$All>0) # Ignore events with no KIA

### Fit power-law vis log-log plotting

# Plot deadly quarrels
kia.lm<-lm(log10(Freq)~log10(KIA),data=kia.counts)
kia.coeff<-as.vector(round(kia.lm$coeff,2))  # Get lm slope
cat(kia.coeff)  # Get lm slope
kia.plot<-ggplot(kia.counts,aes(x=KIA,y=Freq))+geom_point()+scale_x_log10()+scale_y_log10()+
    stat_smooth(method="lm",se=FALSE)
kia.plot<-kia.plot+annotate("text",x=50,y=150,label=paste("- alpha==",abs(kia.coeff[2])),size=7,parse=TRUE)+
    annotate("text",x=80,y=210,label="With linear regression",size=7)+
    xlab("log(Number Killed in Action)")+ylab("log(Freqeuncy)")+
    opts(title="Frequency and Magnitude of Only KIA Events in WikiLeaks Data")+theme_bw()
ggsave(plot=kia.plot,filename="images/kia_freq_mag.png",width=10,height=10,dpi=100)

# Plot all casualty events
all.lm<-lm(log10(Freq)~log10(All),data=all.counts)
all.coeff<-as.vector(round(all.lm$coeff,2))  # Get lm slope
cat(all.coeff)  # Get lm slope
all.plot<-ggplot(all.counts,aes(x=All,y=Freq))+geom_point()+scale_x_log10()+scale_y_log10()+
    stat_smooth(method="lm",se=FALSE)
all.plot<-all.plot+annotate("text",x=50,y=150,label=paste("- alpha==",abs(all.coeff[2])),size=7,parse=TRUE)+
    annotate("text",x=80,y=210,label="With linear regression",size=7)+
    xlab("log(Number of Casualties)")+ylab("log(Freqeuncy)")+
    opts(title="Frequency and Magnitude of All Casualty Events in WikiLeaks Data")+theme_bw()
ggsave(plot=all.plot,filename="images/all_freq_mag.png",width=10,height=10,dpi=100)

### Now do it the correct wawy from Clauset, et al.

# Estimate the scaling parameter via MLE
kia.alpha<-power.law.fit(kia.only,xmin=1)
cas.alpha<-power.law.fit(all.cas,xmin=1)


