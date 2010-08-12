# File-Name:       wikileaks_tm.R          
# Date:            2010-08-11                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Perform basic text mining operations on WL data
# Data Used:       
# Packages Used:          
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

source("load_data.R")
source("utils.R")

# Create corpus from Summary column for all regions
regions<-levels(as.factor(afg$Region))
years<-levels(as.factor(afg$Year))
afg.terms<-list()
dtm.control<-list(stemming = FALSE, stopwords = FALSE, minWordLength = 3,removeNumbers = TRUE)

# Create nested list of Summary topic models for all Year/Region pairs
for(y in years){
    y.sub<-subset(afg,afg$Year==y)
    for(r in regions){
        r.sub<-subset(y.sub,y.sub$Region==r)
        yr.corp<-Corpus(VectorSource(r.sub$Summary))
        dtm<-DocumentTermMatrix(yr.corp,control=dtm.control)
        dtm<-removeSparseTerms(dtm,0.95)
        afg.terms[[y]][[r]]<-get_terms(LDA(dtm,control = list(alpha = 0.1), k = 10),5)
    }
    names(afg.terms[[y]])<-regions
}
names(afg.terms)<-years