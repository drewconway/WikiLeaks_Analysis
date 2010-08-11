# File-Name:       wikileaks_spatial.R                 
# Date:            2010-08-11                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         
# Data Used:       
# Packages Used:          
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

library(maptools)
library(ggplot2)


# Afghanistan data and shapefile
afg.shp<-readShapePoly("admin3_poly_32.shp")

