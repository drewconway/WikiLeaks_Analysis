#!/usr/bin/env python
# encoding: utf-8
"""
timeline_3d.py

Purpose:  Create a three-dimensional map representation of six year progresssion of war

Author:   Drew Conway
Email:    drew.conway@nyu.edu
Date:     2011-02-14

Copyright (c) 2011, under the Simplified BSD License.  
For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
All rights reserved.
"""

import sys
import os
import visvis as vv             # For visualization
import rpy2.robjects as robj    # To load in data from R (see get_data.R for details)
import numpy as np              # For working with arrays


def main():
    
    # First load in data by opening an R instance
    robj.r('''source("get_data.R")''')
    
    # Get three columns of data into numpy arrays
    lat=np.array(robj.r('''afg_ied$Latitude'''))
    lon=np.array(robj.r('''afg_ied$Longitude'''))
    year=np.array(robj.r('''afg_ied$Year'''))
    record_type=np.array(robj.r('''afg_ied$Type'''))
    
    # Let the list comprehension magic begin!
    # First, get all data as a tuple
    afg_tup=map(lambda i: (lat[i], lon[i], year[i], record_type[i]), xrange(lat.size))

    afg_counts=[]
    # Create counts of data
    for y in np.unique(year):
        for lat in np.unique(lat):
            for lon in np.unique(lon):
                match=[(a,b,c) for (a,b,c,d) in afg_tup if a==lat and b==lon and c==y]
                afg_counts.append((y,lat,lon,len(match)))
    print(afg_counts[0:10])

if __name__ == '__main__':
	main()

