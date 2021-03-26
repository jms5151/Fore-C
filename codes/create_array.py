# -*- coding: utf-8 -*-
"""
Created on Thu Mar 25 12:01:40 2021

@author: Jamie
"""
import numpy as np

# create data
medColSize = np.arange(0, 201, 50) # add one number to send of even numbers used in R
FishAbundance = np.arange(0.01, 1., 0.3)
Lights = np.arange(0, 255, 50)
waveSD = np.arange(0, 42, 20)
SST = np.arange(20, 31, 2)
months  = np.arange(1, 12, 2)

# get sizes of data
Lcol = medColSize.size
Lfish = FishAbundance.size
Llights = Lights.size
LwaveSD = waveSD.size
Lsst = SST.size
Lmonths = months.size

# create unique values for every element in array (previously "V1..VN"; this corresponds to column in lookup table)
# remember that python indexes as 0, so 0 will correspond with the first column of the csv
Nunique = Lcol * Lfish * Llights * LwaveSD * Lsst * Lmonths

# create array
gangArray = np.arange(Nunique).reshape(Lcol, Lfish, Llights, LwaveSD, Lsst, Lmonths, order = 'F')

# check dimensions are correct
gangArray.shape

# compare id with R (in R: gangArray[2,1,2,1,3,1] = "V742")
gangArray[1,0,1,0,2,0] # = 741 (equivalent to "V742" in R)
