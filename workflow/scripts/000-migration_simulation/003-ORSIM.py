from sys import argv
import csv
import numpy as np
import os
from math import radians, cos, sin, asin, sqrt, exp
import copy
import random
import pyemd
from pyemd import emd
from pyemd import emd_with_flow

#os.chdir('/Volumes/Marius_SSD/Yale-MPIO/Galapagos/Galapagos-tortoise-migration/galapagos-tortoises')

# Load hexagon grid with NDVI data
dataloaded = np.loadtxt(snakemake.input['data'], skiprows=1, delimiter=';')
#dataloaded = np.loadtxt('results/output/000/galapagos_hexagons_data.csv', skiprows=1, delimiter=';')
dataloaded = dataloaded[(dataloaded[:,2]!=-1),:]
NDVI_highland = dataloaded[:,3] * dataloaded[:,5]
NDVI_lowland = dataloaded[:,4] * dataloaded[:,6]
NDVI_highland = NDVI_highland / sum(NDVI_highland)
NDVI_lowland = NDVI_lowland / sum(NDVI_lowland)

distanceMatrix = np.loadtxt(snakemake.input['distMat'], delimiter=';')
#distanceMatrix = np.loadtxt('results/output/000/distanceMatrix.csv', delimiter=';')

ORSIM_results = emd_with_flow(NDVI_highland, NDVI_lowland, distanceMatrix)

np.savetxt(snakemake.output['connectivity'], ORSIM_results[1], delimiter=',')
#np.savetxt("results/output/000/ORSIMresults.csv", ORSIM_results[1], delimiter=',')
