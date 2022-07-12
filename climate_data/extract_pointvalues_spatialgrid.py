#!/usr/bin/env python3
# Extract environmental values for points
# THIS SCRIPT ASSUMES AND SKIPS A HEADER, ASSUMES COLUMN 1 X COLUMN 2 Y

import csv # To process tab-delimited files
import sys
import argparse # Parse arguments
import os
import gdal,osr
from osgeo import ogr
import re
import struct

from gdalconst import *

# Actual extraction routine; convert GPS coordinates to pixel coordinates and query the layer
def extract_point_from_raster(x, y, source_layer): 
	#Convert from map to pixel coordinates.
	px = int((float(x) - geotransform[0]) / geotransform[1]) # Get X pixel coordinate
	py = int((float(y) - geotransform[3]) / geotransform[5]) # Get Y pixel coordinate
	structval = rasterband.ReadRaster(px, py, 1, 1, buf_type=gdal.GDT_Float32) # Result of this is binary
	intval = struct.unpack('f' , structval)
	val=intval[0] # Now get integer
	return val, px, py, x, y # Also returning pixel coordinates to check for pixel-wise duplicates, AND original coordinates
    
parser = argparse.ArgumentParser(description='Script to automatically extract climatic values from occurrence records.')
parser.add_argument('input_raster', action='store', help='List of names of the rasters; missing data should be -9999.')
parser.add_argument('occurrence_directory', action='store', help='Directory of occurrence files; should be CSV.')
args = parser.parse_args()

variable = re.sub(".*/", "", args.input_raster) # Manipulate with regex for nice file name
variable = re.sub("\.asc", "", variable)
print("On variable:")
print(variable)

print("Opening raster layer.")
source_layer=gdal.Open(args.input_raster) # Open layer (can be asc)
print("Determining raster layer projection.")
geotransform = source_layer.GetGeoTransform() # Infer projection
print("Loading raster layer into memory.")
rasterband = source_layer.GetRasterBand(1) # Open raster band


files = [os.path.abspath(os.path.join(args.occurrence_directory, p)) for p in os.listdir(args.occurrence_directory)]


for points_raw in files:
	layer = re.sub(".*/", "", points_raw) # Manipulate with regex for nice file name
	layer = re.sub("\.csv", "", layer)
	print("On layer:")
	print(layer)
	print("On records in {0}.".format(points_raw))
	points = []
	with open(points_raw, 'r') as datafile:
		reader=csv.reader((line.replace('\0','') for line in datafile),delimiter=',',quoting=csv.QUOTE_NONE) 
		next(reader)
		for r in reader:
			points.append([r[0], r[1]])
	climate_values = []
	#try:
	for point in points: # Run the value extraction on each point
		
		try:
			climate_value = extract_point_from_raster(point[0], point[1], source_layer)
			climate_values.append(climate_value)
		except:
			print("Skipped incorrectly formatted record.")
	
	print(climate_values)
	print("Number of occurrences in source file:")
	print(len(points))
	print("Number of successful occurrence environmental extractions (should be only a little lower):")
	print(len(climate_values))

	# Set species variable (not used)
	with open(points_raw, 'r') as datafile:
		reader=csv.reader((line.replace('\0','') for line in datafile),delimiter=',',quoting=csv.QUOTE_NONE) 
		r = next(reader)
		species = r[0]
		
	try:
		os.mkdir("./spatial_data_climate")
	except: 
		pass
	with open("./spatial_data_climate/{0}_{1}.csv".format(variable, layer), 'w+') as writefile:
			writer = csv.writer(writefile, delimiter=',')
			writer.writerows([[variable,'x','y']]) 
			for i in climate_values:
				writer.writerows([[i[0], i[3], i[4]]]) 

