Products to use for population analysis of BBS data

Even better classification by Mode of Action (June 10, 2015)
Data from Resistance Action Committee's formed for herbicides, insecticides, and fungicides. (HRAC, IRAC, FRAC)
Not all USGS pesticides included, but it does cover 95% of pesticide use by kilogram.
Now can be grouped by ~50 Modes of Action among the 3 pesticide types, each with a corresponding factor based on RAC classification.
OHpesticidesMOA.csv
estimated_pesticides_OHcounty_MOA.csv

PAN classification
OH_pest_classes.csv
I used Pesticide Action Network database to try and reduce complexity of pesticide compounds.
Grouped them by chemical class and use type (herbicide, etc.) Use type can include multiple uses, separated by commas.
We should consider using other data sources (IRAC, etc) to confirm/figure out unclassified chemicals.

pest_LC_analysis.RMD
This is the final document of the analysis I used for combining the landcover and pesticide data.
Many chunks cannot be run because GIS data not uploaded.

pest_buff_overtime_MOA.csv
Pesticide classes, not individuals chemicals. For each buffer around BBS sites, 
total pesticide use for each MOA group is estimated using USGS high estimate and 
proportions of different counties (and their pesticide use rates) included in buffer.
"MOAuse" is a code I made combining 3 types of pesticides (her, ins, fun) and the MOA class determined by the respective RAC.
"buffer" is in meters surrounding transect
"kg_buff" are estimated kg of the compound class used in the buffer that year
"ag_km2_buffer" are km2 of agriculture in the buffer for calculating rates
"meanbuff_kgkm2ag" is the rate of application (kg/area) used for a class in the buffer that year
"RTENO" is unique identifier for each transect (2-digit state number + 3-digit route number)

LC_buffers_overtime.csv
Landcover analysis for each BBS buffer by year in both 30x30m pixels and km2.
"BBS_route" uses codes found in BBSrtcodes.RData column "BBSRTSL020"
"reclass" uses ALI codes (0=nodata, 1=water, 2=developed, 3=barren, 4=forest, 5=grassland/shrub, 6=agriculture, 7=wetlands)
"buffer" is in meters surrounding transect
"RTENO" is unique identifier for each transect (2-digit state number + 3-digit route number)

BBSrtcodes.RData
This will allow conversions of BBS route names to RTENO (which I think others are using as the ID)
In my analysis, I used BBSRTSL020 which was in the shapefile I downloaded of all routes.
Should have just used RTENO.
Fixed! Can now merge fifty7_withRTENO.csv with landcover and pesticide data