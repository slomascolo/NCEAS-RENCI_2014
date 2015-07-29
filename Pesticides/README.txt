To use: (as of June 17, 2015)

pest_LC_analysis.RMD
This is the code for the analysis I used for combining the landcover and pesticide data.
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

pest_bfly_buff_overtime_MOA.csv
Similar to above file, but for butterfly site buffers.

Other:
estimated_pesticides_OHcounty_MOA.csv
This has rates of pesticide use (grouped by MOA) for each county x year, scaled by amount of agricultural land-use
"high_kg_km2ag" is the KG of a group applied per kilometer of agriculture (dervied from high USGS estimate)
Useful for visualizations maybe

OHpesticidesMOA.csv
This has all individual pesticides by county and year, but with added columns for their MOA and pesticide type.
Essentially the raw USGS data, but merged with MOA and county traits.
Useful for visualizations maybe.

If you need individual compounds rather than Mode of Action groups, comparable files are still in the folder. 
Named just like the above, only without "_MOA"




PestGrouping folder
Various info sources and files showing pesticide compounds grouping.

Even better classification by Mode of Action (June 10, 2015)
Data from Resistance Action Committee's formed for herbicides, insecticides, and fungicides. (HRAC, IRAC, FRAC)
Not all USGS pesticides included, but it does cover 95% of pesticide use by kilogram.
Now can be grouped by ~50 Modes of Action among the 3 pesticide types, each with a corresponding factor based on RAC classification.

PAN classification
OH_pest_classes.csv
I used Pesticide Action Network database to try and reduce complexity of pesticide compounds.
Grouped them by chemical class and use type (herbicide, etc.) Use type can include multiple uses, separated by commas.
We should consider using other data sources (IRAC, etc) to confirm/figure out unclassified chemicals.

BBSrtcodes.RData
This will allow conversions of BBS route names to RTENO (which I think others are using as the ID)
In my analysis, I used BBSRTSL020 which was in the shapefile I downloaded of all routes.
Should have just used RTENO.
Fixed! Can now merge fifty7_withRTENO.csv with landcover and pesticide data