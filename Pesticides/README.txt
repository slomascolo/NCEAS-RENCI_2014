Products to use for population analysis of BBS data

pest_LC_analysis.RMD
This is the final document of the analysis I used for combining the landcover and pesticide data.
Many chunks cannot be run because GIS data not uploaded.

pest_buff_overtime.csv
Includes neonicotinoids and a few other pesticides. For each buffer around BBS sites, 
total pesticide use for each compound is estimated using USGS high and low estimates and 
proportions of different counties (and their pesticide use rates) included in buffer.
"buffer" is in meters surrounding transect
"high_kg_*" are estimated kg of the compound used in the buffer that year
"ag_pix_buff" are 30x30m pixels of agriculture in the buffer (for most landcover analysis, use 
"BBS_route" uses codes found in BBSrtcodes.RData column "BBSRTSL020"
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