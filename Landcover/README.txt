6/17/15

Data to use:
LC_bflybuff_overtime.csv has estimated landcover for each buffer size, year, and butterfly site.

LC_buffers_overtime.csv has estimated landcover for each buffer size, year, and BBS transect.

LC_county_overtime.csv has estimated landcover for each OH county and year.

columns descriptions for these 3:
"reclass" is Anderson Level I land cover: (0=nodata, 1=water, 2=developed, 3=barren, 4=forest, 5=grassland/shrub, 6=agriculture, 7=wetlands)
"buffer" is the length of the radius in meters used around transects or sites
"total_pix" is the number of Landsat 30x30m pixels in a grouping
"km2" is derived by multiplying pixels by 0.009 to get square kilometers.
"RTENO" is BBS unique ID for transects



Other:
landcover_bflybuff is an intermediate product with only the 4 years of USGS rasters
landcover_all.csv is an intermediate product of landcover for the 4 USGS rasters by county
"GEOID" is county identifier
"reclass" is Anderson Level I land cover: (0=nodata, 1=water, 2=developed, 3=barren, 4=forest, 5=grassland/shrub, 6=agriculture, 7=wetlands)
"total" is the number of pixels (multiply by 0.009 to get square km)
"year" is 1992, 2001, 2006, or 2011
Important note: 1992 is off, was made with different methods. Even using the change product made to compare 1992/2001, 
it really doesn't fit well with the 2001-2011 trends. I think I would ignore the 1992 for now.
(NLCD has plans to combine all years in comparable format for the 2016 update, great...)

NLCD processing summary in QGIS (point and click).

1. Raw downloads from NLCD website were clipped to same extent of box around OH region.
2. Projections were standardized at Albers Equal Area, EPSG:5070.
3. Some difficulty for 1992/2001 change product, which had to be cobbled together from 4 regions.
	a. Eventually 4 separate clipped rasters merged successfully to cover same bounding box as other rasters.
4. All files exported (using Raster > Conversion > Translate) as GEOTIFF with no compression for use in R for additional processing.

Notes:
Impervious layers have special value for open water like Lake Erie. Should remove pixels with values > 100.
Figure out how land classification schemes changed between 1992 and 2001 for consistency.
As of now, I have not uploaded GIS layers into Github because it's about 8GB just for the maps of the OH area. 


