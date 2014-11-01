10/22/14

landcover_all.csv is the one to use!
"GEOID" is county identifier
"reclass" is Anderson Level I land cover: (0=nodata, 1=water, 2=developed, 3=barren, 4=forest, 5=grassland/shrub, 6=agriculture, 7=wetlands)
"total" is the number of pixels (multiply by 0.009 to get square km)
"year" is 1992, 2001, 2006, or 2011

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


