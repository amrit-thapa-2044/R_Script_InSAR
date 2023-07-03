rm(list=ls())

library(raster)

x_min=217113.68602150536 #x1 in opensarlab prepare mintpy
y_max=3215769.746236559 #y1 opensarlab prepare mintpy
x_max=464587.92258064507 #x2 opensarlab prepare mintpy
y_min=3049103.423655914 #y2 opensarlab prepare mintpy

e=extent(x_min,x_max,y_min,y_max)  
p <- as(e, 'SpatialPolygons')
crs(p) <- "+init=epsg:32645"

mapview::mapview(p)

coherence=raster('C:/Users/athapa2/Downloads/S1AA_20191126T122215_20200101T122214_VVP036_INT40_G_weF_E190_unw_phase_clip.tif')
plot(coherence)
p_wgs84=spTransform(p,CRS('+init=epsg:4326'))
plot(p_wgs84,add=T)

np=shapefile('C:/shapefile_all/NepalLocalUnits0/NepalLocalUnits0.shp')
unique(np$GaPa_NaPa)

my_aoi_name="Kathmandu"
my_aoi=np[np$GaPa_NaPa==my_aoi_name,]
plot(my_aoi)

my_aoi=shapefile('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/Kathmandu_valley-20230402T200134Z-001/Kathmandu_valley/KTM_Watershed.shp')
my_aoi_wgs84=spTransform(my_aoi,CRS('+init=epsg:4326'))
plot(my_aoi_wgs84,add=T)
