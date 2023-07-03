rm(list=ls())
library(raster)
require("plyr")
library(tidyverse)


data_path='S:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/kathmandu geological map/'

geo_shp=raster::shapefile(paste0(data_path,'geomap123.shp'))
plot(geo_shp)
plot(geo_shp[geo_shp$GEOMAP=='?',])

which(geo_shp$GEOMAP=='?')

velocity=raster('S:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/ERA5_atmospheric_correction/velocity.tif')
plot(velocity)
geo_shp=spTransform(geo_shp,crs(velocity))
plot(geo_shp,add=T)




geo_shp@data$id = rownames(geo_shp@data)
geo_shp.points = fortify(geo_shp, region="id")
df = join(geo_shp.points, geo_shp@data, by="id")

ggplot(df) + 
  aes(long,lat,group=group,fill=GEOMAP) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() 

