rm(list=ls())

library(terra)
library(ggplot2);library(tidyterra);library(sf)

setwd('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/output_from_OpenSarLab/velocity_with-different_rp')
v_list=list.files(getwd(),pattern = 'velocity_.*tif')
v_list_full=list.files(getwd(),pattern = 'velocity_.*tif',full.names=T)

# Print the file list
print(v_list_full)

v_stack=rast(v_list_full)*100
plot(v_stack)

maskConnComp=rast('maskConnComp_Chovar_27.665812_85.291326.tif')
plot(maskConnComp)

shp_r=raster::shapefile('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/Kathmandu_valley-20230402T200134Z-001/Kathmandu_valley/KTM_Watershed.shp')
wgs84='+init=epsg:4326'

library(raster)
shp_r=spTransform(shp_r,CRS(wgs84))
shp=vect(shp_r)

# Extract the desired substring
lat <- stringr::str_extract(v_list, "\\d+\\.\\d+")
lon<- stringr::str_extract(v_list, "(?<=_)\\d+\\.\\d+")
df_rp=data.frame(x=lon,y=lat)

v_stack=mask(v_stack,shp)
v_stack[maskConnComp==0]=NA
plot(v_stack)

data_max=max(global(v_stack,'max',na.rm=T))
data_min=min(global(v_stack,'min',na.rm=T))
my_label=round(seq(data_min,data_max,length.out=5),0)

p=ggplot(shp) +
  geom_spatvector(fill = NA,col='black',size=2)+
  geom_spatraster(data = v_stack) +
  facet_wrap(~lyr, ncol = 3)+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, space = "Lab",na.value='transparent',limits=c(data_min,data_max))+
  theme(axis.text =element_text(size=4),strip.text = element_text(size=4))+
  theme(
    legend.position = "right",
    legend.title.align = 1,
    legend.key.height  = unit(4,"line")) +
  guides(fill = guide_colourbar(title.position = "bottom"))+
  labs( x=NULL,y=NULL,
        fill = expression(paste(cm/yr), sep = ""))


print(p)

ggsave(filename = "ktm_velocity_subsidence.jpg", plot = p,
       path = getwd(),
       scale = 1.5, width = 12,
       height = 8, units = 'cm',
       dpi = 500)


