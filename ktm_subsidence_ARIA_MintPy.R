rm(list=ls())

setwd('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/ARIA_product_ktm_subsidence')
df_4_date=read.csv('SBAS_mintpy_displacement_my_points.csv')
head(df_4_date);dim(df_4_date)
  
library(terra)
timeseries=rast('timeseries.h5')*100
timeseries_ERA5=rast('timeseries_ERA5.h5')*100
timeseries_ERA5_ramp=rast('timeseries_ERA5_ramp.h5')*100
timeseries_ERA5_ramp_demErr=rast('timeseries_ERA5_ramp_demErr.h5')*100

maskConnComp=rast('maskConnComp.tif')

ext(timeseries)=ext(maskConnComp);crs(timeseries)=crs('+init=epsg:4326');plot(timeseries[[2]])
ext(timeseries_ERA5)=ext(maskConnComp);crs(timeseries_ERA5)=crs('+init=epsg:4326');plot(timeseries_ERA5[[2]])
ext(timeseries_ERA5_ramp)=ext(maskConnComp);crs(timeseries_ERA5_ramp)=crs('+init=epsg:4326');plot(timeseries_ERA5_ramp[[2]])
ext(timeseries_ERA5_ramp_demErr)=ext(maskConnComp);crs(timeseries_ERA5_ramp_demErr)=crs('+init=epsg:4326');plot(timeseries_ERA5_ramp_demErr[[2]])

mapview::mapview(raster(timeseries[[2]]))
xy <- cbind(85.326669, 27.656686) # NAST
xy <- cbind(85.244, 27.788)
xy <- cbind(85.312012, 27.561826)


df_ts_all=data.frame(Date=as.Date(df_4_date$Date,'%Y-%m-%d'),
                     timeseries=as.vector(t(extract(timeseries, xy))[,1]),
                     timeseries_ERA5=as.vector(t(extract(timeseries_ERA5, xy))[,1]),
                     timeseries_ERA5_ramp=as.vector(t(extract(timeseries_ERA5_ramp, xy))[,1]),
                     timeseries_ERA5_ramp_demErr=as.vector(t(extract(timeseries_ERA5_ramp_demErr, xy))[,1]))
head(df_ts_all)
#------------------------------------------------------------------------------------------------------------
df_melt <- reshape2::melt(df_ts_all, id='Date',variable.name = "Type",value.name='value')
head(df_melt)

library(ggplot2)
p <- ggplot(df_melt, aes(x = Date, y = value,color=Type)) +
  geom_line(linewidth=1)+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text= element_text(size = 16,color='black'))+
  theme(plot.title = element_text(size = 20))+
  theme(legend.position = c(0.3,0.8),legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  ylab("Displacement[cm]")


print(p)
