rm(list=ls())

library(terra);library(rhdf5);library(ggplot2);library(tidyterra);library(rhdf5)


setwd('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/displacement_elevation_cor_atmospheric_correction')
outpath='./output_figures/'

ts_h5='timeseries.h5'
ts_era5='timeseries_tropHgt.h5'
ts_era5_ramp='timeseries_tropHgt_ramp.h5'
ts_era5_ramp_demErr='timeseries_tropHgt_ramp_demErr.h5'
maskTempCoh_tif='maskTempCoh_Chovar_27.665812_85.291326.tif'
heo_h5='geometryGeo.h5'
df_4_date=read.csv('SBAS_mintpy_displacement_my_points27.665812_85.291326.csv')
head(df_4_date);dim(df_4_date)

rf_point<- cbind(85.291326,27.66581) # reference point

# AOI to plot time series
xy <- cbind(85.326669, 27.656686) # NAST

#---------------------------------------------------------------------------------------------

timeseries=rast(ts_h5)*100
timeseries_ERA5=rast(ts_era5)*100
timeseries_ERA5_ramp=rast(ts_era5_ramp)*100
timeseries_ERA5_ramp_demErr=rast(ts_era5_ramp_demErr)*100

height=h5read(heo_h5, "height")
r_height=terra::rast(t(height)) # rows and columns are flipped while reading h5 so need to transpose
terra::plot(r_height)

final_mask=rast(maskTempCoh_tif);plot(final_mask)
ext(r_height)=ext(final_mask);crs(r_height)=crs('+init=epsg:4326');terra::plot(r_height)

ext(timeseries)=ext(final_mask);crs(timeseries)=crs('+init=epsg:4326');plot(timeseries[[2]])
ext(timeseries_ERA5)=ext(final_mask);crs(timeseries_ERA5)=crs('+init=epsg:4326');plot(timeseries_ERA5[[2]])
ext(timeseries_ERA5_ramp)=ext(final_mask);crs(timeseries_ERA5_ramp)=crs('+init=epsg:4326');plot(timeseries_ERA5_ramp[[2]])
ext(timeseries_ERA5_ramp_demErr)=ext(final_mask);crs(timeseries_ERA5_ramp_demErr)=crs('+init=epsg:4326');plot(timeseries_ERA5_ramp_demErr[[2]])

timeseries[final_mask==0]=NA
timeseries_ERA5[final_mask==0]=NA
timeseries_ERA5_ramp[final_mask==0]=NA
timeseries_ERA5_ramp_demErr[final_mask==0]=NA
r_height[final_mask==0]=NA

# r_p=raster::raster(timeseries[[2]])
# mapview::mapview(r_p)

#------------------------------------------------------------------------------------------------------------
df_ts_all=data.frame(Date=as.Date(df_4_date$Date,'%Y-%m-%d'),
                     timeseries=as.vector(t(terra::extract(timeseries, xy))[,1]),
                     timeseries_ERA5=as.vector(t(terra::extract(timeseries_ERA5, xy))[,1]),
                     timeseries_ERA5_ramp=as.vector(t(terra::extract(timeseries_ERA5_ramp, xy))[,1]),
                     timeseries_ERA5_ramp_demErr=as.vector(t(terra::extract(timeseries_ERA5_ramp_demErr, xy))[,1]))
head(df_ts_all)
df_melt <- reshape2::melt(df_ts_all, id='Date',variable.name = "Type",value.name='value')
head(df_melt)

p_d <- ggplot(df_melt, aes(x = Date, y = value,color=Type)) +
  geom_line(linewidth=1)+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text= element_text(size = 16,color='black'))+
  theme(plot.title = element_text(size = 20))+
  theme(legend.position = c(0.7,0.8),legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  ylab("Displacement[cm]")


print(p_d)

ggsave(filename = paste0(outpath,"rf_",rf_point[1],"_",rf_point[2],"_ktm_displacement_time_series_all_",xy[1],'_',xy[2],".jpg"), plot = p_d,
       path = getwd(),
       scale = 1.5, width = 16,
       height = 9, units = 'cm',
       dpi = 500)

#---------------------------------------------------------------------------------------
# IMPORTANT: the rows and columns are flipped when imported as h5
h5_raw=h5read(ts_h5, "timeseries")
h5_era5=h5read(ts_era5, "timeseries")
h5_era5_ramp=h5read(ts_era5_ramp, "timeseries")
h5_era5_ramp_demErr=h5read(ts_era5_ramp_demErr, "timeseries")

data_date=h5read(ts_h5, "date")
data_date=as.Date(data_date,format='%Y%m%d')

# flip rows and column
# NOTE aperm transpose rows and column. in our case first dimention is col, second is row and third is date, we want transpose rows and cols [first two]
a_raw=aperm(h5_raw,c(2, 1, 3))                             #;plot(rast(a_raw[,,2]))
a_era5=aperm(h5_era5,c(2, 1, 3))                             #;plot(rast(a_era5[,,2]))
a_era5_ramp=aperm(h5_era5_ramp,c(2, 1, 3))                  #;plot(rast(a_era5_ramp[,,2]))
a_era5_ramp_demErr=aperm(h5_era5_ramp_demErr,c(2, 1, 3))   #;plot(rast(a_era5_ramp_demErr[,,2]))

m_final_mask=as.matrix(raster::raster(final_mask))
m_height=as.matrix(raster::raster(r_height))
m_height[!m_final_mask]<-NA

a_raw[!m_final_mask]<-NA
plot(rast(a_raw[,,2]))
a_era5[!m_final_mask]<-NA
a_era5_ramp[!m_final_mask]<-NA
a_era5_ramp_demErr[!m_final_mask]<-NA

df_correlation=data.frame(Date=data_date)

for(i in 1:(dim(h5_raw)[3])){
  
  # df_correlation$Correlation_ts_elv[i]=cor(as.vector(t(height)),as.vector(a_raw))
  # df_correlation$Correlation_ts_ERA5_elv[i]=cor(as.vector(t(height)),as.vector(a_era5))
  # df_correlation$Correlation_ts_era5_ramp_elv[i]=cor(as.vector(t(height)),as.vector(a_era5_ramp))
  # df_correlation$Correlation_ts_era5_ramp_demErr[i]=cor(as.vector(t(height)),as.vector(a_era5_ramp_demErr))
  
  df_correlation$Raw[i]=cor(as.vector(m_height),as.vector(a_raw[,,i]),use = "pairwise.complete.obs")
  df_correlation$After_elv_cor[i]=cor(as.vector(m_height),as.vector(a_era5[,,i]),use = "pairwise.complete.obs")
  df_correlation$After_elv_cor_ramp[i]=cor(as.vector(m_height),as.vector(a_era5_ramp[,,i]),use = "pairwise.complete.obs")
  df_correlation$After_elv_cor_ramp_dem_cor[i]=cor(as.vector(m_height),as.vector(a_era5_ramp_demErr[,,i]),use = "pairwise.complete.obs")
  
  print(paste0('processing done for ',data_date[i]))
}

head(df_correlation)

df_correlation$Month<- format(df_correlation$Date, "%m")
df_melt <- reshape2::melt(df_correlation, id=c('Date','Month'),variable.name = "Displacement")
head(df_melt)

d_c=ggplot(df_melt, aes(x = value)) +
  geom_density() +
  facet_wrap(~ Displacement)+xlab('Correlation')

print(d_c)

d_c_a=ggplot(df_melt, aes(x = value,color=Displacement)) +
  geom_density() +
  xlab('Correlation')+theme(legend.position = c(0.2,0.8))

print(d_c_a)

ggsave(filename = paste0(outpath,"rf_",rf_point[1],"_",rf_point[2],"_kathmandu_correlation_elevation_displacement.jpg"), plot =d_c_a,
       path = getwd(),
       scale = 4, width = 6,
       height = 5, units = 'cm',
       dpi = 500)

c_m=ggplot(df_melt, aes(x=Month,y=value))+geom_boxplot()+
  facet_wrap(~ Displacement)+ylab('Correlation')

print(c_m)

ggsave(filename = paste0(outpath,"rf_",rf_point[1],"_",rf_point[2],"_kathmandu_correlation_monthly_boxplot.jpg"), plot =c_m,
       path = getwd(),
       scale = 4, width = 6,
       height = 5, units = 'cm',
       dpi = 500)

#------------------------------------------------------------------------------------------------

indx_max_cor=which(df_correlation$Raw==max(df_correlation$Raw,na.rm=T))


s_all_compare=c(timeseries[[indx_max_cor]],timeseries_ERA5[[indx_max_cor]],
                timeseries_ERA5_ramp[[indx_max_cor]],timeseries_ERA5_ramp_demErr[[indx_max_cor]])

names(s_all_compare)=paste0(c('Raw','After_elv_cor','After_elv_cor_ramp','After_elv_cor_ramp_dem_cor'),sep="_",data_date[indx_max_cor])

data_max=max(global(s_all_compare,'max',na.rm=T))
data_min=min(global(s_all_compare,'min',na.rm=T))
my_label=round(seq(data_min,data_max,length.out=5),0)

points <- data.frame(
  x =rep(rf_point[1],nlyr(s_all_compare)),
  y = rep(rf_point[2],nlyr(s_all_compare)))

p_r=ggplot(points) +
  geom_spatraster(data = s_all_compare) +
  facet_wrap(~lyr, ncol = 2)+
  #scale_fill_gradient2(low = "red", mid = 'white', high = "blue",
  #midpoint = 0, space = "Lab",na.value='transparent',limits=c(data_min,data_max))+
  scale_fill_gradientn(colours = terrain.colors(10))+
  
  theme(axis.text =element_text(size=8),strip.text = element_text(size=8))+
  theme(
    legend.position = "right",
    legend.title.align = 1,
    legend.key.height  = unit(4,"line")) +
  guides(fill = guide_colourbar(title.position = "bottom"))+
  labs( x=NULL,y=NULL,
        fill = expression(paste(cm), sep = ""))+
  geom_point(aes(x = x, y = y, color = 'black'), size = 3)+guides(color = 'none')

print(p_r)

ggsave(filename = paste0(outpath,"rf_",rf_point[1],"_",rf_point[2],"_kthmandu_comparision_raw_and_corrected_displacement.jpg"), plot =p_r,
       path = getwd(),
       scale = 4, width = 6,
       height = 5, units = 'cm',
       dpi = 500)

