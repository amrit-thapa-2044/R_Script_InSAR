rm(list=ls())

library(terra);library(rhdf5);library(ggplot2);library(tidyterra)

p_era5='C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/ERA5_atmospheric_correction/'
p_cor_d_e='C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/displacement_elevation_cor_atmospheric_correction/'

velocity_era5=rast(paste0(p_era5,'velocity.tif'))
velocity_elv=rast(paste0(p_cor_d_e,'velocity_Chovar_27.665812_85.291326.tif'))

mask_era5=rast(paste0(p_era5,'maskTempCoh.tif'))*100
mask_elv=rast(paste0(p_cor_d_e,'maskTempCoh_Chovar_27.665812_85.291326.tif'))*100

velocity_era5[mask_era5==0]=NA
velocity_elv[mask_elv==0]=NA

plot(velocity_era5)
plot(velocity_elv)

plot(as.vector(velocity_era5),as.vector(velocity_elv),xlab='ERA5',ylab='Displacement_Elevation')


h5_v_era5=paste0(p_era5,'velocity_at_cor_with_era5.h5')
h5_v_elv=paste0(p_cor_d_e,'velocity_atm_cor_cor_dis_elevation.h5')
  
v_std_era5=h5read(h5_v_era5, "velocityStd");v_std_era5=terra::rast(t(v_std_era5))*100
ext(v_std_era5)=ext(mask_era5);crs(v_std_era5)=crs('+init=epsg:4326')

v_std_elv=h5read(h5_v_elv, "velocityStd");v_std_elv=terra::rast(t(v_std_elv))*100
ext(v_std_elv)=ext(mask_era5);crs(v_std_elv)=crs('+init=epsg:4326')

v_std_era5[mask_era5==0]=NA
v_std_elv[mask_era5==0]=NA

std_s=c(v_std_era5,v_std_elv)
names(std_s)=c('ERA5','cor(D,E)')

ggplot() +
  geom_spatraster(data = std_s) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_gradientn(colours = terrain.colors(10))

hist(v_std_era5-v_std_elv)

boxplot(std_s)

