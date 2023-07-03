rm(list=ls())
library(terra)

setwd('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/reference_point_checkpost')

maskTempCoh=rast('maskTempCoh.tif');plot(maskTempCoh)
#plotKML::plotKML(raster::raster(maskTempCoh))

maskConnectedComponent=rast('maskConnComp.tif');plot(maskConnectedComponent)
final_mask=maskTempCoh*maskConnectedComponent;plot(final_mask)


spatial_coherence=rast('avgSpatialCoh.tif');plot(spatial_coherence)

shp_r=raster::shapefile('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/Kathmandu_valley-20230402T200134Z-001/Kathmandu_valley/KTM_Watershed.shp')
wgs84='+init=epsg:4326'

library(sf);library(raster)
shp_r=spTransform(shp_r,CRS(wgs84))
shp=vect(shp_r)

#-------------------------------------------------------------------------------
spatial_coherence=mask(crop(spatial_coherence,shp),shp)
plot(spatial_coherence);plot(shp,add=T)

#-------------------------------------------------------------------------------
temporal_coherence=rast('TemporalCoherence.tif');plot(temporal_coherence)

temporal_coherence=mask(crop(temporal_coherence,shp),shp)
plot(temporal_coherence);plot(shp,add=T)

coherence_stack= c(spatial_coherence,temporal_coherence)
coherence_stack[spatial_coherence==0]=NA
xy <- cbind(85.227218,27.688954) # reference point
p <- vect(xy, crs="+proj=longlat +datum=WGS84")
extract(coherence_stack,p)
library(tidyterra)
library(ggplot2)

# Facet all layers

p=ggplot(shp)+
  geom_spatraster(data = coherence_stack) +
  facet_wrap(~lyr, ncol = 4) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "")
  ) +
  labs(fill = "Coherence")+
  theme(legend.position = c(0.5, 0.1),
        legend.direction = 'horizontal',legend.key.size = unit(.5, "cm"),
        legend.key.width = unit(0.8,"cm"))+
  geom_spatvector(fill = NA,col='black',size=1)+
  geom_point(aes(x=xy[1], y=xy[2]), colour="black",size=3,shape=15)+ylab(NULL)+xlab(NULL)

print(p)

ggsave(filename = paste0("ktm_subsidence_coherence",xy[1],'_',xy[2],".jpg"), plot = p,
       path = getwd(),
       scale = 1.5, width = 16,
       height = 8, units = 'cm',
       dpi = 500)


#-------------------------------------------------------------------------------

velocity=rast('velocity.tif')*100;plot(velocity)
velocity=mask(crop(velocity,shp),shp)
plot(velocity);plot(shp,add=T)

maskTempCoh=project(maskTempCoh,velocity)
final_mask=mask(crop(final_mask,shp),shp)
velocity[final_mask==0]=NA
plot(velocity)
names(velocity)='Velocity'

mapview::mapview(raster(velocity))

plot_velocity=ggplot(shp)+
  geom_spatraster(data =velocity)  +
  #facet_wrap(~lyr, ncol = 4)+
  geom_spatraster_contour(data = velocity, breaks = -5,color = "grey30")+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, space = "Lab",na.value='transparent',limits=c(-20,5)) +
  labs( x=NULL,y=NULL,
        fill = expression(paste(cm/yr), sep = ""))+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15,
                                family = "serif",
                                margin = margin(t = .5, unit = "cm")),
    legend.text = element_text(size = 13,
                               family = "serif"),
    legend.title.align = 1,
    legend.key.height  = unit(6,"line"),
    plot.title = element_text(family = "serif", size = 25),
    plot.subtitle = element_text(family = "serif", size = 18)) +
  guides(fill = guide_colourbar(title.position = "bottom"))+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text= element_text(size = 16,color='black'))+
  theme(plot.title = element_text(size = 20))+
  theme(strip.text = element_text(size=20))+
  geom_spatvector(fill = NA,col='black',size=2)+
  geom_point(aes(x=xy[1], y=xy[2]), colour="black",size=3,shape=15)+
  annotate("text", x=85.34, y=27.719888, label= "Lazimpat",color='black',size=4)+
  annotate("text", x=85.290321, y=27.7, label= "Kalimati",color='black',size=4)+
  annotate("text", x=85.325961, y=27.671760, label= "Patan",color='black',size=4)+
  annotate("text", x=85.285682, y=27.715, label= "Sano Bharang",color='black',size=4)+
  annotate("text", x=85.396056, y=27.68, label= "Gankhu",color='black',size=4)+
  annotate("text", x=85.336996, y=27.690337, label= "New Baneshwor",color='black',size=4)+
  annotate("text", x=85.348481, y=27.678234, label= "Koteshwor",color='black',size=4)+
  annotate("text", x=85.344, y=27.6619, label= "Imadol",color='black',size=4)


print(plot_velocity)


ggsave(filename = paste0("Velocity_",xy[1],'_',xy[2],".jpg"), plot =plot_velocity,
       path = getwd(),
       scale = 4, width = 6,
       height = 5, units = 'cm',
       dpi = 500)


#---------------- 3d plot -------------------------------------------
library(rasterVis)

ppp=gplot(velocity) +  
  geom_tile(aes(fill=value))+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, space = "Lab",na.value='transparent',limits=c(-20,5)) +
  labs( x=NULL,y=NULL,
        fill = expression(paste(cm/yr), sep = ""),
        title = "Land Subsidence in Kathmandu")+
        #subtitle =  paste0("Data Source: Sentinel 1 using SBAS \nAnalyzed by amrit THAPA")) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15,
                                family = "serif",
                                margin = margin(t = .5, unit = "cm")),
    legend.text = element_text(size = 13,
                               family = "serif"),
    legend.title.align = 1,
    legend.key.height  = unit(3,"line"),
    plot.title = element_text(family = "serif", size = 25),
    plot.subtitle = element_text(family = "serif", size = 18)) +
  guides(fill = guide_colourbar(title.position = "bottom"))+
  geom_polygon(data = shp_r, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
print(ppp)

library(rayshader)
plot_gg(ppp,
        multicore = T,
        width=6,
        height=6,
        scale=100,
        #shadow_intensity = .75,
        sunangle = 225,
        theta = 45, 
        zoom = .4, 
        phi = 30,
        windowsize = c(1600,1800))


render_movie(paste0('ktm_subsidence_',xy[1],'_',xy[2],'.mp4'),progbar=T,frames = 720)

#---------------------------------------------------------------------------------
my_aoi=read.csv('SBAS_mintpy_displacement_my_points.csv')
head(my_aoi)
tail(my_aoi)[6,-9]*100
dim(my_aoi)


my_aoi$Date=as.Date(my_aoi$Date,format='%Y-%m-%d')
df_melt <- reshape2::melt(my_aoi, id='Date',variable.name = "Place")

# Create the plot
p <- ggplot(df_melt, aes(x = Date, y = value*100,color=Place)) +
  geom_line(linewidth=1)+
  labs(title = "Displacement Time Series, Reference Date: 2017-01-04 , Date: {frame_along}",y='Displacement [cm]')+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text= element_text(size = 16,color='black'))+
  theme(plot.title = element_text(size = 20))+
  theme(legend.position = c(0.2,0.4),legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  shadow_mark(color = 'grey', size = 0.2, past = FALSE, future = TRUE)

print(p)

library(gganimate)
q=p + transition_reveal(Date)
gganimate::animate(q, width = 800, height = 400)

# Save the animation as a GIF
gganimate::anim_save(paste0("ts_displacement_animation_",xy[1],'_',xy[2],".gif"),gganimate::animate(q, width = 800, height = 400), animator = gifski_renderer(),duration=30)

qq <- ggplot(df_melt, aes(x = Date, y = value*100,color=Place)) +
  geom_line(linewidth=1)+
  #labs(title = "Displacement Time Series, Reference Date: 2017-01-04 , Date: {frame_along}",y='Displacement [cm]')+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text= element_text(size = 16,color='black'))+
  theme(plot.title = element_text(size = 20))+
  theme(legend.position = c(0.2,0.4),legend.text = element_text(size = 16),legend.title = element_text(size = 16))+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+ylab('Displacement [cm]')

print(qq)

ggsave(filename = paste0("ktm_displacement_time_series_",xy[1],'_',xy[2],".jpg"), plot = qq,
       path = getwd(),
       scale = 1.5, width = 16,
       height = 9, units = 'cm',
       dpi = 500)

#---------------------------------------------------------------------------------------------------
timeseries=rast('timeseries_ERA5_ramp_demErr.h5')
#plot(timeseries)
ext(timeseries)=ext(maskTempCoh)
crs(timeseries)=crs('+init=epsg:4326')
plot(timeseries[[2]])

timeseries=mask(crop(timeseries,final_mask),final_mask)
timeseries[final_mask==0]=NA
plot(timeseries[[1:4]])

plot(timeseries[[nlyr(timeseries)]]*100)
hist(timeseries[[nlyr(timeseries)]]*100)

# Load the animation package
library(animation)

t_timeseries=timeseries[[-1]]*100
names(t_timeseries)=paste0('Displacement_',my_aoi$Date[-1])

last_layer=t_timeseries[[nlyr(t_timeseries)]]
last_layer=mask(last_layer,shp)
names(last_layer)=paste('Total displacement from ',my_aoi$Date[1], ' to ',my_aoi$Date[length(my_aoi$Date)])

p=ggplot(shp)+
  geom_spatraster(data =last_layer)  +
  facet_wrap(~lyr, ncol = 4)+
  # geom_spatraster_contour(data = last_layer, breaks = seq(-100, 40, 40),color = "grey30")+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, space = "Lab",na.value='transparent',limits=c(-120,40))+
  labs(fill = "Displacement [cm]")+
  theme(legend.position = c(0.1, 0.2),
        legend.direction = 'vertical',legend.key.size = unit(.5, "cm"),
        legend.key.width = unit(0.8,"cm"),legend.key.height = unit(1.2,'cm'))+
  geom_spatvector(fill = NA,col='black',size=1)

print(p)

plot_total_displacement <-  ggplot(shp)+
  geom_spatraster(data =last_layer)  +
  #facet_wrap(~lyr, ncol = 4)+
  #geom_spatraster_contour(data = last_layer, breaks = seq(-100, 40, 40),color = "grey30")+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, space = "Lab",na.value='transparent',limits=c(-120,40))+
  labs( x=NULL,y=NULL,
        fill = expression(paste(cm), sep = ""))+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15,
                                family = "serif",
                                margin = margin(t = .5, unit = "cm")),
    legend.text = element_text(size = 13,
                               family = "serif"),
    legend.title.align = 1,
    legend.key.height  = unit(7,"line"),
    plot.title = element_text(family = "serif", size = 25),
    plot.subtitle = element_text(family = "serif", size = 18)) +
  guides(fill = guide_colourbar(title.position = "bottom"))+
  annotate("text", x=85.34, y=27.719888, label= "Lazimpat",color='black',size=4)+
  annotate("text", x=85.290321, y=27.7, label= "Kalimati",color='black',size=4)+
  annotate("text", x=85.325961, y=27.671760, label= "Patan",color='black',size=4)+
  annotate("text", x=85.285682, y=27.715, label= "Sano Bharang",color='black',size=4)+
  annotate("text", x=85.396056, y=27.68, label= "Gankhu",color='black',size=4)+
  annotate("text", x=85.336996, y=27.690337, label= "New Baneshwor",color='black',size=4)+
  annotate("text", x=85.348481, y=27.678234, label= "Koteshwor",color='black',size=4)+
  annotate("text", x=85.344, y=27.6619, label= "Imadol",color='black',size=4)+
  theme(axis.text.y = element_text(angle = 90, hjust = 1))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text= element_text(size = 16,color='black'))+
  theme(plot.title = element_text(size = 20))+
  theme(strip.text = element_text(size=20))+
  geom_spatvector(fill = NA,col='black',size=2)+
  geom_point(aes(x=xy[1], y=xy[2]), colour="black",size=3,shape=15)


print(plot_total_displacement)
hist(last_layer)


ggsave(filename = paste0("kathmandu_total_displacement_",xy[1],'_',xy[2],".jpg"), plot =plot_total_displacement,
       path = getwd(),
       scale = 4, width = 6,
       height = 5, units = 'cm',
       dpi = 500)

