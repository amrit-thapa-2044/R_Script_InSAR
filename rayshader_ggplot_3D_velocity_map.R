rm(list=ls())
library(raster)
library(ggplot2)

out_path='C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/'

velocity=raster('C:/Users/athapa2/Downloads/velocity (1).tif')*100
plot(velocity)

shp=shapefile('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/Kathmandu_valley-20230402T200134Z-001/Kathmandu_valley/KTM_Watershed.shp')
shp=spTransform(shp,CRS('+init=epsg:4326'))

np=shapefile('C:/shapefile_all/NepalLocalUnits0/NepalLocalUnits0.shp')
unique(np$GaPa_NaPa)

my_aoi_name="Kathmandu"
my_aoi=np[np$GaPa_NaPa=='Kathmandu'|np$GaPa_NaPa=='Bhaktapur'|np$GaPa_NaPa=='Lalitpur',]
plot(my_aoi,axes=T)


velocity=crop(velocity,shp)
plot(velocity);plot(shp,add=T)
velocity=mask(velocity,shp)

df=as.data.frame(velocity,xy=T)
head(df)
colnames(df)=c('x','y','Velocity')



library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))      # Create reverse Spectral palette

plot_velocity <-  ggplot(df, aes(x = x, y = y, fill = Velocity)) +
  geom_tile()+
  #geom_contour(aes(x=x,y=y,z=velocity_ktm),bins = 5,color="black")+
  scale_fill_viridis_c(option = "magma",na.value = "transparent")+
  labs( x=NULL,y=NULL,
    fill = expression(paste(cm/yr), sep = "")) +
  theme(
        legend.position = "right",
        legend.title = element_text(size = 15,
                                    family = "serif",
                                    margin = margin(t = .5, unit = "cm")),
        legend.text = element_text(size = 13,
                                   family = "serif"),
        legend.title.align = 1,
        legend.key.height  = unit(2.5,"line"),
        plot.title = element_text(family = "serif", size = 25),
        plot.subtitle = element_text(family = "serif", size = 18)) +
  guides(fill = guide_colourbar(title.position = "bottom"))+
  ylim(c(27.56,27.81803))+
  geom_contour(aes(z=Velocity), breaks=-5,colour = "black",size = 0.2)+
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "dark green", fill = NA)
  

plot(plot_velocity)

ggsave(filename = "kathmandu_subsidence.jpg", plot = plot_velocity,
       path = out_path,
       scale = 1.5, width = 8,
       height = 6, units = 'cm',
       dpi = 500)

library(rasterVis)

ppp=gplot(velocity) +  
  geom_tile(aes(fill=value))+
  #scale_fill_gradientn(colours=myPalette(4),na.value = "transparent") + 
  scale_fill_viridis_c(option = "magma",na.value = "transparent")+
  labs( x=NULL,y=NULL,
        fill = expression(paste(cm/yr), sep = ""),
        title = "Land Subsidence in Kathmandu",
        subtitle =  paste0("Data Source: Sentinel 1 using SBAS \nAnalyzed by amrit THAPA")) +
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
  ylim(c(27.56,27.81803))
  #geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "dark green", fill = NA)

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
         zoom = .8, 
         phi = 30,
         windowsize = c(1200, 1400))

render_movie(filename = paste0(out_path,'ktm_subsidence.mp4'),progbar=T)


plot_gg(ppp
        , width=7
        , height = 7
        , multicore = TRUE
        , windowsize = c(1400,866)
        , sunangle=225
        , zoom = 0.60
        , phi = 30
        , theta = 45
)

render_movie(filename = paste0(out_path,'ktm_subsidence_c.mp4'),progbar=T)

# Render to 3D
plot_gg(ppp,
        width = 6, height = 6,
        scale = 300,
        multicore = TRUE,
        fov = 60, theta = 270, phi = 25,
        zoom = 0.20,
        offset_edges = FALSE,
        windowsize = c(1200, 1400)
)

render_snapshot(paste0(out_path,'ktm_subsidence_c.png'))
render_highquality("ktm_subsidence.png", lightdirection = c(270))
#render_movie(paste0(out_path,"ktm_subsidence.mp4"), title_text = "Population Density of New Jersey")
