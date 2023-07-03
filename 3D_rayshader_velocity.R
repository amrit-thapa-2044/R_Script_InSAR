rm(list=ls())

library(terra);library(rhdf5);library(ggplot2);library(tidyterra);library(rhdf5)

setwd('S:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/displacement_elevation_cor_atmospheric_correction')
outpath='./output_figures/'

heo_h5='geometryGeo.h5'
maskTempCoh_tif='maskTempCoh_Chovar_27.665812_85.291326.tif'
velocity_tif='velocity_Chovar_27.665812_85.291326.tif'

height=h5read(heo_h5, "height")
r_height=terra::rast(t(height)) # rows and columns are flipped while reading h5 so need to transpose
terra::plot(r_height)

final_mask=rast(maskTempCoh_tif);plot(final_mask)
ext(r_height)=ext(final_mask);crs(r_height)=crs('+init=epsg:4326');terra::plot(r_height)

my_extent=extent(85.18847, 85.52487, 27.56, 27.8183)
r_height=crop(r_height,my_extent);plot(r_height)

r_velocity=rast(velocity_tif)*100;names(r_velocity)='velocity'
plot(r_velocity)
r_velocity[final_mask==0]=NA
r_velocity=crop(r_velocity,r_height)
plot(r_velocity)

source("S:/UAF_PhD_AMRIT/R_script_UAF_amrit/amrit_rayshader/plot_raster_3d.R")

rr_velocity=raster::raster(r_velocity);plot(rr_velocity)
rr_height=raster::raster(r_height)

# Visualize tpi in 3d
p_v <- plot_raster_3d(rr_velocity, rr_height,
                                   min_value = -20,
                                   max_value = 5,
                                   z_scale = 10, 
                                   colour_ramp = diverging_hcl(100, palette = "Purple-Brown"))
print(p_v)
#save_plot("./velocity.png", p_v)


p_r=ggplot() +
  geom_spatraster(data = r_velocity) +
  scale_fill_gradientn(colours = diverging_hcl(100, palette = "Purple-Brown"),limit=c(-20,5))+
  labs( x=NULL,y=NULL,fill = expression(paste(cm/yr), sep = ""))+
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 20,vjust = -0.3, hjust = 0),
        legend.spacing.x = unit(-0.4, 'cm'),
        legend.key.height = unit(0.5, "cm"),legend.key.width = unit(0.3, "cm"))
  

print(p_r)

library("cowplot")                                 
ggp_legend <- get_legend(p_r)                   

p_hist_rast <- ggdraw(p_v) +
  draw_plot(ggp_legend, x=.98, y=.73, width=.05 ,height =0.2)+
  ggspatial::annotation_north_arrow(rotation=-45,
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

print(p_hist_rast)

#save_plot("./velocity_with_legend.png",p_hist_rast)

ggsave(filename = "./velocity_with_legend.jpg", plot = p_hist_rast,
       path = getwd(),
       scale = 1, width = 8,
       height = 6, units = 'cm',
       dpi = 500)


#------------------------------------------
save_plot("./scale_arrow.png",
          add_scale_north(raster_plot_3d, scale_label_text = paste0(side_length, " m")),
          base_height = 4,
          base_aspect_ratio = 1)

