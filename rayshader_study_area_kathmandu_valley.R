# https://milospopovic.net/making-3d-topographic-maps-in-r/
#https://stackoverflow.com/questions/61759804/how-can-i-convert-a-rayshader-map-into-a-mp4-video-while-keeping-the-labels-com


rm(list = ls())
library(tidyverse)
library(osmdata)
library(sf)
library(rayshader) # To install rayshader, must install XQuartz first: https://github.com/tylermorganwall/rayshader/issues/86
library(raster)
library(elevatr)

my_aoi_path='C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/Kathmandu_valley-20230402T200134Z-001/Kathmandu_valley/KTM_Watershed.shp'
my_aoi_r=shapefile(my_aoi_path)
my_aoi=st_read(my_aoi_path)
plot(my_aoi)

L = st_cast(my_aoi,"LINESTRING")
plot(L)

library(mapview)
mapview(L)

new = st_crs(4326)
my_aoi_wgs84 = st_transform(my_aoi, new)
plot(my_aoi_wgs84)

L = st_cast(my_aoi_wgs84,"LINESTRING")
plot(L)


med_bbox <- st_bbox(c(xmin = 85.18881, xmax = 85.52508, 
                      ymin =27.53724, ymax = 27.81803),
                    crs = 4326)

med_bbox_df <- data.frame(x = c(85.18881, 85.52508),
                          y = c(27.53724, 27.81803))


extent_zoomed <- raster::extent(med_bbox)

prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
elev_med <- get_elev_raster(med_bbox_df, prj =  prj_dd, z = 12, clip = "bbox")
plot(elev_med)

elev_med=mask(elev_med,spTransform(my_aoi_r,CRS('+init=epsg:4326')))
plot(elev_med)
#-----------------------------------------------------------------------------------------
elev_med_mat <- raster_to_matrix(elev_med)

base_map = elev_med_mat %>% 
  height_shade() %>%
  add_overlay(sphere_shade(elev_med_mat, texture = "desert", colorintensity = 5), alphalayer=0.2)

plot_map(base_map)


med_roads <- med_bbox %>%
  opq() %>%
  add_osm_feature("highway",
                  c("motorway",
                    "trunk",
                    "primary", 
                    "secondary")) %>%
  osmdata_sf()

med_road_lines <- med_roads$osm_lines

med_rivers <- med_bbox %>%
  opq() %>%
  add_osm_feature("waterway",
                  c("river")) %>%
  osmdata_sf()

med_river_lines <- med_rivers$osm_lines




stream_layer = generate_line_overlay(med_river_lines,extent = extent_zoomed,
                                     linewidth = 4, color="blue", 
                                     heightmap = elev_med_mat)

med_buildings <- med_bbox %>%
  opq() %>%
  add_osm_feature("building") %>%
  osmdata_sf()

med_buildings_lines <- med_buildings$osm_points



house_layer=generate_point_overlay(
  med_buildings_lines, extent = extent_zoomed,
  pch = 20,color='red',
  heightmap = elev_med_mat,size=0.5
)

my_boundary=generate_line_overlay(L ,color="#D55E00",linewidth = 8,
                                     extent =extent_zoomed, heightmap = elev_med_mat)

my_roads=generate_line_overlay(med_road_lines,extent = extent_zoomed,
                      linewidth = 4, color="black",
                      heightmap = elev_med_mat)


final_map <- base_map %>%  
  add_overlay(my_roads)%>%
  add_overlay(stream_layer) %>%
  add_overlay(house_layer)%>%
  add_overlay(my_boundary)%>%
  plot_3d(elev_med_mat, 
        zscale = 5, 
        fov = 0, 
        theta = -45, 
        zoom = .9, 
        phi = 30,
        windowsize = c(1200, 1200))

render_label(elev_matrix, x = 700, y = 650, z = 1500, 
             zscale = 4, text = "Wellington", linecolor="black", freetype=FALSE)

render_label(elev_matrix, x = 1405, y = 1190, z = 800, 
             zscale = 4, text = "Mount Victoria", textcolor = "black", linecolor="darkred",dashed = TRUE, freetype=FALSE)

render_scalebar(limits=c(0, 5, 10,15),label_unit = "km",position = "W", y=50,
                scale_length = c(0,1))

render_compass(position = "N")

outpath='C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/'
w=2
h=2
render_highquality(filename=paste0(outpath,"ktm_dem.png"),
                   lightintensity=1500,
                   lightaltitude=90,
                   title_text="Topography of Kathmandu",
                   title_font="Nepal",
                   title_color="grey20",
                   title_size=100,
                   title_offset=c(360,180),
                   width=w*3, 
                   height=h*3)
