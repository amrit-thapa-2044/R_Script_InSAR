rm(list=ls())

setwd('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/ERA5_atmospheric_correction')
outpath='./output_figures/'

my_aoi=read.csv('SBAS_mintpy_displacement_my_points.csv')

my_aoi$Date=as.Date(my_aoi$Date,format='%Y-%m-%d')
df_melt <- reshape2::melt(my_aoi, id='Date',variable.name = "Place")

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

i=2
d_i=my_aoi[,i]

d_df=data.frame(x=1:length(d_i),y=d_i)
out.lm <- lm(y ~ x, data = d_df)

library(segmented)
o <- segmented(out.lm, seg.Z = ~x,control = seg.control(display = FALSE))

dat2 = data.frame(x = d_df$x, y = broken.line(o)$fit)

library(ggplot2)
ggplot(d_df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue')+
  ggtitle(colnames(my_aoi)[i])
