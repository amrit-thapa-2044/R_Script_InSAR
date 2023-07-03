
rm(list=ls())
library(raster)
library(pracma)
library(ptw)
library(shiny)
library(shinydashboard)
library(leaflet)

setwd('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/HyP3_MintPy_output_ktm_subsidence/ERA5_atmospheric_correction')

suppressWarnings(ras_c <- stack('timeseries_ERA5_ramp_demErr.h5')*100)
ras=ras_c[[2:nlayers(ras_c)]]
maskTempCoh=raster('maskTempCoh.tif')
extent(ras)=extent(maskTempCoh)
crs(ras)=crs('+init=epsg:4326')
shp_r=raster::shapefile('C:/UAF_PhD_AMRIT/GEOS_657_Microwave_Remote_Sensing/class_project_land_subsidence_kathmandu/Kathmandu_valley_shapefile/KTM_Watershed.shp')
shp_r=spTransform(shp_r,CRS('+init=epsg:4326'))

# test plot
plot(ras[[1]],col=rainbow(100))

server <- function(input, output) {
  
  #Get Coordinates for Basemap
  xBase <- (extent(ras)[2] + extent(ras)[1]) / 2
  yBase <- (extent(ras)[4] + extent(ras)[3]) / 2
  
  
  sp <- SpatialPoints(data.frame(xBase ,yBase))
  crs(sp) <- crs(ras)
  sp <- spTransform(sp, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  
  plot(ras[[2]])
  points(sp)
  
  output$rasPlot <- renderPlot({
    plot(ras[[input$layer]],main=names(ras[[input$layer]]))
    plot(shp_r,add=T)
  }, height = 400)

  
  output$rasProj       <- renderText(projection(ras))
  output$rasRes        <- renderText(res(ras))
  output$rasDim        <- renderText(dim(ras))
  output$rasNlayers    <- renderText(nlayers(ras))
  
  
  output$cellnumber <- renderText(round(Coords(),3))
  
  
  output$rasvalue   <- renderText(value())
  
  Coords <- reactive({
    req(input$rasPlot_click$x)
    
    c(input$rasPlot_click$x, input$rasPlot_click$y)
    
  })
  
  
  value  <- eventReactive(input$rasPlot_click$x,{
    extract(ras,cellFromXY(ras,Coords()))
  })
  
  
  output$Map <- renderLeaflet({
    leaflet() %>% 
      setView(sp@coords[1],sp@coords[2], 8) %>% 
      addProviderTiles("Esri.WorldImagery") 
  })
  
  
  output$TSplot <- renderPlot({
    
    req(input$rasPlot_click$x)
    plot(1:nlayers(ras),value(), type = "l", xlab="Time", ylab="Displacement[cm]")

    
    if(input$filterCheckSav) {
      lines(1:nlayers(ras),savgol(value(),5), col = "red")
    }
    
    if(input$filterLinear) {
      lines(1:nlayers(ras),whit2(value(),5), col = "green")
    }
    
  })
}

#-------------------------------------------------------------------------------------------------------

ui = dashboardPage(
  dashboardHeader(
    title = "Displacement Time Series Analysis",
    titleWidth = 500
  ),
  dashboardSidebar(
    h4("Filter:"),
    checkboxInput("filterCheckSav", "Savitzky-Golay", value = FALSE),
    checkboxInput("filterCheckWhit", "Whittaker", value = FALSE),
    checkboxInput("filterLinear", "Linear", value = FALSE)
    ),
  
  dashboardBody(
    fluidRow(
      tabBox(
        title = "Interactive Image Analysis", id = "tabset",
        tabPanel("Raster", 
                 plotOutput("rasPlot", click = "rasPlot_click"),
                 sliderInput("layer", "Plot Timestep", min = 1, max = nlayers(ras),1, width="100%")),
        tabPanel("Basemap", leafletOutput("Map", width = "100%"))
      ),
      box(
        title = "Properties", status = "info", solidHeader = TRUE,
        
        HTML("<b>Layers:</b>"),
        textOutput("rasNlayers"),
        HTML("<b>Resolution:</b>"),
        textOutput("rasRes"),
        HTML("<b>Projection:</b>"),
        textOutput("rasProj"),
        
        h4("Selected Coordinates"),
        textOutput("cellnumber")
      )
    ),
    fluidRow(
      box(
        title = "Time Series [Click in above image to visualize]", status = "warning", solidHeader = TRUE, width="100%",
        plotOutput("TSplot")
      )
    )
    )
  )


#----------------------------------------------------------------------------------------


shinyApp(ui = ui, server = server)
