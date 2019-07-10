### App to assist with TDS impairments for roadsalt project ###
### Author: Kevin Zolea ###
### Date:02/2019 ###
########## Download/load necessary packages to make script run ############################################
if (!require(pacman)) {
  install.packages('pacman')
  
}

pacman::p_load("ggplot2","tidyr","plyr","dplyr","readxl","htmlwidgets",
               "readr","cowplot","lubridate","ggpubr","scales","rbokeh","shinyWidgets",
               "gridExtra","stringr","data.table","rlang","purrr","rmapshaper","shinyalert",
               "shinydashboard","shiny","shinycssloaders","plotly","sf","leaflet","leaflet.extras")
###############################################################################
### Read data in ###
### Read discrete data in ###
discrete_roadsalt<-read_tsv("cleanest_qa_dataset.tsv",col_names = T)
### Need to take HUC out of HUC number to match with continuous ###
discrete_roadsalt$HUC14<-gsub("HUC","",discrete_roadsalt$HUC14)
### Read in continuous data ###
continuous_roadsalt<-read_xlsx("specific_conductance_calculated_tds_data.xlsx",col_names = T)
continuous_roadsalt<-continuous_roadsalt%>%
  mutate(stdate =  as.Date(Date_Time))%>%
  mutate(year = year(stdate))
### Read in chloride & TDS duration exceedance datasets ###
TDS_duration<-read_xlsx("TDS_duration_exceedances.xlsx",col_names = T)%>%
  dplyr::mutate(month_hrs = case_when( ### This adds these values in a new column based on month
    str_detect(month,"01")~"744",
    str_detect(month,"02")~"672",
    str_detect(month,"03")~"744",
    str_detect(month,"04")~"720",
    str_detect(month,"05")~"744",
    str_detect(month,"06")~"720",
    str_detect(month,"07")~"744",
    str_detect(month,"08")~"744",
    str_detect(month,"09")~"720",
    str_detect(month,"10")~"744",
    str_detect(month,"11")~"720",
    str_detect(month,"12")~"744",
    TRUE~month),month_hrs = as.numeric(month_hrs),percent_exceeding_hours = round((timehrs/month_hrs)*100))%>%
    complete(month,Station,fill = list('Total Number of Exceedance' = 0, 'Huc14' = 0, 'timehrs' = 0, ### Complete function will fill in missing rows for unrepresented factor levels, you can additionally specify the values to fill with by passing the values in a list
                                       "month_hrs" = 0,"percent_exceeding_hours" = 0))

cl_acute<-read_xlsx("Acute_Chloride_Exceedances.xlsx",col_names = T)
cl_chronic<-read_xlsx("Chronic_Chloride_Exceedances.xlsx",col_names = T)
### Need to take HUC out of HUC number for couple of them to match with discrete data ###
continuous_roadsalt$Huc14<-gsub("HUC","",continuous_roadsalt$Huc14)
### Change the column names ###
colnames(continuous_roadsalt)[colnames(continuous_roadsalt)=="Calculated_TDS"]<-"val"
colnames(continuous_roadsalt)[colnames(continuous_roadsalt)=="Huc14"]<-"HUC14"
### Change character strings for HUC #s in continuous data to match with discrete data ###
continuous_roadsalt$HUC14<-gsub("2030103140010","02030103140010",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2030103140020","02030103140020",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2040301030010","02040301030010",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2030105120050","02030105120050",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2030105110010","02030105110010",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2030105070010","02030105070010",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2030105050080","02030105050080",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2030105100130","02030105100130",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("2040104240020","02040104240020",continuous_roadsalt$HUC14)
continuous_roadsalt$HUC14<-gsub("002040301030010","02040301030010",continuous_roadsalt$HUC14)
###############################################################################
### Read in shapefile to get WMA areas ###
### Change Projection to match leaflet map ###
### drop z dimension and simplify shapefile to render faster ###
WMA_NJ<-st_read(getwd(),layer= "WMAs")%>%
  st_transform(WMA_NJ,crs="+init=epsg:4326")%>%
  st_zm(WMA_NJ, drop = T, what = "ZM")%>%
  ms_simplify(.)
### Need this to polygon won't be plotted this is b/c ms_simplify() adds names to geometry column ###
names(st_geometry(WMA_NJ))= NULL
### Read in HUC14s ###
### Change Projection to match leaflet map ###
### drop z dimension and simplify shapefile to render faster ###
hucs<-st_read(getwd(),layer = "2014_NJ_Integrated_Report_AU")%>%
  st_transform(hucs,crs="+init=epsg:4326")%>%
  st_zm(hucs,drop = T, what = "ZM")%>%
  ms_simplify(.)
### Need this to polygon won't be plotted this is b/c ms_simplify() adds names to geometry column ###
names(st_geometry(hucs)) = NULL
### Change column name and take out "HUC" in string ###
colnames(hucs)[colnames(hucs)=="HUC14TXT"]<-"HUC14"
hucs$HUC14<-gsub("HUC","",hucs$HUC14)
### Vector of impaired HUCs to use in filter ###
impaired_HUCS<-c("02030103010140","02030103140010","02030103140020",
                 "02030103180010","02030105120050","02040301030010")
################################################################################
### Filter just for impaired HUCS ###
discrete_roadsalt<-discrete_roadsalt%>%
  #filter(HUC14 %in% impaired_HUCS)%>%
  filter(charnam == "Total dissolved solids")%>%
  mutate(year= year(stdate))
################################################################################
### theme for plots ####
theme_graphs<- theme_linedraw()+
  theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.text=element_text(size=10, face="bold"),
        plot.subtitle = element_text(size=15, face="bold",vjust=0.5,hjust = 0.5))
#########################################################################################################################
### Creates header with the title for dashboard ###
header<-dashboardHeader(title="NJDEP Water Quality Data Viewer App:
                        Freshwater Streams Impacted by Total Dissolved Solids",titleWidth = 900)
### Creates left sidebar with widgets ###
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Time Series Plot",
            tabName = "home",
              icon = icon("home")),                 
 actionButton("preview","Click to learn more about app",style='padding:4px; font-size:80%'),
 
  radioButtons("button","Choose Which Type of HUC14\n(based on the 2016 303(d) list):",
               choices = c("Impaired","Not Impaired"),
               selected = "Impaired",inline = T),
  #selectInput("huc","Select HUC:",choices = impaired_HUCS),
  uiOutput("huc"),
  sliderInput("alpha","Select Shade of measured\ndata points (blue dots) and SWQS (red line):",min = 0,max = 0.8,value=0.5),
  sliderInput("alpha2","Select shade of estimated TDS based on continuous specific conductance data:",min = 0,max = 0.5,value=0.5),
 
 menuItem("Duration Exceedance Plots",
          tabName = "dur",
          icon = icon("bar-chart-o"))),
          #selectizeInput("station","Select Monitoring Station:",TDS_duration$Station)),
 HTML("<h4>&nbsp; &nbsp; &nbsp; Author: Kevin Zolea </h4>"),
 tags$a(href="https://www.nj.gov/dep/", target="_blank",
        img(width= 100,height = 100,src="https://www.nj.gov/dep/awards/images/deplogoB.jpg",class="road_pics",
            class="NJDEP_pic")))
  #sliderInput("year","Select Year Range",
              #min=2000,
              #max=2018,
              #value = c(2000,2018),
              #sep="",
              #step=1))
################################################################################
### Creates body of app###
body<-dashboardBody(
  tags$head(tags$style(HTML(
    '.NJDEP_pic{
        width: auto;
        height: 100%;
        max-height: 20vh;
        display: block;
        margin-left: auto;
        margin-right: auto;
    }
    '
  ))),
  tabItems(
    tabItem(tabName = "home",
            fluidRow(
              #box(width = 12,plotlyOutput("plot")%>%withSpinner(type = 5, color = "blue"))),
              #downloadButton('download','Download Plot'))),
              box(width = 12,rbokehOutput("plot",width = "100%",height = "800px")%>% withSpinner(type = 5, color = "blue"))),
            
            fluidRow(
              box(width = 12,leafletOutput("wma")%>%withSpinner(type = 5, color = "blue")))),
    tabItem(tabName = "dur",
            pickerInput("station","Select Station:",choices = TDS_duration$Station,
                        options = list(title = "Please choose station")),
            rbokehOutput("plot2",width = "100%",height = "800px")%>%withSpinner(type = 5, color = "blue"),
            rbokehOutput("plot3",width = "100%",height = "800px")%>%withSpinner(type = 5, color = "blue"))))
################################################################################
### Create ui ###
ui<- dashboardPage(header = header,
                   sidebar = sidebar,
                   body = body

)
################################################################################
### Create server of app ###
server<- function(input,output,session){
  
### Creates pop up giving instructions on how to use app...only pops up when action button is clicked ###
################################################################################
  observeEvent(input$preview,{
    showModal(modalDialog(
      title = "Information About App",
      "Water Quality data indicates that there is a long-term trend of increasing concentrations of
     TDS in NJ's freshwater streams.",br(),br(),a("NJDEP's biennial water quality assessment",
                                                   href = "https://www.state.nj.us/dep/wms/bears/assessment.htm",target = "_blank"),
                                                    "found that 26 HUC14 subwatersheds are impaired by TDS concentrations over the",
                                                    a("surface water quality standard",
                                                     href="https://www.state.nj.us/dep/wms/bears/swqs.htm",target = "_blank"),
                                                    "of 500 mg/L. The primary cause is winter application of road salts.",br(),br(),
                                  "Use this app to view a graph of the actual and estimated TDS data from 1997-2018.",br(),
      "The map highlights the location of your selected HUC14. Use the cursor to pan around map. Click to see the HUC14 name and number.+ or scroll up to zoom in 
      - or scroll down to zoom out
       world icon to view the entire state."
    ))
  })
################################################################################  
  ### Creates radio buttons and drop down menu, which is reactive to which button is clicked on radio button menu ###
  output$huc<-renderUI({
    if(input$button == "Impaired"){
      selectInput("huc","Select HUC14:",choices = impaired_HUCS)
    }
    else{
      selectInput("huc","Select HUC14:",choices = c("02030105110010","02030105070010","02030103010070",
                                                  "02030103180010","02030105050080","02040104240020",
                                                  "02030103170010","02040105240010"))
    }
  })
################################################################################  
### Creates a reactive dataframe for both discrete and continuous data ###
  test_df<-reactive({
    discrete_roadsalt%>%
      filter(HUC14 == input$huc)#%>%
      #filter(year >= input$year[1])%>%
      #filter(year <= input$year[2])
  })
  
  
  test_df2<-reactive({
    continuous_roadsalt%>%
      filter(HUC14 == input$huc)
  })
  
  tds_dur_reac<-reactive({
    TDS_duration%>%
      filter(Station == input$station)
  })
################################################################################
  ### Gets line shapes for plots ###
  #my_shapes = c(20,20,NA)
################################################################################   
### Creates interactive graph###
  output$plot<- renderRbokeh({
    req(input$huc)
    figure(xlab = "Year",ylab = "TDS Concentration (mg/L)",
           title = paste("Total Dissolved Solids Concentration(mg/L)\n",
                         "HUC14#:",input$huc,sep = ""),
           legend_location = "top_left",width=1800,height = 800)%>%
      ly_points(test_df(),x = stdate, y= val,alpha = input$alpha,legend = "Discrete",
                hover = c(stdate,val,locid),glyph = 15)%>%
      ly_points(test_df2(),x = stdate, y= val,alpha = input$alpha2,
                legend = "Continuous (estimated)",
                hover = c(stdate,val,Station),glyph = 1)%>%
      ly_abline(h=500,color="red",legend = "Freshwater Aquatic Life Criteria\nfor TDS = 500 mg/L",
                alpha = input$alpha,width=5)%>%
      theme_legend(background_fill_alpha = 0.5)%>%
      theme_title(text_font_style = "bold",text_align = "center")
    
  })
###############################################################################
  ### Interactive plot using plotly package...keeping for reference in future ###
  
  ### Creates time-series plot ###
  #output$plot<-renderPlotly({
  #  req(input$huc)
  #  p<-ggplot(test_df(), aes(x = stdate, y = val)) +
  #    geom_point(aes(colour="Discrete"),size=3,alpha = input$alpha) +
  #    geom_point(data=test_df2(),aes(color = "Continuous (estimated)"),
  #               size=3,alpha = input$alpha2)+
  #    geom_hline(aes(yintercept = 500,color="Freshwater Aquatic Life Criteria\nfor TDS = 500 mg/L"),size=1.3,alpha = input$alpha)+
  #    scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
  #    ggtitle("Total Dissolved Solids Concentration(mg/L)")+
  #    labs(subtitle = paste("HUC14#:",input$huc,sep = ""))+
  #    xlab("Year") + ylab(" TDS Concentration (mg/L)")+
  #    scale_color_manual("",
  #                       values = c("Discrete"="blue","Continuous (estimated)"="#037907",
  #                                  "Freshwater Aquatic Life Criteria\nfor TDS = 500 mg/L"="red"),
  #                       guide = guide_legend(override.aes = list(
  #                         linetype = c("blank", "blank","solid"),
  #                         shape = my_shapes)))+
  #    
  #    theme_graphs
  #  
  #  p<-ggplotly(p,dynamicTicks = "x")%>%
  #    config(collaborate = F,displaylogo=F,modeBarButtonsToRemove = list("lasso2d","hoverClosestCartesian",
  #                                                                       "hoverCompareCartesian","select2d"))
  #  
  #  p<-toWebGL(p)
#
  #  p
  #})
  
  #observeEvent(input$huc,{
    #plotlyProxy("plot",session)%>%
     # plotlyProxyInvoke("relayout")
 # })
################################################################################
### Creates interactive map of WMAs and HUC14s ###
  output$wma<-renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -74.4 ,lat =40, zoom = 7)%>%
      addPolygons(data = WMA_NJ,color = "#1111B8",weight = 2,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.1,fillColor = "white",stroke = T)%>%
      addPolygons(data = hucs,color = "#636060",fillColor = "#00FFFFFF",layerId = ~HUC14,weight = 1,
                  opacity = 0.5, fillOpacity = 0.1,
                  popup = paste("HUC14 Name:\n",hucs$AU_NAME,"<br>",
                                "HUC14#:\n",hucs$HUC14,"<br>",
                                "WMA#:",hucs$WMA,sep = ""),
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      
      addLayersControl(
        baseGroups = c("Grey", "Satellite"),
        options = layersControlOptions(collapsed = FALSE))
  })
################################################################################
### Highlights polygon when huc is clicked on drop down menu ###
  observeEvent(input$huc,{
    
    proxy<- leafletProxy("wma")
    
    click<-input$wma_shape_click
    
    hucsub<-subset(hucs, HUC14 == input$huc)
    
    selected<- hucs[hucs$HUC14 == input$huc,]
    
    bbox<-st_bbox(selected)%>%
      as.vector()
    
    if(nrow(hucsub) == 0){
      proxy %>% removeShape(layerId = "Selected")
    } else if(length(click$id) && input$huc != click$id){
      proxy %>% addPolygons(data = selected,
                            fillColor = "yellow",
                            fillOpacity = 0.67,
                            color = "orange",
                            opacity = 2,
                            weight = 5,
                            stroke = T,
                            layerId = "Selected",
                            popup = paste("HUC14 Name:\n",selected$AU_NAME,"<br>",
                                          "HUC14#:\n",selected$HUC14,"<br>",
                                          "WMA#:",selected$WMA,sep = ""))%>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      
    } else if(!length(click$id)){
      proxy %>% addPolygons(data = selected,
                            fillColor = "yellow",
                            fillOpacity = 0.67,
                            color = "orange",
                            opacity = 2,
                            weight = 5,
                            stroke = T,
                            layerId = "Selected",
                            popup = paste("HUC14 Name:\n",selected$AU_NAME,"<br>",
                                          "HUC14#:\n",selected$HUC14,"<br>",
                                          "WMA#:",selected$WMA,sep = ""))%>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])}
    
  })
################################################################################
### Allows user to have map zoomed in when impaired HUC is clicked ###
  #observe({
    #click <- input$wma_shape_click
    #if(is.null(click))
    #  return()
    #else
     # leafletProxy("wma")%>%
     # setView(lng = click$lng , lat = click$lat, zoom=10)
 # })
  
  #output$download<- downloadHandler(
    #filename = function(){
      #paste("Plot",".png",sep = "")
   # },
   # content = function(file){
    #  ggsave(file,width =11.5 , height = 8)
   # }
 # )
################################################################################   
### Make reactive dataframe for duration exceedance plot based on station dropdown ###
  TDS_duration_react<-reactive({
    TDS_duration%>%
      dplyr::filter(Station == input$station)
  })
  
################################################################################ 
### Make Length of exceedance plots under the duration exceedance plots tab ###
  output$plot2<-renderRbokeh({
    req(input$station)
    figure(xlab = "month",ylab = "% Of Time Exceeding Per Hours in Month",
           title = paste0("% Of Time Exceeding Per Hours in Month of Calculated TDS Exceedances > 500 mg/L\n(Based on Specific Conductance Measurements)\n","Station: ",input$station,sep = ""),
           width = 1800, height = 800)%>%
      ly_bar(x=month, y=percent_exceeding_hours,data=TDS_duration_react())%>%
      y_axis(number_formatter = "numeral")

  })
  
### Make plot of total # of hours of exceedance under the duration exceedance plots tab ###
  output$plot3<-renderRbokeh({
    req(input$station)
    figure(xlab = "month",ylab = "Number of Hours with Calculated TDS concentration >500 (mg/L)",
           title = paste0("Number of Hours with Calculated TDS concentration >500 (mg/L)(Based on Continuous Specific Conductance) Station: ",
                          input$station,sep = ""),width = 1800, height = 800)%>%
      ly_bar(x=month, y=timehrs,data=TDS_duration_react())
    
  })

}
################################################################################
### runs app ###
shinyApp(ui,server)

