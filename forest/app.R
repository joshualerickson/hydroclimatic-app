
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leafpop)
library(leafem)
library(splines)
library(DT)
library(kableExtra)
library(scales)
library(latex2exp)
library(raster)
library(feather)
library(ggrepel)
library(googleCloudStorageR)
library(Hmisc)
library(httr)
library(jsonify)
library(extRemes)
library(streamstats)
library(geojsonsf)
library(jsonlite)
library(AOI)
library(sf)
library(tidyverse)


gcs_auth("gcs_auth_file_shiny.json")
##### Data Prepping #####

#### Snotel data ####

inland_northwest_snotel_min_max_year <- read_feather("inland_northwest_snotel_min_max_year.csv")

inland_northwest_snotel_min_max_month <- read_feather("inland_northwest_snotel_min_max_month.csv")

inland_northwest_snotel_min_max_year_month <- read_feather("inland_northwest_snotel_min_max_year_month.csv")

inland_northwest_snotel_site_all <- read_feather("inland_northwest_snotel_site_all.csv")

phenology <- read_feather("phenology.csv")

mapping_snotel <- gcs_get_object("inland_northwest_hourly.csv", bucket = "joshualerickson")

mapping_snotel <- mapping_snotel %>% group_by(site_name) %>% mutate(snow_depth = impute(snow_depth))  %>% ungroup()

mapping_snotel_today <- mapping_snotel %>% group_by(site_name) %>% arrange(desc(date)) %>% slice(n = 1) %>% ungroup()

##### USGS Data #####

usgs_min_max_wy <- read_feather("usgs_min_max_wy.csv")

usgs_min_max_wy_month <- read_feather("usgs_min_max_wy_month.csv")

usgs_min_max_month <- read_feather("usgs_min_max_month.csv")

usgs_full_months <- read_feather("usgs_full_months.csv")

usgs_hourly <- gcs_get_object("usgs_hourly.csv", bucket = "joshualerickson")

usgs_stats <- gcs_get_object("usgs_stats.csv", bucket = "joshualerickson")

usgs_stats <- usgs_stats %>% filter(!history %in% c("historic readings"))

flood_freq <- read_feather("flood_freq.csv")

flood_freq_norm <- read_feather("flood_freq_norm.csv")

grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
         "USGS Shaded Relief", "Hydrography")
att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

map <- leaflet("Map") 
map <- leaflet::addWMSTiles(map, GetURL("USGSTopo"),
                            group = grp[1], attribution = att, layers = "0")
map <- leaflet::addWMSTiles(map, GetURL("USGSImageryOnly"),
                            group = grp[2], attribution = att, layers = "0")
map <- leaflet::addWMSTiles(map, GetURL("USGSImageryTopo"),
                            group = grp[3], attribution = att, layers = "0")
map <- leaflet::addWMSTiles(map, GetURL("USGSShadedReliefOnly"),
                            group = grp[4], attribution = att, layers = "0")

opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                            group = grp[5], options = opt, layers = "0")
map <- leaflet::hideGroup(map, grp[5])
opt <- leaflet::layersControlOptions(collapsed = TRUE)
map <- leaflet::addLayersControl(map, baseGroups = grp[1:4],
                                 overlayGroups = grp[5], options = opt)


#### NOAA ####

# UI pre-settings ---------------------------------------------------------


# * header -----
header = dashboardHeader(title = "Hydroclimatic Data")

# * sidebar ----
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Welcome",
      tabName = "get_started",
      icon = icon("door-open")
    ),
    menuItem(
      "Snotel",
      tabName = "swe_graphs",
      icon = icon("snowflake"),
      menuSubItem("Snotel", tabName = "swe_graphs", icon = icon("chart-line"))
      #menuSubItem("Snotel Phenology", tabName = "swe_phen", icon = icon("chart-line"))
    ),
    menuItem(
      "USGS",
      tabName = "usgs_graphs",
      icon = icon("tint"),
      menuSubItem("USGS-Raw", tabName = "usgs_graphs", icon = icon("chart-line")),
      menuSubItem("USGS-Normalized", tabName = "usgs_norm", icon = icon("chart-line")),
      menuSubItem("Stream Stats", tabName = "stream_stats", icon = icon("chart-line"))
    ),
    menuItem(
      "NWS",
      tabName = "nws_graphs",
      icon = icon("cloud-sun-rain"),
      menuSubItem("NWS", tabName = "nws_graphs", icon = icon("chart-line"))
    )
    
  ))

# * body ----
body = dashboardBody(
  # * * -- Welcome ----
  tabItems(
    tabItem(
      tabName = "get_started",
      tags$style(type = 'text/css', '#welcome {height: calc(100vh - 80px) !important;}'),
      p("Thank you for visting this site provided by the USDA - Forest Service. There is a lot going on so please use this as a roadmap to the rest of the app.
        ")
      ),
    
    
    # * * -- Snotel - Raw ----
    tabItem(
      tabName = "swe_graphs",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart",
               tabPanel("Map", style = "height:92vh;",
                        column(7,leafletOutput("swe_maps")), 
                               column(5,plotOutput("swe_ggplot"))),
               
               tabPanel("Month", plotOutput("swe_month", brush = "swe_month_brush"),dataTableOutput("swe_month_dt"),
                        pickerInput("Site", "Please select a site:", 
                                    choices = paste0(unique(inland_northwest_snotel_min_max_year_month$site_name)),
                                    multiple = TRUE, selected = "Stahl Peak ", options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE)),
                        pickerInput("Month", "and a Month", options = list(`actions-box` = TRUE), multiple = TRUE,
                                    choices= paste0(unique(inland_northwest_snotel_min_max_year_month$month_abb)), selected = "Jan"),
                        pickerInput("swe_metric", "Pick a metric", selected = "Mean",  options = list(`actions-box` = TRUE), 
                                    choices = c("Maximum", "Minimum", "Mean", "Median")),
                        dateRangeInput("Date_swe", "and a Date Range", start = "1996-01-01", end = Sys.Date(), format = "yyyy-mm-dd", separator = "-"),
                        downloadButton(outputId = "downLoadFilter_swe_month",
                                       label = "Download Filtered Data")),
               
               tabPanel("Minima - Maxima", plotOutput("swe_minmax")),
               
               tabPanel("Histogram", plotOutput("swe_hist"),
                        selectInput("maxmin", "Dististribution of ?", choices = c("Maximum", "Minimum", "Mean","Median")),
                        numericInput("n", "Enter Value for Binwidth", 5),
                        actionButton("binwidth_swe", label = "Submit Binwidth")),
               
               tabPanel(title = "Summary Stats", value = "swe_stats",DT::dataTableOutput("swe_stats"),
                        actionButton("action1", label = "Summarize"),
                        pickerInput("Report_swe", "Group by?", c("Year", "Month", "Year & Month", "Site (All)"), selected = "year"),
                        downloadButton(outputId = "downLoadFilter_swe",
                                       label = "Download Filtered Data")),
               
               tabPanel("Frequency", numericInput("t", "Enter Year (T), e.g. for 20 year event; T = 20", 20), 
                        numericInput("m", "Enter Mean of Site", 20), 
                        numericInput("sd", "Enter SD of Site", 5), 
                        selectInput("normal_swe", "Normal or Lognormal?", c("Normal", "Lognormal")),
                        actionButton("calculate", label = "Calculate"),
                        verbatimTextOutput("freq_swe_text"))
               ))),
    # # * * -- Snotel - Phen ----
    # tabItem(
    #   tabName = "swe_phen",
    #   tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
    #   fluidRow(
    #     tabBox(width = 12, id = "tabchart",
    #            tabPanel("DOY", plotOutput("phen_month"),
    #                     pickerInput("Site_phen", "Please select a site:", 
    #                                 choices = paste0(unique(phenology$site_name)),
    #                                 multiple = TRUE, selected = "Stahl Peak", options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE)),
    #                     pickerInput("Month_phen", "and Metric", options = list(`actions-box` = TRUE), 
    #                                 choices= paste0(c("First Snow Accumulation", "Last Snow Accumulation", "Continuous Snow Accumulation")), 
    #                                 selected = "Last Snow Accumulation")),
    #            tabPanel("Histogram", plotOutput("phen_hist"),pickerInput("phen", "Metric", options = list(`actions-box` = TRUE), 
    #                                                                        choices= paste0(c("First Snow Accumulation", "Last Snow Accumulation", "Continuous Snow Accumulation")), 
    #                                                                        selected = "Last Snow Accumulation"),
    #                     numericInput("n_phen", "Enter Value for Binwidth", 5),
    #                     actionButton("binwidth_phen", label = "Submit Binwidth"))))),
    
    # * * -- USGS-Raw ----
    tabItem(
      tabName = "usgs_graphs",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart_usgs",
               tabPanel("Map", style = "height:92vh;",
                        column(7,leafletOutput("usgs_maps")),
               column(5,plotOutput("usgs_ggplot")), dataTableOutput("usgs_map_table")),
               
               tabPanel("Month", plotOutput("usgs_month", brush = "usgs_month_brush"), dataTableOutput("usgs_month_dt"), 
                        pickerInput("Site_usgs", "Please select a site(s):",  options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE),
                                    choices = paste0(unique(usgs_min_max_wy_month$Station)),
                                    multiple = TRUE, selected = "Tobacco River near Eureka, MT"),
                        pickerInput("usgs_metric", "Pick a metric", selected = "Maximum",  options = list(`actions-box` = TRUE), 
                                    choices = c("Maximum", "Minimum", "Mean", "Median")),
                        pickerInput("Month_usgs", "and a Month", multiple = TRUE, selected = "Jan",  options = list(`actions-box` = TRUE), 
                                    choices= paste0(unique(usgs_min_max_wy_month$month_abb))),
                        dateRangeInput("Date", "and a Date Range", start = "1900-01-01", end = Sys.Date(), format = "yyyy-mm-dd", separator = "-"),
                        downloadButton(outputId = "downLoadFilter_usgs",
                                       label = "Download Filtered Data")),
               
               #  tabPanel("Classic", plotOutput("usgs_classic"),
               # #          dateRangeInput("Date", "", start = "2010-01-01", end = Sys.Date(), format = "yyyy-mm-dd", separator = "-"),
               #          # pickerInput("Month_usgs_clas", "and a Month", multiple = TRUE, selected = "Jan",  options = list(`actions-box` = TRUE),
               #          #             choices= paste0(unique(usgs_min_max_wy_month$month_abb))),
               #          actionButton("dates", label = "Date Range")),
               
               tabPanel("Minima - Maxima", plotOutput("max_usgs")),
               
               tabPanel("Histogram", plotOutput("usgs_hist"),
                        selectInput("maxmin2", "Dististribution of ?", choices = c("Maximum", "Minimum", "Mean","Median")),
                        numericInput("n2", "Enter Value for Binwidth", 50),
                        actionButton("binwidth", label = "Submit Binwidth")),
               
               tabPanel(title = "Summary Stats", dataTableOutput("usgs_stats2"), 
                        pickerInput("Report", "Group by?", c("wy", "month", "wy & month"), selected = "wy"),
                        downloadButton(outputId = "downLoadFilter",
                                       label = "Download Filtered Data")),
               
               tabPanel("Flood Frequency",
                        plotOutput("usgs_freq", brush = "user_brush"), dataTableOutput("flood_dt")),
               
               tabPanel(title = "Flood Freq Groups", plotOutput("freq_g"),
                        pickerInput("dist1", "Method",choices = c("LogPearson", "GEV", "Gumbel"), selected = "GEV",
                                    options = list(`actions-box` = TRUE)),
                        pickerInput("drain", "Method",choices = paste(unique(flood_freq$drainage_area_cut)), selected = "0-50",
                                    options = list(`actions-box` = TRUE)),
               pickerInput("stream_stats", "Stream Stats",choices = c(names(flood_freq[,-c(1:5)])), selected = "LC01DEV",
                           options = list(`actions-box` = TRUE)),box(paste("<b>SLOP30_30M = Percent area with slopes greater than 30 percent from 30-meter DEM.</b> \n
               LC01DEV = Percentage of land-use from NLCD 2001 classes 21-24. \n
                            FOREST = Percentage of area covered by forest. \n
                             
                            ")))
               
               
               ))),
    
    # * * -- USGS-norm ----
    tabItem(
      tabName = "usgs_norm",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart_usgs_norm",
               tabPanel("Month", plotOutput("usgs_month_norm"),
                        pickerInput("Site_norm", "Please select a site:",  options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE), 
                                    choices = paste0(unique(usgs_min_max_wy$Station)),
                                    multiple = TRUE, selected = "Tobacco River near Eureka, MT"),
                        pickerInput("norm_metric", "Pick a normalization metric", selected = "Maximum",  options = list(`actions-box` = TRUE), 
                                    choices = c("Maximum", "Minimum", "Mean", "Median")),
                        pickerInput("norm_method", "and a method", selected = "Drainage Area",  options = list(`actions-box` = TRUE), 
                                    choices = c("Drainage Area", "Standard Deviation", "Average All Time"))
                        ),
               
               tabPanel("Exploring Categories", plotOutput("usgs_cat_norm")),
               
               tabPanel("Flood Frequency",
                        plotOutput("usgs_freq_norm")),
               
               tabPanel(title = "Flood Freq Groups", plotOutput("freq_group"),
                        pickerInput("dist", "site",choices = c("GEV", "LogPearson", "Gumbel"), selected = "GEV",
                                    options = list(`actions-box` = TRUE)))))),
    # * * -- USGS-Stream Stats ----

    tabItem(
      tabName = "stream_stats",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(
        width = 12, id = "tabchart_ss",
        tabPanel("Map", style = "height:92vh;",
                 column(6,leafletOutput("ss_maps")), column(6, leafletOutput("ss_poly")),
                 dataTableOutput("ss_table") 
        ),
        tabPanel("Peak Plot", style = "height:92vh;", 
                 plotOutput("ss_peak"), 
                 dataTableOutput("ss_peak_table"),
                 textInput("wkID", "Workspace ID"), pickerInput("state", "States", choices = state.abb, multiple = F),
                 actionButton("peak", label = "Peak Flow"))
        ))),
    
    # * * -- NWS ----
    
    tabItem(
      tabName = "nws_graphs",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(
          width = 12, id = "tabchart_nws",
          
          tabPanel("Map", style = "height:92vh;",
                   column(7,leafletOutput("nws_maps")),
                   dataTableOutput("nws_table")
                   ),
          
          tabPanel("Forecast", 
                   numericInput("lat", "Latitude", 48.91167), 
                   numericInput("long", "Longitude", -114.90246),
                   actionButton("Forecast", label = "Forecast"),dataTableOutput("nws_forecast_table"))
        )
      )
    )
    
    ) #all tab items
) #closes body


# UI ----------------------------------------------------------------------

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  
  
  # Welcome -----------------------------------------------------------------
  
  
  # Graphs ------------------------------------------------------------------
  
  
  # * 1  Snotel ----
  
  
  output$swe_maps <- renderLeaflet({
    
    labs <- as.list(mapping_snotel_today$site_name)
    
    map %>% addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
      addCircles( data = mapping_snotel_today, lng = mapping_snotel_today$longitude, lat = mapping_snotel_today$latitude,weight = 10,
                 fillOpacity = 0.9, popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                   "<b>Site Name: </b>",str_to_title(mapping_snotel_today$site_name),
                                                   "<br>", "<b>Site Description: </b>", mapping_snotel_today$description,
                                                   "<br>", "<b>Site #: </b>", mapping_snotel_today$site_id,
                                                   "<br>", "<b>Current Reading (SWE): </b>", round(mapping_snotel_today$snow_water_equivalent,2), " in",
                                                   "<br>", "<b>Current Reading (Snow Depth): </b>", round(mapping_snotel_today$snow_depth,2), " in",
                                                   "<br>", "<b>Current Reading (Precip. Cumulative): </b>", round(mapping_snotel_today$precipitation_cumulative,2), " in",
                                                   "<br>", "<b>Elevation: </b>", comma(mapping_snotel_today$elev*3.28084,1), " ft",
                                                   "<br>", "<b>Observation Range: </b>", paste(mapping_snotel_today$start, " to ", mapping_snotel_today$end),"</p>"),
                 label = lapply(labs, HTML),layerId = ~unique(mapping_snotel_today$site_name))
    
  })
  
  swe_ggplot_data <- reactive({
    site <- input$swe_maps_shape_click$id
    mapping_snotel %>% filter(site_name %in% site)
  })

  output$swe_ggplot <- renderPlot({
    ggplot(data = swe_ggplot_data(), aes(date, snow_depth)) + geom_line() + geom_smooth(se = F) + theme_light() +
      labs(title = paste("Last 7 days of hourly Snow Depth: ", input$swe_maps_shape_click$id ), x = "Date", y = "Snow Depth (in)")

  })

  
  
  # ** 1.1 -- Months ----

  ym <- reactive({inland_northwest_snotel_min_max_year_month %>% filter(site_name %in% input$Site, month_abb %in% input$Month, year_month %in% input$Date_swe[1] : input$Date_swe[2])})
  
  output$swe_month = renderCachedPlot({
    
    
    shiny::validate(
      need(input$Site, 'Please select at least one site'),
      need(input$Month, 'Please choose at least one Month'))
  
    
    if (input$swe_metric == "Mean") {
    ggplot(ym(), aes(year_month, Mean)) + 
                     geom_line(size = 1,aes(color = site_name)) + 
                     geom_point(size = 1) + geom_smooth(alpha = 0.2,aes(color = site_name)) + 
                     labs(title = paste0(input$Site, " Mean SWE for ", input$Month), y = "Mean SWE per Month", x = "Year", color = "Site Name") +  
      theme_light() +theme(text = element_text(size=20))
      
    } else if (input$swe_metric == "Maximum") {
      ggplot(ym(), aes(year_month, Maximum)) + 
        geom_line(size = 1,aes(color = site_name)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.2,aes(color = site_name)) + 
        labs(title = paste0(input$Site, " Maximum SWE for ", input$Month), y = "Maximum SWE per Month", x = "Year", color = "Site Name") +  
        theme_light() +theme(text = element_text(size=20))
      
    }    else if (input$swe_metric == "Minimum") {
      ggplot(ym(), aes(year_month, Minimum)) + 
        geom_line(size = 1,aes(color = site_name)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.2,aes(color = site_name)) + 
        labs(title = paste0(input$Site, " Minimum SWE for ", input$Month), y = "Minimum SWE per Month", x = "Year", color = "Site Name") +  
        theme_light() +theme(text = element_text(size=20))
      
    }    else if (input$swe_metric == "Median") {
      ggplot(ym(), aes(year_month, Median)) + 
        geom_line(size = 1,aes(color = site_name)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.2,aes(color = site_name)) + 
        labs(title = paste0(input$Site, " Median SWE for ", input$Month), y = "Median SWE per Month", x = "Year", color = "Site Name") +  
        theme_light() +theme(text = element_text(size=20))
      
    } else {"SOL"}
  },
  cacheKeyExpr = { list(input$Site, input$Month,input$swe_metric, ym()) })
  
  swe_month_table <- reactive({
    
    swe_month_brush <- input$swe_month_brush
    sel_swe <- brushedPoints(ym() %>% select(site_name, month_abb, 3:7, input$swe_metric, year_month), swe_month_brush)
    return(sel_swe)
    
    
    
  })
  
  output$swe_month_dt <- DT::renderDataTable(DT::datatable(swe_month_table()))
  
  output$downLoadFilter_swe_month <- downloadHandler(
    filename = function() {
      paste('filtered_data_',ym()$site_name, Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(ym(),file)
    }
  )

  # ** 1.2 -- Min - Max ----
  
    site_minmax <- reactive({inland_northwest_snotel_min_max_year_month %>%  filter(site_name %in% input$Site, month_abb %in% input$Month, year_month %in% input$Date_swe[1] : input$Date_swe[2])%>% 
        pivot_longer(cols = c("Maximum", "Minimum"), names_to = "Metric")  })
 
  output$swe_minmax = renderCachedPlot({
     shiny::validate(
    need(input$Site, 'Please select at least one site')
  )
    ggplot(site_minmax(), aes(year_month, value, color = site_name, group = site_name)) + 
                     geom_line()  + 
                     geom_smooth(alpha = 0.2, se = F) +
                   labs(title = paste0(input$Site2, " Daily Discharge max-min per year"), x = "Year", y = "Min-Max of Site Daily SWE")+
                   theme_light() + facet_wrap(~Metric, scales = "free_y") + theme(text = element_text(size=20))

  },
  cacheKeyExpr = { list(input$Site, input$Month, input$Date_swe, site_minmax()) })
  
  
  
  # ** 1.3 -- Histogram ----
  
  
  

  
  observeEvent(input$binwidth_swe, ignoreInit = TRUE, {
      hist_react <- reactive({
    
    inland_northwest_snotel_min_max_year  %>% filter(site_name %in% input$Site)
    
  })
    output$swe_hist = renderCachedPlot({
      ggplot(hist_react(),aes_string(input$maxmin)) + 
                       geom_histogram(aes(fill = site_name), binwidth = isolate(input$n), alpha = .5) + 
                       geom_density(adjust = .5  ,aes(y = isolate(input$n) * ..count.., color = site_name), size = 1)  +
                       labs(xlab = "Snow Water Equivalent (mm)",
                            title = paste0(input$Site, " histogram of Annual", input$maxmin, " SWE"))+ 
                       guides(color = guide_legend(override.aes = list(alpha = 1))) + theme_light()+theme(text = element_text(size=20))
    },
    cacheKeyExpr = { list(input$binwidth_swe, input$maxmin, hist_react()) })
  })
  
  
  
  
  
  
  
  # ** 1.4 -- Summary Stats ----
  
  summary_stats1 <- reactive({
    if(input$Report_swe == "Year") { 
      
      inland_northwest_snotel_min_max_year %>% 
        filter(site_name %in% input$Site) %>% group_by(site_name, year)
      
    } else if (input$Report_swe == "Month") {
      
      inland_northwest_snotel_min_max_month %>% filter(site_name %in% input$Site)
      
    } else if (input$Report_swe == "Year & Month") {
      
      inland_northwest_snotel_min_max_year_month %>% 
        filter(site_name %in% input$Site)
      
    } else if (input$Report_swe == "Site (All)") {
      
      inland_northwest_snotel_site_all %>% 
        filter(site_name %in% input$Site)
      
    } else { "No Data"}
    
  })
  observeEvent(input$action1, ignoreInit = TRUE, {
    output$swe_stats = DT::renderDataTable({summary_stats1() %>% DT::datatable(filter = 'top', options = list(autoWidth = TRUE,list(className = 'dt-center'), scrollX = T))})
  })
  
  output$downLoadFilter_swe <- downloadHandler(
    filename = function() {
      paste('filtered_data_',summary_stats1()$site_name, Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(summary_stats1(),file)
    }
  )
  
  
  
  # ** 1.5 -- Frequency ----
  
  frequency_swe <- reactive({
    
    if (input$normal_swe == "Normal") {
      
      K_n <- qnorm(1-(1/input$t))
      
      paste(round(input$m + K_n*input$sd,2), " in. for", input$t, "year SWE")
      
    } else if (input$normal_swe == "Lognormal") {
      
      log_m <- 0.5*log(((input$m)^2)/(((input$sd/input$m)^2)+1))
      
      log_sd <- sqrt(log(((input$sd/input$m)^2)+1))
      
      K_n <- qnorm(1-(1/input$t))
      
      paste(round(exp(log_m + K_n*log_sd),2), " in. for", input$t, "year SWE")
      
    } else {"SOL"}
    
  })
  
  observeEvent(input$calculate, ignoreInit = TRUE, {
    output$freq_swe_text = renderText({frequency_swe()})
  })
  
  # output$swe_freq_plot <- renderPlot({inland_northwest_snotel_min_max_year %>% filter(site_name %in% input$Site) %>% 
  #     as.data.frame() %>% fevd(Maximum,., time.units = "years", type = "GEV", units = "SWE (in)") %>% plot()})
  
  # * 2  USGS-Raw ----
  # 
  
  output$usgs_maps <- renderLeaflet({
    
    labs_usgs <- as.list(usgs_stats$Station)
    
    tbl_usgs <- usgs_stats %>% select(Station,p25_va, p50_va, p75_va) %>% 
      group_by(Station) %>% nest() %>%
      mutate(table = map(data, ~kable(.) %>% kable_styling(.,bootstrap_options = c("striped", "hover", "condensed", "responsive"))))%>%pull(table)
    
   
    
    map %>%
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
      addCircles(data = usgs_stats, lng = usgs_stats$long, lat = usgs_stats$lat, weight = 10,opacity = 0.9,
                 popup = paste0(
                   "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",usgs_stats$Station,
                   "<br>", "<b>Current Reading: </b>", paste(ifelse(usgs_stats$fl_val == "Good", comma(round(usgs_stats$value,0),1), "Error")), " <sup>ft<sup>3</sup></sup>&frasl;<sub>s</sub>",
                   "<br>", "<b>Drainage Area: </b>", paste(comma(round(usgs_stats$drainage_area,0),1), " sq.mi"),
                   "<br>","<b>Longitude: </b>",usgs_stats$long,
                   "<br>","<b>Latitude: </b>",usgs_stats$lat,
                   "<br>", "<b>Quantiles for </b>",paste("<b>", "'", usgs_stats$month_day, "'", " :", "</b>"),tbl_usgs ,"</p>"), 
                 label = lapply(labs_usgs, HTML),layerId = ~unique(usgs_stats$Station)) %>% addMeasure()
  })
  
  usgs_ggplot_data <- reactive({
    site <- input$usgs_maps_shape_click$id
    usgs_hourly %>% filter(Station %in% site)
  })
  
  usgs_ggplot_data2 <- reactive({
    site <- input$usgs_maps_shape_click$id
    usgs_stats %>% select(Station,13:27) %>% filter(Station %in% site) %>% pivot_longer(cols = -Station, names_to = "Stats")

  })
  
  output$usgs_ggplot <- renderPlot({
    ggplot(data = usgs_ggplot_data(), aes(date, value, color = fl_val)) + geom_line() + geom_smooth(se = F) + theme_light() + 
      labs(title = paste("Last 7 days of hourly Discharge: ", input$usgs_maps_shape_click$id ), x = "Date", y = "Discharge (cfs)", color = "Flagged")+theme(text = element_text(size=11))
    
  })
  
  
  output$usgs_map_table <- renderDataTable({ DT::datatable(usgs_ggplot_data2(), options = list(pageLength = 25))})
  
  
# ** 3.1 -- Months  ----
  
    
    year_month2 <- reactive({ usgs_min_max_wy_month %>% 
        filter(Station %in% input$Site_usgs, month_abb %in% input$Month_usgs, year_month %in% input$Date[1] : input$Date[2]) })
  output$usgs_month = renderCachedPlot({
    shiny::validate(
      need(input$Site_usgs, 'Please select at least one site'),
      need(input$usgs_metric, 'Please select at least one site'),
      need(input$Month_usgs, 'Please select at least one Month')
    )
    
    if (input$usgs_metric == "Maximum")  {
      
      ggplot(year_month2(), aes(year_month, Maximum)) + 
        geom_line(size = 1, aes(color = Station)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
        labs(title = paste0(input$Site_usgs, " Monthly Maximum Discharge (cfs) for ", input$Month_usgs), 
             y = "Maximum Discharge (cfs) per Month", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
      
    } else if (input$usgs_metric == "Minimum") {
  
      ggplot(year_month2(), aes(year_month, Minimum)) + 
        geom_line(size = 1, aes(color = Station)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
        labs(title = paste0(input$Site_usgs, " Monthly Minimum Discharge (cfs) for ", input$Month_usgs), 
             y = "Minimum Discharge (cfs) per Month", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
    } else if (input$usgs_metric == "Mean") {
      
      ggplot(year_month2(), aes(year_month, Mean)) + 
        geom_line(size = 1, aes(color = Station)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
        labs(title = paste0(input$Site_usgs, " Monthly Mean Discharge (cfs) for ", input$Month_usgs), 
             y = "Mean Discharge (cfs) per Month", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
      
    } else if (input$usgs_metric == "Median") {
      
      ggplot(year_month2(), aes(year_month, Median)) + 
        geom_line(size = 1, aes(color = Station)) + 
        geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
        labs(title = paste0(input$Site_usgs, " Monthly Median Discharge (cfs) for ", input$Month_usgs), 
             y = "Median Discharge (cfs) per Month", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
    } else {"SOL"}
   },
  cacheKeyExpr = { list(input$Site_usgs,input$usgs_metric,input$Date, input$Month_usgs,year_month2())})
  
  usgs_month_table <- reactive({
    
    usgs_month_brush <- input$usgs_month_brush
    sel_usgs <- brushedPoints(year_month2() %>% select(Station, wy,month_abb, 3:7, input$usgs_metric, year_month), usgs_month_brush)
    return(sel_usgs)
    
    
    
  })
  
  output$usgs_month_dt <- DT::renderDataTable(DT::datatable(usgs_month_table()))
  
  output$downLoadFilter_usgs <- downloadHandler(
    filename = function() {
      paste('filtered_data_',year_month2()$Station, Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(year_month2(),file)
    }
  )
  
  # ** 3.2 -- Classic----
  # observeEvent(input$dates, {
  # output$usgs_classic = renderCachedPlot({
  #   
  #   classic_rec <- reactive({usgs_min_max_wy_month %>% filter(Station == input$Site_usgs, month_abb %in% input$Month_usgs, year_month %in% input$Date[1] : input$Date[2])})
  #   
  #   
  #   if (input$usgs_metric == "Maximum")  {
  #   ggplot(classic_rec(), aes(year_month, Maximum)) + 
  #     geom_line(size = 1, aes(color = Station)) + 
  #     geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
  #     labs(title = paste0(input$Site_usgs, " Maximum Discharge (cfs) from ", input$Date[1], " to ", input$Date[2]), 
  #          y = "Maximum Discharge (cfs)", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
  #     
  #   } else if (input$usgs_metric == "Minimum") {
  #     
  #     
  #     ggplot(classic_rec(), aes(year_month, Minimum)) + 
  #       geom_line(size = 1, aes(color = Station)) + 
  #       geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
  #       labs(title = paste0(input$Site_usgs, " Minimum Discharge (cfs) from ", input$Date[1], " to ", input$Date[2]), 
  #            y = "Minimum Discharge (cfs)", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
  #     
  #   }else if (input$usgs_metric == "Mean") {
  #     
  #     
  #     ggplot(classic_rec(), aes(year_month, Mean)) + 
  #       geom_line(size = 1, aes(color = Station)) + 
  #       geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
  #       labs(title = paste0(input$Site_usgs, " Mean Discharge (cfs) from ", input$Date[1], " to ", input$Date[2]), 
  #            y = "Mean Discharge (cfs)", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
  #     
  #   }else if (input$usgs_metric == "Median") {
  #     
  #     
  #     ggplot(classic_rec(), aes(year_month, Median)) + 
  #       geom_line(size = 1, aes(color = Station)) + 
  #       geom_point(size = 1) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
  #       labs(title = paste0(input$Site_usgs, " Median Discharge (cfs) from ", input$Date[1], " to ", input$Date[2]), 
  #            y = "Median Discharge (cfs)", x = "Water Year")+ theme_light()+theme(text = element_text(size=20))
  #   } else {"SOL"}
  # },
  # cacheKeyExpr = {list(input$Site_usgs,input$Month_usgs, input$Date, input$usgs_metric)})
  # 
  # })
  

  
  # ** 3.3 -- Min - Max ----
    site_max <- reactive({usgs_min_max_wy_month %>% filter(Station %in% input$Site_usgs, month_abb %in% input$Month_usgs, year_month %in% input$Date[1] : input$Date[2]) %>% 
    pivot_longer(cols = c("Maximum", "Minimum"), names_to = "Metric")})
  output$max_usgs = renderCachedPlot({
    shiny::validate(
      need(input$Site_usgs, 'Please select at least one site')
    )
    
    ggplot(site_max(), aes(year_month, value, group = Station, color = Station)) + geom_line() + geom_smooth(alpha = 0.2) +
      labs(title = paste0(input$Site_usgs, " ", input$Month_usgs, " Discharge max-min per year"), x = "Water Year", y = "Min-Max of Station Daily Discharge")+ 
      theme_light() + facet_wrap(~Metric, scales = "free_y")+theme(text = element_text(size=20))
    
  },
  cacheKeyExpr = { list(input$Site_usgs,input$Month_usgs, input$Date, site_max()) })

  
  
  
  # ** 3.4 -- Histogram ----
  
   hist_react2 <- reactive({
    usgs_min_max_wy_month %>% 
      filter(Station %in% input$Site_usgs) })
  
  observeEvent(input$binwidth, {
    
   
 
     
    output$usgs_hist = renderCachedPlot({
      ggplot(hist_react2(),aes_string(input$maxmin2)) + 
                       geom_histogram(aes(fill = Station, text = wy), binwidth = isolate(input$n2), alpha = .5) + 
                       geom_density(adjust = .5  ,aes(y = isolate(input$n2) * ..count.., color = Station),size = 1)  +
                       labs(xlab = "Discharge (cfs)",
                            title = paste0(input$Site_usgs, " histogram of Annual ", input$maxmin2, " Discharge "))+ 
                       theme_light() + guides(color = guide_legend(override.aes = list(alpha = 1)))+theme(text = element_text(size=20))
    }, cacheKeyExpr =  {list(input$maxmin2, input$binwidth, hist_react2())} )
   }) 
 
  
  # ** 3.5 -- Summary Stats ----
  
  summary_stats <- reactive({
    if(input$Report == "wy") { 
      
      usgs_min_max_wy %>% select(Station, wy, Maximum, Minimum, Mean, Median, Standard_Deviation, Drainage_area) %>% 
        filter(Station %in% input$Site_usgs)
      
    } else if (input$Report == "month") {
      
      usgs_min_max_month %>% select(-month_abb) %>%  filter(Station %in% input$Site_usgs)
      
    } else if (input$Report == "wy & month") {
      
      usgs_min_max_wy_month %>% select(-month_abb, -year_month) %>% 
        filter(Station %in% input$Site_usgs)
      
    }  else { "No Data"}
    
  })

    
    output$usgs_stats2 = renderDataTable({summary_stats() %>% datatable(filter = list(position = 'top')) })
    
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('filtered_data_',summary_stats()$Station, Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(summary_stats(),file)
    }
  ) 
  # ** 3.6 -- Frequency ----
  
  freq <- reactive({flood_freq %>% 
        filter(Station %in% input$Site_usgs)})
  output$usgs_freq = renderCachedPlot({
    shiny::validate(
      need(input$Site_usgs, 'Please select at least one site')
    )
    
    ggplot(freq(), aes(x= ReturnInterval, y = Flow, color = Distribution )) + 
      geom_point(alpha = 0.8, size = 2.5) + geom_line() +
      #geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light()+theme(text = element_text(size=15)) + facet_wrap(~Station, scales = "free")
  },
  cacheKeyExpr = { list(input$Site_usgs, freq()) })
  
  
  flood_table <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(flood_freq[,1:4], user_brush)
    return(sel)
    
    
    
  })
  
  output$flood_dt <- DT::renderDataTable(DT::datatable(flood_table()))
  
  # ** 3.7 Freq groups ----
  observeEvent(input$stream_stats, {
  freq_group <- reactive({flood_freq %>% filter(Distribution %in% input$dist1, drainage_area_cut %in% input$drain)})
  freq_station <- reactive(freq_group() %>% group_by(Station) %>% filter(ReturnInterval == 100) %>% slice(n = 1))
  
  output$freq_g = renderCachedPlot({
    
    if (input$stream_stats == "LC01DEV") {
    
    ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = LC01DEV))  + 
      geom_point(alpha = 0.8) + 
      geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "MINBELEV") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = MINBELEV))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "LC01DEV") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = LC01DEV))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "FOREST") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = FOREST))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "PRECIP") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = PRECIP))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  + theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "RELIEF") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = RELIEF))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "NFSL30_30M") {
    
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = NFSL30_30M))  + 
      geom_point(alpha = 0.8) + 
      geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
    
    } else if (input$stream_stats == "ELEVMAX") {
    
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = ELEVMAX))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "BSLDEM30M") {
    
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = BSLDEM30M))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "ELEV") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = ELEV))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "SLOP30_30M") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = SLOP30_30M))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free")  +theme(strip.text = element_text(size=25,color = "black"),text = element_text(size=20)) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else {"SOL"}
    
  },
  cacheKeyExpr = { list(input$dist1,input$stream_stats, freq_group()) })
  
  })
  
  
  
  
  
  
  
  
  # * 4 USGS-Norm ----
  
  
  
  # ** 4.1 -- Months ----
  
    year_month_norm <- reactive({usgs_full_months %>% filter(Station %in% input$Site_norm)})
  
  output$usgs_month_norm = renderCachedPlot({
    
    if (input$norm_metric == "Maximum") {
    
    if (input$norm_method == "Drainage Area") {
      
      ggplot(year_month_norm(), aes(wy, Max_dnorm, color = Station)) + 
        geom_line() + 
        geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light() + labs(x = "Water Year", y = "log(Annual Maximum Flow) / log(Drainage Area)")
      
    } else if (input$norm_method == "Standard Deviation") {
      
      ggplot(year_month_norm(), aes(wy, Max_sdnorm, color = Station)) + 
        geom_line() + 
        geom_point() + geom_smooth(alpha = 0.2, se = FALSE)   + theme_light() + labs(x = "Water Year", y = "log(Annual Maximum Flow) / log(Annual Standard Deviation)")
      
      
    }  else if (input$norm_method == "Average All Time") {
      
      ggplot(year_month_norm(), aes(wy, Max_avg, color = Station)) + 
        geom_line() + 
        geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Maximum Flow) / log(All time Mean Flow)")
      
    } else {
      
      "SOL"
      } 
      
      }  else if (input$norm_metric == "Minimum") {
      
      
      if (input$norm_method == "Drainage Area") {
        
        ggplot(year_month_norm(), aes(wy, Min_dnorm, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Minimum Flow) / log(Drainage Area)")
        
      } else if (input$norm_method == "Standard Deviation") {
        
        ggplot(year_month_norm(), aes(wy, Min_sdnorm, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE)   + theme_light() + labs(x = "Water Year", y = "log(Annual Minimum Flow) / log(Annual Standard Deviation)")
        
      } else if (input$norm_method == "Average All Time") {
        
        ggplot(year_month_norm(), aes(wy, Min_avg, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Minimum Flow) / log(All time mean flow)")
        
      } else {
        "SOL"
        }
      
    } else if (input$norm_metric == "Mean") {
      
      
      if (input$norm_method == "Drainage Area") {
        
        ggplot(year_month_norm(), aes(wy, Mean_dnorm, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Mean Flow) / log(Drainage Area)")
        
      } else if (input$norm_method == "Standard Deviation") {
        
        ggplot(year_month_norm(), aes(wy, Mean_sdnorm, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE)   + theme_light() + labs(x = "Water Year", y = "log(Annual Mean Flow) / log(Annual Standard Deviation)")
        
        
      } else if (input$norm_method == "Average All Time") {
        
        ggplot(year_month_norm(), aes(wy, Mean_avg, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Mean Flow) / log(All time mean flow)")
        
      } else {
        "SOL"
        }
      
    } else if (input$norm_metric == "Median") {
      
      
      if (input$norm_method == "Drainage Area") {
        
        ggplot(year_month_norm(), aes(wy, Med_dnorm, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Median Flow) / log(Drainage Area)")
        
      } else if (input$norm_method == "Standard Deviation") {
        
        ggplot(year_month_norm(), aes(wy, Med_sdnorm, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE)   + theme_light() + labs(x = "Water Year", y = "log(Annual Median Flow) / log(Annual Standard Deviation)")
        
      } else if (input$norm_method == "Average All Time") {
        
        ggplot(year_month_norm(), aes(wy, Med_avg, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Median Flow) / log(All time mean flow)")
        
      } else {
        "SOL"
        }
      
    } else {
      "SOL"
      }
    }, cacheKeyExpr = {list(input$norm_metric, input$norm_method,  year_month_norm()) })
  
  
  # ** 4.3 -- Categories----
  
  output$usgs_cat_norm = renderCachedPlot({
 
    
    ggplot(usgs_min_max_wy, aes(log_drainage, Max_dnorm, color = drainage_area_cut)) + 
                     geom_point()   + 
                     labs() + 
                     theme_light()  
    
    
  }
  ,
  cacheKeyExpr = {} )
  
  
  
  # ** 4.4 -- Discharge Variability ----
  
  output$egret_sd = renderCachedPlot({
    
    plotSDLogQ(egret_final[[input$egret2]])
  }, cacheKeyExpr = input$egret2)
  
  
  
  
  
  
  # ** 4.5 -- Frequency ----
  
  freq_norm <- reactive({flood_freq_norm %>% 
      filter(Station %in% input$Site_norm)})
  output$usgs_freq_norm = renderCachedPlot({
    
    ggplot(freq_norm(), aes(x= ReturnInterval, y = Flow, color = Distribution)) + 
      geom_point(alpha = 0.8, size = 2.5) + 
      geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light() + facet_wrap(~input$Site_norm, scales = "free")
  },
  cacheKeyExpr = { list(input$Site_norm, freq()) })
  
  # ** 4.6 normalized groups ----
  
  freq_g <- reactive({flood_freq_norm %>% filter(Distribution %in% input$dist)})
  
  output$freq_group = renderCachedPlot({
    
    ggplot(freq_g(), aes(ReturnInterval, Flow, group = Station, color = Station))  + 
      geom_point(alpha = 0.8) + 
      geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light() + facet_wrap(~drainage_area_cut, scales = "free") + theme(legend.position = "none", strip.text = element_text(size=9,color = "black"))
    
  },
  cacheKeyExpr = { list(input$dist, freq_g()) })
  
  
  
  # * 5 USGS stream stats ----
  
 
  
  output$ss_maps <- renderLeaflet({
    

      map %>% 
        addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
        setView(lat = 48.91167, lng = -114.90246, zoom = 10) 
      
   
    
    
     })
  
  observeEvent(input$ss_maps_click, {
    
    
      

    click <- input$ss_maps_click
    clat <- click$lat
    clng <- click$lng
    content <- paste(
      "<b>Lat: ",round(clat, 5),"</b>",
      "<br>",
      "<b>Long: ", round(clng,5), "</b>"    )
    map %>% 
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
      addCircles(lng=clng, lat=clat) %>%
      addPopups(lng = clng, lat = clat, content)


     ss_stats <- reactive({

    state <-   geocode_rev(c(clat,clng)) %>% select(state)
    state <-  state.abb[grep(paste(state$state), state.name)]

    df1 <- streamstats::delineateWatershed(clng,clat, rcode = state, crs = 4326)

   

     df_poly <- df1 %>%
        writeGeoJSON(., file.path(tempdir(),"ss_tmp.json")) %>%
        geojson_sf() %>% st_as_sf() %>% mutate(ID = state, long = clng, lat = clat)

      wkID <- df1$workspaceID
      
      incProgress(detail = paste("Computing Basin Characteristics"))
     
      stats <- streamstats::computeChars(workspaceID = wkID, rcode = state)
      

      stats <- stats$parameters %>% mutate(ID = state, workspaceID = wkID)
      
      flow_stats <- stats %>% select(ID, code, value) %>%
        pivot_wider(names_from = "code")
      
      df_poly <- df_poly %>% select(Shape_Area, ID, lat, long) %>% left_join(flow_stats, by = "ID")
      
     # incProgress(detail = paste("Computing Peak Flow Stats"))
# 
#       base_url <- paste0(
#         "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
#         wkID,
#         "&includeflowtypes=true"
#       )
# 
# 
#       # try to download the data
#      error <- httr::GET(url = base_url,
#                          httr::write_disk(path = file.path(tempdir(),
#                                                            "peak_tmp.json"),overwrite = TRUE))
# 
#      if (httr::http_error(error)) {
#        warning(sprintf("Downloading site %s failed, removed empty file."))
#      } else {
#       peak <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))
# 
#       peak <- (peak$RegressionRegions[[1]])[[6]][[1]] %>% select(c(2, 5, 7, 8)) %>% mutate(ReturnInterval = parse_number(Name))}

      list(stats = stats, df_poly = df_poly, state = state, wkID = wkID)})
     

    
    
    
    output$ss_table = renderDataTable({DT::datatable(ss_stats()$stats, options = list(pageLength = 25))})
    
    
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Delineating Watershed"))
        
        ss_stats()
        
        
        
        
        
      }
      )
      
    output$ss_poly <- renderLeaflet({
    
    
    map %>% 
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
        addCircleMarkers(data = ss_stats()$df_poly, lng = ss_stats()$df_poly$long, lat = ss_stats()$df_poly$lat, layerId = ~paste("Drain Point"), color = "red") %>% 
        addPolygons(data = ss_stats()$df_poly,popup = paste0(
          "<p style=line-height:30px;margin:0px;>",
          "<b>Drainage Area: </b>", paste(ss_stats()$df_poly$CONTDA, " sq.mi"),
          "<br>","<b>Temperature: </b>",ss_stats()$df_poly$TEMP,
          "<br>","<b>Precipitation: </b>",ss_stats()$df_poly$PRECIP,
          "<br>","<b>Max Elevation: </b>",ss_stats()$df_poly$ELEVMAX,
          "<br>","<b>Forest (per): </b>",ss_stats()$df_poly$FOREST,
          "<br>","<b>Slope abv 30% (per): </b>",ss_stats()$df_poly$SLOP30_30M,
          "<br>","<b>Slope abv 50% (per): </b>",ss_stats()$df_poly$SLOP50_30M)) 
  })
     })
    
 #   * *  5.1 Plot ----
    
    observeEvent(input$peak, {
      
    state <- input$state
    wkID <- input$wkID
    
      ss_peak <- reactive({
      base_url <- paste0(
        "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
        wkID,
        "&includeflowtypes=true"
      )
      
      
      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "peak_tmp.json"),overwrite = TRUE))
      
      # if (httr::http_error(error)) {
      #   warning(sprintf("Downloading site %s failed, removed empty file."))
      # } else {
        peak <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))
        
        peak <- (peak$RegressionRegions[[1]])[[6]][[1]] %>% select(c(2, 5, 7, 8)) %>% mutate(ReturnInterval = parse_number(Name))
      })
    
    
    output$ss_peak <- renderPlot({
      ggplot(ss_peak(), aes(ReturnInterval, Value)) + geom_point() + 
        geom_line() +geom_ribbon(aes(ymin=IntervalBounds$Lower, ymax=IntervalBounds$Upper), linetype=1, alpha=0.3)+ 
        theme_light() + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Predicted Flood Frequency")
    })
    
    output$ss_peak_table = renderDataTable({DT::datatable(ss_peak(), options = list(pageLength = 25))})
    })
    

  
 
  
  

  
  
 # * 6 NWS ----
  
  output$nws_maps <- renderLeaflet({
    
    
    map %>%
      setView(lat = 48.91167, lng = -114.90246, zoom = 10) 
    
  })
  
  observeEvent(input$nws_maps_click, {
    
    click <- input$nws_maps_click
    clat <- click$lat
    clng <- click$lng
    content <- paste(
                     "<b>Lat: ",round(clat, 5),"</b>",
                     "<br>",
                     "<b>Long: ", round(clng,5), "</b>"    )
    map %>% 
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
      setView(lat = 48.91167, lng = -114.90246, zoom = 10)  %>% 
      addPopups(lng = clng, lat = clat, content)
    
    nws_forecast <- reactive({
      df <- httr::GET(url = paste0(
        "https://api.weather.gov/points/",
        
        click$lat, ",",
        
        click$lng))
      
      
      df <- from_json(df$url)
      
      city <- df$properties$relativeLocation$properties$city
      state <- df$properties$relativeLocation$properties$state
      location <- paste(city, state, sep = ", ")
      
      df <- df$properties$forecast
      
      df <- httr::GET(url = paste0(df))
      df <- from_json(df$url)
      
      df <- df$properties$periods
      df <- df %>% mutate(temp = str_c(temperature, temperatureUnit, sep = "-"), wind = str_c(windSpeed, windDirection, sep = "-"), location = location,
                          Date = lubridate::as_date(endTime))
      df <- df %>% select(name,Date, temp, wind, shortForecast, location) })
    
    
    withProgress(
      
      message = 'Forecast in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Couple Seconds..."))
        
        nws_forecast()
        
        
        
        
        
      }
    )
    
      output$nws_table = renderDataTable({DT::datatable(nws_forecast(), options = list(pageLength = 25))})
  })

  
  # usgs_ggplot_data <- reactive({
  #   site <- input$usgs_maps_shape_click$id
  #   usgs_hourly %>% filter(Station %in% site)
  # })
  # 
  # usgs_ggplot_data2 <- reactive({
  #   site <- input$usgs_maps_shape_click$id
  #   usgs_stats %>% select(Station,13:27) %>% filter(Station %in% site)
  # })
  # 
  # output$usgs_ggplot <- renderPlot({
  #   ggplot(data = usgs_ggplot_data(), aes(date, value)) + geom_line() + geom_smooth(se = F) + theme_light() + 
  #     labs(title = paste("Last 7 days of hourly Discharge: ", input$usgs_maps_shape_click$id ), x = "Date", y = "Discharge (cfs)")
  #   
  # })
  

# ** 6.1 -- Forecast ----
  

    # try to download the data  
  
nws_forecast <- reactive({
df <- httr::GET(url = paste0(
    "https://api.weather.gov/points/",

    input$lat, ",",

    input$long))


df <- from_json(df$url)

city <- df$properties$relativeLocation$properties$city
state <- df$properties$relativeLocation$properties$state
location <- paste(city, state, sep = ", ")

df <- df$properties$forecast/hourly

df <- httr::GET(url = paste0(df))
df <- from_json(df$url)

df <- df$properties$periods
df <- df %>% mutate(temp = str_c(temperature, temperatureUnit, sep = "-"), wind = str_c(windSpeed, windDirection, sep = "-"), location = location,
                    startTime = as_date(startTime), endTime = as_date(endTime),
)
df <- df %>% select(name,startTime, endTime, temp, wind, shortForecast, location) })
  

observeEvent(input$Forecast, ignoreInit = TRUE, {
  output$nws_forecast_table = renderDataTable({nws_forecast()})
})
}






  









##### RUN APPLICATION #####
shinyApp(ui = ui, server = server)

