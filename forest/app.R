
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leafpop)
library(leafem)
library(mapview)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(splines)
library(DT)
library(kableExtra)
library(htmltools)
library(scales)
library(latex2exp)
library(feather)
library(ggrepel)
library(googleCloudStorageR)
library(Hmisc)
library(httr)
library(leaflet.extras)
library(streamstats)
library(geojsonsf)
library(jsonlite)
library(AOI)
library(sf)
library(nhdplusTools)
library(tidyverse)
library(shinydisconnect)
library(shinyalert)
library(lwgeom)
library(units)

if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }

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

#usgs tiles

grp <- c("USGS Topo", "USGS Imagery Only", "USGS Imagery Topo",
         "USGS Shaded Relief", "Hydrography")
att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")
GetURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}

map <- leaflet() 
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
    ),
    menuItem(
      "USGS",
      tabName = "usgs_graphs",
      icon = icon("tint"),
      menuSubItem("USGS-Raw", tabName = "usgs_graphs", icon = icon("chart-line")),
      menuSubItem("USGS-Normalized", tabName = "usgs_norm", icon = icon("chart-line")),
      menuSubItem("Stream Stats", tabName = "stream_stats", icon = icon("chart-line")),
      menuSubItem("NLDI", tabName = "NLDI", icon = icon("chart-line"))
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
      tags$style(type = 'text/css', '#welcome {height: calc(100vh - 80px) !important;}'),htmltools::tags$iframe(src = "welcome.html", width = '100%',  height = 1000,  style = "border:none;")
      ),
    
    
    # * * -- Snotel - Raw ----
    tabItem(
      tabName = "swe_graphs",
      tags$style(type = 'text/css', '#swe_maps {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart",
               tabPanel("Map", style = "height:92vh;",
                        column(7,leafletOutput("swe_maps")), 
                               column(5,plotOutput("swe_ggplot"))),
               
               tabPanel("Month",tags$style(type = 'text/css', '#swe_month {height: calc(100vh - 250px) !important;}'), plotOutput("swe_month", brush = "swe_month_brush"),dataTableOutput("swe_month_dt"),
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
               
               tabPanel("Minima - Maxima",tags$style(type = 'text/css', '#swe_minmax {height: calc(100vh - 250px) !important;}'), plotOutput("swe_minmax")),
               
               tabPanel("Histogram",tags$style(type = 'text/css', '#swe_hist {height: calc(100vh - 250px) !important;}'), plotOutput("swe_hist"),
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
 
    # * * -- USGS-Raw ----
    tabItem(
      tabName = "usgs_graphs",
      tags$style(type = 'text/css', '#usgs_maps {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart_usgs",
               tabPanel("Map", style = "height:92vh;",
                        column(6,leafletOutput("usgs_maps")),
               column(6,imageOutput("usgs_ggplot")), dataTableOutput("usgs_map_table")),
               
               tabPanel("Month",tags$style(type = 'text/css', '#usgs_month {height: calc(100vh - 250px) !important;}'), plotOutput("usgs_month", brush = "usgs_month_brush"), dataTableOutput("usgs_month_dt"), 
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
               
               tabPanel("Minima - Maxima",tags$style(type = 'text/css', '#max_usgs {height: calc(100vh - 250px) !important;}'), plotOutput("max_usgs")),
               
               tabPanel("Histogram",tags$style(type = 'text/css', '#usgs_hist {height: calc(100vh - 250px) !important;}'), plotOutput("usgs_hist"),
                        selectInput("maxmin2", "Dististribution of ?", choices = c("Maximum", "Minimum", "Mean","Median")),
                        numericInput("n2", "Enter Value for Binwidth", 50),
                        actionButton("binwidth", label = "Submit Binwidth")),
               
               tabPanel(title = "Summary Stats", dataTableOutput("usgs_stats2"), 
                        pickerInput("Report", "Group by?", c("wy", "month", "wy & month"), selected = "wy"),
                        downloadButton(outputId = "downLoadFilter",
                                       label = "Download Filtered Data")),
               
               tabPanel("Flood Frequency",tags$style(type = 'text/css', '#usgs_freq {height: calc(100vh - 250px) !important;}'),
                        plotOutput("usgs_freq", brush = "user_brush"), dataTableOutput("flood_dt")),
               
               tabPanel(title = "Flood Freq Groups", plotOutput("freq_g"),
                        pickerInput("dist1", "Method",choices = c("LogPearson", "GEV", "Gumbel"), selected = "GEV",
                                    options = list(`actions-box` = TRUE)),
                        pickerInput("drain", "Method",choices = paste(unique(flood_freq$drainage_area_cut)), selected = "0-50",
                                    options = list(`actions-box` = TRUE)),
               pickerInput("stream_stats", "Stream Stats",choices = c(names(flood_freq[,-c(1:5)])), selected = "LC01DEV",
                           options = list(`actions-box` = TRUE)))))),
    
    # * * -- USGS-norm ----
    tabItem(
      tabName = "usgs_norm",
      tags$style(type = 'text/css', '#usgs_month_norm {height: calc(100vh - 250px) !important;}'),
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
               
               tabPanel("Exploring Categories",tags$style(type = 'text/css', '#usgs_cat_norm {height: calc(100vh - 250px) !important;}'), plotly::plotlyOutput("usgs_cat_norm")),
               
               tabPanel("Flood Frequency",tags$style(type = 'text/css', '#usgs_freq_norm {height: calc(100vh - 250px) !important;}'),
                        plotOutput("usgs_freq_norm")),
               
               tabPanel(title = "Flood Freq Groups",tags$style(type = 'text/css', '#freq_group {height: calc(100vh - 250px) !important;}'), plotOutput("freq_group"),
                        pickerInput("dist", "site",choices = c("GEV", "LogPearson", "Gumbel"), selected = "GEV",
                                    options = list(`actions-box` = TRUE)))))),
    # * * -- USGS-Stream Stats ----

    tabItem(
      tabName = "stream_stats",tags$style(type = 'text/css', '#ss_maps {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(
        width = 12, id = "tabchart_ss",
        tabPanel("Map", style = "height:92vh;",tags$style(type = 'text/css',
      '#ss_maps {cursor: crosshair !important;}'),
                 useShinyalert(),leafletOutput("ss_maps"),
                 dataTableOutput("ss_table")
        ),
        tabPanel("Peak Plot", style = "height:92vh;", 
                 useShinyalert(),
                 plotOutput("ss_peak"), 
                 dataTableOutput("ss_peak_table"),
                 box(textInput("wkID", "Workspace ID"), pickerInput("state", "States", choices = state.abb, multiple = F, selected = "MT"),
                 actionButton("peak", label = "Peak Flow"))),
        
        tabPanel("Culvert Size", style = "height:92vh",tags$style(type = 'text/css', '#culvert_plot {height: calc(100vh - 250px) !important;}'),
                 column(6,box(selectInput("use_reg", "Do you want to use known bankfull widths?", choices = c("Yes", "No")),
                 numericInput("drain_area", "Enter Drainage Area (sq.mi)", 2),
                 numericInput("precip_drain", "Enter Precipitation (in)", 20),
                 numericInput("for_known", "Enter Percent Forested", 95),
                 numericInput("bf_known", "Enter Bankfull Width", 5),
                 numericInput("acw_known", "Enter Active Channel Width", 5),
                 numericInput("geo_known", "Enter Geographic Factor", 1),
                 actionButton("calculate_culvert", label = "Calculate"))),
                column(6,plotOutput("culvert_plot")), dataTableOutput("culvert_table")),
        tabPanel("Report", style = "height:92vh",
                 textInput("author", "Enter Author."),
                 textInput("drain_name", "Enter a Name or ID for the drain point location."),
                 radioButtons('format', 'Document format', c('HTML', 'Word'),
                              inline = TRUE),
                 downloadButton("report_culvert", "Generate report"))
        
        ))),
    
    # * * -- NLDI ----
    
    tabItem(
      tabName = "NLDI", tags$style(type = 'text/css', '#nldi_maps {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(
          width = 12, id = "nldi_explore",
          tabPanel("Map", style = "height:92vh;",tags$style(type = 'text/css',
                                                            '#nldi_maps {cursor: crosshair !important;}'),
                   leafletOutput("nldi_maps"), dataTableOutput("nldi_table")),
          tabPanel("Catchments",tags$style(type = 'text/css',
                                           '#nldi_catch {height: calc(100vh - 250px) !important;}'),
                   leafletOutput("nldi_catch"), selectInput(inputId = "cat", label = "Select a variable:", 
                                                            choices = c("Recharge", "Actual Evapotranspiration (AET)",
                                                                        "Potential Evapotranspiration (PET)", "Precipitation (PRISM)",
                                                                        "Base Flow Index (BFI)", "Topographic Wetness Index (TWI)"), 
                                                            selected = "Precipitation (PRISM)"), 
                   actionButton("nldi_start", "Visualize Catchments"))
        ))),
    
    # * * -- NWS ----
    
    tabItem(
      tabName = "nws_graphs",tags$style(type = 'text/css', '#nws_maps {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(
          width = 12, id = "tabchart_nws",
          
          tabPanel("MRMS-QPM", style = "height:92vh;",
                   useShinyalert(),disconnectMessage(
                     text = "An error occurred. Please refresh the page and try again.",
                     refresh = "Refresh",
                     background = "#FFFFFF",
                     colour = "#444444",
                     refreshColour = "#337AB7",
                     overlayColour = "#000000",
                     overlayOpacity = 0.6,
                     width = 450,
                     top = 50,
                     size = 22,
                     css = ""
                   ),
                   leafletOutput("nws_maps"),
                   dataTableOutput("nws_table")
                   ),
          
          tabPanel("GOES West",tags$style(type = 'text/css', '#goes_west {height: calc(100vh - 250px) !important;}'),leafletOutput("goes_west")),
          
          tabPanel("NEXRAD",tags$style(type = 'text/css', '#nexrad_map {height: calc(100vh - 250px) !important;}'),leafletOutput("nexrad_map"))
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
                   labs(title = paste0(input$Site2, " Daily Discharge max-min per year"), x = "Year", y = "Min-Max of Site Daily SWE", color = "Site Name")+
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
    
    tbl_usgs <- usgs_stats %>% mutate(Min = paste0(min_va, " ('", min_va_yr, "')"),
                                       Max = paste0(max_va, " ('",max_va_yr, "')")) %>% select(Station, Min , `25%`= p25_va, `50%`= p50_va, `75%`= p75_va, Max) %>% 
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
                 label = lapply(labs_usgs, HTML), layerId = ~unique(usgs_stats$Station)) %>% addMeasure()
  })
  observeEvent(input$usgs_maps_shape_click, { 
  usgs_ggplot_data <- reactive({
    site <- input$usgs_maps_shape_click$id
    site_no <- usgs_stats %>% filter(Station %in% site) %>% select(site_no)
    
  })
  
  usgs_ggplot_data2 <- reactive({
    site <- input$usgs_maps_shape_click$id
    usgs_stats %>% select(Station,13:27) %>% filter(Station %in% site) %>% pivot_longer(cols = -Station, names_to = "Stats")

  })
  
  output$usgs_ggplot <- renderImage({

    real_time_usgs <- paste0("https://labs.waterdata.usgs.gov/api/graph-images/monitoring-location/", paste(usgs_ggplot_data()),"/?parameterCode=00060&width=1200&title=true")
    error <- httr::GET(url = real_time_usgs,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "usgs_real_time_tmp.png"),overwrite = TRUE))
    outfile <- file.path(tempdir(),"usgs_real_time_tmp.png")
  
    list(src = outfile,
         alt = "This is alternate text",
         width = 700,
         height = 500)
  
    # ggplot(data = usgs_ggplot_data(), aes(date, value, color = fl_val)) + geom_line() + geom_smooth(se = F) + theme_light() +
    #   labs(title = paste("Last 7 days of hourly Discharge: ", input$usgs_maps_shape_click$id ), x = "Date", y = "Discharge (cfs)", color = "Flagged")+theme(text = element_text(size=11))
    #
  }, deleteFile = TRUE)
  
  
  output$usgs_map_table <- renderDataTable({ DT::datatable(usgs_ggplot_data2(), options = list(pageLength = 25))})
  })
  
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
    sel_usgs <- brushedPoints(year_month2() %>% select(Station, wy,month_abb, 3:7,`Precip Sum` = ppt, input$usgs_metric, year_month), usgs_month_brush)
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
  
  output$usgs_cat_norm = plotly::renderPlotly({
 
    norm_filt <- reactive({ usgs_min_max_wy_month %>% filter(wy > 1983, wy <2020) %>% filter(Station %in% input$Site_norm) %>% mutate(sd_flow = sd(flow_sum), sd_ppt = sd(ppt), Flow = flow_sum/sd_flow, Precip = ppt/sd_ppt) %>% 
      pivot_longer(cols = c("Flow", "Precip"), names_to = "Metric")})
    
  
      print(plotly::ggplotly(ggplot(norm_filt(),aes( year_month, value, color = Metric)) + geom_point() + geom_line() + 
                               labs(title = "Normalized Precip and Flow per basin", x = "Water Year", y = "Standard Value (sum/sd)")))
    
    
  })
  
  
  
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
      setView(lat = 48.91167, lng = -114.90246, zoom = 4)
      
   
    
    
     })
  

    
  observeEvent(input$ss_maps_click, {

     click <- input$ss_maps_click
    clat <- click$lat
    clng <- click$lng
    content <- paste(
      "<b>Lat: ",round(clat, 5),"</b>",
      "<br>",
      "<b>Long: ", round(clng,5), "</b>"    )

    
    leafletProxy("ss_maps") %>% 
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
      addCircles(lng=clng, lat=clat) %>%
      addPopups(lng = clng, lat = clat, content)

ss_stats <- reactive({
  
  state <-   geocode_rev(c(clat,clng)) %>% dplyr::select(state)
       state <-  state.abb[grep(paste(state$state), state.name)]
      
    df1 <- streamstats::delineateWatershed(clng,clat, rcode = state, crs = 4326)
    
     df_poly <- df1 %>%
        streamstats::writeGeoJSON(., file.path(tempdir(),"ss_tmp.json"))
 

df_poly <- df_poly %>% geojsonsf::geojson_sf() %>% 
  sf::st_as_sf() %>% dplyr::mutate(ID = state, long = clng, lat = clat)
     
  

      wkID <- df1$workspaceID
      
      incProgress(detail = paste("Computing Basin Characteristics"))
     
      stats <- streamstats::computeChars(workspaceID = wkID, rcode = state)
      
      

      stats <- stats$parameters %>% mutate(ID = state, workspaceID = wkID)
      
      flow_stats <- stats %>% dplyr::select(ID, code, value) %>%
        pivot_wider(names_from = "code")
      
      df_poly <- df_poly %>% dplyr::select(Shape_Area, ID, lat, long) %>% left_join(flow_stats, by = "ID")
      
      write_rds(stats, file.path(tempdir(), "stats"), compress = "none")
      #st_write(df_poly, dsn = file.path(tempdir()), layer = "df_poly", driver = "ESRI Shapefile", delete_layer = TRUE)
      
      list(stats = stats, df_poly = df_poly, state = state, wkID = wkID)
     
     })

    
    
    
    output$ss_table = renderDataTable({DT::datatable(ss_stats()$stats, options = list(pageLength = 25))})
    
    
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/3, detail = paste("Delineating Watershed"))
        
        ss_stats()
        
        
        
      }
    )
    
        
      map_leaf <- reactive({
        
        map %>% 
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
        addCircleMarkers(data = ss_stats()$df_poly, lng = ss_stats()$df_poly$long, lat = ss_stats()$df_poly$lat, layerId = ~paste("Drain Point"), color = "red") %>% 
        addPolygons(data = ss_stats()$df_poly,popup = paste0(
          "<p style=line-height:30px;margin:0px;>",
          "<b>Drainage Area: </b>", paste(ss_stats()$df_poly$CONTDA, " sq.mi"),
          "<br>","<b>Precipitation: </b>",ss_stats()$df_poly$PRECIP,
          "<br>","<b>Forest (per): </b>",ss_stats()$df_poly$FOREST,
          "<br>","<b>Temperature: </b>",ss_stats()$df_poly$TEMP,
          "<br>","<b>Max Elevation: </b>",ss_stats()$df_poly$ELEVMAX,
          "<br>","<b>Slope abv 30% (per): </b>",ss_stats()$df_poly$SLOP30_30M,
          "<br>","<b>Slope abv 50% (per): </b>",ss_stats()$df_poly$SLOP50_30M), group = "poly") %>% addLayersControl(baseGroups = grp[1:4], overlayGroups = "poly")})
 
    output$ss_maps <- renderLeaflet({

      map_leaf()
  })
      
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Generating Map Image"))
        
       
          mapshot(x = map_leaf()
                 , file = file.path(tempdir(), "customleaf.png")
        )
        
        
        
        
      }
    )
    
   
      
        
  })

 #   * *  5.1 Plot ----
  
  
    observeEvent(input$peak, {
      
    wkID <- input$wkID
    
    state <- input$state
    
    
    
    
    
      ss_peak_ <- reactive({
      base_url <- paste0(
        "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
        wkID,
        "&includeflowtypes=true"
      )
      
      
      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "peak_tmp.json"),overwrite = TRUE))
  
     
        peak <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))
        
        peak <- (peak$RegressionRegions[[1]])[[6]][[1]] %>%  mutate(ReturnInterval = parse_number(Name)) 
        
        # param <- (peak$RegressionRegions[[1]])[[5]][[1]] 
        # 
        # param <- param %>% select(Name, Code, Value)
        
      write_rds(peak, path = file.path(tempdir(), "culvert_usgs"), compress = "none")
      
      list(peak = peak)
      })
      
      withProgress(
        
        message = 'Calculation in progress',
        detail = 'This may take about a minute...', value = 0,{
          
          
          setProgress(1/2, detail = paste("Calculating Peak Flow"))
          
          ss_peak_()
          })
    
    output$ss_peak <- renderPlot({ 
      
    if (length(ss_peak_()$peak) > 7) {
      
    ggplot(ss_peak_()$peak , aes(ReturnInterval, Value)) + geom_point() + 
        geom_line() + geom_ribbon(aes(ymin=IntervalBounds$Lower, ymax=IntervalBounds$Upper), linetype=1, alpha=0.3)+ 
        theme_light() + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "USGS RRE")+theme(text = element_text(size=20))
    } else {
    
      ggplot(ss_peak_()$peak , aes(ReturnInterval, Value)) + geom_point(size = 2) + 
        geom_line(size = 1.5) + # geom_ribbon(aes(ymin=IntervalBounds$Lower, ymax=IntervalBounds$Upper), linetype=1, alpha=0.3)+ 
        theme_light() + 
        annotate("text", x = 200, y = max(ss_peak_()$peak$Value*0.35), label = "One or more of the parameters \n is outside the suggested range. \n Estimates were extrapolated with unknown errors.", size = 9, color = "red") +
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "USGS RRE")+theme(text = element_text(size=20))
      
    }
    })
    
    output$ss_peak_table = renderDataTable({
      
      if (length(ss_peak_()$peak) > 7) {
      DT::datatable(ss_peak_()$peak %>% select(Name,ReturnInterval,Description,IntervalBounds, Value), options = list(pageLength = 25))
      
      } else {
        
        
        DT::datatable(ss_peak_()$peak %>% select(Name,ReturnInterval,Description, Value), options = list(pageLength = 25))
        
      }
      
      })
    })
  

  
  
  # ** 5.2 Culvert sizing ----
  
  observeEvent(input$peak,{
    
      precipUpdate <- read_rds(path = file.path(tempdir(), "stats")) %>% 
        filter(str_detect(description, "Mean Annual Precip"))
      
      daUpdate <- read_rds(path = file.path(tempdir(), "stats")) %>% 
        filter(str_detect(description, "Area that contributes flow to a point on a stream"))
      
      
      forUpdate <- read_rds(path = file.path(tempdir(), "stats")) %>% 
        filter(str_detect(description, "Percentage of area covered by forest"))
      
  # This will change the value of input$inText, based on x
  updateTextInput(session, "precip_drain", value = precipUpdate$value)
  updateTextInput(session, "drain_area", value = daUpdate$value)
  updateTextInput(session, "for_known", value = forUpdate$value)
  })

  
  
  observeEvent(input$calculate_culvert, {
  culvert <- reactive({


    drain_area <- isolate(input$drain_area)

    precip_drain <- isolate(input$precip_drain)

    bf_known <- isolate(input$bf_known)

    bd_known <- isolate(input$bd_known)

    acw_known <- isolate(input$acw_known)

    geo_known <- isolate(input$geo_known)

    for_known <- isolate(input$for_known)



    bf_regres <- if(precip_drain < 30) {
      3.99*drain_area^0.441
    } else if (precip_drain > 45) {

      7.7*drain_area^0.441
    } else {

      6.04*drain_area^0.441
    }
    



    if (isolate(input$use_reg) == "Yes") {
    Omang_parrett_hull_flows <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                           basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                                          0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                                          0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                                          0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                                           bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_known^1.14),
                                                              0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_known^1.02),
                                                              0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_known^1.01),
                                                              0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_known^1)),
                                           source = c("Omang, Parrett and Hull"))

    parrett_and_johnson <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                       basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                                      8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                                      13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                                      18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                                       bankfull_width = c(0.281*(bf_known^1.98),
                                                          1.75*(bf_known^1.72),
                                                          2.34*(bf_known^1.69),
                                                          2.99*(bf_known^1.66)),
                                       active_width = c(1.11*(acw_known^1.74),
                                                        5.81*(acw_known^1.51),
                                                        7.61*(acw_known^1.48),
                                                        9.57*(acw_known^1.45)),
                                       source = c("Parrett & Johnson"))
    } else {

      Omang_parrett_hull_flows <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                             basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                                            0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                                            0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                                            0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                                             bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_regres^1.14),
                                                                0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_regres^1.02),
                                                                0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_regres^1.01),
                                                                0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_regres^1)),
                                             source = c("Omang, Parrett and Hull"))

      parrett_and_johnson <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                         basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                                        8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                                        13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                                        18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                                         bankfull_width = c(0.281*(bf_regres^1.98),
                                                            1.75*(bf_regres^1.72),
                                                            2.34*(bf_regres^1.69),
                                                            2.99*(bf_regres^1.66)),
                                         active_width = c("No Calculation"),
                                         source = c("Parrett & Johnson"))
    }

#read in some files for reporting and finishing the culvert estimations
    
    culvert_usgs <- read_rds(path = file.path(tempdir(), "culvert_usgs"))
    
    stats_usgs_cul <- read_rds(path = file.path(tempdir(), "stats"))
    
    #customleaf <- file.path(tempdir(), "customleaf.png")
    
    
    
 
if (is.null(culvert_usgs)) {
  
  culvert_usgs <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                             basin_char = rep(0))
    
} else {
  
  
  
   
   culvert_size <- function(x) {
     ifelse(x < 11, "(24 in)", 
            ifelse(x >= 11 & x < 30,"(36 in)", 
                   ifelse(x >= 30 & x < 65,"(48 in)",
                          ifelse(x >= 65 & x <110,"(60 in)",
                                 ifelse(x >= 110 & x < 180,"(72 in)",
                                        ifelse(x >= 180 & x < 290,"(84 in)",
                                               ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)")))))))}
      
      
    culvert_usgs <- culvert_usgs %>% select(basin_char = Value, ReturnInterval = Name) %>% 
      mutate(source = "USGS Regression") %>% filter(ReturnInterval %in% c("2 Year Peak Flood", "25 Year Peak Flood",
                                                                          "50 Year Peak Flood", "100 Year Peak Flood"))}

   together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson, culvert_usgs)
   
   together <- together %>% mutate(RI = parse_number(ReturnInterval))
   
   if (isolate(input$use_reg) == "No") {
     
     together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>% 
       mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))
     
   } else {
   together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width", "active_width"), names_to = "Method") %>% 
     mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))}
   
   write_rds(together_long, file.path(tempdir(), "together_long"))
   
      list(together_long = together_long)
  })
  
 
  #observeEvent(input$calculate_culvert, {
    
    
    output$culvert_plot <- renderPlot({
      
      culvert()$together_long %>% ggplot(aes(RI, value, color = source)) + geom_point() + geom_line() + facet_wrap(~Method)
      
      
    })
    
    output$culvert_table <- renderDataTable(DT::datatable(culvert()$together_long %>% filter(RI %in% c(50,100)) %>% 
                                                            select(ReturnInterval,Source = source, Method, value, Size), options = list(pageLength = 25)))
    
  })
    
   final_cul <- reactive({
     
     customleaf <- file.path(tempdir(), "customleaf.png")
     
     stats_usgs_cul <- read_rds(path = file.path(tempdir(), "stats"))
     
     together_long <- read_rds(path = file.path(tempdir(), "together_long"))
     
     drain_name <- input$drain_name
     
     list(together_long = together_long, 
          stats_usgs_cul = stats_usgs_cul, drain_name = drain_name,
          customleaf = customleaf)
   })
  
  output$report_culvert <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)


      out <- render('report.Rmd',
                    params = list(set_author = input$author),

                    switch(
        input$format,
        HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

  # generateReport <- function() {
  #   out.markdown <- ''
  #   withProgress(message = 'Generating Report',
  #                value = 0, {
  #                  out.markdown <- rmarkdown::render(
  #                    input = "report.Rmd",
  #                    output_format = "html_document")
  #                  
  #                  setProgress(1)
  #                })
  #   
  #   read_file(out.markdown)
  # }
  # 
  # output$report2 <- renderText({ HTML(generateReport()) })
  
  
  # * NLDI ----
  
  
  
  output$nldi_maps <- renderLeaflet({
    
    
    map %>% 
      addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
      setView(lat = 48.91167, lng = -114.90246, zoom = 4)
    
    
    
    
  })
  
  
  
  observeEvent(input$nldi_maps_click, {
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Calculating Basin"))
        
        
      
    click <- input$nldi_maps_click
    clat <- click$lat
    clng <- click$lng
    content <- paste(
      "<b>Lat: ",round(clat, 5),"</b>",
      "<br>",
      "<b>Long: ", round(clng,5), "</b>"    )
    
    ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                       clng,"%20", clat, "%29")
    
    error_ids <- httr::GET(url = ids,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "nld_tmp.json"),overwrite = TRUE))
    
    nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))
    


    #write_rds(nld$features$properties$identifier, file.path(tempdir(), "nld"))
    
    nldiURLs <- list(site_data = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                                        clng,"%20", clat, "%29"),
                     basin_boundary = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin"),
                     UT = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigate/UT"),
                     UM = paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/navigate/UM"))
    
    nldi_data <- list()
    
    
    for(n in names(nldiURLs)) {
      nldi_data[n] <- list(sf::read_sf(nldiURLs[n][[1]]))
      print(paste(n, "is of class", class(nldi_data[[n]]), "and has", nrow(nldi_data[[n]]), "features"))
    }
    
    
    base_url <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/tot")

    error <- httr::GET(url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "catch_tmp.json"),overwrite = TRUE))

    catch_tmp <- jsonlite::fromJSON(file.path(tempdir(),"catch_tmp.json"))
    catch_tmp <- catch_tmp %>% flatten() %>% as_tibble()
    chars <- discover_nldi_characteristics()$total
    catch_tmp <- left_join(catch_tmp, chars, by = "characteristic_id") %>% 
      select(comid, Description = "characteristic_description", Value = "characteristic_value",Units = "units",  Theme = "theme_label") %>% arrange(Theme)
    
    #write_rds(catch_tmp, file.path(tempdir(), "catch_tmp"))
    
    map_nldi <- leaflet::addPolygons(map, 
                                data=nldi_data$basin_boundary, 
                                color = "black", 
                                fill = FALSE, 
                                weight = 2,
                                opacity = 1)
    
 
       
    map_nldi <- leaflet::addPolylines(map_nldi, 
                                 data = nldi_data$UT,
                                 color = "blue",
                                 weight = 1,
                                 opacity = 1)
    
       map_nldi <- leaflet::addPolylines(map_nldi,
                                 data = nldi_data$UM, 
                                 color = "red", 
                                 weight = 3, 
                                 opacity = 0.5)
       
    map_nldi <- leaflet::addCircleMarkers(map_nldi, lng=clng, lat=clat,
                                     radius = 5, 
                                     color = "red", popup = paste0("<b>","Drainage Area (DA): ","</b>", round(units::set_units(st_area(nldi_data$basin_boundary), mi^2), 1), " Sq.Miles",
                                                                   "<br>","<b>", "DA acres: ", "</b>", comma(as.numeric(round(units::set_units(st_area(nldi_data$basin_boundary), acres), 1)),1), " Acres",
                                                                   "<br>", "<b>", "Length of Main Stem: ", "</b>", round(sum(units::set_units(st_length(nldi_data$UM), mi)), 1), " Miles",
                                                                   "<br>", "<b>", "Total length of Tribs: ", "</b>", round(sum(units::set_units(st_length(nldi_data$UT), mi)), 1), " Miles",
                                                                   "<br>", "<b>", "DA/Length: ", "</b>", round((sum(units::set_units(st_length(nldi_data$UM), mi))+sum(units::set_units(st_length(nldi_data$UT), mi)))/(units::set_units(st_area(nldi_data$basin_boundary), mi^2)),1), " Miles"))
    output$nldi_maps <- renderLeaflet({map_nldi})

     output$nldi_table <- renderDataTable({ DT::datatable(catch_tmp, options = list(pageLength = 33))})
     
  })
    
    
    
  })
  
  # *** NLDI catch----
  
  
  observeEvent(input$nldi_start, {
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Calculating Catchment Characteristics"))
        
    nld2 <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))
    
    
    
    nldi_feature <- list(featureSource = "comid",
                         featureID = nld2$features$properties$identifier)
    
    outlet_comid <- discover_nhdplus_id(nldi_feature = nldi_feature)
    
    nhd_catch <- plot_nhdplus(nldi_feature, flowline_only = FALSE, actually_plot = FALSE)
    
    ###part 2
    
    local_characteristic <-  data.frame(COMID = data$flowline$COMID) %>% mutate(ID = row_number()) %>% 
      group_by(ID) %>% 
      nest() %>% mutate(chars = map(data, ~get_nldi_characteristics(list(featureSource = "comid", featureID = as.character(.$COMID)),
        type = "local"))) %>% unnest() %>% unnest() %>% ungroup() %>% 
      filter(str_detect(characteristic_id, "CAT_RECHG|CAT_ET|CAT_PET|CAT_PPT7100_ANN|CAT_TWI|CAT_BFI")) %>% 
      select(COMID, characteristic_id, characteristic_value) %>% 
      pivot_wider(names_from = "characteristic_id", values_from = "characteristic_value") %>% 
      mutate(across(starts_with("CAT"), as.numeric),
             CAT_PPT7100_ANN = CAT_PPT7100_ANN*0.0393701)
    
    
    cat <- right_join(nhd_catch$catchment, local_characteristic, by = c("FEATUREID" = "COMID")) %>% st_as_sf()
    
      }
    )
    
    
    

   output$nldi_catch <- renderLeaflet({
     
     if (input$cat == "Recharge") {
       
     qpal <- colorQuantile("RdBu", cat$CAT_RECHG)
     map %>% addPolygons(data = cat, fillOpacity = 0.6, color = ~qpal(CAT_RECHG), weight = 1.5,
                         popup = paste0(
                           "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
                           "<br>", "<b>Polygon Area: </b>", paste(comma(as.numeric(round(units::set_units(st_area(cat), mi^2),0))), " sq.mi"),
                           "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
                           "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
                           "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
                           "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
                           "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
                           "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
                           "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)), 
                         label = htmlEscape(paste("Recharge: ", round(cat$CAT_RECHG, 1)))) %>% 
       addLegend(position = "bottomright", pal = qpal, values = cat$CAT_RECHG, title = "Quantiles of Recharge")
     
     } else if (input$cat == " Actual Evapotranspiration (AET)") {
       qpal <- colorQuantile("RdBu", cat$CAT_ET,reverse = TRUE)
     map %>% addPolygons(data = cat, fillOpacity = 0.6, color = ~qpal(CAT_ET), weight = 1.5,
                         popup = paste0(
                           "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
                           "<br>", "<b>Polygon Area: </b>", paste(comma(as.numeric(round(units::set_units(st_area(cat), mi^2),0))), " sq.mi"),
                           "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
                           "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
                           "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
                           "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
                           "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
                           "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
                           "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)), 
                         label = htmlEscape(paste("ET: ", round(cat$CAT_ET, 1)))) %>% 
       addLegend(position = "bottomright", pal = qpal, values = cat$CAT_ET, title = "Quantiles of ET")
     
     
   } else if (input$cat == "Base Flow Index (BFI)") {
     qpal <- colorQuantile("RdBu", cat$CAT_BFI)
     map %>% addPolygons(data = cat, fillOpacity = 0.6, color = ~qpal(CAT_BFI), weight = 1.5,
                         popup = paste0(
                           "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
                           "<br>", "<b>Polygon Area: </b>", paste(comma(as.numeric(round(units::set_units(st_area(cat), mi^2),0))), " sq.mi"),
                           "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
                           "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
                           "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
                           "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
                           "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
                           "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
                           "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)), 
                         label = htmlEscape(paste("BFI: ", round(cat$CAT_BFI, 1)))) %>% 
       addLegend(position = "bottomright", pal = qpal, values = cat$CAT_BFI, title = "Quantiles of BFI")
     
     } else if (input$cat == "Potential Evapotranspiration (PET)") {
     
     qpal <- colorQuantile("RdBu", cat$CAT_PET, reverse = TRUE)
   map %>% addPolygons(data = cat, fillOpacity = 0.6, color = ~qpal(CAT_PET), weight = 1.5,
                       popup = paste0(
                         "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
                         "<br>", "<b>Polygon Area: </b>", paste(comma(as.numeric(round(units::set_units(st_area(cat), mi^2),0))), " sq.mi"),
                         "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
                         "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
                         "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
                         "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
                         "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
                         "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
                         "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)), 
                       label = htmlEscape(paste("PET: ", round(cat$CAT_PET, 1)))) %>% 
     addLegend(position = "bottomright", pal = qpal, values = cat$CAT_PET, title = "Quantiles of PET")
   
  } else if (input$cat == "Precipitation (PRISM)") {
    
    qpal <- colorQuantile("RdBu", cat$CAT_PPT7100_ANN)
    map %>% addPolygons(data = cat, fillOpacity = 0.6, color = ~qpal(CAT_PPT7100_ANN), weight = 1.5,
                        popup = paste0(
                          "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
                          "<br>", "<b>Polygon Area: </b>", paste(comma(as.numeric(round(units::set_units(st_area(cat), mi^2),0))), " sq.mi"),
                          "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
                          "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
                          "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
                          "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
                          "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
                          "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
                          "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)), 
                        label = htmlEscape(paste("Recharge: ", round(cat$CAT_PPT7100_ANN, 1)))) %>% 
      addLegend(position = "bottomright", pal = qpal, values = cat$CAT_PPT7100_ANN, title = "Quantiles of Precipitation")
    
  } else if (input$cat == "Topographic Wetness Index (TWI)") {
    
    qpal <- colorQuantile("RdBu", cat$CAT_TWI)
    map %>% addPolygons(data = cat, fillOpacity = 0.6, color = ~qpal(CAT_TWI), weight = 1.5,
                        popup = paste0(
                          "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",cat$FEATUREID,
                          "<br>", "<b>Polygon Area: </b>", paste(comma(as.numeric(round(units::set_units(st_area(cat), mi^2),0))), " sq.mi"),
                          "<br>","<b>Base Flow Index (BFI): </b>",paste(round(cat$CAT_BFI,1), " %"),
                          "<br>","<b>Recharge: </b>",paste(round(cat$CAT_RECHG,1), " mm/year"),
                          "<br>","<b>Actual ET: </b>",paste(round(cat$CAT_ET,1), " mm/year"),
                          "<br>","<b>Potential ET: </b>",paste(round(cat$CAT_PET,1), " mm/year"),
                          "<br>", "<b>Water Deficit: </b>", paste(round(cat$CAT_PET-cat$CAT_ET,1), " mm/year"),
                          "<br>","<b>Precip (PRISM 30 year mean): </b>",paste(round(cat$CAT_PPT7100_ANN,1), " in."),
                          "<br>","<b>Topographic Wetness Index (TWI): </b>",round(cat$CAT_TWI,1)), 
                        label = htmlEscape(paste("Recharge: ", round(cat$CAT_TWI, 1)))) %>% 
      addLegend(position = "bottomright", pal = qpal, values = cat$CAT_TWI, title = "Quantiles of TWI")
  }
     
     
   })
   
   
   
   
  })
  
  
  
  
  
 # * 6 NWS ----
  
  
  # ** 6.1 -- MRMS-QPM ----
  
  #mesonet tiles
  grp_meso <- c("72-hour (precip)", "48-hour (precip)", "24-hour (precip)", "1-hour (precip)","NWS warnings", "States")
  
  map_2 <- leaflet("Map")
  
  
  map_2 <- leaflet::addWMSTiles(map, GetURL("USGSTopo"),
                                group = grp[1], attribution = att, layers = "0")
  
  map_2 <- leaflet::addWMSTiles(map_2,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi",
                                layers = "mrms_p72h",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_meso[1]
  ) %>% leaflet.extras::addWMSLegend("https://mesonet.agron.iastate.edu/images/mrms_q3_p24h.png")
  
  map_2 <- leaflet::addWMSTiles(map_2,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi",
                                layers = "mrms_p48h",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_meso[2]
  )
  map_2 <- leaflet::addWMSTiles(map_2,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi",
                                layers = "mrms_p24h",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_meso[3]
  )
  map_2 <- leaflet::addWMSTiles(map_2,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi",
                                layers = "mrms_p1h",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_meso[4]
  )
  map_2 <-  leaflet::addWMSTiles(map_2,"https://mesonet.agron.iastate.edu/cgi-bin/wms/us/wwa.cgi?",
                                  layers = "wwa", group = grp_meso[5],
                                  options = WMSTileOptions(format = "image/png", transparent = T),
                                  attribution = "Weather data © 2012 IEM Nexrad"
  ) %>% 
    leaflet.extras::addWMSLegend("https://mesonet.agron.iastate.edu/cgi-bin/wms/us/wwa.cgi?version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=warnings_p&format=image/png&STYLE=default")
  
  
 map_2 <- leaflet::addWMSTiles(map_2, "https://carto.nationalmap.gov/arcgis/services/govunits/MapServer/WMSServer?",
                       layers = "0",group = grp_meso[6],
                       options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )
  
  
  opt2 <- leaflet::layersControlOptions(collapsed = TRUE)
  map_2 <- leaflet::addLayersControl(map_2, baseGroups = grp_meso[1:4],
                                     overlayGroups = grp_meso[5:6], options = opt2)
  
  output$nws_maps <- renderLeaflet({
    

    map_2 %>% setView(lat = 48.91167, lng = -114.90246, zoom = 4)
      
    
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
    
    
    state <-   geocode_rev(c(clat,clng)) %>% dplyr::select(state)
    state <-  state.abb[grep(paste(state$state), state.name)]
    
    nws_forecast <- reactive({
      
      df1 <- httr::GET(url = paste0(
        "https://api.weather.gov/points/",
        
        click$lat, ",",
        
        click$lng))
      
    
        
        df <- jsonlite::fromJSON(url(df1$url, "rb"))
      
      city <- df$properties$relativeLocation$properties$city
      state <- df$properties$relativeLocation$properties$state
      location <- paste(city, state, sep = ", ")
      

        
      df <- df$properties$forecast
      
      df <- httr::GET(url = paste0(df))
      
      df <- jsonlite::fromJSON(url(df$url, "rb"))
      
      df <- df$properties$periods
      
      df <- df %>% mutate(temp = str_c(temperature, temperatureUnit, sep = "-"), wind = str_c(windSpeed, windDirection, sep = "-"), location = location,
                          Date = lubridate::as_date(endTime))
      df <- df %>% select(name,Date, temp, wind, detailedForecast, location)
      
      })
    
    
    withProgress(
      
      message = 'Forecast in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Couple Seconds..."))
        
        nws_forecast()
        
        
        
        
        
      }
    )
    
      output$nws_table = renderDataTable({DT::datatable(nws_forecast(), options = list(pageLength = 25))})
  })

  

# ** 6.2 -- GOES West ----
  
  grp_gw <- c("GOES West (05)", "GOES West (10)","GOES West IR (13)", "GOES West IR (14)", "States")
  
  map_gw <- leaflet("Map")
  
  
  map_gw <- leaflet::addWMSTiles(map, GetURL("USGSTopo"),
                                group = grp[1], attribution = att, layers = "0")
  
  map_gw <- leaflet::addWMSTiles(map_gw,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/goes_west.cgi",
                                layers = "conus_ch05",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_gw[1]
  )
  
  map_gw <- leaflet::addWMSTiles(map_gw,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/goes_west.cgi",
                                layers = "conus_ch10",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_gw[2]
  )
  
  map_gw <- leaflet::addWMSTiles(map_gw,
                                 "https://mesonet.agron.iastate.edu/cgi-bin/wms/goes_west.cgi",
                                 layers = "conus_ch13",
                                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                 attribution = "Weather data © 2012 IEM Nexrad", group = grp_gw[3]
  )
  
  map_gw <- leaflet::addWMSTiles(map_gw,
                                "https://mesonet.agron.iastate.edu/cgi-bin/wms/goes_west.cgi",
                                layers = "conus_ch14",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                attribution = "Weather data © 2012 IEM Nexrad", group = grp_gw[4]
  )
  
  map_gw <- leaflet::addWMSTiles(map_gw, "https://carto.nationalmap.gov/arcgis/services/govunits/MapServer/WMSServer?",
                                layers = "0",group = grp_gw[5],
                                options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )

  
  
  gw_opt<- leaflet::layersControlOptions(collapsed = TRUE)
  map_gw <- leaflet::addLayersControl(map_gw, baseGroups = grp_gw[1:4],
                                     overlayGroups = grp_gw[5], options = gw_opt) 

  #now render map_gw
  
  output$goes_west <- renderLeaflet({
    
    
    map_gw %>% setView(lat = 48.91167, lng = -114.90246, zoom = 4)
    
    
  })


  
  # ** 6.2 -- NEXRAD ----
  
  grp_nx <- c("NEXRAD BR-10m", "NEXRAD BR-30m", "NEXRAD BR-50m", "States")
  
  map_nx <- leaflet()
  
  
  map_nx <- leaflet::addWMSTiles(map, GetURL("USGSTopo"),
                                 group = grp[1], attribution = att, layers = "0")
  
  map_nx <- leaflet::addWMSTiles(map_nx,
                                 "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                                 layers = "nexrad-n0q-900913-m05m",
                                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                 attribution = "Weather data © 2012 IEM Nexrad", group = grp_nx[1]
  )
  
  map_nx <- leaflet::addWMSTiles(map_nx,
                                 "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                                 layers = "nexrad-n0q-900913-m30m",
                                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                 attribution = "Weather data © 2012 IEM Nexrad", group = grp_nx[2]
  )
  
  
  
  map_nx <- leaflet::addWMSTiles(map_nx,
                                 "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                                 layers = "nexrad-n0q-900913-m50m",
                                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                                 attribution = "Weather data © 2012 IEM Nexrad", group = grp_nx[3]
  )

  map_nx <- leaflet::addWMSTiles(map_nx, "https://carto.nationalmap.gov/arcgis/services/govunits/MapServer/WMSServer?",
                                 layers = "0",group = grp_nx[4],
                                 options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )
  
  
  
  nx_opt <- leaflet::layersControlOptions(collapsed = TRUE)
  map_nx <- leaflet::addLayersControl(map_nx, baseGroups = grp_nx[1:3],
                                      overlayGroups = grp_nx[4], options = nx_opt) 
  
  #now render map_nexrad
  
  output$nexrad_map <- renderLeaflet({
    
    
    map_nx %>% setView(lat = 48.91167, lng = -114.90246, zoom = 4)
    
    
  })




} # last curl






  









##### RUN APPLICATION #####
shinyApp(ui = ui, server = server)