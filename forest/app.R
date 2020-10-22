
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leafpop)
library(leafem)
library(EGRET)
library(splines)
library(DT)
library(kableExtra)
library(scales)
library(latex2exp)
library(tidyverse)
library(feather)
library(ggrepel)
library(googleCloudStorageR)
library(Hmisc)

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

usgs_min_max_wy <- read_feather( "usgs_min_max_wy.csv")

usgs_min_max_wy_month <- read_feather( "usgs_min_max_wy_month.csv")

usgs_min_max_month <- read_feather( "usgs_min_max_month.csv")

mapping_usgs <- read_feather("mapping_usgs.csv")

flood_freq <- read_feather("flood_freq.csv")

flood_freq_norm <- read_feather("flood_freq_norm.csv")

usgs_EGRET <- read_feather("usgs_EGRET.csv")

names <- unique(usgs_EGRET$station_nm)

usgs_info <- read_feather("usgs_info.csv")

usgs_info <- usgs_info %>% filter(station_nm %in% names)



usgs_daily_nest <- usgs_EGRET %>% group_by(station_nm) %>% nest(.key = "daily")


usgs_info_nest <- usgs_info %>% group_by(station_nm) %>% nest() 

egret_final <- list()

for (i in 1:nrow(usgs_info_nest)){
  eg_day <- pluck(usgs_daily_nest$daily) %>% pluck(i)
  eg_info <- pluck(usgs_info_nest$data) %>% pluck(i)
  
  egret <- as.egret(Daily = eg_day,INFO = eg_info, NA, NA)
  name <- paste(egret[["INFO"]][["shortName"]][[1]])
  egret <- list(egret)
  names(egret) <- name
  egret_final <- append(egret_final, egret)
  
}




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
      icon = icon("table"),
      menuSubItem("Snotel", tabName = "swe_graphs", icon = icon("chart-line")),
      menuSubItem("Snotel Phenology", tabName = "swe_phen", icon = icon("chart-line"))
    ),
    menuItem(
      "USGS",
      tabName = "usgs_graphs",
      icon = icon("table"),
      menuSubItem("USGS-Raw", tabName = "usgs_graphs", icon = icon("chart-line")),
      menuSubItem("USGS-Normalized", tabName = "usgs_norm", icon = icon("chart-line")),
      menuSubItem("EGRET", tabName = "egret_graphs", icon = icon("chart-line"))
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
               tabPanel("Month", plotOutput("swe_month"),
                        pickerInput("Site", "Please select a site:", 
                                    choices = paste0(unique(inland_northwest_snotel_min_max_year_month$site_name)),
                                    multiple = TRUE, selected = "Stahl", options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE)),
                        pickerInput("Month", "and a Month", options = list(`actions-box` = TRUE), multiple = TRUE,
                                    choices= paste0(unique(inland_northwest_snotel_min_max_year_month$month_abb)), selected = "Jan")),
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
                        actionButton("calculate", label = "Calculate"),verbatimTextOutput("freq_swe_text"))))),
    # * * -- Snotel - Phen ----
    tabItem(
      tabName = "swe_phen",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart",
               tabPanel("DOY", plotOutput("phen_month"),
                        pickerInput("Site_phen", "Please select a site:", 
                                    choices = paste0(unique(phenology$site_name)),
                                    multiple = TRUE, selected = "Stahl", options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE)),
                        pickerInput("Month_phen", "and Metric", options = list(`actions-box` = TRUE), 
                                    choices= paste0(c("First Snow Accumulation", "Last Snow Accumulation", "Continuous Snow Accumulation")), 
                                    selected = "Last Snow Accumulation")),
               tabPanel("Histogram", plotOutput("phen_hist"),pickerInput("phen", "Metric", options = list(`actions-box` = TRUE), 
                                                                           choices= paste0(c("First Snow Accumulation", "Last Snow Accumulation", "Continuous Snow Accumulation")), 
                                                                           selected = "Last Snow Accumulation"),
                        numericInput("n_phen", "Enter Value for Binwidth", 5),
                        actionButton("binwidth_phen", label = "Submit Binwidth"))))),
    
    # * * -- USGS-Raw ----
    tabItem(
      tabName = "usgs_graphs",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(width = 12, id = "tabchart_usgs",
               tabPanel("Map", style = "height:92vh;",
                        leafletOutput("usgs_maps", width = "100%", height = "100%")),
               tabPanel("Month", plotOutput("usgs_month"), 
                        pickerInput("Site_usgs", "Please select a site:",  options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE),
                                    choices = paste0(unique(usgs_min_max_wy_month$Station)),
                                    multiple = TRUE, selected = "Tobacco River near Eureka, MT"),
                        pickerInput("Month_usgs", "and a Month", multiple = TRUE, selected = "Jan",  options = list(`actions-box` = TRUE), 
                                    choices= paste0(unique(usgs_min_max_wy_month$month_abb)))),
               tabPanel("Classic", plotOutput("usgs_classic"), 
                        pickerInput("eg", "site",choices = names(egret_final), selected = "Tobacco River near Eureka, MT",
                                    options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE)),
                        numericInput("start", "Enter Start Date", ""),
                        numericInput("finish", "Enter Finish Date", ""),
                        actionButton("dates", label = "Date Range")),
               tabPanel("Minima - Maxima", plotOutput("max_usgs")),
               tabPanel("Histogram", plotOutput("usgs_hist"),
                        selectInput("maxmin2", "Dististribution of ?", choices = c("Maximum", "Minimum", "Mean","Median")),
                        numericInput("n2", "Enter Value for Binwidth", 50),
                        actionButton("binwidth", label = "Submit Binwidth")),
               tabPanel(title = "Summary Stats", value = "usgs_stats",DT::dataTableOutput("usgs_stats"), 
                        pickerInput("Report", "Group by?", c("wy", "month", "wy & month"), selected = "wy"),
                        actionButton("action", label = "Summarize"),
                        downloadButton(outputId = "downLoadFilter",
                                       label = "Download Filtered Data")),
               tabPanel("Flood Frequency",
                        plotOutput("usgs_freq", brush = "user_brush"), dataTableOutput("flood_dt")),
               tabPanel(title = "Flood Freq Groups", plotOutput("freq_g"),
                        pickerInput("dist1", "Method",choices = c("Lognormal", "LogPearson", "Gumbel"), selected = "LogPearson",
                                    options = list(`actions-box` = TRUE)),
                        pickerInput("drain", "Method",choices = paste(unique(flood_freq$drainage_area_cut)), selected = "LogPearson",
                                    options = list(`actions-box` = TRUE)),
               pickerInput("stream_stats", "Stream Stats",choices = c(names(flood_freq[,c(7,10,12,13,14,21:26)])), selected = "TEMP",
                           options = list(`actions-box` = TRUE)))))),
    
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
                                    choices = c("Drainage Area", "Standard Deviation", "Average", "Average All Time"))
                        ),
               tabPanel("Exploring Categories", plotOutput("usgs_cat_norm")),
               tabPanel(title = "Q-var: EGRET", plotOutput("egret_sd"),
                        pickerInput("egret2", "site",choices = names(egret_final), selected = "Tobacco River near Eureka, MT",
                                    options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE))),
               tabPanel("Flood Frequency",
                        plotOutput("usgs_freq_norm")),
               tabPanel(title = "Flood Freq Groups", plotOutput("freq_group"),
                        pickerInput("dist", "site",choices = c("Lognormal", "LogPearson", "Gumbel"), selected = "LogPearson",
                                    options = list(`actions-box` = TRUE)))))),
    # * * -- USGS-EGRET ----
    
    tabItem(
      tabName = "egret_graphs",
      tags$style(type = 'text/css', '#graphs {height: calc(100vh - 250px) !important;}'),
      fluidRow(
        tabBox(
        width = 12, id = "tabchart_egret",
        tabPanel(title = "Minimum 1-day", plotOutput("stat1"),
                 pickerInput("egret", "site",choices = names(egret_final), selected = "Tobacco River near Eureka, MT",
                 options = list(`actions-box` = TRUE, size = 6, `live-search`=TRUE))),
        tabPanel(title = "Minimum 7-day", plotOutput("stat2")),
        tabPanel(title = "Minimum 30-day", plotOutput("stat3")),
        tabPanel(title = "Median daily", plotOutput("stat4")),
        tabPanel(title = "Mean daily", plotOutput("stat5")),
        tabPanel(title = "Maximum 30-day", plotOutput("stat6")),
        tabPanel(title = "Maximum 7-day", plotOutput("stat7")),
        tabPanel(title = "Maximum 1-day", plotOutput("stat8"))
    )))
    
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
    
    labs <- as.list(str_to_title(mapping_snotel_today$site_name))
    
    leaflet("Map", data = mapping_snotel_today) %>% 
      addTiles(group = "OpenStreetMap") %>% addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
      addCircles(lng = mapping_snotel_today$longitude, lat = mapping_snotel_today$latitude,weight = 10,
                 fillOpacity = 0.9, popup = paste0("<p style=line-height:30px;margin:0px;>",
                                                   "<b>Site Name: </b>",str_to_title(mapping_snotel_today$site_name),
                                                   "<br>", "<b>Site Description: </b>", mapping_snotel_today$description,
                                                   "<br>", "<b>Site #: </b>", mapping_snotel_today$site_id,
                                                   "<br>", "<b>Current Reading (SWE): </b>", round(mapping_snotel_today$snow_water_equivalent,2), " in",
                                                   "<br>", "<b>Current Reading (Snow Depth): </b>", round(mapping_snotel_today$snow_depth,2), " in",
                                                   "<br>", "<b>Current Reading (Precip. Cumulative): </b>", round(mapping_snotel_today$precipitation_cumulative,2), " in",
                                                   "<br>", "<b>Elevation: </b>", comma(mapping_snotel_today$elev*3.28084,1), " ft",
                                                   "<br>", "<b>Observation Range: </b>", paste(mapping_snotel_today$start, " to ", mapping_snotel_today$end),"</p>"),
                 label = lapply(labs, HTML),layerId = ~unique(str_to_title(mapping_snotel_today$site_name)))
    
  })
  
  swe_ggplot_data <- reactive({
    site <- input$swe_maps_shape_click$id
    mapping_snotel %>% filter(str_to_title(site_name) %in% site)
  })
  
  output$swe_ggplot <- renderPlot({
    ggplot(data = swe_ggplot_data(), aes(date, snow_depth)) + geom_line() + geom_smooth(se = F) + theme_light() + 
      labs(title = paste("Last 7 days of hourly Snow Depth: ", input$swe_maps_shape_click$id ), x = "Date", y = "Snow Depth (in)")
    
  })
  # ** 1.1 -- Months ----

  year_month <- reactive({inland_northwest_snotel_min_max_year_month %>% filter(site_name %in% input$Site, month_abb %in% input$Month)})
  
  output$swe_month = renderCachedPlot({
    validate(
      need(input$Site, 'Please select at least one site'),
      need(input$Month, 'Please choose at least one Month')
    )
    ggplot(year_month(), aes(year, Mean)) + 
                     geom_line(size = 1,aes(color = site_name)) + 
                     geom_point(size = 2.5) + geom_smooth(alpha = 0.2,aes(color = site_name)) + 
                     labs(title = paste0(input$Site, " Mean SWE for ", input$Month), y = "Mean SWE per Month") +  
      theme_light() 
  },
  cacheKeyExpr = { list(input$Site, input$Month, year_month()) })
  
  
  # ** 1.2 -- Min - Max ----
  
    site_minmax <- reactive({inland_northwest_snotel_min_max_year %>%  filter(site_name %in% input$Site)})
  
  output$swe_minmax = renderCachedPlot({
    
    ggplot(site_minmax()) + 
                     geom_line(aes(year, Maximum), color = "blue")  + 
                     geom_smooth(alpha = 0.2, aes(year, Maximum), se = F, color = "black") + 
      geom_line(aes(year, Mean), color = "black")  + 
      geom_smooth(alpha = 0.2, aes(year, Mean), se = F, color = "black") + 
                     geom_line(aes(year, Minimum), color = "red")  + 
                     geom_smooth(alpha = 0.2, aes(year, Minimum), se = F, color = "black")+ 
                     labs(title = paste0(input$Site2, " Daily Discharge max-min per year"), x = "Year", y = "Min-Max of Site Daily SWE")+ 
                     theme_light() + facet_wrap(~site_name, scales = "free_y")
    
  },
  cacheKeyExpr = { list(input$Site2, site_minmax()) })
  
  
  
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
                       guides(color = guide_legend(override.aes = list(alpha = 1))) + theme_light()
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
  
  # * 2 Phenology ----
  
  # ** 2.1 -- Months ----
    
    phen <- reactive({phenology %>% filter(site_name %in% input$Site_phen)})
  output$phen_month = renderCachedPlot({
    
    if (input$Month_phen == "First Snow Accumulation") {
      validate(
        need(input$Site_phen, 'Please select at least one site')
      )
      ggplot(phen(), aes(max_swe, first_snow_acc, color = site_name, label = first_snow_acc_m)) + 
                       geom_line() + 
                       geom_point(size = 2) + geom_smooth(alpha = 0.2) + 
                       labs(x = "Max SWE") +  
                       theme_light()+ facet_wrap(~site_name, scales = "free")
    } else if (input$Month_phen == "Last Snow Accumulation") {
      validate(
        need(input$Site_phen, 'Please select at least one site')
      )
      ggplot(phen(), aes(max_swe, last_snow_melt, color = site_name, label = last_snow_melt_m )) + 
                       geom_line() + 
                       geom_point(size = 2) + geom_smooth(alpha = 0.2) + 
                       labs( x = "Max SWE") +  
                       theme_light() + facet_wrap(~site_name, scales = "free")
      
    } else if (input$Month_phen == "Continuous Snow Accumulation") {
      validate(
        need(input$Site_phen, 'Please select at least one site')
      )
     ggplot(phen(), aes(max_swe, cont_snow_acc, color = site_name, label = cont_snow_acc_m)) + 
                       geom_line(size = 0.75) + 
                       geom_point(size = 2) + geom_smooth(alpha = 0.2) + 
                       labs(x = "Max SWE") +  
                       theme_light()+ facet_wrap(~site_name, scales = "free")
      
    } else {"SOL"}
    
    
  },
  cacheKeyExpr = { list(input$Month_phen, phen()) })
  
  
  
  # ** 2.2 -- Histogram P ----
  
    phen2 <- reactive({phenology %>% filter(site_name %in% input$Site_phen)})
  
  observeEvent(input$binwidth_phen, ignoreInit = TRUE, {
    output$phen_hist = renderCachedPlot({
      
      if (input$phen == "First Snow Accumulation"){
      ggplot(phen2(),aes(first_snow_acc)) + 
                       geom_histogram(aes(fill = site_name), binwidth = isolate(input$n_phen), alpha = .5) + 
                       geom_density(adjust = .5  ,aes(y = isolate(input$n_phen) * ..count.., color = site_name), size = 1)  + 
                       labs(x = "DOY (julian)") +  
                       theme_light()
      } else if (input$phen == "Last Snow Accumulation") {
        
       ggplot(phen2(),aes(last_snow_melt)) + 
                         geom_histogram(aes(fill = site_name), binwidth = isolate(input$n_phen), alpha = .5) + 
                         geom_density(adjust = .5  ,aes(y = isolate(input$n_phen) * ..count.., color = site_name), size = 1)  + 
                         labs(x = "DOY (julian)") +  
                         theme_light()
        
      } else if (input$phen == "Continuous Snow Accumulation") {
        
        
        ggplot(phen2(),aes(cont_snow_acc)) + 
                         geom_histogram(aes(fill = site_name), binwidth = isolate(input$n_phen), alpha = .5) + 
                         geom_density(adjust = .5  ,aes(y = isolate(input$n_phen) * ..count.., color = site_name), size = 1)  + 
                         labs(x = "DOY (julian)") +  
                         theme_light()
      } else {"SOL"}
    },
    cacheKeyExpr = { list(input$binwidth_phen,input$phen, phen2()) })
  })
  
  # ** 2.3 -- Across Time ----
  
  
  
  # * 3  USGS-Raw ----
  # 
  
  output$usgs_maps <- renderLeaflet({
    
    labs_usgs <- as.list(mapping_usgs$Station)
    
    tbl_usgs <- mapping_usgs %>% select(Station, c(`0%`, `25%`,`50%`, `75%`, `100%`)) %>% 
      group_by(Station) %>% nest() %>%
      mutate(table = map(data, ~kable(.) %>% kable_styling(.,bootstrap_options = c("striped", "hover", "condensed", "responsive")))) %>% pull(table)
    
    leaflet("Map", data = mapping_usgs) %>%
      addTiles(group = "OpenStreetMap") %>% addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
      addCircles(lng = mapping_usgs$long, lat = mapping_usgs$lat, weight = 10,
                 fillOpacity = 0.9, popup = paste0(
                   "<p style=line-height:30px;margin:0px;>","<b>Station: </b>",mapping_usgs$Station,
                   "<br>", "<b>Current Reading: </b>", paste(comma(round(mapping_usgs$Flow,0),1)), " <sup>ft<sup>3</sup></sup>&frasl;<sub>s</sub>",
                   "<br>", "<b>Drainage Area: </b>", paste(comma(round(mapping_usgs$drainage_area,0),1), " sq.mi"),
                   "<br>", "<b>Quantiles for </b>",paste("<b>", "'", mapping_usgs$month_day, "'", " :", "</b>"),
                   tbl_usgs,"</p>"), 
                 label = lapply(labs_usgs, HTML), group = "Point_usgs", color = mapping_usgs$month_day)
    
  })
  
  
  
  
# ** 3.1 -- Months  ----
  
    
    year_month2 <- reactive({ usgs_min_max_wy_month %>% 
        filter(Station %in% input$Site_usgs, month_abb %in% input$Month_usgs) })
  output$usgs_month = renderCachedPlot({
    validate(
      need(input$Site_usgs, 'Please select at least one site'),
      need(input$Month_usgs, 'Please select at least one Month')
    )
      ggplot(year_month2(), aes(wy, Mean)) + 
                       geom_line(size = 1, aes(color = Station)) + 
                       geom_point(size = 2.5) + geom_smooth(alpha = 0.1, aes(color = Station), se = TRUE) + 
                       labs(title = paste0(input$Site_usgs, " Monthly Mean Discharge (cfs) for ", input$Month2), 
                            y = "Mean Discharge (cfs) per Month")+ theme_light() 
  },
  cacheKeyExpr = { list(input$Site_usgs, input$Month2, year_month2()) })
  
  # ** 3.2 -- Classic----
  observeEvent(input$dates, {
  output$usgs_classic = renderCachedPlot({
    
    EGRET::plotQTimeDaily(egret_final[[input$eg]], 
                          yearStart = input$start, 
                          yearEnd = input$finish)
  },
  cacheKeyExpr = { list(input$eg, input$start, input$finish)})
  
  })
  
  
  # ** 3.3 -- Min - Max ----
    site_max <- reactive({usgs_min_max_wy %>% filter(Station %in% input$Site_usgs) %>% pivot_longer(cols = c("Maximum", "Minimum"))})
  output$max_usgs = renderCachedPlot({
    validate(
      need(input$Site_usgs, 'Please select at least one site')
    )
    
    ggplot(site_max(), aes(wy, value, group = Station, color = Station)) + geom_line() + geom_smooth(alpha = 0.2) +
      labs(title = paste0(input$Site_usgs, " Discharge max-min per year"), x = "Water Year", y = "Min-Max of Station Daily Discharge")+ 
      theme_light() + facet_wrap(~name, scales = "free_y")
    
  },
  cacheKeyExpr = { list(input$Site_usgs, site_max()) })

  
  
  
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
                            title = paste0(input$Site_usgs, " histogram of Annual", input$maxmin2, "Discharge "))+ 
                       theme_light() + guides(color = guide_legend(override.aes = list(alpha = 1)))
    }, cacheKeyExpr =  {list(input$maxmin2, input$binwidth, hist_react2())} )
   }) 
 
  
  # ** 3.5 -- Summary Stats ----
  
  summary_stats <- reactive({
    if(input$Report_swe == "wy") { 
      
      usgs_min_max_wy %>% 
        filter(Station %in% input$Site) %>% group_by(site_name, year)
      
    } else if (input$Report_swe == "Month") {
      
      usgs_min_max_month %>% filter(Station %in% input$Site)
      
    } else if (input$Report_swe == "wy & Month") {
      
      usgs_min_max_wy_month %>% 
        filter(Station %in% input$Site)
      
    }  else { "No Data"}
    
  })
  observeEvent(input$action, ignoreInit = TRUE, {
    
    output$usgs_stats = renderDataTable({summary_stats() %>% datatable(filter = list(position = 'top')) })
    
    
  })
  
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
    validate(
      need(input$Site_usgs, 'Please select at least one site')
    )
    
    ggplot(freq(), aes(x= ReturnInterval, y = Flow, color = Distribution )) + 
      geom_point(alpha = 0.8, size = 2.5) + 
      geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light() + facet_wrap(~Station, scales = "free")
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
    
    if (input$stream_stats == "TEMP") {
    
    ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = TEMP))  + 
      geom_point(alpha = 0.8) + 
      geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
      expand_limits(y = 0) + 
      scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
      labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
      theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black")) +
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "ET0306MOD") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = ET0306MOD))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "ET0710MOD") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = ET0710MOD))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
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
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
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
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
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
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
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
      theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
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
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "SLOP50_30M") {
    
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = SLOP50_30M))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
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
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    } else if (input$stream_stats == "CONTDA") {
      
      
      ggplot(freq_group(), aes(ReturnInterval, Flow, group = Station, color = CONTDA))  + 
        geom_point(alpha = 0.8) + 
        geom_smooth(method = "glm", formula = y ~ ns(log(x), 4), se = FALSE, size = 1) + 
        expand_limits(y = 0) + 
        scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
        labs(x = "Return Interval", y = "Discharge (cfs)", title = "Flood Frequency") +
        theme_light() + facet_wrap(~drainage_area_cut, scales = "free") +theme(strip.text = element_text(size=9,color = "black"))+
        geom_label_repel(data = freq_station(), aes(label = Station),
                         label.size = NA,  
                         label.padding=.1, 
                         na.rm=TRUE,
                         fill = alpha(c("white"),0.65))
      
    }  else {"SOL"}
    
  },
  cacheKeyExpr = { list(input$dist1,input$stream_stats, freq_group()) })
  
  })
  
  
  
  
  
  
  
  
  # * 4 USGS-Norm ----
  
  
  
  # ** 4.1 -- Months ----
  
    year_month_norm <- reactive({usgs_min_max_wy %>% filter(Station %in% input$Site_norm)})
  
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
      
    } else if (input$norm_method == "Average") {
      
      ggplot(year_month_norm(), aes(wy, Max_avg, color = Station)) + 
        geom_line() + 
        geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Maximum Flow) / log(Annual Mean)")
      
    } else if (input$norm_method == "Average All Time") {
      
      ggplot(year_month_norm(), aes(wy, Max_avg_at, color = Station)) + 
        geom_line() + 
        geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Maximum Flow) / log(All time mean flow)")
      
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
        
      } else if (input$norm_method == "Average") {
        
        ggplot(year_month_norm(), aes(wy, Min_avg, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Minimum Flow) / log(Annual Mean)")
        
      } else if (input$norm_method == "Average All Time") {
        
        ggplot(year_month_norm(), aes(wy, Min_avg_at, color = Station)) + 
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
        
      } else if (input$norm_method == "Average") {
        
        ggplot(year_month_norm(), aes(wy, Mean_avg, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Mean Flow) / log(Annual Mean)")
        
      } else if (input$norm_method == "Average All Time") {
        
        ggplot(year_month_norm(), aes(wy, Mean_avg_at, color = Station)) + 
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
        
      } else if (input$norm_method == "Average") {
        
        ggplot(year_month_norm(), aes(wy, Med_avg, color = Station)) + 
          geom_line() + 
          geom_point() + geom_smooth(alpha = 0.2, se = FALSE) + theme_light()+ labs(x = "Water Year", y = "log(Annual Median Flow) / log(Annual Mean)")
        
      } else if (input$norm_method == "Average All Time") {
        
        ggplot(year_month_norm(), aes(wy, Med_avg_at, color = Station)) + 
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
  
  output$usgs_cat_norm = renderPlot({
 
    
    ggplot(usgs_min_max, aes(log_drainage, Max_dnorm, color = drainage_area_cut)) + 
                     geom_point(size = 4.5)   + 
                     labs() + 
                     theme_light()  
    
    
  })
  #,
  #cacheKeyExpr = { list(input$Site_norm, year_month_norm2()) })
  
  
  
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
  
  # * 5 USGS-EGRET ----
  
  
  # ** 5.1 ----
  
  output$stat1 = renderCachedPlot({
    
   plotFlowSingle(egret_final[[input$egret]],istat = 1)
  }, cacheKeyExpr = input$egret)
  
  
  # ** 5.2 ----
  
  output$stat2 = renderCachedPlot({
    
    egret_final[[input$egret]] %>%  plotFlowSingle(istat = 1)
  }, cacheKeyExpr = input$egret)
  
  # ** 5.3 ----
  
  output$stat3 = renderCachedPlot({
    
    plotFlowSingle(egret_final[[input$egret]],istat = 3)
  }, cacheKeyExpr = input$egret)
  
  # ** 5.4 ----
  
  output$stat4 = renderCachedPlot({
    
    plotFlowSingle(egret_final[[input$egret]],istat = 4)
  }, cacheKeyExpr = input$egret)
  
  # ** 5.5 ----
  
  output$stat5 = renderCachedPlot({
    
    plotFlowSingle(egret_final[[input$egret]],istat = 5)
  }, cacheKeyExpr = input$egret)
  
  # ** 5.6 ----
  
  output$stat6 = renderCachedPlot({
    
    plotFlowSingle(egret_final[[input$egret]],istat = 6)
  }, cacheKeyExpr = input$egret)
  
  # ** 5.7 ----
  
  output$stat7 = renderCachedPlot({
    
    plotFlowSingle(egret_final[[input$egret]],istat = 7)
  }, cacheKeyExpr = input$egret)
  
  # ** 5.8 ----
  
  output$stat8 = renderCachedPlot({
    
    plotFlowSingle(egret_final[[input$egret]],istat = 8)
  }, cacheKeyExpr = input$egret)
  
  
  # filtered_q <- reactive({district_gauging %>% filter(Stream == input$Station)})
  # 
  # output$q_station = renderPlotly({
  #   
  #   filtered_q <- reactive({district_gauging %>% filter(Stream == input$Station)})
  #   
  #   print(ggplotly(ggplot(filtered_q(), aes(Date, Q, group = Stream)) + geom_line() + theme_light() +
  #                    labs(title = paste0("Collected Discharge: ", input$Station))))
  #   
  # })
  
}

##### RUN APPLICATION #####
shinyApp(ui = ui, server = server)

