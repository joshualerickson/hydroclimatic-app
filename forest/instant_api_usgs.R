
library(tidyverse)
library(purrr)
library(lubridate)
library(googleCloudStorageR)
library(jsonify)
library(httr)
GCS_DEFAULT_BUCKET="joshualerickson"

gcs_auth("gcs_auth_file_shiny.json")

allSitesIDMT <- read_csv("D:/R_folder/Apps/hca_final/hydroclimatic-app/forest/allSitesIDMT.csv")
sites <- unique(allSitesIDMT$site_no)

usgs_download_hourly <- data.frame() 

for (i in seq_along(sites)){
  # loop over selection, and download the data
  tryCatch({usgs_data <- # some feedback on the download progress
                         message(sprintf("Downloading site: %s, with id: %s\n",
                                         allSitesIDMT$site_no[i],
                                         allSitesIDMT$station_nm[i]))
                         
                         # download url (metric by default!)
                         base_url <- paste0(
                           "https://waterservices.usgs.gov/nwis/iv/?format=json&sites=",
                           sites[i],
                           "&period=P7D&parameterCd=00060&siteStatus=all"
                         )
                         
                         # try to download the data
                         error <- httr::GET(url = base_url, 
                                            httr::write_disk(path = file.path(tempdir(),
                                                                              "usgs_tmp.json"),overwrite = TRUE))
                         
                      
                         http_error(error)
                         
                         # read in the usgs data
                         df <- from_json(file.path(tempdir(),"usgs_tmp.json"))
                         df <- df$value$timeSeries$values$value %>% flatten() %>% rbind.data.frame() 
                         df <- df %>% mutate(Station = allSitesIDMT$station_nm[i])
                         
                         usgs_download_hourly <- plyr::rbind.fill(usgs_download_hourly, df)
                      
                         
                       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  }


usgs_download_hourly <- usgs_download_hourly %>% mutate(date = ymd_hms(dateTime),
                                                        value = as.numeric(value)) %>% select(-qualifiers, -dateTime)


gcs_upload(inland_northwest_hourly, bucket = "joshualerickson", name = "inland_northwest_hourly")