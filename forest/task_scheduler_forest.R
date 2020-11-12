library(taskscheduleR)
myscript <- "D:/R_folder/Apps/API folder/api_district.R"

taskscheduler_create(taskname = "myfancyscript_2hr", rscript = myscript,
                     startdate = "11/08/2020", starttime = "11:09",
                     schedule = "HOURLY", modifier = 2)

taskscheduler_delete("myfancyscript_2hr")
getwd()


format(Sys.time() + 62, "%H:%M")

getwd()
file.exists("D:/R_folder/Apps/hca_final/hydroclimatic-app/forest/instant_api.R")
