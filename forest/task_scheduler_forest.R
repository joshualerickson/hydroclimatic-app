library(taskscheduleR)
myscript <- "D:/R_folder/Apps/hca_final/forest/instant_api.R"
taskscheduler_create(taskname = "myfancyscript_2hr", rscript = myscript,startdate = "10/21/2020",
                     schedule = "HOURLY", starttime = "11:35", modifier = 2)

taskscheduler_delete("myfancyscript_2hr")
