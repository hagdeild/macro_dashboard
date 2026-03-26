# Schedule tasks

library(taskscheduleR)

rexe <- "C:/PROGRA~1/R/R-4.5.0/bin/x64/Rscript.exe"

# Macro data daily update
taskscheduler_create(
  taskname = "macro_data_update",
  rscript = "c:/Users/vidar/Documents/Rwd/macro_dashboard/R/macro_data_update.R",
  schedule = "DAILY",
  starttime = "20:00",
  startdate = format(Sys.Date(), "%d.%m.%Y"),
  Rexe = rexe
)


# weekly update of dashbaord main data prep
taskscheduler_create(
  taskname = "macro_full_data",
  rscript = "c:/Users/vidar/Documents/Rwd/macro_dashboard/R/get_data.R",
  schedule = "WEEKLY",
  starttime = "17:00",
  days = "MON",
  Rexe = rexe
)

