# Schedule tasks

library(taskscheduleR)


# Macro data daily update
taskscheduler_create(
  taskname = "macro_data_update",
  rscript = "c:/Users/vidar/Documents/Rwd/macro_dashboard/R/macro_data_update.R",
  schedule = "DAILY",
  starttime = "20:00",
  format(Sys.Date(), "%d.%m.%Y")
)


# weekly update of dashbaord main data prep
taskscheduler_create(
  taskname = "macro_full_data",
  rscript = "c:/Users/vidar/Documents/Rwd/macro_dashboard/R/get_data.R",
  schedule = "WEEKLY",
  starttime = "09:05",
  days = "MON"
)

