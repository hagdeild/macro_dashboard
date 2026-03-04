# Schedule tasks

library(vr)


# Macro data daily update
schedule_r_script(
  rscript = "macro_dashboard/R/macro_data_update.R",
  taskname = "macro_data_update",
  schedule = "DAILY",
  starttime = "20:00"
)


# weekly update of dashbaord main data prep
schedule_r_script(
  rscript = "R/get_data.R",
  taskname = "macro_full_data",
  schedule = "WEEKLY",
  day      = "MON",
  starttime = "09:05"
)
