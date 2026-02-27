# Schedule tasks

library(vr)

schedule_r_script(
  rscript = "macro_dashboard/R/macro_data_update.R",
  taskname = "macro_data_update",
  schedule = "DAILY",
  starttime = "20:00"
)
