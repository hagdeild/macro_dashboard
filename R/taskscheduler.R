# Schedule tasks

library(taskscheduleR)

# Find the newest R x64 Rscript.exe under the standard install root, rather than
# hardcoding a version. Several R majors are installed side by side on this
# machine and pinned paths drifted out of date during the vidar -> hd account
# migration. Re-run this script after an R upgrade to re-point the tasks.
find_newest_rscript <- function() {
  roots <- file.path(Sys.getenv("PROGRAMFILES"), "R")
  dirs <- list.dirs(roots, recursive = FALSE)
  dirs <- dirs[grepl("R-\\d+\\.\\d+\\.\\d+$", basename(dirs))]
  if (length(dirs) == 0) stop("No R installation found under ", roots)
  ver <- numeric_version(sub("^R-", "", basename(dirs)))
  newest <- dirs[order(ver, decreasing = TRUE)][1]
  rscript <- file.path(newest, "bin", "x64", "Rscript.exe")
  if (!file.exists(rscript)) stop("Rscript.exe not found at ", rscript)
  # Return the 8.3 SHORT path (C:/PROGRA~1/...). This is essential, not
  # cosmetic. taskscheduleR interpolates this path *unquoted* into the string it
  # hands to `schtasks /TR`, so a long path containing "Program Files" splits on
  # the space and schtasks fails with a misleading "ERROR: Access is denied."
  # The registration scripts this replaced hardcoded "C:/PROGRA~1/..." for
  # exactly this reason; shortPathName() keeps that property while still
  # auto-detecting the version.
  gsub("\\\\", "/", utils::shortPathName(rscript))
}

rexe <- find_newest_rscript()
message("Scheduling tasks with: ", rexe)

# schtasks parses `startdate` with the machine's short-date locale, which is
# en-US (M/d/yyyy) on vr-hagreiknivel. The original "%d.%m.%Y" here fails.
startdate <- format(Sys.Date(), "%m/%d/%Y")

# The two tasks do different jobs and are not redundant: get_data.R (weekly) is
# the heavy full rebuild of every series; macro_data_update.R (daily) only
# appends the two series that have to be scraped every business day -- the yield
# curve from lanamal.is and gengisvisitala from sedlabanki.is. See workflow.md.

# Macro data daily update
taskscheduler_create(
  taskname = "macro_data_update",
  rscript = "c:/Users/hd/Rwd/macro_dashboard/R/macro_data_update.R",
  schedule = "DAILY",
  starttime = "20:00",
  startdate = startdate,
  # /F overwrites an existing task of the same name. Without it schtasks asks
  # "already exists, replace? (Y/N)", which nothing can answer under Rscript,
  # so re-registration silently fails.
  schtasks_extra = "/F",
  Rexe = rexe
)


# weekly update of dashbaord main data prep
taskscheduler_create(
  taskname = "macro_full_data",
  rscript = "c:/Users/hd/Rwd/macro_dashboard/R/get_data.R",
  schedule = "WEEKLY",
  starttime = "17:00",
  days = "MON",
  startdate = startdate,
  # /F overwrites an existing task of the same name. Without it schtasks asks
  # "already exists, replace? (Y/N)", which nothing can answer under Rscript,
  # so re-registration silently fails.
  schtasks_extra = "/F",
  Rexe = rexe
)
