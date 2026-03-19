# Einskiptis uppsetning á gagnasettum

# 1.0.0 SETUP ----
library(tidyverse)


# 2.0.0 STÝRIVEXTIR ----
styrivextir_tbl <- read_csv2("data/styrivextir.csv") |>
  set_names("date", "styrivextir") |>
  mutate(
    date = dmy(date),
    styrivextir = styrivextir / 100
  )

styrivextir_tbl |>
  write_csv("data/styrivextir.csv")


# 3.0.0 KRAFA RÍKISSKULDABRÉFA ----

# 6.4.1 Hagvísar ----
krafa_historical_tbl <- readxl::read_excel(
  "data/raw/HV_Tolur_i_myndir_VIII_Fjarmalamarkadir.xlsx",
  sheet = "VIII-13",
  skip = 11
) |>
  janitor::clean_names()

krafa_historical_tbl <- krafa_historical_tbl |>
  rename("date" = "x1") |>
  #select(date, contains("overdtr")) |>
  mutate(
    date = date(date),
    date = floor_date(date, "month")
  ) |>
  group_by(date) |>
  summarise(
    overdtryggd_5_ara = mean(overdtryggd_5_ara),
    overdtryggd_10_ara = mean(overdtryggd_10_ara),
    verdtryggd_5_ara = mean(verdtryggd_5_ara),
    verdtryggd_10_ara = mean(verdtryggd_10_ara)
  )


# 6.4.2 Peningamál ----
# krafa_pm_tbl <- readxl::read_excel(
#   "data/raw/PM2025_4_Myndir_Kafli_II.xlsx",
#   sheet = "II-3",
#   skip = 11
# ) |>
#   janitor::clean_names()

# krafa_pm_tbl <- krafa_pm_tbl |>
#   rename("date" = "x1") |>
#   #select(date, contains("overdtr")) |>
#   mutate(
#     date = date(date),
#     date = floor_date(date, "month")
#   ) |>
#   group_by(date) |>
#   summarise(
#     overdtryggd_5_ara = mean(overdtryggd_5_ara),
#     overdtryggd_10_ara = mean(overdtryggd_10_ara),
#     verdtryggd_5_ara = mean(verdtryggd_5_ara),
#     verdtryggd_10_ara = mean(verdtryggd_10_ara)
#   )

# krafa_historical_tbl <- krafa_historical_tbl |>
#   filter(date <= max(krafa_pm_tbl$date)) |>
#   bind_rows(krafa_pm_tbl)

krafa_historical_tbl |>
  write_csv("data/krafa.csv")

# Laga kröfu fyrir 2026 með gögnum af lanamal.is
# villa í aðferðarfræðinni hjá mér þegar ég reiknaði kröfuna fyrir 2026
# með upplýsingum af lanamal.is, tók óvart allt, RIKB og RIKS til að reikna kröfu.

library(YieldCurve)
source("R/hjalparfoll.R")

# Helper to pivot xlsx from wide to long with TTM
pivot_bonds_long <- function(df) {
  df |>
    mutate(date = dmy(date)) |>
    pivot_longer(-date, names_to = "bond", values_to = "krafa") |>
    filter(!is.na(krafa)) |>
    mutate(
      maturity_year = paste0("20", str_sub(bond, 6, 7)),
      maturity_month = str_sub(bond, 9, 10),
      maturity_day = str_sub(bond, 11, 12),
      maturity_date = as.Date(paste(
        maturity_year,
        maturity_month,
        maturity_day,
        sep = "-"
      )),
      ttm = as.numeric(maturity_date - date) / 365.25
    )
}

# Read and pivot both datasets
nominal_long <- readxl::read_excel("data/lanamal/non-index.xlsx") |>
  pivot_bonds_long()
indexed_long <- readxl::read_excel("data/lanamal/index.xlsx") |>
  pivot_bonds_long()

# Fit Nelson-Siegel per date for each type
nominal_yields <- nominal_long |>
  group_by(date) |>
  group_map(~ calculate_yields(.x) |> mutate(date = .y$date)) |>
  bind_rows() |>
  select(date, yield_5y, yield_10y) |>
  set_names("date", "overdtryggd_5_ara", "overdtryggd_10_ara")

indexed_yields <- indexed_long |>
  group_by(date) |>
  group_map(~ calculate_yields(.x) |> mutate(date = .y$date)) |>
  bind_rows() |>
  select(date, yield_5y, yield_10y) |>
  set_names("date", "verdtryggd_5_ara", "verdtryggd_10_ara")

# Combine and aggregate to monthly
krafa_2026_tbl <- nominal_yields |>
  left_join(indexed_yields, by = "date") |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(
    overdtryggd_5_ara = mean(overdtryggd_5_ara, na.rm = TRUE),
    overdtryggd_10_ara = mean(overdtryggd_10_ara, na.rm = TRUE),
    verdtryggd_5_ara = mean(verdtryggd_5_ara, na.rm = TRUE),
    verdtryggd_10_ara = mean(verdtryggd_10_ara, na.rm = TRUE)
  )

# Replace 2026 data in historical and save
krafa_historical_tbl <- krafa_historical_tbl |>
  filter(date < as.Date("2026-01-01")) |>
  bind_rows(krafa_2026_tbl)

krafa_historical_tbl |>
  write_csv("data/krafa.csv")

# Create daily CSV from the xlsx-based daily yields
krafa_daily_tbl <- nominal_yields |>
  left_join(indexed_yields, by = "date") |>
  arrange(date)

write_csv(krafa_daily_tbl, "data/lanamal/krafa_daily.csv")

# Clean up: delete old per-day CSV files
old_daily_files <- list.files(
  "data/lanamal/",
  pattern = "^\\d{4}-\\d{2}-\\d{2}_krafa\\.csv$",
  full.names = TRUE
)
file.remove(old_daily_files)

# Move xlsx files to data/raw/
file.rename("data/lanamal/non-index.xlsx", "data/raw/non-index.xlsx")
file.rename("data/lanamal/index.xlsx", "data/raw/index.xlsx")
