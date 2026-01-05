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
  select(date, contains("overdtr")) |>
  mutate(
    date = date(date),
    date = floor_date(date, "month")
  ) |>
  group_by(date) |>
  summarise(
    overdtryggd_5_ara = mean(overdtryggd_5_ara),
    overdtryggd_10_ara = mean(overdtryggd_10_ara)
  )


# 6.4.2 Peningamál ----
krafa_pm_tbl <- readxl::read_excel(
  "data/raw/PM2025_4_Myndir_Kafli_II.xlsx",
  sheet = "II-3",
  skip = 11
) |>
  janitor::clean_names()

krafa_pm_tbl <- krafa_pm_tbl |>
  rename("date" = "x1") |>
  select(date, contains("overdtr")) |>
  mutate(
    date = date(date),
    date = floor_date(date, "month")
  ) |>
  group_by(date) |>
  summarise(
    overdtryggd_5_ara = mean(overdtryggd_5_ara),
    overdtryggd_10_ara = mean(overdtryggd_10_ara)
  )

krafa_historical_tbl <- krafa_historical_tbl |>
  filter(date <= max(krafa_pm_tbl$date)) |>
  bind_rows(krafa_pm_tbl)

krafa_historical_tbl |>
  write_csv("data/krafa.csv")
