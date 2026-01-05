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
