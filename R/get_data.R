# Sæki gögn fyrir mælaborð

# 1.0.0 SETUP ----
library(tidyverse)
library(zoo)
library(tempdisagg)
library(readxl)

history_back <- "2015-01-01"

data_ls <- list()

# 1.1.0 Sameiginleg gögn ----
mannfjoldi_qrt_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/8df08676-27f3-4f88-bdc2-7b2fec97329b"
) |>
  select(1, 3) |>
  set_names("date", "mannfjoldi") |>
  mutate(
    date = str_replace(date, "Á", "Q"),
    date = date(zoo::as.yearqtr(date))
  )


# Convert to ts object (tempdisagg requires this)
mannfjoldi_qrt_ts <- ts(
  mannfjoldi_qrt_tbl$mannfjoldi,
  start = c(2011, 1),
  frequency = 4
)

# Disaggregate to monthly (using Denton-Cholette method)
monthly_td <- td(
  mannfjoldi_qrt_ts ~ 1,
  to = "monthly",
  method = "denton-cholette"
)

# Convert back to tibble
mannfjoldi_monthly_tbl <- tibble(
  date = seq.Date(
    from = as.Date("2011-01-01"),
    by = "month",
    length.out = length(predict(monthly_td))
  ),
  mannfjoldi = as.numeric(predict(monthly_td))
)

# 2.0.0 Landsframleiðsla ----
# Keðjutengt verðmæti, árstíðarleiðrétt

gdp_comp_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/6247de37-c7a0-4070-9c76-a5d011eb2bb9"
) |>
  select(-1) |>
  set_names("skipting", "date", "value")

gdp_comp_tbl <- gdp_comp_tbl |>
  mutate(
    date = str_replace(date, "Á", "Q"),
    date = date(as.yearqtr(date)),
    skipting = str_remove(skipting, "^[0-9\\.]+\\s*"),
    skipting = case_when(
      skipting == "Fjármunamyndun" ~ "Fjárfesting alls",
      skipting == "Atvinnuvegir alls" ~ "Fjárfesting - Einkaaðilar",
      skipting == "Íbúðarhús, alls" ~ "Fjárfesting - Íbúðarhús",
      skipting == "Starfsemi hins opinbera" ~ "Fjárfesting - Opinber",
      skipting == "Innflutningur alls" ~ "Innflutningur",
      skipting == "Útflutningur alls" ~ "Útflutningur",
      skipting == "Þjóðarútgjöld alls" ~ "Þjóðarútgjöld",
      TRUE ~ skipting
    )
  )

data_ls$gdp_comp <- gdp_comp_tbl

# 2.1.0 Á mann ----
# Aðeins landsframleiðsla og einkaneysla

gdp_per_cap_tbl <- gdp_comp_tbl |>
  filter(str_detect(skipting, "Einkaneysla|Verg")) |>
  left_join(mannfjoldi_qrt_tbl) |>
  drop_na() |>
  mutate(value_per_cap = value / mannfjoldi) |>
  filter(date >= history_back) |>
  arrange(skipting, date) |>
  group_by(skipting) |>
  mutate(index = value_per_cap / value_per_cap[1] * 100) |>
  ungroup()

data_ls$gdp_per_cap <- gdp_per_cap_tbl

# 3.0.0 Viðskiptajöfnuður ----
vidskiptajofnudur_tbl <-
  read_excel("data/Greidslujofnudur.xlsx", skip = 5, sheet = "Lóðrétt") |>
  select(1, contains("Viðskiptajöfnuður")) |>
  set_names("date", "vidskiptajofnudur", "an_gomlu") |>
  mutate(date = date(as.yearqtr(date)))

vidskiptajofnudur_tbl <- vidskiptajofnudur_tbl |>
  mutate(
    an_gomlu = if_else(is.na(an_gomlu), 0, an_gomlu),
    vidskiptajofnudur = if_else(an_gomlu == 0, vidskiptajofnudur, an_gomlu)
  ) |>
  select(date, vidskiptajofnudur)

data_ls$vidskiptajofnudur <- vidskiptajofnudur_tbl

#
