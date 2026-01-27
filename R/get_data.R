# Sæki gögn fyrir mælaborð

# 1.0.0 SETUP ----
library(tidyverse)
library(zoo)
library(tempdisagg)
library(readxl)
library(janitor)
library(chromote)
library(rvest)
library(timetk)
library(YieldCurve)
library(fredr)
library(tidyquant)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

source("R/hjalparfoll.R")

history_back <- "2015-01-01"

data_ls <- list()


# 1.0.1 Hjálparföll ----
fix_date <- function(data) {
  data %>%
    mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7)))
}


# 1.1.0 dates ----
year_use <- year(today())
month_use <- month(today())
month_use <- if_else(
  nchar(month_use) == 1,
  paste0("0", month_use),
  as.character(month_use)
)
day_use <- day(today())
day_use <- if_else(
  nchar(day_use) == 1,
  paste0("0", day_use),
  as.character(day_use)
)

base_path <- paste0(year_use, "-", month_use, "-", day_use, "_")

# manudir
man_tbl <- tibble(
  manudir = c(
    "Janúar",
    "Febrúar",
    "Mars",
    "Apríl",
    "Maí",
    "Júní",
    "Júlí",
    "Ágúst",
    "September",
    "Október",
    "Nóvember",
    "Desember"
  ),
  man_no = 1:12
)

# 1.1.0 Sameiginleg gögn ----

# 1.1.1 Mannfjöldi ----
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
  method = "denton-cholette",
  conversion = "average"
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


# 1.1.2 Mannfjöldi á vinnufærum aldri ----
# Vinnufær aldur 16-74 ára líkt og POWA í QMM líkninu
mannfjoldi_vinnufaer_year_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/684f28b4-edf3-4422-8995-8b9a8f505d98"
  ) |>
  set_names("ar", "kyn", "aldur", "mannfjoldi") |>
  group_by(ar) |>
  summarise(mannfjoldi = sum(mannfjoldi))


mannfjoldi_vinnufaer_year_ts <- ts(
  mannfjoldi_vinnufaer_year_tbl$mannfjoldi,
  start = c(1990, 1),
  frequency = 1
)

monthly_vinnufaer_td <- td(
  mannfjoldi_vinnufaer_year_ts ~ 1,
  to = "monthly",
  method = "denton-cholette",
  conversion = "average"
)

mannfjoldi_vinnufaer_monthly_tbl <-
  tibble(
    date = seq.Date(
      from = as.Date("1990-01-01"),
      by = "month",
      length.out = length(predict(monthly_vinnufaer_td))
    ),
    mannfjoldi = as.numeric(predict(monthly_vinnufaer_td))
  )

# 1.1.3 QMM gagnasett Seðlabankans ----
qmm_tbl <- read_excel("data/qmm.xlsx", skip = 1, sheet = 3) |>
  janitor::clean_names()

qmm_tbl <- qmm_tbl |>
  slice(-c(1:3)) |>
  rename("date" = "x1") |>
  mutate(
    date = date(as.yearqtr(date)),
    across(where(is.character), as.numeric)
  )

data_ls$qmm <- qmm_tbl

# 1.1.4 Vísitala neysluverðs ----
vnv_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/50d1a327-4780-42cf-9f89-57d7b9e4ff8b"
  ) |>
  clean_names() |>
  fix_date() |>
  select(-manudur) |>
  set_names("vnv", "date") |>
  mutate(vnv = vnv / 10)

vnv_qrt_tbl <- qmm_tbl |>
  select(date, cpi)

# 1.1.5 VLF á nafnvirði ----
gdp_nominal_qrt_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/e5cc940e-34fd-40da-b1d3-4b83e803e626"
  ) |>
  select(-2) |>
  set_names("date", "gdp_q_nominal") |>
  mutate(
    date = str_replace(date, "Á", "Q"),
    date = date(as.yearqtr(date)),
    gdp_q_sum = rollapplyr(gdp_q_nominal, width = 4, FUN = sum, partial = TRUE)
  )

gdp_nominal_yearly_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/e6ee51bd-5313-43e0-a325-cc940b2a87b9",
    locale = locale(encoding = "latin1")
  ) |>
  select(-2) |>
  set_names("date", "gdp_nominal")

# * ----

# 2.0.0 MACRO/FISCAL ----
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

# Landsframleiðsla
gdp_tbl <- gdp_comp_tbl |>
  filter(skipting == "Verg Landsframleiðsla") |>
  select(date, value) |>
  rename("gdp" = "value")

data_ls$gdp <- gdp_tbl

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

# 2.2.0 Viðskiptajöfnuður ----
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
  select(date, vidskiptajofnudur) |>
  left_join(gdp_tbl) |>
  drop_na() |>
  mutate(hlutfall = vidskiptajofnudur / gdp)

data_ls$vidskiptajofnudur <- vidskiptajofnudur_tbl


# 2.3.0 Undirliggjandi verðbólga ----

# eldri
vnv_undirliggjandi_eldri_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/aa9cd45e-3e8b-4deb-9d48-ac20afa7636b",
    na = "."
  ) |>
  drop_na() |>
  clean_names() |>
  select(-lidur) |>
  mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7))) |>
  select(-manudur) |>
  set_names("visitala", "gildi", "date")


# ný
vnv_undirliggjandi_nyrri_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/6ab25269-7342-4a08-b850-e677f94e0bab"
  ) |>
  clean_names() |>
  select(-lidur) |>
  mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7))) |>
  select(-manudur) |>
  set_names("visitala", "gildi", "date")

# Merge the two datasets
# First, standardize visitala names
vnv_undirliggjandi_eldri_tbl <- vnv_undirliggjandi_eldri_tbl |>
  mutate(
    visitala = case_when(
      str_detect(visitala, "Kjarnavísitala 1") ~ "Kjarnavísitala_1",
      str_detect(visitala, "Kjarnavísitala 2") ~ "Kjarnavísitala_2",
      str_detect(visitala, "Kjarnavísitala 3") ~ "Kjarnavísitala_3",
      TRUE ~ "Kjarnavísitala_4"
    )
  )

vnv_undirliggjandi_nyrri_tbl <- vnv_undirliggjandi_nyrri_tbl |>
  mutate(
    visitala = case_when(
      str_detect(visitala, "Kjarnavisitala_1") ~ "Kjarnavísitala_1",
      str_detect(visitala, "Kjarnavisitala_2") ~ "Kjarnavísitala_2",
      str_detect(visitala, "Kjarnavisitala_4") ~ "Kjarnavísitala_4",
      str_detect(visitala, "Kjarnavisitala_5") ~ "Kjarnavísitala_5",
      TRUE ~ visitala
    )
  )

# Chain the new series to the old one
# Find the first date of the new series to use as cutoff
first_date_new <- min(vnv_undirliggjandi_nyrri_tbl$date)

# Get the old series value at the last date BEFORE the new series starts
scaling_factors_tbl <- vnv_undirliggjandi_eldri_tbl |>
  filter(date < first_date_new) |>
  group_by(visitala) |>
  filter(date == max(date)) |>
  select(visitala, last_value_old = gildi, last_date_old = date) |>
  ungroup() |>
  left_join(
    vnv_undirliggjandi_nyrri_tbl |>
      filter(date == first_date_new) |>
      select(visitala, first_value_new = gildi),
    by = "visitala"
  ) |>
  mutate(scaling_factor = last_value_old / first_value_new)

# Apply scaling to new series
vnv_undirliggjandi_nyrri_scaled_tbl <- vnv_undirliggjandi_nyrri_tbl |>
  left_join(
    scaling_factors_tbl |> select(visitala, scaling_factor),
    by = "visitala"
  ) |>
  mutate(
    gildi = if_else(is.na(scaling_factor), gildi, gildi * scaling_factor)
  ) |>
  select(-scaling_factor)

# Combine: old series up to (but not including) the new series start date,
# then the scaled new series
vnv_undirliggjandi_tbl <- bind_rows(
  vnv_undirliggjandi_eldri_tbl |> filter(date < first_date_new),
  vnv_undirliggjandi_nyrri_scaled_tbl
) |>
  arrange(visitala, date)

data_ls$undirliggjandi_verdbolga <- vnv_undirliggjandi_tbl


# 2.4.0 Verðbólguvæntingar -----
get_infl_exp <- function(exp_path, rows, sheet_name, date_from) {
  read_excel(exp_path, sheet = sheet_name) |>
    slice(rows) |>
    pivot_longer(cols = everything()) |>
    select(2) |>
    slice(-1) |>
    mutate(
      date = seq.Date(
        from = as.Date(date_from),
        length.out = nrow(cur_data()),
        by = "quarter"
      ),
      value = as.numeric(value) / 100
    ) |>
    drop_na()
}

exp_path_1 <- "data/expectations/Verdbolguvaentingar-a-mismunandi-maelikvarda.xlsx"
exp_path_2 <- "data/expectations/Vaentingar_markadsadila.xlsx"

# 2.4.1 heimili ----
infl_exp_heimili_tbl <- get_infl_exp(
  exp_path = exp_path_1,
  rows = 9,
  sheet_name = "Heimili_Households",
  date_from = "2003-01-01"
) |>
  mutate(key = "Heimili")

# 2.4.2 fyrirtæki ----
infl_exp_fyrirtaeki_tbl <- get_infl_exp(
  exp_path = exp_path_1,
  rows = 9,
  sheet_name = "Fyrirtæki_Businesses",
  date_from = "2003-01-01"
) |>
  mutate(key = "Fyrirtæki")

# 2.4.3 markaðsaðilar ----
infl_exp_markadsadilar_tbl <- read_excel(
  exp_path_2,
  sheet = "III-a"
) |>
  slice(9) |>
  select(-1) |>
  pivot_longer(cols = everything()) |>
  select(-name) |>
  mutate(
    date = seq.Date(
      from = as.Date("2012-01-01"),
      length.out = nrow(cur_data()),
      by = "quarter"
    ),
    value = as.numeric(value) / 100
  ) |>
  mutate(key = "Markaðsaðilar")


# 2.4.4 sameina ----
infl_exp_tbl <- bind_rows(
  infl_exp_heimili_tbl,
  infl_exp_fyrirtaeki_tbl,
  infl_exp_markadsadilar_tbl
)

data_ls$verdbolgu_vaentingar <- infl_exp_tbl

# 2.4.5 skuldabréfamarkaður ----
infl_exp_breakeven_tbl <- read_excel(
  exp_path_1,
  sheet = "Verðbólguálag_Breakeven rates"
) |>
  slice(4:7) |>
  pivot_longer(cols = -1) |>
  select(-name) |>
  set_names("bref", "value") |>
  mutate(value = as.numeric(value) / 100) |>
  pivot_wider(names_from = bref, values_from = value) |>
  unnest(everything()) |>
  set_names(
    "Verðbólguálag til 1 árs",
    "Verðbólguálag til 2 ára",
    "Verðbólguálag til 5 ára",
    "Verðbólguálag til 10 ára"
  ) |>
  mutate(
    date = seq.Date(
      from = as.Date("2003-01-01"),
      length.out = nrow(cur_data()),
      by = "quarter"
    ),
  ) |>
  pivot_longer(cols = -date) |>
  rename("key" = "name")

data_ls$verdbolga_vaentingar_skuldabref <- infl_exp_breakeven_tbl


# 2.5.0 Erlend staða þjóðarbúsins ----
erlend_stada_tbl <-
  read_excel(
    "data/ErlendStadaThjodarbusinsISL.xlsx",
    sheet = "Lóðrétt",
    skip = 5
  ) |>
  select(1:2, "Hrein staða þjóðarbúsins án ILST í slita.") |>
  set_names("date", "erlend_stada", "stada_an_slita") |>
  mutate(
    erlend_stada = if_else(stada_an_slita != 0, stada_an_slita, erlend_stada)
  ) |>
  mutate(date = date(as.yearqtr(date))) |>
  select(date, erlend_stada)

erlend_stada_tbl <- erlend_stada_tbl |>
  left_join(gdp_nominal_qrt_tbl) |>
  mutate(erlend_stada_hlutfall = erlend_stada / gdp_q_sum)

data_ls$erlend_stada <- erlend_stada_tbl

# 2.6.0 Skuldir ríkisins ----
# https://sedlabanki.is/frettir-og-utgefid-efni/grein/hagvisar-sedlabanka-islands-22-desember-2025

public_debt_tbl <-
  read_excel(
    "data/HV_Tolur_i_myndir_V_Opinber_fjarmal.xlsx",
    sheet = "V-12",
    skip = 11
  ) |>
  rename("date" = "...1") |>
  pivot_longer(cols = -date) |>
  drop_na()

public_debt_tbl <- public_debt_tbl |>
  mutate(
    date = parse_number(date),
    date = if_else(date >= 80, paste0(19, date), paste0(20, date))
  )

data_ls$public_debt <- public_debt_tbl

# * -----
# 3.0.0 VINNUMARKAÐURINN ----

# 3.0.1 vmst ----
# Næ í nýjustu upplýsingar af heimasíðu Vinnumálastofnunnar

vmst_page <- read_html(
  "https://island.is/s/vinnumalastofnun/maelabord-og-toelulegar-upplysingar"
)

vmst_url <- vmst_page |>
  html_nodes("a") |>
  html_attr("href") |>
  str_subset("\\.xlsm$") |>
  first()

temp_file <- tempfile(fileext = ".xlsm")
download.file(vmst_url, temp_file, mode = "wb")

# 3.1.0 Hlutfall starfandi ----
starfandi_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/8e6f494f-efba-4d10-9b09-731f29bf2dde"
) |>
  set_names("date", "starfandi") |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  left_join(mannfjoldi_vinnufaer_monthly_tbl) |>
  mutate(hlutfall_starfandi = starfandi / mannfjoldi) |>
  drop_na()

data_ls$starfandi <- starfandi_tbl

# 3.2.0 Atvinnuleysi ----
# Nota ekki atvinnuleysi eins og Vinnumálastofnun reiknar.
# Nota sem hlutfall af vinnuafli sem gefur ranga mynd.
# Laga til minnkað starfshlutfall, fæ þetta sem hlutfall og
# nota svo á mína útreikninga

# Minnkað starfshlutfall
minnkad_starfshlutfall_tbl <- tibble::tibble(
  date = as.Date(c(
    "2020-04-01",
    "2020-05-01",
    "2020-06-01",
    "2020-07-01",
    "2020-08-01",
    "2020-09-01",
    "2020-10-01",
    "2020-11-01",
    "2020-12-01",
    "2021-01-01",
    "2021-02-01",
    "2021-03-01",
    "2021-04-01",
    "2021-05-01"
  )),
  minnkad_starfshlutfall = c(
    0.103,
    0.056,
    0.021,
    0.009,
    0.009,
    0.008,
    0.012,
    0.014,
    0.014,
    0.012,
    0.011,
    0.011,
    0.011,
    0.009
  )
)


# 3.2.1 Lagfæring vegna minnkaðst starfshlutfalls ----

vmst_unemp_tbl <- read_excel(
  temp_file,
  sheet = "G2",
  range = "C7:RA9"
) |>
  slice(2) |>
  pivot_longer(cols = everything()) |>
  mutate(
    date = seq.Date(
      from = as.Date("2000-02-01"),
      by = "month",
      length.out = nrow(cur_data())
    )
  ) |>
  drop_na() |>
  select(date, value)

# bæti við minnkuðu
minnkad_ratio_tbl <- vmst_unemp_tbl |>
  left_join(minnkad_starfshlutfall_tbl) |>
  mutate(
    minnkad_starfshlutfall = if_else(
      is.na(minnkad_starfshlutfall),
      0,
      minnkad_starfshlutfall
    ),
    minnkad_ratio = 1 - minnkad_starfshlutfall / value
  ) |>
  select(date, minnkad_ratio)


# 3.2.2 Atvinnuleysi ----
atvinnuleysi_tbl <- read_excel(
  temp_file,
  sheet = "G3",
  range = "D6:RA8"
) |>
  slice(2) |>
  pivot_longer(cols = everything()) |>
  mutate(
    date = seq.Date(
      from = as.Date("2000-02-01"),
      by = "month",
      length.out = nrow(cur_data())
    )
  ) |>
  drop_na() |>
  select(date, value) |>
  rename("atvinnulausir" = "value") |>
  left_join(mannfjoldi_vinnufaer_monthly_tbl) |>
  mutate(atvinnuleysi = atvinnulausir / mannfjoldi)


# laga minnkað starfshlutfall
atvinnuleysi_tbl <- atvinnuleysi_tbl |>
  left_join(minnkad_ratio_tbl) |>
  mutate(atvinnuleysi = atvinnuleysi * minnkad_ratio) |>
  select(date, atvinnuleysi)

data_ls$atvinnuleysi <- atvinnuleysi_tbl


# 3.2.3 Atvinnuleysi eftir lengd ----
atvinnuleysi_lengd_tbl <- read_excel(
  temp_file,
  sheet = "G4",
  range = "B40:RA43"
)

atvinnuleysi_langtima_tbl <- atvinnuleysi_lengd_tbl |>
  janitor::clean_names() |>
  select(-x2) |>
  pivot_longer(cols = -allir) |>
  drop_na() |>
  select(-name) |>
  pivot_wider(names_from = allir, values_from = value) |>
  unnest(everything()) |>
  mutate(
    date = seq.Date(
      from = as.Date("2000-02-01"),
      by = "month",
      length.out = nrow(cur_data())
    )
  ) |>
  pivot_longer(cols = -date) |>
  filter(str_detect(name, "\\+12")) |>
  select(date, value) |>
  left_join(mannfjoldi_monthly_tbl) |>
  mutate(atvinnuleysi = value / mannfjoldi) |>
  drop_na() |>
  left_join(minnkad_ratio_tbl) |>
  mutate(atvinnuleysi = atvinnuleysi * minnkad_ratio) |>
  select(date, atvinnuleysi)

data_ls$atvinnuleysi_langtima <- atvinnuleysi_langtima_tbl

# 3.2.4 atvinnuleysi ungs fólks ----
# sjá til

# 3.2.5 Laus störf ----
laus_storf_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/c57c066c-dea4-4e3f-b816-cb6b95678a9d"
  ) |>
  select(-c(2, 5)) |>
  set_names("date", "fjoldi_starfa", "laus_storf") |>
  mutate(
    date = str_replace(date, "Á", "Q"),
    date = date(as.yearqtr(date))
  )

data_ls$laus_storf <- laus_storf_tbl

# 3.2.6 Vinnulitlir ----
vinnulitlir_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/570ecedc-2d61-48f0-8723-0c448065b94d"
  ) |>
  janitor::clean_names() |>
  select(arsfjordungur, eining, contains("mannfjoldi")) |>
  set_names("date", "eining", "fjoldi") |>
  pivot_wider(names_from = eining, values_from = fjoldi) |>
  pivot_longer(cols = -c(date, contains("Mannfjöldi"))) |>
  set_names("date", "mannfjoldi", "eining", "value")

vinnulitlir_tbl <- vinnulitlir_tbl |>
  mutate(
    hlutfall = value / mannfjoldi,
    date = date(as.yearqtr(str_replace(date, "Á", "Q")))
  )

data_ls$vinnulitlir <- vinnulitlir_tbl

# 3.2.7 Framleiðni ----
framleidni_tbl <- qmm_tbl |>
  select(date, prod) |>
  drop_na()

data_ls$framleidni <- framleidni_tbl

# 3.2.8 hlutfall erlendra af fjölda starfandi ----
hlutfall_erlendir_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/4c22bb0f-e459-418d-87e5-bea637a72303"
  ) |>
  set_names("date", "alls", "erlendir") |>
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    hlutfall = erlendir / alls
  )

data_ls$starfandi_erlendir <- hlutfall_erlendir_tbl

# * -----

# 4.0.0 LAUN ----

# 4.1.0 Launavísitala Hagstofu ----
laun_hagstofa_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/a98a5289-5a3c-402e-b345-fc0678bd3962"
  ) |>
  set_names("date", "launavisitala") |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)))

data_ls$launavisitala <- laun_hagstofa_tbl

# 4.2.0 Laun starfsstétt ----
laun_starfstett_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/ba7c3a6f-685a-4897-8ccc-7fd9c3859ece"
  ) |>
  janitor::clean_names() |>
  fix_date() |>
  select(date, starfsstett, contains("visitolur")) |>
  set_names("date", "starfsstett", "visitala") |>
  arrange(date, starfsstett) |>
  group_by(starfsstett) |>
  mutate(visitala = visitala / visitala[1] * 100) |>
  ungroup()

data_ls$laun_starfsstett <- laun_starfstett_tbl

# 4.3.0 Launþegahópar ----
laun_hopar_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/10a91d93-f14f-42d3-9c35-c5f6800d077c"
  ) |>
  clean_names() |>
  fix_date() |>
  select(date, launthegahopur, contains("visitolur")) |>
  set_names("date", "hopur", "visitala") |>
  arrange(date, hopur) |>
  group_by(hopur) |>
  mutate(visitala = visitala / visitala[1] * 100) |>
  ungroup()

data_ls$laun_hopur <- laun_hopar_tbl


# 4.4.0 Atvinnugrein ----
laun_atvinnugrein_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/90f7a12f-a610-4135-9bbd-81b0e0310902"
  ) |>
  clean_names() |>
  fix_date() |>
  select(date, atvinnugrein, contains("visitolur")) |>
  set_names("date", "atvinnugrein", "visitala") |>
  mutate(
    visitala = as.numeric(visitala),
    atvinnugrein = str_remove(atvinnugrein, "\\s*\\([^)]+\\)$")
  ) |>
  arrange(date, atvinnugrein) |>
  group_by(atvinnugrein) |>
  mutate(visitala = visitala / visitala[1] * 100) |>
  ungroup() |>
  drop_na()

data_ls$laun_atvinnugrein <- laun_atvinnugrein_tbl

# 4.5.0 Ráðstöfunartekjur ----
radstofunartekjur_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/26e59868-9cb6-4272-be9e-338502460692",
    na = ".."
  ) |>
  drop_na()

radstofunartekjur_tbl <- radstofunartekjur_tbl |>
  set_names("date", "value") |>
  mutate(
    date = date(as.yearqtr(str_replace(date, "Á", "Q")))
  ) |>
  left_join(vnv_qrt_tbl) |>
  mutate(value = value / cpi) |>
  select(-cpi) |>
  mutate(value = value / value[1] * 100)

data_ls$radstofunartekjur <- radstofunartekjur_tbl

# * -----

# 5.0.0 HÚSNÆÐISMARKAÐURINN ----

# 5.1.0 Leiguverð ----
# Gögn: https://hms.is/gogn-og-maelabord/visitolur

leiguverd_tbl <- read_csv(
  "https://frs3o1zldvgn.objectstorage.eu-frankfurt-1.oci.customer-oci.com/n/frs3o1zldvgn/b/public_data_for_download/o/leiguvisitala.csv"
) |>
  clean_names() |>
  mutate(date = make_date(ar, as.numeric(manudur))) |>
  rename("leiguverd" = "visitala") |>
  select(date, leiguverd)

data_ls$leiguverd <- leiguverd_tbl


# 5.2.0 Íbúðaverð ----
kaupverd_tbl <- read_csv(
  "https://frs3o1zldvgn.objectstorage.eu-frankfurt-1.oci.customer-oci.com/n/frs3o1zldvgn/b/public_data_for_download/o/kaupvisitala.csv"
) |>
  clean_names() |>
  mutate(date = make_date(ar, as.character(manudur))) |>
  select(-c(ar, manudur, utgafudagur)) |>
  select(date, everything()) |>
  pivot_longer(-date)

data_ls$kaupverd <- kaupverd_tbl

# 5.3.0 Kaupsrká fasteigna ----
# kaupskra_tbl <- read_csv2("data/kaupskra.csv", locale = locale(encoding = "latin1")) |>
#   clean_names() |>
#   mutate(date = floor_date(utgdag, "month"))

# kaupskra_fjoldi_tbl <- kaupskra_tbl |>
#   group_by(date) |>
#   summarise(fjoldi = n_distinct(faerslunumer))

# 5.4.0 Týpískt leiguverð ----
# Scrapa gögn af myigloo.
# Forsendur: 80-100fm, 2-3 herbergi, íbúð

#myigloo_url <- "https://myigloo.is/listings?min_rooms=2&max_rooms=3&min_size=80&max_size=100&listing_type=1&sw_lat=64.03228552326259&sw_lng=-21.997847324902345&ne_lat=64.21971434210539&ne_lng=-21.707396275097658&order_by=-published_at"
myigloo_url <- "https://myigloo.is/listings?min_size=80&max_size=120&listing_type=1&order_by=-published_at"

# Start Chrome session
b <- ChromoteSession$new()

# Navigate to the page
b$Page$navigate(myigloo_url)
b$Page$loadEventFired() # Wait for page load
Sys.sleep(3) # Extra wait for JS rendering

# Function to scroll and load content
scroll_and_load <- function(session, n_scrolls = 5, pause = 2) {
  for (i in seq_len(n_scrolls)) {
    session$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(pause)
    message(glue::glue("Scroll {i} of {n_scrolls} complete"))
  }
}

# Scroll to load more listings
scroll_and_load(b, n_scrolls = 40, pause = 2)

# Get the page HTML
html_result <- b$Runtime$evaluate("document.documentElement.outerHTML")
page_html <- read_html(html_result$result$value)

# Extract data using your CSS selectors
myigloo_data <- page_html |>
  html_elements(".text-color .text-muted-color , .p-tag-label") |>
  html_text2()

myigloo_data <- myigloo_data[!myigloo_data == "New"]

myigloo_tbl <- myigloo_data |>
  matrix(ncol = 2, byrow = TRUE) |>
  as.data.frame() |>
  as_tibble() |>
  set_names("verd", "stadur") |>
  mutate(
    verd = str_replace(verd, ",", ""),
    verd = str_remove(verd, " kr"),
    verd = as.numeric(verd)
  )

# Clean up
b$close()

data_ls$myigloo <- myigloo_tbl

# * -----

# 6.0.0 FJÁRMÁLAMARKAÐURINN ----

# 6.1.0 Hlutabréfaverð ----

# 6.1.1 OMX15

# one time
# omxi15_hist_tbl <- data.table::fread("data/raw/omxi15.csv") |>
#   as_tibble() |>
#   clean_names() |>
#   select(date, closing_price)

# omxi15_hist_tbl <- omxi15_hist_tbl |>
#   set_names("date", "price") |>
#   mutate(
#     price = as.numeric(str_remove(price, ",")),
#     date = date(date)
#   ) |>
#   arrange(date)

omxi15_hist_tbl <- read_csv("data/raw/omxi15.csv")


omx15_tbl <- read_csv(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=NASDAQOMXI15"
) |>
  set_names("date", "price") |>
  fill(price, .direction = "down") |>
  filter(date > max(omxi15_hist_tbl$date))

omx15_tbl <- omx15_tbl |>
  bind_rows(omxi15_hist_tbl)

omx15_tbl |>
  write_csv("data/raw/omxi15.csv")

# 6.1.2 S&P 500
sp500 <- tq_get("^GSPC", from = "2020-01-01") |>
  select(date, close) |>
  set_names("date", "SP500")

# 6.1.3 save stock prices

stocks_tbl <- omx15_tbl |>
  rename("OMXI15" = "price") |>
  left_join(sp500) |>
  drop_na() |>
  pivot_longer(cols = -date)

data_ls$stocks <- stocks_tbl


# 6.2.0 Skuldabréf ----

skuldabref_old_tbl <- read_csv("data/skuldabref.csv")

skuldabref_tbl <- read_html("https://lanamal.is/") |>
  html_nodes(".text-right , .table-striped .text-center , .fixed-width") |>
  html_text() |>
  str_squish() |>
  matrix(ncol = 4, byrow = TRUE) |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("bond", "empty", "price", "yield")) |>
  select(-empty) |>
  mutate(
    price = parse_number(price, locale = locale(decimal_mark = ",")),
    yield = parse_number(yield, locale = locale(decimal_mark = ",")),
    indexation = if_else(str_detect(bond, "RIKB"), "non-index", "indexed"),
    date = today()
  ) |>
  drop_na()

skuldabref_uppdated_tbl <- bind_rows(
  skuldabref_old_tbl,
  skuldabref_tbl
) |>
  distinct()

skuldabref_uppdated_tbl |>
  write_csv("data/skuldabref.csv")

data_ls$skuldabref <- skuldabref_uppdated_tbl

# 6.3.0 Stýrivextir ----
styrivextir_tbl <- read_csv("data/styrivextir.csv")

styrivextir_new_tbl <- get_iceland_rate() |>
  set_names("date", "styrivextir")


styrivextir_upd_tbl <- bind_rows(
  styrivextir_tbl,
  styrivextir_new_tbl
) |>
  pad_by_time(.date_var = "date", .pad_value = NA) |>
  fill(styrivextir, .direction = "down")


data_ls$styrivextir <- styrivextir_upd_tbl

styrivextir_upd_tbl |>
  write_csv("data/styrivextir.csv")


# 6.4.0 Krafa ríkisskuldabréfa ----

krafa_historical_tbl <- read_csv("data/krafa.csv")

krafa_new_ls <- daily_yield_update()

krafa_new_ls$yields |>
  write_csv(paste0("data/lanamal/", base_path, "krafa.csv"))

if (day(today()) == days_in_month(today())) {
  temp_krafa_tbl <- list.files("data/lanamal/") |>
    (\(x) paste0(getwd(), "/data/lanamal/", x))() |>
    map(.f = ~ read_csv(.)) |>
    bind_rows()

  kraf_new_tbl <- temp_krafa_tbl |>
    mutate(date = floor_date(date, "month")) |>
    group_by(date) |>
    mutate(
      overdtryggd_5_ara = mean(overdtryggd_5_ara, na.rm = TRUE),
      overdtryggd_10_ara = mean(overdtryggd_10_ara, na.rm = TRUE)
    )

  krafa_updated_tbl <- krafa_historical_tbl |>
    bind_rows(kraf_new_tbl)

  krafa_updated_tbl |>
    write_csv("data/krafa.csv")

  data_ls$krafa <- krafa_updated_tbl
}

data_ls$krafa <- krafa_historical_tbl


# 6.5.0 Gengi krónunnar ----
gengi_tbl <- read_csv2("data/raw/gengisvisitala.csv") |>
  select(1:2) |>
  set_names("date", "gengi") |>
  mutate(date = dmy(date)) |>
  arrange(date)


b <- ChromoteSession$new()

# Navigate and wait for JS to render
b$Page$navigate(
  "https://sedlabanki.is/gagnatorg/opinber-gengisskraning/#tab-gengisvisitolur"
)
b$Page$loadEventFired()
Sys.sleep(3) # Give JS time to populate the data

# Get rendered HTML
html_result <- b$Runtime$evaluate("document.documentElement.outerHTML")
page_html <- read_html(html_result$result$value)

# Extract the values
gengisvísitala <- page_html |>
  html_node(".currency-data-bar-currency-latest-value") |>
  html_text() |>
  str_replace(",", ".") |>
  as.numeric()

date_text <- page_html |>
  html_node("div[b-vcnxm8ta2s]") |>
  html_text() |>
  str_extract("\\d{2}\\.\\d{2}\\.\\d{4}") |>
  dmy()

# Clean up
b$close()

gengi_new <-
  tibble(
    date = date_text,
    gengi = gengisvísitala
  )

if (!date_text %in% unique(gengi_tbl$date)) {
  gengi_tbl <- gengi_tbl |>
    bind_rows(gengi_new)
}

data_ls$gengi <- gengi_tbl

# 6.6.0 Hrávöruverð ----
commodity_series <- tribble(
  ~series_id     , ~name     ,
  "DCOILBRENTEU" , "brent"   ,
  "PCOFFOTMUSDM" , "coffee"  ,
  "PSUGAISAUSDM" , "sugar"   ,
  "PCOCOUSDM"    , "cocoa"   ,
  "PWHEAMTUSDM"  , "wheat"   ,
  "PBANSOPUSDM"  , "bananas" ,
  "PRICENPQUSDM" , "rice"    ,
  "PMAIZMTUSDM"  , "corn"
)

commodities_tbl <- commodity_series |>
  mutate(data = map(series_id, \(x) fredr(x))) |>
  unnest(data, names_sep = "_") |>
  select(date = data_date, name, value = data_value) |>
  drop_na() |>
  mutate(
    name = recode(
      name,
      "brent" = "Olíuverð",
      "coffee" = "Kaffi",
      "sugar" = "Sykur",
      "cocoa" = "Kakó",
      "wheat" = "Hveiti",
      "bananas" = "Bananar",
      "rice" = "Hrísgrjón",
      "corn" = "Maís"
    )
  )

data_ls$commodities <- commodities_tbl

# * ----

# 7.0.0 FERÐAÞJÓNUSTAN OG KORTAVELTA----

# 7.1.0 Ferðamenn ----
ferdamenn_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/851de9af-0207-44b4-ac0f-0731124053f8"
) |>
  set_names("date", "Íslendingar", "Útlendingar") |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7))) |>
  pivot_longer(cols = -date)

data_ls$ferdamenn <- ferdamenn_tbl


# 7.2.0 Gistinætur ----
gisting_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/10c45e6f-ea41-49ff-90d5-5fae6a444ea0"
) |>
  set_names("manudir", "rikisfang", "ar", "eining", "fjoldi") |>
  select(-eining) |>
  left_join(man_tbl)

gisting_tbl <- gisting_tbl |>
  mutate(date = make_date(ar, man_no)) |>
  select(date, rikisfang, fjoldi)

data_ls$gistinaetur <- gisting_tbl

# 7.3.0 Kortavelta ----
kortavelta_tbl <- read_excel("data/raw/kortavelta.xlsx", sheet = "Sheet1")

# 7.3.1 heimili ----
kortavelta_heimila_tbl <- kortavelta_tbl |>
  slice(8) |>
  select(-c(1:3)) |>
  mutate(across(!where(is.numeric), as.numeric)) |>
  pivot_longer(everything()) |>
  drop_na() |>
  select(value) |>
  mutate(
    date = seq.Date(
      from = as.Date("1997-12-01"),
      by = "month",
      length.out = nrow(cur_data())
    )
  )

data_ls$kortavelta_heimili <- kortavelta_heimila_tbl

# 7.3.2 erlend ----
kortavelta_erlend_tbl <- kortavelta_tbl |>
  slice(62) |>
  select(-c(1:3)) |>
  mutate(across(!where(is.numeric), as.numeric)) |>
  pivot_longer(everything()) |>
  drop_na() |>
  select(value) |>
  mutate(
    date = seq.Date(
      from = as.Date("2002-09-01"),
      by = "month",
      length.out = nrow(cur_data())
    )
  )

data_ls$kortavelta_erlend <- kortavelta_erlend_tbl


# * -----

# 8.0.0 MANNFJÖLDI ----

# 8.1.0 Pýramídi ----
pyramidi_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/dc2cdec7-ee1f-4cc2-9385-7fba68f694ea"
  ) |>
  set_names("country", "aldur", "kyn", "ar", "fjoldi")


cur_max_year <- max(pyramidi_tbl$ar)

pyramidi_tbl <- pyramidi_tbl |>
  filter(ar %in% c(2000, 2010, 2020, cur_max_year)) |>
  mutate(
    fjoldi_plot = if_else(kyn == "Karlar", -fjoldi, fjoldi),
    aldur = fct_inorder(aldur)
  )

data_ls$aldurspyramidi <- pyramidi_tbl

# ggplot(pyramidi_plot_tbl, aes(x = fjoldi_plot, y = aldur, fill = kyn)) +
#   geom_col() +
#   facet_grid(ar ~ country) +
#   scale_x_continuous(
#     labels = \(x) scales::comma(abs(x)),
#     name = "Fjöldi"
#   ) +
#   scale_fill_manual(
#     values = c("Karlar" = "#4575b4", "Konur" = "#d73027"),
#     name = NULL
#   ) +
#   labs(y = NULL, title = "Aldurspýramídi") +
#   theme_minimal() +
#   theme(
#     legend.position = "top",
#     strip.text = element_text(face = "bold")
#   )

# 8.2.0 Aðfluttir umfram brottflutta ----
flutningar_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/57b160b4-1553-49b5-8815-c3246d9c5f8d"
) |>
  set_names("date", "event", "age", "rikisfang", "value") |>
  select(-age) |>
  mutate(date = date(as.yearqtr(str_replace(date, "Á", "Q"))))

data_ls$adfluttir_brottfluttir <- flutningar_tbl


# SAVE DATA ----
data_ls |>
  write_rds("full_data.rds")
