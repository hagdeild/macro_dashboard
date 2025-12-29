# Sæki gögn fyrir mælaborð

# 1.0.0 SETUP ----
library(tidyverse)
library(zoo)
library(tempdisagg)
library(readxl)
library(janitor)

history_back <- "2015-01-01"

data_ls <- list()

# 1.0.1 Hjálparföll ----
fix_date <- function(data) {
  data %>%
    mutate(date = make_date(str_sub(manudur, 1, 4), str_sub(manudur, 6, 7)))
}


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

# 2.0.0 ÞJÓÐHAG STÆRÐIR ----
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


# 3.0.0 VINNUMARKAÐURINN ----

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
# Nota sem hlutfall af vinnuafli sem gefur mjög ranga mynd.
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
  "data/vmst.xlsm",
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
  "data/vmst.xlsm",
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
  "data/vmst.xlsm",
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

# 3.2.4 Laus störf ----
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

# 3.2.5 Vinnulitlir ----
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

# 3.2.5 Framleiðni ----
framleidni_tbl <- qmm_tbl |>
  select(date, prod) |>
  drop_na()

data_ls$framleidni <- framleidni_tbl

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

# 5.0.0 HÚSNÆÐISMARKAÐURINN ----

# 5.1.0 Leiguverð ----
