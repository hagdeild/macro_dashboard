# Uppfærsla gagna

# 1.0.0 SETUP ----
library(tidyverse)
library(YieldCurve)
library(chromote)
library(rvest)

source("R/hjalparfoll.R")

# dagsetningar
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

# 2.0.0 KRAFA ----

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

# 3.0.0 GENGISVÍSITALA ----
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

gengi_tbl |>
  write_csv("data/gengisvisitala.csv")

# 4.0.0 MYIGLOO ----

myigloo_url <- "https://myigloo.is/listings?min_size=80&max_size=120&listing_type=1&order_by=-published_at"

b <- ChromoteSession$new()

b$Page$navigate(myigloo_url)
Sys.sleep(3)
#b$Page$loadEventFired()
Sys.sleep(3)

scroll_and_load <- function(session, n_scrolls = 5, pause = 2) {
  for (i in seq_len(n_scrolls)) {
    session$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(pause)
    message(glue::glue("Scroll {i} of {n_scrolls} complete"))
  }
}

scroll_and_load(b, n_scrolls = 40, pause = 2)

html_result <- b$Runtime$evaluate("document.documentElement.outerHTML")
page_html <- read_html(html_result$result$value)

myigloo_data <- page_html |>
  html_elements(".text-color .text-muted-color , .p-tag-label") |>
  html_text2()

myigloo_data <- myigloo_data[!myigloo_data == "New"]

myigloo_new_tbl <- myigloo_data |>
  matrix(ncol = 2, byrow = TRUE) |>
  as.data.frame() |>
  as_tibble() |>
  set_names("verd", "stadur") |>
  mutate(
    verd = str_replace(verd, ",", ""),
    verd = str_remove(verd, " kr"),
    verd = as.numeric(verd),
    date = today()
  )

b$close()

myigloo_historical_tbl <- read_csv("data/myigloo.csv")

myigloo_updated_tbl <- bind_rows(myigloo_historical_tbl, myigloo_new_tbl)

myigloo_updated_tbl |>
  write_csv("data/myigloo.csv")
