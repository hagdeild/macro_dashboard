# Hjálparföll

library(rvest)
library(httr)
library(stringr)
library(tidyverse)

#' Get Iceland's current central bank rate
#'
#' @return A tibble with two columns: date (today) and cbrates (current rate)
#' @export
#' @examples
#' get_iceland_rate()
get_iceland_rate <- function() {
  # Fetch page
  response <- GET(
    "https://www.cbrates.com/",
    add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    ),
    timeout(30)
  )

  if (status_code(response) != 200) {
    stop("Failed to fetch page. Status: ", status_code(response))
  }

  # Get raw content and convert with encoding handling
  raw_content <- content(response, as = "raw")
  html_content <- iconv(
    rawToChar(raw_content),
    from = "UTF-8",
    to = "UTF-8",
    sub = "byte"
  )

  # Parse HTML
  page <- read_html(html_content)

  # Extract table rows
  rows <- page %>% html_nodes("tr")

  # Find Iceland's rate
  iceland_rate <- NA_real_

  for (i in seq_along(rows)) {
    cells <- rows[[i]] %>% html_children() %>% html_text(trim = TRUE)
    cells <- cells[nchar(cells) > 0]

    # Check if this row contains Iceland
    if (any(str_detect(cells, "Iceland"))) {
      # Find the rate (pattern: XX.XX %)
      rate_cell <- str_subset(cells, "^\\d+\\.\\d+\\s*%$")
      if (length(rate_cell) > 0) {
        iceland_rate <- as.numeric(str_extract(rate_cell[1], "\\d+\\.?\\d*"))
        break
      }
    }
  }

  if (is.na(iceland_rate)) {
    stop("Could not find Iceland's rate on the page")
  }

  # Return tibble with today's date and the rate
  result <- tibble(
    date = Sys.Date(),
    cbrates = iceland_rate
  )

  return(result)
}

# Lánamál

scrape_lanamal <- function() {
  url <- "https://lanamal.is/"

  # Fetch page with proper encoding handling
  response <- GET(url, timeout(30))

  if (status_code(response) != 200) {
    stop("Failed to fetch page. Status: ", status_code(response))
  }

  # Parse HTML with explicit UTF-8 encoding
  page <- content(response, as = "text", encoding = "UTF-8") |>
    read_html()

  # Find the non-indexed bonds section
  bond_names <- url |>
    read_html() |>
    html_nodes("td.fixed-width") |>
    html_text() |>
    str_trim()

  kaup <- url |>
    read_html() |>
    html_nodes(".text-center+ .text-center") |>
    html_text() |>
    str_trim() |>
    str_replace(",", ".") |>
    as.numeric() |>
    na.omit() |>
    as.numeric()

  krafa <- url |>
    read_html() |>
    html_nodes("td.text-right") |>
    html_text() |>
    str_trim() |>
    str_replace(",", ".") |>
    as.numeric()

  # Create tibble
  bonds <- tibble(
    date = Sys.Date(),
    bond = bond_names,
    kaup = kaup,
    krafa = krafa
  ) %>%
    filter(!is.na(krafa))

  # Parse maturity date from bond name
  bonds <- bonds %>%
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
    ) %>%
    select(date, bond, maturity_date, ttm, kaup, krafa)

  return(bonds)
}


# Function to calculate yield using Nelson-Siegel formula
ns_yield <- function(tau, beta_0, beta_1, beta_2, lambda) {
  term1 <- (1 - exp(-lambda * tau)) / (lambda * tau)
  term2 <- term1 - exp(-lambda * tau)
  beta_0 + beta_1 * term1 + beta_2 * term2
}

# Main function to calculate 5 and 10 year yields
calculate_yields <- function(bonds_df) {
  # Fit Nelson-Siegel curve
  ns_fit <- Nelson.Siegel(
    rate = bonds_df$krafa,
    maturity = bonds_df$ttm
  )

  # Extract parameters
  beta_0 <- ns_fit[1, "beta_0"]
  beta_1 <- ns_fit[1, "beta_1"]
  beta_2 <- ns_fit[1, "beta_2"]
  lambda <- ns_fit[1, "lambda"]

  # Calculate 5 and 10 year yields
  yield_5y <- ns_yield(5, beta_0, beta_1, beta_2, lambda)
  yield_10y <- ns_yield(10, beta_0, beta_1, beta_2, lambda)

  # Return results
  results <- tibble(
    date = unique(bonds_df$date),
    yield_5y = yield_5y,
    yield_10y = yield_10y,
    beta_0 = beta_0,
    beta_1 = beta_1,
    beta_2 = beta_2,
    lambda = lambda
  )

  return(results)
}

# Complete daily update function
daily_yield_update <- function() {
  # Scrape today's data
  bonds_today <- scrape_lanamal()

  # Calculate yields
  yields_today <- calculate_yields(bonds_today) |>
    select(date, yield_5y, yield_10y) |>
    set_names("date", "overdtryggd_5_ara", "overdtryggd_10_ara")

  # # Append to historical file
  # if(file.exists(output_file)) {
  #   #historical <- read_csv(output_file, show_col_types = FALSE)
  #   updated <- bind_rows(historical, yields_today) %>%
  #     distinct(date, .keep_all = TRUE) %>%
  #     arrange(date)
  # } else {
  #   updated <- yields_today
  # }

  return(list(bonds = bonds_today, yields = yields_today))
}
