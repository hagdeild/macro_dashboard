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
