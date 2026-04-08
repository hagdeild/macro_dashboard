# Render both language versions of the dashboard

setwd("c:/Users/vidar/Documents/Rwd/macro_dashboard")

# Icelandic version → _site/index.html
system2("quarto", c(
  "render", "index.qmd",
  "-P", "lang:is"
))

# English version → _site/index_en.html
system2("quarto", c(
  "render", "index.qmd",
  "-P", "lang:en",
  "--output", "index_en.html"
))

message("Both language versions rendered successfully.")
