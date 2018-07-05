library(shiny)
currency <- read.csv("all_currencies", stringsAsFactors = FALSE)
head(currency)
plot("currencies")