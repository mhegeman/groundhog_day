library(tidyverse)
library(rvest)

# Read the webpage
url <- "https://www.groundhog.org/groundhog-day/history-past-predictions/"
page <- read_html(url)

# Extract the table
groundhog_table <- page %>%
  html_element("table") %>% # Gets the first table on the page
  html_table() |>
  janitor::clean_names() |>
  mutate(
    prediction_clean = case_when(
      str_detect(tolower(prediction), "more winter") ~ "More winter",
      str_detect(tolower(prediction), "early spring") ~ "Early spring",
      TRUE ~ "Other"
    ),
    x = case_when(
      prediction_clean == "More winter" ~ 1L,
      prediction_clean == "Early spring" ~ -1L,
      TRUE ~ 0L
    )
  ) |>
  select(-prediction)

# View the result
groundhog_table

write_csv(groundhog_table, "data/groundhog_day.csv")
