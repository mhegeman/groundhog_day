# library(tidyverse)
# library(rvest)
# library(jsonlite)
#
# # Read the webpage
# url <- "https://www.groundhog.org/groundhog-day/history-past-predictions/"
# page <- read_html(url)
#
# # Extract the table
# groundhog_table <- page %>%
#   html_element("table") %>% # Gets the first table on the page
#   html_table() |>
#   janitor::clean_names() |>
#   mutate(
#     prediction_clean = case_when(
#       str_detect(tolower(prediction), "more winter") ~ "More winter",
#       str_detect(tolower(prediction), "early spring") ~ "Early spring",
#       TRUE ~ "Other"
#     ),
#     x = case_when(
#       prediction_clean == "More winter" ~ 1L,
#       prediction_clean == "Early spring" ~ -1L,
#       TRUE ~ 0L
#     )
#   ) |>
#   select(-prediction)
#
# # View the result
# groundhog_table
#
# write_csv(groundhog_table, "data/groundhog_day.csv")
#
#
# url2 <- "https://groundhog-day.com/api/v1/groundhogs?country=usa"
# data <- fromJSON(url2)
#
# groundhogs <- data$groundhogs %>%
#   select(-predictions) %>%
#   as_tibble()
#
#
# predictions <- data$groundhogs %>%
#   select(id, slug, name, predictions) %>%
#   unnest(predictions) %>%
#   as_tibble() %>%
#   mutate(prediction = case_when(shadow == 1 ~ "More winter", shadow == 0 ~ "Early spring", TRUE ~ "Uncertain"))
#
#
# write_csv(groundhogs, "data/all_groundhogs.csv")
# write_csv(predictions, "data/predictions.csv")
