library(tidyverse)
library(gt)

groundhog_table <- read_csv("data/groundhog_day.csv") |>
  filter(year >= 1900)

graph_title <- "Annual Groundhog Day Predictions"
graph_subtitle <- "1900 - 2025"

palette <- c(
  "More winter" = "#4575b4",
  "Early spring" = "#d73027",
  "Uncertain" = "#ffffbf"
)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
)

# Create decade tick labels for the y axis
yrs <- sort(unique(groundhog_table$year))
decade_breaks <- as.character(seq(
  from = 2025,
  to = min(yrs, na.rm = TRUE),
  by = -10
))


# diverging bar chart ----------------------------------------------------

p <- ggplot(groundhog_table, aes(x = x, y = factor(year))) +
  geom_col(aes(fill = factor(x))) +
  scale_x_continuous(
    breaks = c(-1, 0, 1),
    labels = c("-1" = "Early Spring", "0" = "Uncertain", "1" = "More Winter")
  ) +
  scale_fill_manual(
    values = c("-1" = "#d73027", "0" = "#ffffbf", "1" = "#4575b4"),
    labels = c("-1" = "Early Spring", "0" = "Uncertain", "1" = "More Winter"),
    name = "Groundhog Prediction"
  ) +
  scale_y_discrete(
    breaks = decade_breaks,
    labels = decade_breaks
  ) +
  labs(
    title = graph_title,
    subtitle = graph_subtitle,
    x = "",
    y = "Year",
    fill = "Category"
  ) +
  theme(
    legend.position = "none"
  )

p


# prediction table -------------------------------------------------------

prediction_count <- groundhog_table %>%
  group_by(prediction_clean) %>%
  summarise(count = n()) %>%
  select(prediction_clean, count) |>
  ggplot(aes(x = prediction_clean, y = count, fill = prediction_clean)) +
  geom_col() +
  geom_text(
    mapping = aes(x = prediction_clean, y = count, label = count),
    hjust = 0.25,
    vjust = 1.5,
    # nudge_x = -0,
    color = 'white'
  ) +
  scale_fill_manual(values = palette) +
  labs(
    title = "How often is an early spring predicted vs. more winter?",
    x = "",
    y = ""
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.y = element_blank()
  )


prediction_count

# activity heat map ------------------------------------------------------

cols <- 10L

plot_heatmap <- groundhog_table |>
  distinct(year, x) |>
  arrange(desc(year)) |>
  mutate(
    idx = seq_len(n()) - 1L,
    col = (idx %% .env$cols) + 1L,
    row = (idx %/% .env$cols) + 1L,
    pred = case_when(
      x == 1L ~ "More winter",
      x == -1L ~ "Early spring",
      TRUE ~ "Uncertain"
    ),
    text_color = if_else(pred == "Uncertain", "black", "white")
  )


p2 <- ggplot(plot_heatmap, aes(x = col, y = row, fill = pred)) +
  geom_tile(width = 0.95, height = 0.95, color = NA) +
  geom_text(
    aes(label = year, color = text_color),
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_fill_manual(values = palette, guide = "none") +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_reverse(expand = c(0, 0), breaks = NULL) +
  labs(
    title = "Annual Groundhog Day Predictions",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

p2
