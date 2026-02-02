library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Groundhog Day",
  sidebar = sidebar(
    p(
      "This app illustrates the history of Puxatawney Phil's Groundhog Day predictions from 1900 to the present."
    )
  ),
  card(
    card_header("How often is an early spring predicted vs. more winter?"),
    plotOutput("count_plot")
  ),
  card(
    card_header("Predictions by Year"),
    selectInput(
      "chart_type",
      "Choose chart",
      choices = c("Heatmap", "Diverging bar"),
      selected = "Heatmap"
    ),
    plotOutput("year_plot")
  )
)


server <- function(input, output, session) {
  # Read the CSV file
  data <- reactive({
    # Check if file exists
    if (file.exists("data/groundhog_day.csv")) {
      read.csv("data/groundhog_day.csv") |>
        filter(year >= 1900)
    } else {
      # Return empty data frame with message if file not found
      data.frame(Message = "groundhog day data not found")
    }
  })

  # Display the data
  output$data_table <- renderTable({
    data()
  })

  # Create ggplot visualization
  output$count_plot <- renderPlot({
    data() %>%
      group_by(prediction_clean) %>%
      summarise(count = n()) %>%
      select(prediction_clean, count) |>
      ggplot(aes(x = prediction_clean, y = count, fill = prediction_clean)) +
      geom_col() +
      geom_text(
        mapping = aes(x = prediction_clean, y = count, label = count),
        size = 8,
        hjust = 0.25,
        vjust = 1.5,
        # nudge_x = -0,
        color = 'white'
      ) +
      scale_fill_manual(values = palette) +
      labs(
        x = "",
        y = "",
        caption = "Data source: https://www.groundhog.org/groundhog-day/history-past-predictions/"
      ) +
      theme(
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.y = element_blank()
      )
  })

  output$year_plot <- renderPlot({
    # Diverging bar chart code would go here
    if (input$chart_type == "Heatmap") {
      cols <- 10L
      data() |>
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
        ) |>
        ggplot(aes(x = col, y = row, fill = pred)) +
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
          y = NULL,
          caption = "Data source: https://www.groundhog.org/groundhog-day/history-past-predictions/"
        ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank()
        )
    } else if (input$chart_type == "Diverging bar") {
      ggplot(data(), aes(x = x, y = factor(year))) +
        geom_col(aes(fill = factor(x))) +
        scale_x_continuous(
          breaks = c(-1, 0, 1),
          labels = c(
            "-1" = "Early Spring",
            "0" = "Uncertain",
            "1" = "More Winter"
          )
        ) +
        scale_fill_manual(
          values = c("-1" = "#d73027", "0" = "#ffffbf", "1" = "#4575b4"),
          labels = c(
            "-1" = "Early Spring",
            "0" = "Uncertain",
            "1" = "More Winter"
          ),
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
          fill = "Category",
          caption = "Data source: https://www.groundhog.org/groundhog-day/history-past-predictions/"
        ) +
        theme(
          legend.position = "none"
        )
    }
  })
}

shinyApp(ui, server)
