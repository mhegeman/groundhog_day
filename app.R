library(shiny)
library(bslib)
library(tidyverse)

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

ui <- page_sidebar(
  title = "Groundhog Day Predictions Since 1900",
  sidebar = sidebar(
    p("There are 74 groundhogs predicting the weather across the US.")
  ),
  card(
    card_header("How often is an early spring predicted vs. more winter?"),
    plotOutput("count_plot")
  ),
  card(
    card_header("Please choose a groundhog"),
    selectInput(
      "groundhog", # ADD: missing inputId
      "Select from this list:", # ADD: missing label
      choices = NULL
    ),
    plotOutput("count_plot_filtered")
  )
)

server <- function(input, output, session) {
  # Load predictions data
  predictions <- reactive({
    if (file.exists("data/predictions.csv")) {
      readr::read_csv("data/predictions.csv", show_col_types = FALSE)
    } else {
      tibble()
    }
  })

  groundhogs <- reactive({
    if (file.exists("data/all_groundhogs.csv")) {
      readr::read_csv("data/all_groundhogs.csv", show_col_types = FALSE)
    } else {
      tibble()
    }
  })

  # Populate dropdown with groundhog names
  observe({
    df <- predictions()
    if (nrow(df) > 0) {
      choices <- sort(unique(df$name))
      updateSelectInput(
        session,
        "groundhog",
        choices = choices,
        selected = choices[1]
      )
    }
  })

  # Overall count plot
  output$count_plot <- renderPlot({
    df <- predictions()
    req(nrow(df) > 0)

    df %>%
      group_by(prediction) %>%
      summarise(count = n(), .groups = "drop") %>%
      ggplot(aes(x = prediction, y = count, fill = prediction)) +
      geom_col() +
      geom_text(aes(label = count), color = "white", vjust = 1.5, size = 5) +
      labs(x = "", y = "") +
      theme(legend.position = "none", axis.ticks = element_blank())
  })

  # Filtered predictions for selected groundhog
  filtered_predictions <- reactive({
    req(input$groundhog)
    df <- predictions()
    req(nrow(df) > 0)
    df %>% filter(name == input$groundhog)
  })

  # Filtered count plot
  output$count_plot_filtered <- renderPlot({
    df <- filtered_predictions()
    req(nrow(df) > 0)

    df %>%
      group_by(prediction) %>%
      summarise(count = n(), .groups = "drop") %>%
      ggplot(aes(x = prediction, y = count, fill = prediction)) +
      geom_col() +
      geom_text(aes(label = count), color = "white", vjust = 1.5, size = 5) +
      labs(x = "", y = "", title = paste("Predictions for", input$groundhog)) +
      theme(legend.position = "none", axis.ticks = element_blank())
  })
}

shinyApp(ui, server)
