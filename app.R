library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(scales)

# Consistent color palette
palette <- c(
  "Early spring" = "#d73027",
  "More winter" = "#4575b4",
  "Uncertain" = "#706E6D"
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

# ui <- page_sidebar(
#   title = "Groundhog Day",
#   sidebar = sidebar(
#     p("Data provided by groundhog.org")
#   ),
#   card(
#     card_header("How often is an early spring predicted vs. more winter?"),
#     plotOutput("count_plot")
#   ),
#   card(
#     card_header("Please choose a groundhog"),
#     selectInput(
#       "groundhog", # ADD: missing inputId
#       "Select from this list:", # ADD: missing label
#       choices = NULL
#     ),
#     plotOutput("count_plot_filtered")
#   )
# )

ui <- page_sidebar(
  sidebar = sidebar(
    h4("Groundhog Day"),
    actionButton("overview", "Overview", class = "btn-primary w-100 mb-2"),
    actionButton("groundhog_view", "Groundhogs", class = "btn-primary w-100 mb-2"),
    actionButton("weather_predictions", "Predictions", class = "btn-primary w-100 mb-2")
  ),
  uiOutput("main_content")
)

server <- function(input, output, session) {
  # Load data
  predictions <- reactive({
    if (file.exists("data/predictions.csv")) {
      readr::read_csv("data/predictions.csv", show_col_types = FALSE)
    } else {
      tibble()
    }
  })

  groundhogs <- reactive({
    if (file.exists("data/all_groundhogs.csv")) {
      readr::read_csv("data/all_groundhogs.csv", show_col_types = FALSE) %>%
        separate(coordinates,
                 into = c("latitude", "longitude"),
                 sep = ",",
                 convert = TRUE)
    } else {
      tibble()
    }
  })



  # Update current view when buttons are clicked

  current_view <- reactiveVal("default_view")

  observeEvent(input$overview, {
    current_view("default_view")
  })

  observeEvent(input$groundhog_view, {
    current_view("all_groundhogs")
  })

  observeEvent(input$weather_predictions, {
    current_view("all_predictions")
  })

  # Render different content based on current view
  output$main_content <- renderUI({
    switch(
      current_view(),
      "default_view" = layout_column_wrap(
        width = 1/2,
        card(
          card_header("Total Groundhog Predictions"),
          plotOutput("count_plot")
        ),
        card(
          card_header("Number of Predictions Each Year"),
          # tableOutput("x"),
          plotOutput("prediction_line_graph")
        ),
        card(
          card_header("Filter by groundhog"),
          uiOutput("groundhog_selector"),
          plotOutput("count_plot_filtered")
        ),
      ),
      "all_groundhogs" = layout_column_wrap(
        width = 1/2,
        card(
          card_header("List of all the groundhogs"),
          tableOutput("groundhog_table")
          ),
        card(
          card_header("Map"),
          # leafletOutput("groundhog_map", height = 500)
          textOutput("Coming soon")
        )
      ),
      "all_predictions" = layout_column_wrap(
        width = 1/2,
        card(
          card_header("Heat Map"),
          uiOutput("groundhog_selector_predictions"),
          # tableOutput("heatmap_filtered"),
          plotOutput("heatmap_filtered_plot")
        )
      )
      )
  })

  output$groundhog_table <- renderTable({
    groundhogs() %>%
      select(name, city, region, latitude, longitude) %>%
      arrange(name)
  })

  output$groundhog_map <- renderLeaflet({
    req(groundhogs())

    d <- groundhogs()

    d %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0("<b>", names, "</b><br>", city, ", ", region)
      )
  })

  output$prediction_table <- renderTable(predictions())

  output$groundhog_selector <- renderUI({
    req(predictions())

    unique_names <- predictions() %>%
      pull(name) %>%
      unique() %>%
      sort()

    selectInput(
      "selected_groundhog",
      "Choose a groundhog: ",
      choices = unique_names
    )
  })

  output$groundhog_selector_predictions <- renderUI({
    req(predictions())

    unique_names <- predictions() %>%
      pull(name) %>%
      unique() %>%
      sort()

    selectInput(
      "selected_groundhog_predictions",
      "Choose a groundhog: ",
      choices = unique_names
    )
  })

  # heatmap

  output$x <- renderTable({
    predictions() %>%
      select(name, year, prediction) %>%
      mutate(year = factor(year)) %>%
      group_by(year) %>%
      summarise(num_predictiosn = n())
    # df %>%
    #   group_by(year, predictions) %>%
    #   summarise(x = n())

  })

  output$prediction_line_graph <- renderPlot({

    yrs <- sort(unique(groundhog_table$year))
    decade_breaks <- as.character(seq(
      from = 2025,
      to = min(yrs, na.rm = TRUE),
      by = -10
    ))

    predictions() %>%
      select(name, year, prediction) %>%
      mutate(year = factor(year)) %>%
      group_by(year) %>%
      summarise(num_predictions = n()) %>%
      ungroup() %>%
      arrange(year) %>%
      ggplot(aes(x = year, y = num_predictions, group = 1)) +
        geom_line() +
        labs(x = "Year",
             y = "Number of Groundhogs") +
      scale_x_discrete(
        breaks = decade_breaks,
        labels = decade_breaks
      )
  })

  filtered_predictions_heatmap <- reactive({
    req(input$selected_groundhog_predictions)

    df <- predictions()
    req(nrow(df) > 0)
    df %>%
      filter(name == input$selected_groundhog_predictions)
  })

  output$heatmap_filtered <- renderTable({

    df <- filtered_predictions_heatmap()
    cols <- 10L

    df |>
      distinct(year, shadow) |>
      arrange(desc(year)) |>
      mutate(
        idx = seq_len(n()) - 1L,
        col = (idx %% .env$cols) + 1L,
        row = (idx %/% .env$cols) + 1L,
        pred = case_when(
          shadow == 1L ~ "More winter",
          shadow == 0L ~ "Early spring",
          TRUE ~ "Uncertain"
        ),
        text_color = if_else(pred == "Uncertain", "black", "white")
      )
  })


    output$heatmap_filtered_plot <- renderPlot({

      df <- filtered_predictions_heatmap()
      cols <- 10L

      df |>
        distinct(year, shadow) |>
        arrange(desc(year)) |>
        mutate(
          idx = seq_len(n()) - 1L,
          col = (idx %% .env$cols) + 1L,
          row = (idx %/% .env$cols) + 1L,
          pred = case_when(
            shadow == 1L ~ "More winter",
            shadow == 0L ~ "Early spring",
            TRUE ~ "Uncertain"
          ),
          text_color = if_else(pred == "Uncertain", "black", "white")
        ) %>%
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
          y = NULL
      ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank()
      )

    })



  # Populate dropdown with groundhog names
  # observe({
  #   df <- predictions()
  #   if (nrow(df) > 0) {
  #     choices <- sort(unique(df$name))
  #     updateSelectInput(
  #       session,
  #       "groundhog",
  #       choices = choices,
  #       selected = choices[1]
  #     )
  #   }
  # })

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
      scale_fill_manual(values = palette) + # Apply consistent palette
      labs(x = "", y = "") +
      theme(legend.position = "none", axis.ticks = element_blank(),
            axis.text.y = element_blank())
  })

  # Filtered predictions for selected groundhog
  filtered_predictions <- reactive({
    req(input$selected_groundhog)
    df <- predictions()
    req(nrow(df) > 0)
    df %>% filter(name == input$selected_groundhog)
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
      scale_fill_manual(values = palette) + # Apply consistent palette
      labs(x = "", y = "", title = paste("Predictions for", input$groundhog)) +
      theme(legend.position = "none", axis.ticks = element_blank())
  })
}

shinyApp(ui, server)
