# ── Libraries ────────────────────────────────────────────────────────────────
library(shiny)      # Core Shiny framework
library(bslib)      # Bootstrap-based UI components (page_sidebar, card, etc.)
library(tidyverse)  # Data manipulation (dplyr, ggplot2, tidyr, readr, etc.)
library(leaflet)    # Interactive maps
library(scales)     # Axis/label formatting helpers for ggplot2
library(DT)


# ── Global: color palette ─────────────────────────────────────────────────────
# Named vector maps each prediction category to a hex color.
# Used consistently across all plots so colors never diverge.
palette <- c(
  "Early spring" = "#2E7D32",
  "More winter" = "#4575b4",
  "Uncertain" = "lightgrey"
)

# ── Global: ggplot2 theme ─────────────────────────────────────────────────────
# Sets a clean minimal theme as the default for every ggplot in the app.
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

# ── UI ────────────────────────────────────────────────────────────────────────
# The UI uses a sidebar layout (bslib::page_sidebar).
# The sidebar contains navigation buttons; the main area is rendered
# dynamically by the server depending on which button was clicked.

ui <- page_sidebar(
  sidebar = sidebar(
    h4("Groundhog Day"),
    # Each button sets the active "view" in the server via reactiveVal
    actionButton("overview", "Overview", class = "btn-primary w-100 mb-2"),
    actionButton("groundhog_view", "Groundhogs", class = "btn-primary w-100 mb-2"),
    actionButton("weather_predictions", "Predictions", class = "btn-primary w-100 mb-2")
  ),
  # The entire main area is replaced by server-rendered UI on each view switch
  uiOutput("main_content")
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Data loading ─────────────────────────────────────────────────────────
  # Reactives read from CSV on first access and cache the result.
  # Returning an empty tibble when files are absent prevents errors during
  # development or before data is downloaded.

  predictions <- reactive({
    if (file.exists("data/predictions.csv")) {
      readr::read_csv("data/predictions.csv", show_col_types = FALSE)
    } else {
      tibble()
    }
  })

  groundhogs <- reactive({
    if (file.exists("data/all_groundhogs.csv")) {
      # The coordinates column is a single "lat,lon" string; split it into
      # two numeric columns so leaflet can use them directly.
      readr::read_csv("data/all_groundhogs.csv", show_col_types = FALSE) %>%
        separate(coordinates,
                 into = c("latitude", "longitude"),
                 sep = ",",
                 convert = TRUE) %>%
        mutate(latitude = as.numeric(latitude),
               longitude = as.numeric(longitude))
    } else {
      tibble()
    }
  })

  # ── Navigation state ──────────────────────────────────────────────────────
  # A reactiveVal acts as a simple state machine for which view is displayed.
  # Defaults to the overview ("default_view") on app start.
  current_view <- reactiveVal("default_view")

  # Each sidebar button updates current_view, which triggers a re-render of
  # main_content below.
  observeEvent(input$overview, {
    current_view("default_view")
  })

  observeEvent(input$groundhog_view, {
    current_view("all_groundhogs")
  })

  observeEvent(input$weather_predictions, {
    current_view("all_predictions")
  })

  # ── Main content (dynamic layout) ─────────────────────────────────────────
  # switch() maps the current view string to a bslib layout.
  # Each layout uses layout_column_wrap to place two cards side by side.
  output$main_content <- renderUI({
    switch(
      current_view(),

      # Overview: overall prediction counts + per-groundhog filter
      "default_view" = layout_column_wrap(
        width = 1/2,
        card(
          card_header("Total Groundhog Predictions"),
          plotOutput("count_plot")
        ),
        card(
          card_header("Filter by groundhog"),
          uiOutput("groundhog_selector"),      # Dropdown rendered by server
          plotOutput("count_plot_filtered")
        )
      ),

      # Groundhogs: table of all groundhogs + interactive map
      "all_groundhogs" = layout_column_wrap(
        width = 1/2,
        # card(
        #   card_header("List of all the groundhogs"),
        #   DTOutput("groundhog_table")
        #   ),
        card(
          card_header("Groundhogs of America"),
          leafletOutput("groundhog_map", height = 500)
          # textOutput("debug_text")
        )
      ),

      # Predictions: line chart of annual counts + per-groundhog heat map
      "all_predictions" = layout_column_wrap(I
        width = 1/2,
        card(
          card_header("Distribution of predictions by year"),
          plotOutput("predictions_each_year")
        ),
        card(
          card_header("Heat Map"),
          uiOutput("groundhog_selector_predictions"),  # Dropdown for heat map
          # tableOutput("heatmap_filtered"),
          plotOutput("heatmap_filtered_plot")
        )
      )
      )
  })

  # ── Groundhogs tab: table & map ───────────────────────────────────────────

  # Simple sorted table of groundhog metadata
  output$groundhog_table <- renderTable({
    groundhogs() %>%
      select(name, city, region, latitude, longitude) %>%
      arrange(name)
  })

  # Leaflet map with a marker for each groundhog; popup shows name and location
  output$groundhog_map <- renderLeaflet({
    req(groundhogs())

    d <- groundhogs() %>%
      select(name, city, region, latitude, longitude) %>%
      arrange(name)

    d %>%
      leaflet() %>%
      addTiles() %>%      # Default OpenStreetMap tile layer
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = seq_len(nrow(d)),
        radius = 5,
        color = "blue",
        fillOpacity = 0.5,
        popup = ~paste0("<b>", name, "</b><br>", city, ", ", region)
      )
  })

  # (Unused) raw predictions table — kept for debugging
  output$prediction_table <- renderTable(predictions())

  # ── Overview tab: dropdowns ───────────────────────────────────────────────
  # These renderUI outputs build a selectInput after the predictions data has
  # loaded, so choices reflect the actual data rather than hard-coded values.

  # Dropdown for the overview tab's filtered bar chart
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

  # Separate dropdown for the predictions tab's heat map
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

  # ── Predictions tab: debug table (unused in UI) ───────────────────────────
  # Counts predictions per year; currently commented out in the UI layout.
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

  # ── Predictions tab: line chart ───────────────────────────────────────────
  # Shows how the total number of participating groundhogs has changed over time.
  # X-axis labels are shown only for every 10th year to avoid crowding.
  output$prediction_line_graph <- renderPlot({

    # Build decade breaks starting from 2025 back to the earliest year in data
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
      # Only label the decade tick marks; all other years are unlabeled
      scale_x_discrete(
        breaks = decade_breaks,
        labels = decade_breaks
      )
  })

  # ── Predictions tab: heat map reactive data ───────────────────────────────
  # Filters predictions to the groundhog chosen in the predictions-tab dropdown.
  filtered_predictions_heatmap <- reactive({
    req(input$selected_groundhog_predictions)

    df <- predictions()
    req(nrow(df) > 0)
    df %>%
      filter(name == input$selected_groundhog_predictions)
  })

  # (Unused) debug table for the heat map data transformation
  output$heatmap_filtered <- renderTable({

    df <- filtered_predictions_heatmap()
    cols <- 10L   # Number of columns in the tile grid

    df |>
      distinct(year, shadow) |>
      arrange(desc(year)) |>
      mutate(
        # Compute grid position: years fill left-to-right, top-to-bottom
        idx = seq_len(n()) - 1L,
        col = (idx %% .env$cols) + 1L,
        row = (idx %/% .env$cols) + 1L,
        # Convert binary shadow flag to human-readable prediction label
        pred = case_when(
          shadow == 1L ~ "More winter",
          shadow == 0L ~ "Early spring",
          TRUE ~ "Uncertain"
        ),
        # Use white text on colored tiles; black on the neutral grey
        text_color = if_else(pred == "Uncertain", "black", "white")
      )
  })

  # ── Predictions tab: heat map plot ────────────────────────────────────────
  # Each tile represents one year for the selected groundhog.
  # Tiles are arranged in a 10-column grid (most recent year first).
  # Color encodes the prediction category; year label is drawn on the tile.
  output$heatmap_filtered_plot <- renderPlot({

    df <- filtered_predictions_heatmap()
    cols <- 10L   # Number of tiles per row

    df |>
      distinct(year, shadow) |>
      arrange(desc(year)) |>
      mutate(
        # Assign each year a (col, row) grid coordinate
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
      # Year label centered on each tile; color adapts for readability
      geom_text(
        aes(label = year, color = text_color),
        size = 3,
        show.legend = FALSE
      ) +
      scale_color_identity() +                           # Use text_color as-is
      scale_fill_manual(values = palette, guide = "none") +
      scale_x_continuous(expand = c(0, 0), breaks = NULL) +
      scale_y_reverse(expand = c(0, 0), breaks = NULL) + # Row 1 at the top
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

  # ── Legacy observer (from earlier UI version) ─────────────────────────────
  # Kept in case the old selectInput with id "groundhog" is re-enabled.
  # Populates its choices once prediction data is available.
  observe({
    df <- predictions()
    req(nrow(df) > 0)

    df %>%
      mutate(pred = case_when(
        shadow == 1L ~ "More winter",
        shadow == 0L ~ "Early spring",
        TRUE ~ "Uncertain")
      )%>%
      count(year, pred) %>%
      ggplot(aes(x = year, y = n, fill = pred)) +
      geom_col(position = "stack") +
      # scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(
        values = c("Early spring" = "#2E7D32",
                   "More winter" = "#4575b4",
                   "Uncertain" = "lightgrey"),
        name = "Prediction"
      ) +
      labs(
        title = "Prediction Patterns Over Time",
        x = "Year",
        y = "Predictions"
      )
  })

  # ── Overview tab: overall bar chart ──────────────────────────────────────
  # Counts every prediction across all groundhogs and years, grouped by
  # prediction category, and displays as a labeled bar chart.
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

  # ── Overview tab: per-groundhog filtered reactive ─────────────────────────
  # Filters predictions to only the groundhog selected in the overview dropdown.
  filtered_predictions <- reactive({
    req(input$selected_groundhog_predictions)
    df <- predictions()
    req(nrow(df) > 0)
    df %>% filter(name == input$selected_groundhog_predictions)
  })

  # ── Overview tab: filtered bar chart ─────────────────────────────────────
  # Same structure as count_plot but scoped to the selected groundhog.
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
      labs(x = "", y = "", title = paste("Predictions by ", input$selected_groundhog_predictions)) +
      theme(legend.position = "none", axis.ticks = element_blank())
  })
}

# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
