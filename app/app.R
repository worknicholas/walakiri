library(shiny)
library(ggplot2)
library(magick)
library(readxl)
library(pdftools)
library(dplyr)
library(sf)

# Load map data once
map_gpkg_path <- list.files("www", pattern = "ADMINISTRASIKECAMATAN_AR\\.gpkg$", full.names = TRUE)[1]
map_sf <- if (!is.null(map_gpkg_path)) {
  sf::st_read(map_gpkg_path, quiet = TRUE) %>% filter(WADMKK == "Sumba Timur")
} else NULL

# Default operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .sticky-main {
      position: sticky;
      top: 0;
      z-index: 1000;
      background: white;
      padding: 10px;
    }
  "))),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("Report Settings"),
      tabsetPanel(
        tabPanel("Report Info",
          textInput("reportTitle", "Report Title", 
                    "Laporan Bukti Dukung Insight Statistik"),
          textAreaInput("reportDescription", "Description", 
                        "Laporan ini dibuat untuk memenuhi bukti dukung capaian kinerja triwulanan untuk kegiatan SAKERNAS",
                        rows = 3),
          textInput("userName", "Author", "John Smith")
        ),
        tabPanel("Data & Layers",
          fileInput("dataFile", "Upload Data (CSV/XLSX)", accept = c(".csv", ".xlsx")),
          selectInput("layer", "Layer", choices = "layer 1"),
          fluidRow(
            column(6, actionButton("add_layer", "➕ Add Layer")),
            column(6, actionButton("delete_layer", "🗑️ Delete Layer"))
          )
        ),
        tabPanel("Chart Variables",
          selectInput("xVar", "X Variable(s)", choices = NULL, multiple = TRUE),
          selectInput("yVar", "Y Variable(s)", choices = NULL, multiple = TRUE),
          selectInput("plotType", "Plot Type", 
                      choices = c("Scatterchart", "Barchart", "Linechart", 
                                  "Piechart", "Mapchart Sumba Timur")),
          selectInput("sortVar", "Sort Order X (numeric)", choices = c("None")),
          selectInput("yTransform", "Transform Y (numeric)", 
                      choices = c("None", "Percentage")),
          fluidRow(
            column(6, actionButton("swap_xy", "↔️ Swap X & Y")),
            column(6, actionButton("reverse_sort", "🔀 Reverse Sort"))
          )
        ),
        tabPanel("Reference Lines",
          uiOutput("y_lines_ui"),
          selectInput("selected_y_line", "Delete Line", choices = NULL),
          fluidRow(
            column(6, actionButton("add_y_line", "➕ Add Line")),
            column(6, actionButton("delete_y_line", "🗑️ Delete Line"))
          )
        ),
        tabPanel("Appearance",
          textInput("title", "Chart Title", "Chart Title"),
          textInput("xLabel", "X Label", "X Variable"),
          checkboxInput("hide_x_label", "Hide X Axis Tick", value = FALSE),
          textInput("yLabel", "Y Label", "Y Variable"),
          checkboxInput("hide_y_label", "Hide Y Axis Tick", value = FALSE),
          selectInput("showValues", "Show Data Values", 
                      choices = c("None", "Y Variable", "X Variable", "Both"),
                      selected = "None"),
          numericInput("pointSize", "Point Size", 3, min = 1, max = 10),
          numericInput("lineSize", "Line Thickness", 1.5, min = 0.5, max = 10, step = 0.5),
          numericInput("tickLabelSize", "Tick Label Size", 11, min = 6, max = 30),
          numericInput("axisLabelSize", "Axis Label Size", 13, min = 6, max = 40),
          numericInput("titleSize", "Title Size", 15, min = 8, max = 50),
          numericInput("dataLabelSize", "Data Label Size", 3, min = 1, max = 10, step = 0.5),
          selectInput("theme", "Theme", choices = c("Minimal", "Classic", "Gray", "BW", "Blank")),
          checkboxInput("showLegend", "Show Legend", value = TRUE),
          checkboxInput("xTilt", "Tilt X Axis Labels (45°)", value = TRUE),
          checkboxInput("yTilt", "Tilt Y Axis Labels (45°)", value = FALSE)
        )
      )
    ),
    mainPanel(
      class = "sticky-main",
      plotOutput("chart", width = "800px", height = "450px"),
      br(),
      wellPanel(
        textAreaInput("description", "Insight", 
                      placeholder = "Write your insights here...", rows = 4),
        downloadButton("downloadPlot", "⬇️ Download PDF Report")
      )
    )
  )

)

# Server
server <- function(input, output, session) {
  # Reactive state
  app_state <- reactiveVal(list(
    `layer 1` = list(
      data = NULL,
      y_lines = list(),
      config = list(
        xVar = NULL, yVar = NULL, title = "Chart Title",
        xLabel = "X Variable", yLabel = "Y Variable",
        plotType = "Scatterchart", yTransform = "None", showValues = "None", sortVar = "None",
        pointSize = 3, lineSize = 1.5, theme = "Minimal",
        tickLabelSize = 11, axisLabelSize = 13, titleSize = 15, dataLabelSize = 3,
        showLegend = TRUE, xTilt = TRUE, yTilt = FALSE, sortAscending = TRUE,
        hide_x_label = FALSE, hide_y_label = FALSE, description = ""
      )
    )
  ))
  layer_list <- reactiveVal(c("layer 1"))

  # Ensure y_lines exists for all layers
  observe({
    state <- app_state()
    for (layer in layer_list()) {
      state[[layer]]$y_lines <- state[[layer]]$y_lines %||% list()
    }
    app_state(state)
  })

  # UI for y lines
  output$y_lines_ui <- renderUI({
    y_lines <- app_state()[[input$layer]]$y_lines
    if (length(y_lines) == 0) return(tags$div("No Y lines added."))
    tagList(lapply(seq_along(y_lines), function(i) {
      fluidRow(
        column(5, numericInput(paste0("y_line_value_", i), "Value", value = y_lines[[i]]$value)),
        column(7, textInput(paste0("y_line_label_", i), "Label", value = y_lines[[i]]$label))
      )
    }))
  })

  # Update delete select
  observe({
    y_lines <- app_state()[[input$layer]]$y_lines
    choices <- if (length(y_lines) > 0) setNames(seq_along(y_lines), paste("Line", seq_along(y_lines))) else NULL
    updateSelectInput(session, "selected_y_line", choices = choices)
  })

  # Add y line
  observeEvent(input$add_y_line, {
    state <- app_state()
    y_lines <- state[[input$layer]]$y_lines
    y_lines[[length(y_lines) + 1]] <- list(value = 0, label = "")
    state[[input$layer]]$y_lines <- y_lines
    app_state(state)
  })

  # Delete y line
  observeEvent(input$delete_y_line, {
    sel <- as.integer(input$selected_y_line)
    if (is.na(sel)) return()
    state <- app_state()
    y_lines <- state[[input$layer]]$y_lines
    y_lines <- y_lines[-sel]
    state[[input$layer]]$y_lines <- y_lines
    app_state(state)
  })

  # Save y line edits
  observe({
    state <- app_state()
    y_lines <- state[[input$layer]]$y_lines
    for (i in seq_along(y_lines)) {
      val <- input[[paste0("y_line_value_", i)]]
      lab <- input[[paste0("y_line_label_", i)]]
      if (!is.null(val)) y_lines[[i]]$value <- val
      if (!is.null(lab)) y_lines[[i]]$label <- lab
    }
    state[[input$layer]]$y_lines <- y_lines
    app_state(state)
  })

  chart_dims <- c(width = 8.33, height = 4.6875)

  # Update inputs on layer change
  observeEvent(input$layer, {
    state <- app_state()[[input$layer]]
    col_names <- if (is.null(state$data)) character(0) else names(state$data)
    num_vars <- c("None", names(state$data)[sapply(state$data, is.numeric)])
    updateSelectInput(session, "xVar", choices = col_names, selected = state$config$xVar)
    updateSelectInput(session, "yVar", choices = col_names, selected = state$config$yVar)
    updateSelectInput(session, "plotType", selected = state$config$plotType)
    updateSelectInput(session, "yTransform", selected = state$config$yTransform)
    updateSelectInput(session, "showValues", selected = state$config$showValues)
    updateSelectInput(session, "sortVar", choices = num_vars, selected = state$config$sortVar)
    updateTextInput(session, "title", value = state$config$title)
    updateTextInput(session, "xLabel", value = state$config$xLabel)
    updateTextInput(session, "yLabel", value = state$config$yLabel)
    updateNumericInput(session, "pointSize", value = state$config$pointSize)
    updateNumericInput(session, "lineSize", value = state$config$lineSize)
    updateNumericInput(session, "tickLabelSize", value = state$config$tickLabelSize)
    updateNumericInput(session, "axisLabelSize", value = state$config$axisLabelSize)
    updateNumericInput(session, "titleSize", value = state$config$titleSize)
    updateNumericInput(session, "dataLabelSize", value = state$config$dataLabelSize)
    updateSelectInput(session, "theme", selected = state$config$theme)
    updateCheckboxInput(session, "showLegend", value = state$config$showLegend)
    updateCheckboxInput(session, "xTilt", value = state$config$xTilt)
    updateCheckboxInput(session, "yTilt", value = state$config$yTilt)
    updateCheckboxInput(session, "hide_x_label", value = state$config$hide_x_label)
    updateCheckboxInput(session, "hide_y_label", value = state$config$hide_y_label)
    updateTextAreaInput(session, "description", value = state$config$description)
  })

  # Save config changes
  observe({
    req(input$layer)
    state <- app_state()
    state[[input$layer]]$config <- list(
      xVar = input$xVar, yVar = input$yVar, title = input$title,
      xLabel = input$xLabel, yLabel = input$yLabel, plotType = input$plotType,
      yTransform = input$yTransform, showValues = input$showValues, sortVar = input$sortVar,
      pointSize = input$pointSize, lineSize = input$lineSize, tickLabelSize = input$tickLabelSize,
      axisLabelSize = input$axisLabelSize, titleSize = input$titleSize, dataLabelSize = input$dataLabelSize,
      theme = input$theme, showLegend = input$showLegend, xTilt = input$xTilt, yTilt = input$yTilt,
      hide_x_label = input$hide_x_label, hide_y_label = input$hide_y_label,
      sortAscending = state[[input$layer]]$config$sortAscending,
      description = input$description
    )
    app_state(state)
  })

  # Data upload
  observeEvent(input$dataFile, {
    req(input$dataFile)
    ext <- tolower(tools::file_ext(input$dataFile$name))
    data <- switch(ext,
                   "csv" = read.csv(input$dataFile$datapath),
                   "xlsx" = readxl::read_excel(input$dataFile$datapath),
                   stop("Unsupported file type")
    )
    state <- app_state()
    state[[input$layer]]$data <- data
    app_state(state)
    col_names <- names(data)
    num_vars <- c("None", names(data)[sapply(data, is.numeric)])
    updateSelectInput(session, "xVar", choices = col_names, selected = col_names[1])
    updateSelectInput(session, "yVar", choices = col_names, selected = col_names[min(2, length(col_names))])
    updateSelectInput(session, "sortVar", choices = num_vars, selected = "None")
  })

  # Add layer
  observeEvent(input$add_layer, {
    layers <- layer_list()
    new_layer <- paste("layer", length(layers) + 1)
    state <- app_state()
    state[[new_layer]] <- list(
      data = NULL, y_lines = list(),
      config = list(
        xVar = NULL, yVar = NULL, title = "Chart Title",
        xLabel = "X Variable", yLabel = "Y Variable", plotType = "Scatterchart",
        yTransform = "None", showValues = "None", sortVar = "None",
        pointSize = 3, lineSize = 1.5, theme = "Minimal",
        tickLabelSize = 11, axisLabelSize = 13, titleSize = 15, dataLabelSize = 3,
        showLegend = TRUE, xTilt = TRUE, yTilt = FALSE, sortAscending = TRUE,
        hide_x_label = FALSE, hide_y_label = FALSE, description = ""
      )
    )
    app_state(state)
    layer_list(c(layers, new_layer))
    updateSelectInput(session, "layer", choices = layer_list(), selected = new_layer)
  })

  # Delete layer
  observeEvent(input$delete_layer, {
    if (input$layer == "layer 1") return()
    state <- app_state()
    state[[input$layer]] <- NULL
    layers <- layer_list()[layer_list() != input$layer]
    layer_list(layers)
    updateSelectInput(session, "layer", choices = layers, selected = layers[1])
    app_state(state)
  })

  # Reverse sort
  observeEvent(input$reverse_sort, {
    state <- app_state()
    state[[input$layer]]$config$sortAscending <- !state[[input$layer]]$config$sortAscending
    app_state(state)
  })

  # Swap X/Y
  observeEvent(input$swap_xy, {
    old_x <- input$xVar
    old_y <- input$yVar
    old_x_label <- input$xLabel
    old_y_label <- input$yLabel
    updateSelectInput(session, "xVar", selected = old_y)
    updateSelectInput(session, "yVar", selected = old_x)
    updateTextInput(session, "xLabel", value = old_y_label)
    updateTextInput(session, "yLabel", value = old_x_label)
  })

  # Label helpers
  add_labels <- function(g, df, show_mode, xvar, yvar, is_percent, label_size, geom_type = "text") {
    if (show_mode == "None") return(g)
    label <- switch(show_mode,
                    "Y Variable" = if (is_percent) paste0(round(df[[yvar]], 1), "%") else df[[yvar]],
                    "X Variable" = df[[xvar]],
                    "Both" = if (is_percent) paste(df[[xvar]], paste0(round(df[[yvar]], 1), "%"), sep = ", ") else paste(df[[xvar]], df[[yvar]], sep = ", ")
    )
    if (geom_type == "text") {
      g + geom_text(aes(label = label), vjust = -0.5, size = label_size)
    } else if (geom_type == "stack") {
      g + geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = label_size)
    } else if (geom_type == "sf_text") {
      g + geom_sf_text(aes(label = label), size = label_size)
    } else g
  }

  # Reorder helper
  get_reorder <- function(df, cfg, xvar) {
    sort_var <- cfg$sortVar
    asc <- cfg$sortAscending
    if (sort_var != "None" && sort_var %in% names(df) && is.numeric(df[[sort_var]])) {
      reorder(xvar, if (asc) df[[sort_var]] else -df[[sort_var]])
    } else {
      factor(xvar, levels = unique(xvar))
    }
  }

  # Build functions
  build_scatter <- function(df, cfg) {
    xvar <- cfg$xVar[1]; yvar <- cfg$yVar[1]
    g <- ggplot(df, aes(x = get_reorder(df, cfg, .data[[xvar]]), y = .data[[yvar]])) + geom_point(size = cfg$pointSize)
    add_labels(g, df, cfg$showValues, xvar, yvar, cfg$y_is_percent, cfg$dataLabelSize)
  }

  build_bar <- function(df, cfg) {
    xvar <- cfg$xVar[1]; yvar <- cfg$yVar[1]
    g <- ggplot(df, aes(x = get_reorder(df, cfg, .data[[xvar]]), y = .data[[yvar]])) + geom_col()
    add_labels(g, df, cfg$showValues, xvar, yvar, cfg$y_is_percent, cfg$dataLabelSize)
  }

  build_line <- function(df, cfg) {
    xvar <- cfg$xVar[1]; yvar <- cfg$yVar[1]
    df[[xvar]] <- get_reorder(df, cfg, df[[xvar]])
    g <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) + geom_line(size = cfg$lineSize, group = 1) + geom_point(size = cfg$pointSize)
    add_labels(g, df, cfg$showValues, xvar, yvar, cfg$y_is_percent, cfg$dataLabelSize)
  }

  build_pie <- function(df, cfg) {
    cat_var <- cfg$xVar[1]
    num_var <- if (length(cfg$yVar) > 0 && is.numeric(df[[cfg$yVar[1]]])) cfg$yVar[1] else if (length(cfg$xVar) > 1 && is.numeric(df[[cfg$xVar[2]]])) cfg$xVar[2] else NULL
    agg_df <- if (!is.null(num_var)) {
      df %>% group_by(category = .data[[cat_var]]) %>% summarise(value = sum(.data[[num_var]], na.rm = TRUE))
    } else {
      as.data.frame(table(df[[cat_var]])) %>% rename(category = Var1, value = Freq)
    }
    if (cfg$yTransform == "Percentage") {
      total <- sum(agg_df$value)
      agg_df$value <- if (total > 0) round(agg_df$value / total * 100, 1) else 0
    }
    g <- ggplot(agg_df, aes(x = "", y = value, fill = category)) + geom_col(width = 1) + coord_polar("y") + theme_void()
    add_labels(g, agg_df, cfg$showValues, "category", "value", cfg$yTransform == "Percentage", cfg$dataLabelSize, "stack")
  }

  build_map <- function(df, cfg) {
    if (is.null(map_sf)) return(NULL)
    if (is.null(df) || length(cfg$xVar) == 0 || length(cfg$yVar) == 0) {
      return(ggplot(map_sf) + geom_sf(fill = "grey90", color = "white") + geom_sf_text(aes(label = NAMOBJ), size = 3) +
               labs(title = "Mapchart: Sumba Timur", subtitle = "No data uploaded or variables selected"))
    }
    join_col <- cfg$xVar[1]; value_col <- cfg$yVar[1]
    if (!(join_col %in% names(df)) || !(value_col %in% names(df))) {
      return(ggplot(map_sf) + geom_sf(fill = "grey90", color = "white") + geom_sf_text(aes(label = NAMOBJ), size = 3) +
               labs(title = "Mapchart: Sumba Timur", subtitle = "Selected columns not found in data"))
    }
    join_data <- df %>% rename(NAMOBJ = !!join_col) %>% select(NAMOBJ, all_of(value_col))
    if (cfg$yTransform == "Percentage" && is.numeric(join_data[[value_col]])) {
      total <- sum(join_data[[value_col]], na.rm = TRUE)
      join_data[[value_col]] <- if (total > 0) round(join_data[[value_col]] / total * 100, 1) else 0
    }
    map_joined <- left_join(map_sf, join_data, by = "NAMOBJ")
    label_expr <- switch(cfg$showValues,
                         "Y Variable" = if (cfg$yTransform == "Percentage") paste0(map_joined[[value_col]], "%") else map_joined[[value_col]],
                         "X Variable" = map_joined$NAMOBJ,
                         "Both" = if (cfg$yTransform == "Percentage") paste0(map_joined$NAMOBJ, ": ", map_joined[[value_col]], "%") else paste0(map_joined$NAMOBJ, ": ", map_joined[[value_col]]),
                         NULL
    )
    g <- ggplot(map_joined) + geom_sf(aes(fill = .data[[value_col]]), color = "white") +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      labs(title = "Mapchart: Sumba Timur", fill = if (cfg$yTransform == "Percentage") paste0(value_col, " (%)") else value_col) +
      theme_minimal()
    if (!is.null(label_expr)) g <- g + geom_sf_text(aes(label = label_expr), size = cfg$dataLabelSize)
    g
  }

  # Build chart
  build_chart <- function(layer_state) {
    df <- layer_state$data; cfg <- layer_state$config; y_lines <- layer_state$y_lines
    if (is.null(df) || length(cfg$xVar) == 0) return(NULL)
    df_trans <- df
    cfg$y_is_percent <- FALSE
    if (cfg$yTransform == "Percentage" && cfg$plotType != "Piechart" && length(cfg$yVar) > 0 && is.numeric(df[[cfg$yVar[1]]])) {
      total <- sum(df[[cfg$yVar[1]]], na.rm = TRUE)
      df_trans[[cfg$yVar[1]]] <- if (total > 0) round(df[[cfg$yVar[1]]] / total * 100, 1) else 0
      cfg$y_is_percent <- TRUE
    }
    p <- switch(cfg$plotType,
                "Scatterchart" = build_scatter(df_trans, cfg),
                "Barchart" = build_bar(df_trans, cfg),
                "Linechart" = build_line(df_trans, cfg),
                "Piechart" = build_pie(df, cfg),
                "Mapchart Sumba Timur" = build_map(df, cfg)
    )
    if (is.null(p)) return(NULL)
    if (length(y_lines) > 0 && cfg$plotType %in% c("Scatterchart", "Barchart", "Linechart")) {
      for (yl in y_lines) {
        if (!is.null(yl$value) && yl$value != "") {
          p <- p + geom_hline(yintercept = yl$value, linetype = "dashed", color = "red")
          if (nzchar(yl$label)) {
            p <- p + annotate("text", x = Inf, y = yl$value, label = yl$label, hjust = 1.1, vjust = -0.5, color = "red", size = cfg$tickLabelSize / 3)
          }
        }
      }
    }
    ylab_out <- if (cfg$y_is_percent || (cfg$plotType == "Piechart" && cfg$yTransform == "Percentage")) paste0(cfg$yLabel, " (%)") else cfg$yLabel
    xlab_final <- if (cfg$theme == "Blank" || cfg$hide_x_label) "" else cfg$xLabel
    ylab_final <- if (cfg$theme == "Blank" || cfg$hide_y_label) "" else ylab_out
    p <- p + labs(title = cfg$title, x = xlab_final, y = ylab_final) +
      switch(cfg$theme, Minimal = theme_minimal(), Classic = theme_classic(), Gray = theme_gray(), BW = theme_bw(), Blank = theme_void())
    if (cfg$hide_x_label) {
      p <- p + theme(axis.text.x = element_blank())
    } else if (cfg$xTilt && cfg$theme != "Blank") {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = cfg$tickLabelSize))
    } else if (cfg$theme != "Blank") {
      p <- p + theme(axis.text.x = element_text(size = cfg$tickLabelSize))
    }
    if (cfg$hide_y_label) {
      p <- p + theme(axis.text.y = element_blank())
    } else if (cfg$yTilt && cfg$theme != "Blank") {
      p <- p + theme(axis.text.y = element_text(angle = 45, hjust = 1, size = cfg$tickLabelSize))
    } else if (cfg$theme != "Blank") {
      p <- p + theme(axis.text.y = element_text(size = cfg$tickLabelSize))
    }
    if (cfg$theme != "Blank") {
      p <- p + theme(axis.title.x = element_text(size = cfg$axisLabelSize), axis.title.y = element_text(size = cfg$axisLabelSize),
                     plot.title = element_text(size = cfg$titleSize))
    } else {
      p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(size = cfg$titleSize))
    }
    if (!cfg$showLegend) p <- p + theme(legend.position = "none")
    p
  }

  # Reactive chart
  chart_reactive <- reactive({
    build_chart(app_state()[[input$layer]])
  })

  # Render chart
  output$chart <- renderPlot({
    p <- chart_reactive()
    if (is.null(p)) {
      plot.new(); text(0.5, 0.5, "Upload data and select variables.")
    } else p
  }, width = 800, height = 450)

  # Header image
  generate_header_image <- function(density = 300) {
    bg_path <- "www/background.pdf"
    if (!file.exists(bg_path)) return(image_blank(600, 400, "lightgray") %>% image_annotate("Background PDF not found", size = 20, color = "red", gravity = "center"))
    img <- image_read_pdf(bg_path, density = density)
    h <- image_info(img)$height
    img %>%
      image_annotate(strwrap(input$reportTitle, 25), size = 26, color = "black", weight = 700, gravity = "north", location = paste0("+0+", round(h * 0.25))) %>%
      image_annotate(strwrap(input$reportDescription, 80), size = 14, color = "black", weight = 700, gravity = "north", location = paste0("+0+", round(h * 0.5))) %>%
      image_annotate(paste("Author:", input$userName), size = 12, color = "black", weight = 700, gravity = "north", location = paste0("+0+", round(h * 0.8))) %>%
      image_annotate(paste("Export:", format(Sys.time() + 8*3600, "%Y-%m-%d %H:%M:%S UTC+8")), size = 12, color = "black", weight = 700, gravity = "north", location = paste0("+0+", round(h * 0.83)))
  }

  # Download
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("report-", Sys.Date(), ".pdf"),
    content = function(file) {
      header_temp <- tempfile(fileext = ".pdf")
      image_write(generate_header_image(), header_temp, format = "pdf")
      temp_pages <- header_temp
      bg_path <- "www/background.pdf"
      bg_img <- if (file.exists(bg_path)) image_read_pdf(bg_path, density = 300) else image_blank(800, 600, "white")
      for (layer in layer_list()) {
        layer_state <- app_state()[[layer]]
        if (is.null(layer_state$data) || length(layer_state$config$xVar) == 0 || length(layer_state$config$yVar) == 0) next
        p <- build_chart(layer_state)
        temp_chart <- tempfile(fileext = ".pdf")
        ggsave(temp_chart, p, width = chart_dims["width"], height = chart_dims["height"], units = "in")
        insights <- trimws(layer_state$config$description)
        lines <- if (nzchar(insights)) strwrap(insights, 120) else "No description provided."
        text_h <- max(length(lines) * 0.2, 0.4)
        temp_ins <- tempfile(fileext = ".pdf")
        pdf(temp_ins, width = chart_dims["width"], height = text_h)
        par(mar = rep(0.2, 4)); plot.new()
        text(0, 1, paste(lines, collapse = "\n"), adj = c(0, 1), cex = 0.8)
        dev.off()
        chart_img <- image_read_pdf(temp_chart, density = 300) %>% image_border("black", "12x12")
        ins_img <- image_read_pdf(temp_ins, density = 300)
        combined <- image_append(c(chart_img, ins_img), stack = TRUE)
        result <- image_composite(bg_img, combined, gravity = "center")
        temp_page <- tempfile(fileext = ".pdf")
        image_write(result, temp_page, format = "pdf")
        temp_pages <- c(temp_pages, temp_page)
        unlink(c(temp_chart, temp_ins))
      }
      last_page <- "www/last_page.pdf"
      if (file.exists(last_page)) temp_pages <- c(temp_pages, last_page)
      pdf_combine(temp_pages, output = file)
      unlink(temp_pages[temp_pages != last_page])
    }
  )
}

shinyApp(ui, server)