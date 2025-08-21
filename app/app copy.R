library(shiny)
library(ggplot2)
library(magick)
library(readxl)
library(pdftools)
library(dplyr)
library(sf)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sticky-main {
        position: -webkit-sticky;
        position: sticky;
        top: 0;              /* stick to the top */
        z-index: 1000;       /* stay above sidebar */
        background: white;   /* prevent overlap transparency */
        padding: 10px;
      }
    "))
  ),
  
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h4("Report Settings"),
      textInput("reportTitle", "Report Title", "Laporan Bukti Dukung Insight Statistik"),
      textInput("reportDescription", "Description", "Laporan ini dibuat untuk memenuhi bukti dukung capaian kinerja triwulanan untuk kegiatan SAKERNAS"),
      textInput("userName", "Author", "John Smith"),
      hr(),
      h4("Chart Layer"),
      selectInput("layer", "Layer", choices = "layer 1"),
      actionButton("add_layer", "Add Layer"),
      actionButton("delete_layer", "Delete Layer"),
      hr(),
      fileInput("dataFile", "Upload CSV/XLSX", accept = c(".csv", ".xlsx")),
      
      selectInput("xVar", "X Variable(s)", choices = NULL, multiple = TRUE),
      selectInput("yVar", "Y Variable(s)", choices = NULL, multiple = TRUE),
      
  selectInput("plotType", "Plot Type",
      choices = c("Scatterchart", "Barchart", "Linechart", "Piechart", "Mapchart Sumba Timur")),
  selectInput("yTransform", "Transform Y Values", choices = c("None", "Percentage"), selected = "None"),
      selectInput("showValues", "Show Data Values", choices = c("None", "Y Variable", "X Variable", "Both"), selected = "None"),
      selectInput("sortVar", "Sort by (numeric variable)", choices = c("None"), selected = "None"),
      actionButton("swap_xy", "Swap X and Y"),
      actionButton("reverse_sort", "Reverse Sort Order"),
      hr(),
      h4("Chart Settings"),
      textInput("title", "Chart Title", "Chart Title"),
      textInput("xLabel", "X Label", "X Variable"),
      textInput("yLabel", "Y Label", "Y Variable"),
      numericInput("pointSize", "Point Size", 3, min = 1, max = 10),
      numericInput("lineSize", "Line Thickness", 1.5, min = 0.5, max = 10, step = 0.5),
  selectInput("theme", "Theme", choices = c("Minimal", "Classic", "Gray", "BW", "Blank")),
      checkboxInput("showLegend", "Show Legend", value = TRUE),
      checkboxInput("xTilt", "Tilt X Axis Labels (45°)", value = TRUE),
      hr(),
      downloadButton("downloadPlot", "Download PDF Report")
    ),
    
    mainPanel(
      class = "sticky-main",   # <- make main panel sticky
      plotOutput("chart", width = "800px", height = "450px"),
      wellPanel(
        textAreaInput("description", "Insight", placeholder = "Add insights here...", rows = 4)
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  chart_dims <- c(width = 8.33, height = 4.6875)
  
  # Reactive state for layers
  app_state <- reactiveVal(list(
    `layer 1` = list(data = NULL, config = list(
      xVar = NULL, yVar = NULL, title = "Chart Title",
      xLabel = "X Variable", yLabel = "Y Variable",
      plotType = "Scatterchart",
      pointSize = 3, theme = "Minimal", description = ""
    ))
  ))
  layer_list <- reactiveVal(c("layer 1"))
  
  # Update inputs when layer changes
  observeEvent(input$layer, {
    state <- app_state()[[input$layer]]
    choices <- if (is.null(state$data)) character(0) else names(state$data)
    updateSelectInput(session, "xVar", choices = choices, selected = state$config$xVar)
    updateSelectInput(session, "yVar", choices = choices, selected = state$config$yVar)
    updateSelectInput(session, "plotType", selected = state$config$plotType)
    updateTextInput(session, "title", value = state$config$title)
    updateTextInput(session, "xLabel", value = state$config$xLabel)
    updateTextInput(session, "yLabel", value = state$config$yLabel)
    updateNumericInput(session, "pointSize", value = state$config$pointSize)
    updateSelectInput(session, "theme", selected = state$config$theme)
    updateTextAreaInput(session, "description", value = state$config$description)
  })
  
  # Save input changes
  observe({
    state <- app_state()
    state[[input$layer]]$config <- list(
      xVar = input$xVar, yVar = input$yVar, title = input$title,
      xLabel = input$xLabel, yLabel = input$yLabel,
      plotType = input$plotType,
      pointSize = input$pointSize,
      lineSize = input$lineSize,
      theme = input$theme,
      description = input$description
    )
    app_state(state)
  })
  
  # Handle data upload
  observeEvent(input$dataFile, {
    ext <- tolower(tools::file_ext(input$dataFile$name))
    data <- if (ext == "csv") read.csv(input$dataFile$datapath)
            else if (ext == "xlsx") readxl::read_excel(input$dataFile$datapath)
            else stop("Unsupported file type")
    state <- app_state()
    state[[input$layer]]$data <- data
    app_state(state)
    col_names <- names(data)
    x_default <- if (length(col_names) >= 1) col_names[1] else NULL
    y_default <- if (length(col_names) >= 2) col_names[2] else NULL
    updateSelectInput(session, "xVar", choices = col_names, selected = x_default)
    updateSelectInput(session, "yVar", choices = col_names, selected = y_default)
    # Update sortVar choices to only numeric columns
    num_vars <- names(data)[sapply(data, is.numeric)]
    updateSelectInput(session, "sortVar", choices = c("None", num_vars), selected = "None")
  })
  
  # Add layer
  observeEvent(input$add_layer, {
    layers <- layer_list()
    new_layer <- paste("layer", length(layers) + 1)
    state <- app_state()
    state[[new_layer]] <- list(data = NULL, config = list(
      xVar = NULL, yVar = NULL, title = "Chart Title",
      xLabel = "X Variable", yLabel = "Y Variable",
      plotType = "Scatterchart",
      pointSize = 3, theme = "Minimal", description = ""
    ))
    app_state(state)
    layer_list(c(layers, new_layer))
    updateSelectInput(session, "layer", choices = layer_list(), selected = new_layer)
  })
  
  # Delete layer
  observeEvent(input$delete_layer, {
    if (input$layer == "layer 1") return()
    state <- app_state()
    state[[input$layer]] <- NULL
    layers <- layer_list()
    layers <- layers[layers != input$layer]
    app_state(state)
    layer_list(layers)
    updateSelectInput(session, "layer", choices = layers, selected = layers[1])
  })
  
  # Sort order state
  sort_ascending <- reactiveVal(TRUE)

  # Reverse sort order button
  observeEvent(input$reverse_sort, {
    sort_ascending(!sort_ascending())
    # Force chart update
    output$chart <- renderPlot({
      if (is.null(chart_reactive())) {
        plot.new()
        text(0.5, 0.5, "Upload data and select variables.", adj = c(0.5, 0.5))
      } else {
        chart_reactive()
      }
    }, width = 800, height = 450)
  })

  # Update chart when sortVar changes
  observeEvent(input$sortVar, {
    output$chart <- renderPlot({
      if (is.null(chart_reactive())) {
        plot.new()
        text(0.5, 0.5, "Upload data and select variables.", adj = c(0.5, 0.5))
      } else {
        chart_reactive()
      }
    }, width = 800, height = 450)
  })

  # Swap X and Y variables
  observeEvent(input$swap_xy, {
    state <- app_state()
    current_layer <- input$layer
    # Swap variables in state
    if (!is.null(state[[current_layer]])) {
      # Store current values
      old_x <- input$xVar
      old_y <- input$yVar
      old_x_label <- input$xLabel
      old_y_label <- input$yLabel
      # Update UI
      updateSelectInput(session, "xVar", selected = old_y)
      updateSelectInput(session, "yVar", selected = old_x)
      updateTextInput(session, "xLabel", value = old_y_label)
      updateTextInput(session, "yLabel", value = old_x_label)
      # Update state
      state[[current_layer]]$config$xVar <- old_y
      state[[current_layer]]$config$yVar <- old_x
      state[[current_layer]]$config$xLabel <- old_y_label
      state[[current_layer]]$config$yLabel <- old_x_label
      app_state(state)
    }
  })
  
  # Build chart based on type
  build_chart <- function(df, cfg) {
    if (is.null(df) || length(cfg$xVar) == 0) return(NULL)
    sort_var <- input$sortVar
    asc <- sort_ascending()
    reorder_x <- function(x) x
    # Only reorder if a sort variable is selected and not None
    if (!is.null(sort_var) && sort_var != "None" && sort_var %in% names(df)) {
      reorder_x <- function(x) reorder(x, if (asc) df[[sort_var]] else -df[[sort_var]])
    } else {
      # If Sort By is None, preserve original data order for barchart and linechart
      reorder_x <- function(x) factor(x, levels = unique(x))
    }
    pt <- cfg$plotType
    show_val_mode <- input$showValues
    show_legend <- input$showLegend
    y_transform <- input$yTransform
    # Transform y values to percentage if selected (Piechart handled below)
    df_trans <- df
    y_is_percent <- FALSE
    if (y_transform == "Percentage" && pt != "Piechart" && length(cfg$yVar) > 0 && is.numeric(df[[cfg$yVar[1]]])) {
      total <- sum(df[[cfg$yVar[1]]], na.rm = TRUE)
      if (total != 0) {
        df_trans[[cfg$yVar[1]]] <- round(df[[cfg$yVar[1]]] / total * 100, 1)
      } else {
        df_trans[[cfg$yVar[1]]] <- 0
      }
      y_is_percent <- TRUE
    }
    p <- switch(pt,
      "Scatterchart" = {
        g <- ggplot(df_trans, aes(x = reorder_x(.data[[cfg$xVar[1]]]), y = .data[[cfg$yVar[1]]])) +
          geom_point(size = cfg$pointSize)
        if (show_val_mode == "Y Variable") {
          g <- g + geom_text(aes(label = if (y_is_percent) paste0(round(.data[[cfg$yVar[1]]],1), "%") else .data[[cfg$yVar[1]]]), vjust = -0.5, size = 3)
        } else if (show_val_mode == "X Variable") {
          g <- g + geom_text(aes(label = .data[[cfg$xVar[1]]]), vjust = -0.5, size = 3)
        } else if (show_val_mode == "Both") {
          g <- g + geom_text(aes(label = if (y_is_percent) paste(.data[[cfg$xVar[1]]], paste0(round(.data[[cfg$yVar[1]]],1), "%"), sep = ", ") else paste(.data[[cfg$xVar[1]]], .data[[cfg$yVar[1]]], sep = ", ")), vjust = -0.5, size = 3)
        }
        g
      },
      "Barchart" = {
        xvar <- cfg$xVar[1]
        yvar <- cfg$yVar[1]
        g <- ggplot(df_trans, aes(x = reorder_x(.data[[xvar]]), y = .data[[yvar]])) +
          geom_col()
        if (show_val_mode == "Y Variable") {
          g <- g + geom_text(aes(label = if (y_is_percent) paste0(round(.data[[yvar]],1), "%") else .data[[yvar]]), vjust = -0.5, size = 3)
        } else if (show_val_mode == "X Variable") {
          g <- g + geom_text(aes(label = .data[[xvar]]), vjust = -0.5, size = 3)
        } else if (show_val_mode == "Both") {
          g <- g + geom_text(aes(label = if (y_is_percent) paste(.data[[xvar]], paste0(round(.data[[yvar]],1), "%"), sep = ", ") else paste(.data[[xvar]], .data[[yvar]], sep = ", ")), vjust = -0.5, size = 3)
        }
        g
      },
      "Linechart" = {
        xvar <- cfg$xVar[1]
        yvar <- cfg$yVar[1]
        plot_df <- df_trans
        # If Sort By is None, preserve original order for x
        if (is.null(sort_var) || sort_var == "None" || !sort_var %in% names(df)) {
          plot_df[[xvar]] <- factor(plot_df[[xvar]], levels = unique(plot_df[[xvar]]))
        }
        g <- ggplot(plot_df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
          geom_line(size = cfg$lineSize, group = 1) +
          geom_point(size = cfg$pointSize)
        if (show_val_mode == "Y Variable") {
          g <- g + geom_text(aes(label = if (y_is_percent) paste0(round(.data[[yvar]],1), "%") else .data[[yvar]]), vjust = -0.5, size = 3)
        } else if (show_val_mode == "X Variable") {
          g <- g + geom_text(aes(label = .data[[xvar]]), vjust = -0.5, size = 3)
        } else if (show_val_mode == "Both") {
          g <- g + geom_text(aes(label = if (y_is_percent) paste(.data[[xvar]], paste0(round(.data[[yvar]],1), "%"), sep = ", ") else paste(.data[[xvar]], .data[[yvar]], sep = ", ")), vjust = -0.5, size = 3)
        }
        g
      },
      "Piechart" = {
        # Use the first selected x variable as the category
        cat_var <- cfg$xVar[1]
        num_var <- NULL
        # Try to find a numeric variable in xVar or yVar
        if (length(cfg$yVar) > 0 && is.numeric(df[[cfg$yVar[1]]])) {
          num_var <- cfg$yVar[1]
        } else if (length(cfg$xVar) > 1 && is.numeric(df[[cfg$xVar[2]]])) {
          num_var <- cfg$xVar[2]
        }
        if (is.null(cat_var) || !cat_var %in% names(df)) return(NULL)
        pie_is_percent <- (y_transform == "Percentage")
        if (!is.null(num_var)) {
          # Aggregate numeric variable by category
          agg_df <- aggregate(df[[num_var]], by = list(category = df[[cat_var]]), FUN = sum, na.rm = TRUE)
          names(agg_df)[2] <- "value"
          if (pie_is_percent) {
            total <- sum(agg_df$value, na.rm = TRUE)
            if (total != 0) {
              agg_df$value <- round(agg_df$value / total * 100, 1)
            } else {
              agg_df$value <- 0
            }
          }
          g <- ggplot(agg_df, aes(x = "", y = value, fill = category)) +
            geom_col(width = 1) +
            coord_polar(theta = "y") +
            theme_void()
          if (show_val_mode == "Y Variable") {
            g <- g + geom_text(aes(label = if (pie_is_percent) paste0(round(value,1), "%") else value), position = position_stack(vjust = 0.5), size = 3)
          } else if (show_val_mode == "X Variable") {
            g <- g + geom_text(aes(label = category), position = position_stack(vjust = 0.5), size = 3)
          } else if (show_val_mode == "Both") {
            g <- g + geom_text(aes(label = if (pie_is_percent) paste(category, paste0(round(value,1), "%"), sep = ", ") else paste(category, value, sep = ", ")), position = position_stack(vjust = 0.5), size = 3)
          }
        } else {
          df_count <- as.data.frame(table(df[[cat_var]]))
          names(df_count) <- c("category", "n")
          if (pie_is_percent) {
            total <- sum(df_count$n, na.rm = TRUE)
            if (total != 0) {
              df_count$n <- round(df_count$n / total * 100, 1)
            } else {
              df_count$n <- 0
            }
          }
          g <- ggplot(df_count, aes(x = "", y = n, fill = category)) +
            geom_col(width = 1) +
            coord_polar(theta = "y") +
            theme_void()
          if (show_val_mode == "Y Variable") {
            g <- g + geom_text(aes(label = if (pie_is_percent) paste0(round(n,1), "%") else n), position = position_stack(vjust = 0.5), size = 3)
          } else if (show_val_mode == "X Variable") {
            g <- g + geom_text(aes(label = category), position = position_stack(vjust = 0.5), size = 3)
          } else if (show_val_mode == "Both") {
            g <- g + geom_text(aes(label = if (pie_is_percent) paste(category, paste0(round(n,1), "%"), sep = ", ") else paste(category, n, sep = ", ")), position = position_stack(vjust = 0.5), size = 3)
          }
        }
        g
      },
      "Mapchart Sumba Timur" = {
        # --- Mapchart Sumba Timur: Choropleth by subdistrict ---
        gpkg_path <- list.files("www", pattern = "ADMINISTRASIKECAMATAN_AR\\.gpkg$", full.names = TRUE)
        if (length(gpkg_path) == 0) return(NULL)
        map_sf <- sf::st_read(gpkg_path[1], quiet = TRUE)
        # Filter to only Sumba Timur
        if ("WADMKK" %in% names(map_sf)) {
          map_sf <- map_sf[map_sf$WADMKK == "Sumba Timur", ]
        }

        # Defensive: require both X and Y variable selection and uploaded data
        if (is.null(df) || is.null(cfg$xVar) || is.null(cfg$yVar) ||
            length(cfg$xVar) == 0 || length(cfg$yVar) == 0) {
          return(ggplot(map_sf) +
                   geom_sf(fill = "grey90", color = "white") +
                   geom_sf_text(aes(label = NAMOBJ), size = 3) +
                   labs(title = "Mapchart: Sumba Timur", subtitle = "No data uploaded or variables selected"))
        }

        # X = subdistrict name in user data, Y = value to color
        join_col <- cfg$xVar[1]
        value_col <- cfg$yVar[1]
        if (!(join_col %in% names(df)) || !(value_col %in% names(df))) {
          return(ggplot(map_sf) +
                   geom_sf(fill = "grey90", color = "white") +
                   geom_sf_text(aes(label = NAMOBJ), size = 3) +
                   labs(title = "Mapchart: Sumba Timur", subtitle = "Selected columns not found in data"))
        }

        # Prepare join: rename user X column to NAMOBJ for join
        join_data <- df %>% dplyr::rename(NAMOBJ = !!join_col)
        # Only keep NAMOBJ and value_col to avoid accidental column overwrite
        join_data <- join_data[, c("NAMOBJ", value_col)]

        # Transform Y values to percentage if selected
        y_transform <- input$yTransform
        y_is_percent <- FALSE
        if (y_transform == "Percentage" && is.numeric(join_data[[value_col]])) {
          total <- sum(join_data[[value_col]], na.rm = TRUE)
          if (total != 0) {
            join_data[[value_col]] <- round(join_data[[value_col]] / total * 100, 1)
          } else {
            join_data[[value_col]] <- 0
          }
          y_is_percent <- TRUE
        }

        # Join user data to map by NAMOBJ
        map_sf <- dplyr::left_join(map_sf, join_data, by = "NAMOBJ")

        # Plot: fill by user Y variable, label by NAMOBJ and/or value, according to showValues
        show_val_mode <- input$showValues
        base <- ggplot(map_sf) +
          geom_sf(aes(fill = .data[[value_col]]), color = "white") +
          scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
          labs(title = "Mapchart: Sumba Timur", fill = if (y_is_percent) paste0(value_col, " (%)") else value_col) +
          theme_minimal()

        # Determine label for each polygon
        label_expr <- switch(show_val_mode,
          "Y Variable" = if (y_is_percent) paste0(map_sf[[value_col]], "%") else paste0(map_sf[[value_col]]),
          "X Variable" = map_sf$NAMOBJ,
          "Both" = if (y_is_percent) paste0(map_sf$NAMOBJ, ": ", map_sf[[value_col]], "%") else paste0(map_sf$NAMOBJ, ": ", map_sf[[value_col]]),
          # Default: None
          NULL
        )
        if (!is.null(label_expr)) {
          base <- base + geom_sf_text(aes(label = label_expr), size = 3)
        }
        base
            }
          )
          # If y is percent, append % to y axis label
          ylab_out <- cfg$yLabel
          if ((y_is_percent && pt != "Piechart") || (pt == "Piechart" && y_transform == "Percentage")) {
            ylab_out <- paste0(cfg$yLabel, " (%)")
          }
          p <- p + labs(title = cfg$title, x = cfg$xLabel, y = ylab_out) +
            switch(cfg$theme,
                   Minimal = theme_minimal(),
                   Classic = theme_classic(),
                   Gray = theme_gray(),
                   BW = theme_bw(),
                   Blank = theme_void())
          if (input$xTilt && cfg$theme != "Blank") {
            p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          } else if (cfg$theme == "Blank") {
            p <- p + theme(axis.text.x = element_blank())
          }
          if (!show_legend) {
            p <- p + theme(legend.position = "none")
          }
          p
        }
  
  # Chart is fully reactive to all relevant user input
  chart_reactive <- reactive({
    # Depend on all relevant inputs for Mapchart Sumba Timur
    input$dataFile
    input$xVar
    input$yVar
    input$plotType
    input$sortVar
    input$showValues
    input$showLegend
    input$yTransform
    input$xTilt
    input$layer
    state <- app_state()[[input$layer]]
    build_chart(state$data, state$config)
  })

  # Render chart
  output$chart <- renderPlot({
    if (is.null(chart_reactive())) {
      plot.new()
      text(0.5, 0.5, "Upload data and select variables.", adj = c(0.5, 0.5))
    } else {
      chart_reactive()
    }
  }, width = 800, height = 450)
  
  # UTC+8 time
  get_utc8_time <- function() {
    format(Sys.time() + 8 * 3600, "%Y-%m-%d %H:%M:%S UTC+8")
  }
  
  # Text wrapping
  wrap_text <- function(text, max_chars = 40) {
    paste(strwrap(text, max_chars), collapse = "\n")
  }
  
  # Generate header image
  generate_header_image <- function(density = 300) {
    bg_path <- "www/background.pdf"
    if (!file.exists(bg_path)) {
      return(image_blank(600, 400, "lightgray") %>%
               image_annotate("Background PDF not found", size = 20, color = "red", gravity = "center"))
    }
    
    image <- image_read_pdf(bg_path, density = density)
    img_height <- image_info(image)$height
    image %>%
      image_annotate(wrap_text(input$reportTitle, 25), size = 26, color = "black", weight = 700,
                     gravity = "north", location = paste0("+0+", round(img_height * 0.25))) %>%
      image_annotate(wrap_text(input$reportDescription, 80), size = 14, color = "black", weight = 700,
                     gravity = "north", location = paste0("+0+", round(img_height * 0.5))) %>%
      image_annotate(paste("Author:", input$userName), size = 12, color = "black", weight = 700,
                     gravity = "north", location = paste0("+0+", round(img_height * 0.8))) %>%
      image_annotate(paste("Export:", get_utc8_time()), size = 12, color = "black", weight = 700,
                     gravity = "north", location = paste0("+0+", round(img_height * 0.83)))
  }
  
  # Download PDF
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("report-", Sys.Date(), ".pdf"),
    content = function(file) {
      # Header page
      header_temp <- tempfile(fileext = ".pdf")
      image_write(generate_header_image(), header_temp, format = "pdf")
      temp_pages <- header_temp
      
      # Background image
      bg_path <- "www/background.pdf"
      bg_image <- if (file.exists(bg_path)) image_read_pdf(bg_path, density = 300) else image_blank(800, 600, "white")
      
      # Process layers
      for (layer in layer_list()) {
        state <- app_state()[[layer]]
        if (is.null(state$data) || length(state$config$xVar) == 0 || length(state$config$yVar) == 0) next
        
        # Create chart using build_chart()
        p <- build_chart(state$data, state$config)
        
        # Save chart
        temp_chart <- tempfile(fileext = ".pdf")
        pdf(temp_chart, width = chart_dims["width"], height = chart_dims["height"])
        print(p)
        dev.off()
        
        # Process insights
        insights <- trimws(state$config$description)
        lines <- if (nchar(insights) > 0) strwrap(insights, width = chart_dims["width"] * 10) else "No description provided."
        text_height <- max(length(lines) * 0.2, 0.4)
        
        # Save insights
        temp_insights <- tempfile(fileext = ".pdf")
        pdf(temp_insights, width = chart_dims["width"], height = text_height)
        par(mar = c(0.2, 0.2, 0.2, 0.2))
        plot.new()
        text(0, 1, paste(lines, collapse = "\n"), adj = c(0, 1), cex = 0.8)
        dev.off()
        
        # Combine images
        chart_img <- image_read_pdf(temp_chart, density = 300) %>% image_border("black", "12x12")
        insights_img <- image_read_pdf(temp_insights, density = 300)
        combined_image <- image_append(c(chart_img, insights_img), stack = TRUE)
        result_image <- image_composite(bg_image, combined_image, gravity = "center")
        
        # Save page
        temp_page <- tempfile(fileext = ".pdf")
        image_write(result_image, temp_page, format = "pdf")
        temp_pages <- c(temp_pages, temp_page)
        unlink(c(temp_chart, temp_insights))
      }
      
      # Combine pages
      last_page_path <- "www/last_page.pdf"
      if (file.exists(last_page_path)) temp_pages <- c(temp_pages, last_page_path)
      pdf_combine(temp_pages, output = file)
      unlink(setdiff(temp_pages, last_page_path))
    }
  )
}

shinyApp(ui, server)
