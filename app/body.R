library(shiny)
library(ggplot2)
library(magick)
library(readxl)
library(pdftools)

ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      selectInput("layer", "Choose Layer", choices = "layer 1"),
      actionButton("add_layer", "Add New Layer"),
      actionButton("delete_layer", "Delete This Layer"),
      hr(),
      fileInput("dataFile", "Upload CSV/XLSX", accept = c(".csv", ".xlsx")),
      selectInput("xVar", "Variabel X", choices = NULL),
      selectInput("yVar", "Variabel Y", choices = NULL),
      hr(),
      h4("Pengaturan Chart"),
      textInput("title", "Title", value = "Judul Chart"),
      textInput("xLabel", "Label X", value = "Variabel X"),
      textInput("yLabel", "Label Y", value = "Variabel Y"),
      numericInput("pointSize", "Besar Poin", value = 3, min = 1, max = 10),
      selectInput("theme", "Tema", choices = c("Minimal", "Classic", "Gray", "BW")),
      hr(),
      downloadButton("downloadPlot", "Download PDF"),
    ),
    mainPanel(
      plotOutput("chart", width = "800px", height = "450px"),
      wellPanel(
        textAreaInput("description", "Insight/Deskripsi", placeholder = "Tambahkan insight/deskripsi disini...", rows = 4)
      )
    )
  )
)

server <- function(input, output, session) {
  chart_width <- 8.33
  chart_height <- 4.6875
  
  # Reactive values for layers
  app_state <- reactiveVal(list(
    `layer 1` = list(
      data = NULL,
      config = list(
        xVar = NULL, yVar = NULL,
        title = "Judul Chart", xLabel = "Variabel X", yLabel = "Variabel Y",
        pointSize = 3, theme = "Minimal", description = ""
      )
    )
  ))
  layer_list <- reactiveVal(c("layer 1"))
  
  # Update UI inputs when layer changes
  observeEvent(input$layer, {
    state <- app_state()[[input$layer]]
    choices <- if (is.null(state$data)) character(0) else names(state$data)
    updateSelectInput(session, "xVar", choices = choices, selected = state$config$xVar)
    updateSelectInput(session, "yVar", choices = choices, selected = state$config$yVar)
    updateTextInput(session, "title", value = state$config$title)
    updateTextInput(session, "xLabel", value = state$config$xLabel)
    updateTextInput(session, "yLabel", value = state$config$yLabel)
    updateNumericInput(session, "pointSize", value = state$config$pointSize)
    updateSelectInput(session, "theme", selected = state$config$theme)
    updateTextAreaInput(session, "description", value = state$config$description)
  }, priority = 1)
  
  # Save all input changes to current layer
  observe({
    state <- app_state()
    state[[input$layer]]$config <- list(
      xVar = input$xVar,
      yVar = input$yVar,
      title = input$title,
      xLabel = input$xLabel,
      yLabel = input$yLabel,
      pointSize = input$pointSize,
      theme = input$theme,
      description = input$description
    )
    app_state(state)
  })
  
  # Handle data upload for current layer
  observeEvent(input$dataFile, {
    ext <- tools::file_ext(input$dataFile$name)
    data <- if (tolower(ext) == "csv") read.csv(input$dataFile$datapath)
            else if (tolower(ext) == "xlsx") readxl::read_excel(input$dataFile$datapath)
            else stop("Tipe file tidak didukung")
    state <- app_state()
    state[[input$layer]]$data <- data
    state[[input$layer]]$config$xVar <- NULL
    state[[input$layer]]$config$yVar <- NULL
    app_state(state)
    choices <- names(data)
    updateSelectInput(session, "xVar", choices = choices)
    updateSelectInput(session, "yVar", choices = choices)
  })
  
  # Add new layer
  observeEvent(input$add_layer, {
    layers <- layer_list()
    num <- length(layers) + 1
    new_layer <- paste("layer", num)
    state <- app_state()
    state[[new_layer]] <- list(
      data = NULL,
      config = list(
        xVar = NULL, yVar = NULL,
        title = "Judul Chart", xLabel = "Variabel X", yLabel = "Variabel Y",
        pointSize = 3, theme = "Minimal", description = ""
      )
    )
    app_state(state)
    layer_list(c(layers, new_layer))
    updateSelectInput(session, "layer", choices = layer_list(), selected = new_layer)
  })
  
  # Delete layer (protect layer 1)
  observeEvent(input$delete_layer, {
    if (input$layer == "layer 1") return()
    state <- app_state()
    state[[input$layer]] <- NULL
    app_state(state)
    layers <- layer_list()
    layers <- layers[layers != input$layer]
    layer_list(layers)
    updateSelectInput(session, "layer", choices = layers, selected = layers[1])
  })
  
  # Reactive chart for current layer
  chart_reactive <- reactive({
    state <- app_state()[[input$layer]]
    if (is.null(state$data) || is.null(state$config$xVar) || is.null(state$config$yVar)) return(NULL)
    ggplot(state$data, aes(x = .data[[state$config$xVar]], y = .data[[state$config$yVar]])) +
      geom_point(size = state$config$pointSize) +
      labs(title = state$config$title, x = state$config$xLabel, y = state$config$yLabel) +
      switch(state$config$theme,
             "Minimal" = theme_minimal(),
             "Classic" = theme_classic(),
             "Gray" = theme_gray(),
             "BW" = theme_bw())
  })
  
  # Render plot with validation
  output$chart <- renderPlot({
    if (is.null(chart_reactive())) {
      plot.new()
      text(0.5, 0.5, "Upload data baru dan pilih variabel X & Y.", adj = c(0.5, 0.5))
    } else {
      chart_reactive()
    }
  }, width = 800, height = 450)
  
  # Download handler for multi-page PDF
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("chart-", Sys.Date(), ".pdf"),
    content = function(file) {
      bg_path <- "www/background.pdf"
      bg_image <- image_read_pdf(bg_path, density = 300)
      temp_pages <- c()
      
      # Sort layers by number
      all_layers <- layer_list()
      layer_nums <- as.integer(gsub("layer ", "", all_layers))
      sorted_layers <- all_layers[order(layer_nums)]
      
      for (layer in sorted_layers) {
        state <- app_state()[[layer]]
        if (is.null(state$data) || is.null(state$config$xVar) || is.null(state$config$yVar)) next
        
        # Create chart
        p <- ggplot(state$data, aes(x = .data[[state$config$xVar]], y = .data[[state$config$yVar]])) +
          geom_point(size = state$config$pointSize) +
          labs(title = state$config$title, x = state$config$xLabel, y = state$config$yLabel) +
          switch(state$config$theme,
                 "Minimal" = theme_minimal(),
                 "Classic" = theme_classic(),
                 "Gray" = theme_gray(),
                 "BW" = theme_bw())
        
        # Create chart PDF
        temp_chart <- tempfile(fileext = ".pdf")
        pdf(temp_chart, width = chart_width, height = chart_height)
        print(p)
        dev.off()
        
        # Process insights text
        insights <- trimws(state$config$description)
        lines <- if (nchar(insights) > 0) {
          words <- unlist(strsplit(insights, "\\s+"))
          lines <- character()
          current_line <- ""
          for (word in words) {
            test_line <- paste0(current_line, if (current_line == "") "" else " ", word)
            if (strwidth(test_line, units = "inches", cex = 0.8) > chart_width * 0.95) {
              lines <- c(lines, current_line)
              current_line <- word
            } else {
              current_line <- test_line
            }
          }
          c(lines, current_line)
        } else {
          "Tidak ada deskripsi yang diberikan."
        }
        
        # Calculate text height
        text_height <- length(lines) * strheight("A", units = "inches", cex = 0.8) * 1.5 + 0.2
        if (text_height < 0.4) text_height <- 0.4
        
        # Create insights PDF
        temp_insights <- tempfile(fileext = ".pdf")
        pdf(temp_insights, width = chart_width, height = text_height)
        par(mar = c(0.2, 0.2, 0.2, 0.2))
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        text(0, 1, paste(lines, collapse = "\n"), adj = c(0, 1), cex = 0.8)
        dev.off()
        
        # Composite images
        chart_img <- image_read_pdf(temp_chart, density = 300) %>% image_border("black", "12x12")
        insights_img <- image_read_pdf(temp_insights, density = 300)
        combined_image <- image_append(c(chart_img, insights_img), stack = TRUE)
        result_image <- image_composite(bg_image, combined_image, gravity = "center")
        
        # Create individual page PDF
        temp_page <- tempfile(fileext = ".pdf")
        image_write(result_image, path = temp_page, format = "pdf")
        temp_pages <- c(temp_pages, temp_page)
        
        unlink(c(temp_chart, temp_insights))
      }
      
      # Combine pages or create empty PDF
      if (length(temp_pages) > 0) {
        pdf_combine(temp_pages, output = file)
        unlink(temp_pages)
      } else {
        pdf(file, width = chart_width, height = chart_height)
        plot.new()
        text(0.5, 0.5, "No valid layers to export.", adj = c(0.5, 0.5))
        dev.off()
      }
    }
  )
}

shinyApp(ui = ui, server = server)