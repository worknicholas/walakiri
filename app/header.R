library(shiny)
library(magick)

ui <- fluidPage(
  titlePanel("Header"),
  sidebarLayout(
    sidebarPanel(
      textInput("reportTitle", "Report Title", "Laporan Bukti Dukung Insight Statistik"),
      textInput("reportDescription", "Report Description", "Laporan ini dibuat untuk memenuhi bukti dukung capaian kinerja triwulanan untuk kegiatan SAKERNAS"),
      textInput("userName", "User Name", "John Smith"),
      downloadButton("downloadPDF", "Download Report PDF")
    ),
    mainPanel(
      imageOutput("previewImage", width = "600px", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  bg_path <- "www/background.pdf"
  bg_exists <- file.exists(bg_path)
  
  # UTC+8 time function
  get_utc8_time <- function() {
    format(Sys.time() + 8 * 3600, "%Y-%m-%d %H:%M:%S UTC+8")
  }
  
  # Text wrapping function
  wrap_text <- function(text, max_chars = 40) {
    words <- strsplit(text, " ")[[1]]
    lines <- character()
    current_line <- ""
    
    for (word in words) {
      if (nchar(current_line) + nchar(word) + 1 <= max_chars) {
        current_line <- ifelse(current_line == "", word, paste(current_line, word))
      } else {
        lines <- c(lines, current_line)
        current_line <- word
      }
    }
    paste(c(lines, current_line), collapse = "\n")
  }
  
  # Generate image function
  generate_image <- function(density = 100) {
    if (!bg_exists) {
      return(image_blank(600, 400, "lightgray") %>%
               image_annotate("Background PDF not found", size = 20, color = "red", 
                             gravity = "center", weight = 700))
    }
    
    image <- image_read_pdf(bg_path, density = density)
    img_height <- image_info(image)$height
    
    image %>%
      image_annotate(wrap_text(input$reportTitle, 25), size = 25.9, color = "black", weight = 700,
                    gravity = "north", location = paste0("+0+", round(img_height * 0.25))) %>%
      image_annotate(wrap_text(input$reportDescription, 80), size = 13.5, color = "black", weight = 700,
                    gravity = "north", location = paste0("+0+", round(img_height * 0.5))) %>%
      image_annotate(paste("Author:", input$userName), size = 11.4, color = "black", weight = 700,
                    gravity = "north", location = paste0("+0+", round(img_height * 0.8))) %>%
      image_annotate(paste("Export:", get_utc8_time()), size = 11.4, color = "black", weight = 700,
                    gravity = "north", location = paste0("+0+", round(img_height * 0.83)))
  }
  
  # Preview image
  output$previewImage <- renderImage({
    temp_img <- tempfile(fileext = ".png")
    generate_image(100) %>% image_write(temp_img)
    list(src = temp_img, contentType = "image/png", width = 600, height = 400)
  }, deleteFile = TRUE)
  
  # Download handler
  output$downloadPDF <- downloadHandler(
    filename = function() paste0("report-", Sys.Date(), ".pdf"),
    content = function(file) {
      generate_image(300) %>% image_write(file, format = "pdf")
    }
  )
}

shinyApp(ui = ui, server = server)