library(shiny)
library(magick)


# ============================================================
# 1. Data / PCA preparation
# ============================================================

# Read one photo as 4,096 numbers
read_shoe <- function(path) {
  
  img <- image_read(path)
  img <- image_resize(img, "64x64!")
  
  x <- as.numeric(
    t(
      matrix(
        as.integer(image_data(img, channels = "gray")),
        64, 64
      )
    )
  )
  
  x
}

# 사진 벡터 
plot_shoe <- function(x, title = "") {
  
  # Remove invalid values
  x[!is.finite(x)] <- 0
  
  # PCA reconstruction can go below 0 or above 255.
  # Clip values only for displaying the image.
  x <- pmin(pmax(x, 0), 255)
  
  img_matrix <- matrix(
    x,
    nrow = 64,
    ncol = 64,
    byrow = TRUE
  )
  
  img_raster <- as.raster(
    matrix(
      gray(img_matrix / 255),
      nrow = 64,
      ncol = 64
    )
  )
  
  par(mar = c(1, 1, 3, 1))
  
  plot.new()
  plot.window(
    xlim = c(0, 1),
    ylim = c(0, 1),
    asp = 1
  )
  
  rasterImage(
    img_raster,
    0, 0, 1, 1
  )
  
  title(main = title)
}

# Read metadata
shoes_meta <- read.csv("shoes_meta.csv")


# File names: shoes/0001.png ~ shoes/2000.png
shoe_files <- sprintf("shoes/%04d.png", 1:2000)


# Read all 2,000 photos
# rows = photos
# columns = 4,096 pixels
X <- t(sapply(shoe_files, read_shoe))


# PCA
pc <- prcomp(
  X,
  center = TRUE,
  scale. = FALSE
)


# ============================================================
# 2. UI
# ============================================================

ui <- navbarPage(
  "Tiny Data",
  
  tabPanel(
    "Shoe Search",
    
    radioButtons(
      "src",
      "Photo",
      c("Catalogue", "Self-check", "Upload"),
      inline = TRUE
    ),
    
    numericInput(
      "cat_id",
      "Catalogue id",
      1, 1, 2000
    ),
    
    selectInput(
      "sc_file",
      "Self-check photo",
      sprintf("selfcheck_%02d.png", 1:4)
    ),
    
    fileInput(
      "photo",
      "Upload",
      accept = c(".jpg", ".jpeg", ".png")
    ),
    
    sliderInput(
      "K",
      "Number of PCs (K)",
      1, 100, 20
    ),
    
    h3("Best Match"),
    
    fluidRow(
      
      column(
        3,
        h4("Input photo"),
        plotOutput("input_plot", height = "200px")
      ),
      
      column(
        3,
        h4("Closest catalogue photo"),
        plotOutput("best_plot", height = "200px")
      ),
      
      column(
        3,
        h4("Rebuilt with K PCs"),
        plotOutput("rebuild_plot", height = "200px")
      ),
      
      column(
        3,
        h4("Average shoe"),
        plotOutput("average_plot", height = "200px")
      )
    ),
    
    h4("Top 5"),
    tableOutput("top5"),
    
    h4("Prediction"),
    textOutput("result")
  )
)


# ============================================================
# 3. Server
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Photo selected by the user
  # ----------------------------------------------------------
  
  selected_photo <- reactive({
    
    if (input$src == "Catalogue") {
      
      X[input$cat_id, ]
      
    } else if (input$src == "Self-check") {
      
      read_shoe(
        file.path("selfcheck", input$sc_file)
      )
      
    } else if (input$src == "Upload") {
      
      req(input$photo)
      
      read_shoe(
        input$photo$datapath
      )
      
    }
    
  })
  
  
  # ----------------------------------------------------------
  # Find Top 5 matches
  # ----------------------------------------------------------
  
  top5_result <- reactive({
    
    target <- selected_photo()
    
    # Target photo's first K PC scores
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:input$K, drop = FALSE]
    )
    
    # Catalogue photos' first K PC scores
    catalogue_score <- pc$x[, 1:input$K, drop = FALSE]
    
    
    # Euclidean distance
    distance <- sqrt(
      rowSums(
        sweep(
          catalogue_score,
          2,
          target_score,
          "-"
        )^2
      )
    )
    
    
    # If the target is a catalogue photo,
    # leave itself out of the search
    if (input$src == "Catalogue") {
      distance[input$cat_id] <- Inf
    }
    
    
    # IDs of the five closest photos
    top5_id <- order(distance)[1:5]
    
    
    data.frame(
      id = top5_id,
      category = shoes_meta$category[
        match(top5_id, shoes_meta$id)
      ],
      distance = round(distance[top5_id], 4)
    )
  })
  
  
  # ----------------------------------------------------------
  # Input photo
  # ----------------------------------------------------------
  
  output$input_plot <- renderPlot({
    
    plot_shoe(
      selected_photo(),
      "Input"
    )
  })
  
  
  # ----------------------------------------------------------
  # Average shoe
  # ----------------------------------------------------------
  
  output$average_plot <- renderPlot({
    
    plot_shoe(
      pc$center,
      "Average"
    )
  })
  
  # ----------------------------------------------------------
  # Rebuilt photo with K PCs
  # ----------------------------------------------------------
  
  output$rebuild_plot <- renderPlot({
    
    target <- selected_photo()
    
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:input$K, drop = FALSE]
    )
    
    rebuilt <- pc$center +
      as.numeric(
        pc$rotation[, 1:input$K, drop = FALSE] %*%
          target_score
      )
    
    plot_shoe(
      rebuilt,
      paste("K =", input$K)
    )
  })
  
  # ----------------------------------------------------------
  # Best match
  # ----------------------------------------------------------
  
  output$best_plot <- renderPlot({
    
    best_id <- top5_result()$id[1]
    
    plot_shoe(
      X[best_id, ],
      paste("ID", best_id)
    )
  })
  
  
  # ----------------------------------------------------------
  # Top 5 table
  # ----------------------------------------------------------
  
  output$top5 <- renderTable({
    top5_result()
  })
  
  
  # ----------------------------------------------------------
  # Prediction
  # ----------------------------------------------------------
  
  output$result <- renderText({
    
    result <- top5_result()
    
    # Most common category among Top 5
    prediction <- names(
      which.max(
        table(result$category)
      )
    )
    
    
    if (input$src == "Catalogue") {
      
      answer <- shoes_meta$category[
        match(input$cat_id, shoes_meta$id)
      ]
      
      paste(
        "Prediction:", prediction,
        "| Answer:", answer,
        "|",
        ifelse(
          prediction == answer,
          "Correct",
          "Incorrect"
        )
      )
      
    } else {
      
      paste(
        "Prediction:",
        prediction
      )
    }
  })
}


shinyApp(ui, server)