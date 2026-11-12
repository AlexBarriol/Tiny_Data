library(shiny)
library(magick)


# ============================================================
# 1. Data / PCA preparation
# ============================================================

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


plot_shoe <- function(x, title = "") {
  
  x[!is.finite(x)] <- 0
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


shoes_meta <- read.csv("shoes_meta.csv")

shoe_files <- sprintf(
  "shoes/%04d.png",
  1:2000
)

X <- t(
  sapply(shoe_files, read_shoe)
)

pc <- prcomp(
  X,
  center = TRUE,
  scale. = FALSE
)


# ============================================================
# Our Shoes information
# ============================================================

our_shoes <- data.frame(
  file = c(
    "yunseo_Shoes_1.jpeg",
    "yunseo_Sandals_2.jpeg",
    "yunseo_Slippers_3.jpeg"
  ),
  name = c(
    "Yunseo",
    "Yunseo",
    "Yunseo"
  ),
  category = c(
    "Shoes",
    "Sandals",
    "Slippers"
  )
)


# ============================================================
# 2. UI
# ============================================================

ui <- navbarPage(
  "Tiny Data",
  
  tabPanel(
    "Our Shoes",
    
    h3("Our Shoes"),
    
    p("Shoes photographed by our team members."),
    
    fluidRow(
      
      column(
        4,
        h4("Yunseo - Shoes"),
        imageOutput("our1", width = "250px", height = "250px")
      ),
      
      column(
        4,
        h4("Yunseo - Sandals"),
        imageOutput("our2", width = "250px", height = "250px")
      ),
      
      column(
        4,
        h4("Yunseo - Slippers"),
        imageOutput("our3", width = "250px", height = "250px")
      )
    ),
    
    hr(),
    
    h3("Find a Match"),
    
    selectInput(
      "our_file",
      "Choose a shoe",
      choices = setNames(
        our_shoes$file,
        paste(
          our_shoes$name,
          "-",
          our_shoes$category
        )
      )
    ),
    
    sliderInput(
      "K",
      "Number of PCs (K)",
      1, 100, 20
    ),
    
    h3("Best Match"),
    
    fluidRow(
      
      column(
        6,
        h4("Our shoe"),
        plotOutput(
          "input_plot",
          height = "250px"
        )
      ),
      
      column(
        6,
        h4("Closest catalogue photo"),
        plotOutput(
          "best_plot",
          height = "250px"
        )
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
  
  
  # Original photos
  output$our1 <- renderImage({
    
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "yunseo_Shoes_1.jpeg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Yunseo Shoes"
    )
    
  }, deleteFile = FALSE)
  
  
  output$our2 <- renderImage({
    
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "yunseo_Sandals_2.jpeg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Yunseo Shoes"
    )
    
  }, deleteFile = FALSE)
  
  
  output$our3 <- renderImage({
    
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "yunseo_Slippers_3.jpeg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Yunseo Shoes"
    )
    
  }, deleteFile = FALSE)
  
  
  # Selected shoe
  selected_photo <- reactive({
    
    read_shoe(
      file.path(
        "our_shoes",
        input$our_file
      )
    )
  })
  
  
  # Actual category
  correct_category <- reactive({
    
    our_shoes$category[
      match(
        input$our_file,
        our_shoes$file
      )
    ]
  })
  
  
  # Top 5 matches
  top5_result <- reactive({
    
    target <- selected_photo()
    
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[
          , 1:input$K,
          drop = FALSE
        ]
    )
    
    catalogue_score <- pc$x[
      , 1:input$K,
      drop = FALSE
    ]
    
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
    
    top5_id <- order(distance)[1:5]
    
    data.frame(
      id = top5_id,
      
      category = shoes_meta$category[
        match(
          top5_id,
          shoes_meta$id
        )
      ],
      
      distance = round(
        distance[top5_id],
        4
      )
    )
  })
  
  
  output$input_plot <- renderPlot({
    
    plot_shoe(
      selected_photo(),
      correct_category()
    )
  })
  
  
  output$best_plot <- renderPlot({
    
    best_id <- top5_result()$id[1]
    
    plot_shoe(
      X[best_id, ],
      paste("ID", best_id)
    )
  })
  
  
  output$top5 <- renderTable({
    
    top5_result()
  })
  
  
  output$result <- renderText({
    
    result <- top5_result()
    
    prediction <- names(
      which.max(
        table(result$category)
      )
    )
    
    answer <- correct_category()
    
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
  })
}


shinyApp(ui, server)