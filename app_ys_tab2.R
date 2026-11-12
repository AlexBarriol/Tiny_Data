library(shiny)
library(magick)
library(ggplot2)


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


# Normal shoe image: pixel values 0 ~ 255
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


# For values that can be negative:
# photo - average, eigenshoes
plot_signed_image <- function(x, title = "") {
  
  x[!is.finite(x)] <- 0
  
  limit <- max(abs(x))
  
  if (limit == 0) {
    scaled <- rep(0.5, length(x))
  } else {
    scaled <- (x / limit + 1) / 2
  }
  
  img_matrix <- matrix(
    scaled,
    nrow = 64,
    ncol = 64,
    byrow = TRUE
  )
  
  img_raster <- as.raster(
    matrix(
      gray(img_matrix),
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


# Metadata
shoes_meta <- read.csv("shoes_meta.csv")


# Catalogue images
shoe_files <- sprintf("shoes/%04d.png", 1:2000)

X <- t(
  sapply(shoe_files, read_shoe)
)


# PCA
pc <- prcomp(
  X,
  center = TRUE,
  scale. = FALSE
)


# Variance explained by each PC
variance_explained <- pc$sdev^2 / sum(pc$sdev^2) * 100


# ============================================================
# 2. UI
# ============================================================

ui <- navbarPage(
  "Tiny Data",
  
  tabPanel(
    "Explain PCA",
    
    tags$div(id = "top"),
    
    h3("Choose a photo"),
    
    radioButtons(
      "src",
      "Photo",
      c("Catalogue", "Self-check", "Upload"),
      inline = TRUE
    ),
    
    conditionalPanel(
      condition = "input.src == 'Catalogue'",
      
      numericInput(
        "cat_id",
        "Catalogue id",
        1, 1, 2000
      )
    ),
    
    conditionalPanel(
      condition = "input.src == 'Self-check'",
      
      selectInput(
        "sc_file",
        "Self-check photo",
        sprintf("selfcheck_%02d.png", 1:4)
      )
    ),
    
    conditionalPanel(
      condition = "input.src == 'Upload'",
      
      fileInput(
        "photo",
        "Upload",
        accept = c(".jpg", ".jpeg", ".png")
      )
    ),
    
    sliderInput(
      "K",
      "Number of PCs (K)",
      1, 100, 20
    ),
    
    fluidRow(
      column(
        12,
        
        tags$a(
          href = "#step1",
          class = "btn btn-default",
          "1 Average"
        ),
        
        tags$a(
          href = "#step2",
          class = "btn btn-default",
          "2 Difference"
        ),
        
        tags$a(
          href = "#step3",
          class = "btn btn-default",
          "3 Score Map"
        ),
        
        tags$a(
          href = "#step4",
          class = "btn btn-default",
          "4 Eigenshoes"
        ),
        
        tags$a(
          href = "#step5",
          class = "btn btn-default",
          "5 Scores"
        ),
        
        tags$a(
          href = "#step6",
          class = "btn btn-default",
          "6 Rebuild"
        )
      )
    ),
    
    
    # --------------------------------------------------------
    # Step 1
    # --------------------------------------------------------
    
    h3(id = "step1", "Step 1. Average shoe"),
    
    p("The average of all 2,000 catalogue photos."),
    
    plotOutput(
      "average_plot",
      height = "250px"
    ),
    
    tags$a(
      href = "#top",
      "Back to top"
    ),
    
    
    # --------------------------------------------------------
    # Step 2
    # --------------------------------------------------------
    
    h3(id = "step2", "Step 2. Photo - average"),
    
    p("Shows where the selected photo is brighter or darker than the average shoe."),
    
    fluidRow(
      
      column(
        6,
        h4("Selected photo"),
        plotOutput(
          "input_plot",
          height = "250px"
        )
      ),
      
      column(
        6,
        h4("Photo - average"),
        plotOutput(
          "difference_plot",
          height = "250px"
        )
      )
    ),
    
    tags$a(
      href = "#top",
      "Back to top"
    ),
    
    
    # --------------------------------------------------------
    # Step 3
    # --------------------------------------------------------
    
    h3(id = "step3", "Step 3. Score map"),
    
    p("The selected photo is highlighted on the PC1-PC2 score map."),
    
    fluidRow(
      
      column(
        6,
        h4("Full score map"),
        plotOutput(
          "score_map",
          height = "450px"
        )
      ),
      
      column(
        6,
        h4("Zoom around selected photo"),
        plotOutput(
          "score_zoom",
          height = "450px"
        )
      )
    ),
    
    tags$a(
      href = "#top",
      "Back to top"
    ),
    
    
    # --------------------------------------------------------
    # Step 4
    # --------------------------------------------------------
    
    h3(id = "step4", "Step 4. Eigenshoes"),
    
    p("The first principal component directions shown as 64 x 64 images."),
    
    fluidRow(
      
      column(
        3,
        plotOutput("pc1_plot", height = "200px")
      ),
      
      column(
        3,
        plotOutput("pc2_plot", height = "200px")
      ),
      
      column(
        3,
        plotOutput("pc3_plot", height = "200px")
      ),
      
      column(
        3,
        plotOutput("pc4_plot", height = "200px")
      )
    ),
    
    
    tags$a(
      href = "#top",
      "Back to top"
    ),
    
    
    # --------------------------------------------------------
    # Step 5
    # --------------------------------------------------------
    
    h3(id = "step5", "Step 5. Scores"),
    
    p("The first K PC scores of the selected photo."),
    
    tableOutput("score_table"),
    
    tags$a(
      href = "#top",
      "Back to top"
    ),
    
    
    # --------------------------------------------------------
    # Step 6
    # --------------------------------------------------------
    
    h3(id = "step6", "Step 6. Rebuild"),
    
    p("The selected photo rebuilt from the average shoe and several numbers of PCs."),
    
    fluidRow(
      
      column(
        3,
        plotOutput("rebuild5", height = "200px")
      ),
      
      column(
        3,
        plotOutput("rebuild20", height = "200px")
      ),
      
      column(
        3,
        plotOutput("rebuild50", height = "200px")
      ),
      
      column(
        3,
        plotOutput("rebuild100", height = "200px")
      )
    ),
    
    tags$a(
      href = "#top",
      "Back to top"
    )
  )
)


# ============================================================
# 3. Server
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Selected photo
  # ----------------------------------------------------------
  
  selected_photo <- reactive({
    
    if (input$src == "Catalogue") {
      
      X[input$cat_id, ]
      
    } else if (input$src == "Self-check") {
      
      read_shoe(
        file.path(
          "selfcheck",
          input$sc_file
        )
      )
      
    } else if (input$src == "Upload") {
      
      req(input$photo)
      
      read_shoe(
        input$photo$datapath
      )
    }
  })
  
  
  # ----------------------------------------------------------
  # Step 1. Average shoe
  # ----------------------------------------------------------
  
  output$average_plot <- renderPlot({
    
    plot_shoe(
      pc$center,
      "Average shoe"
    )
  })
  
  
  # ----------------------------------------------------------
  # Selected photo
  # ----------------------------------------------------------
  
  output$input_plot <- renderPlot({
    
    plot_shoe(
      selected_photo(),
      "Selected photo"
    )
  })
  
  
  # ----------------------------------------------------------
  # Step 2. Photo - average
  # ----------------------------------------------------------
  
  output$difference_plot <- renderPlot({
    
    difference <- selected_photo() - pc$center
    
    plot_signed_image(
      difference,
      "Photo - average"
    )
  })
  
  
  # ----------------------------------------------------------
  # Step 3. PC1 / PC2 score map
  # ----------------------------------------------------------
  
  output$score_map <- renderPlot({
    
    target <- selected_photo()
    
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:2, drop = FALSE]
    )
    
    
    score_data <- data.frame(
      PC1 = pc$x[, 1],
      PC2 = pc$x[, 2],
      category = shoes_meta$category
    )
    
    
    ggplot(
      score_data,
      aes(
        x = PC1,
        y = PC2,
        color = category
      )
    ) +
      
      geom_point(
        alpha = 0.5
      ) +
      
      geom_point(
        data = data.frame(
          PC1 = target_score[1],
          PC2 = target_score[2]
        ),
        aes(
          x = PC1,
          y = PC2
        ),
        inherit.aes = FALSE,
        shape = 21,
        size = 4.5,
        stroke = 1.7,
        color = "navy",
        fill = "white"
      ) +
      
      annotate(
        "text",
        x = target_score[1],
        y = target_score[2],
        label = "Selected",
        hjust = -0.2,
        vjust = -0.8,
        size = 4,
        color = "navy"
      ) +
      
      labs(
        title = "PC1 - PC2 Score Map",
        color = "Category"
      ) +
      
      theme_minimal()
  })
  
  output$score_zoom <- renderPlot({
    
    target <- selected_photo()
    
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:2, drop = FALSE]
    )
    
    score_data <- data.frame(
      PC1 = pc$x[, 1],
      PC2 = pc$x[, 2],
      category = shoes_meta$category
    )
    
    score_distance <- sqrt(
      (score_data$PC1 - target_score[1])^2 +
        (score_data$PC2 - target_score[2])^2
    )
    
    near_id <- order(score_distance)[1:30]
    
    zoom_data <- score_data[near_id, ]
    
    ggplot(
      zoom_data,
      aes(
        x = PC1,
        y = PC2,
        color = category
      )
    ) +
      
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      
      geom_point(
        data = data.frame(
          PC1 = target_score[1],
          PC2 = target_score[2]
        ),
        aes(
          x = PC1,
          y = PC2
        ),
        inherit.aes = FALSE,
        shape = 21,
        size = 4.5,
        stroke = 1.7,
        color = "navy",
        fill = "white"
      ) +
      
      annotate(
        "text",
        x = target_score[1],
        y = target_score[2],
        label = "Selected",
        hjust = -0.2,
        vjust = -0.8,
        size = 4,
        color = "navy"
      ) +
      
      labs(
        title = "30 Nearest Points",
        color = "Category"
      ) +
      
      theme_minimal()
  })
  
  
  # ----------------------------------------------------------
  # Step 4. Eigenshoes
  # ----------------------------------------------------------
  
  output$pc1_plot <- renderPlot({
    
    plot_signed_image(
      pc$rotation[, 1],
      paste0(
        "PC1: ",
        round(variance_explained[1], 2),
        "%"
      )
    )
  })
  
  
  output$pc2_plot <- renderPlot({
    
    plot_signed_image(
      pc$rotation[, 2],
      paste0(
        "PC2: ",
        round(variance_explained[2], 2),
        "%"
      )
    )
  })
  
  
  output$pc3_plot <- renderPlot({
    
    plot_signed_image(
      pc$rotation[, 3],
      paste0(
        "PC3: ",
        round(variance_explained[3], 2),
        "%"
      )
    )
  })
  
  
  output$pc4_plot <- renderPlot({
    
    plot_signed_image(
      pc$rotation[, 4],
      paste0(
        "PC4: ",
        round(variance_explained[4], 2),
        "%"
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # Step 5. First K scores
  # ----------------------------------------------------------
  
  output$score_table <- renderTable({
    
    target <- selected_photo()
    
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:input$K, drop = FALSE]
    )
    
    score_table <- as.data.frame(
      t(round(target_score, 4))
    )
    
    names(score_table) <- paste0(
      "PC",
      1:input$K
    )
    
    score_table
  })
  
  
  # ----------------------------------------------------------
  # Reconstruction function
  # ----------------------------------------------------------
  
  rebuild_photo <- function(target, K) {
    
    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:K, drop = FALSE]
    )
    
    rebuilt <- pc$center +
      as.numeric(
        pc$rotation[, 1:K, drop = FALSE] %*%
          target_score
      )
    
    rebuilt
  }
  
  
  # ----------------------------------------------------------
  # Step 6. Rebuild with several K
  # ----------------------------------------------------------
  
  output$rebuild5 <- renderPlot({
    
    plot_shoe(
      rebuild_photo(
        selected_photo(),
        5
      ),
      "K = 5"
    )
  })
  
  
  output$rebuild20 <- renderPlot({
    
    plot_shoe(
      rebuild_photo(
        selected_photo(),
        20
      ),
      "K = 20"
    )
  })
  
  
  output$rebuild50 <- renderPlot({
    
    plot_shoe(
      rebuild_photo(
        selected_photo(),
        50
      ),
      "K = 50"
    )
  })
  
  
  output$rebuild100 <- renderPlot({
    
    plot_shoe(
      rebuild_photo(
        selected_photo(),
        100
      ),
      "K = 100"
    )
  })
}


shinyApp(ui, server)