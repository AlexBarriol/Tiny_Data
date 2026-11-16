library(shiny)
library(magick)
library(ggplot2)
library(dplyr)


# ============================================================
# 1. Common functions
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


# ============================================================
# 2. Shoe data / PCA
# ============================================================

shoes_meta <- read.csv("shoes_meta.csv")
selfcheck_labels <- read.csv("selfcheck/labels.csv")

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

variance_explained <-
  pc$sdev^2 /
  sum(pc$sdev^2) * 100


# ============================================================
# 3. Our Shoes information
# ============================================================

our_shoes <- data.frame(
  file = c(
    "yunseo_Shoes_1.jpeg",
    "yunseo_Sandals_2.jpeg",
    "yunseo_Slippers_3.jpeg",
    "Afina_Shoes_1.jpg",
    "Afina_Shoes_2.jpg",
    "Afina_Boots_3.jpg",
    "gayeon_Slippers_2.jpeg",
    "gayeon_Slippers_1.jpeg",
    "gayeon_Shoes_3.jpeg"
  ),

  name = c(
    "Yunseo", "Yunseo", "Yunseo",
    "Afina", "Afina", "Afina",
    "Gayeon", "Gayeon", "Gayeon"
  ),

  category = c(
    "Shoes",
    "Sandals",
    "Slippers",
    "Shoes",
    "Shoes",
    "Boots",
    "Slippers",
    "Slippers",
    "Shoes"
  )
)


# ============================================================
# 4. Data 1
# ============================================================

bike <- read.csv(
  "data/bike_summary_2020_2025.csv"
)

bike$date <- as.Date(bike$date)


# ============================================================
# 5. UI
# ============================================================

ui <- navbarPage(
  "Tiny Data",


  # ==========================================================
  # TAB 1. Shoe Search
  # ==========================================================

  tabPanel(
    "Shoe Search",

    h3("Shoe Search"),

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

    fluidRow(
      column(2, plotOutput("top1_plot", height = "150px")),
      column(2, plotOutput("top2_plot", height = "150px")),
      column(2, plotOutput("top3_plot", height = "150px")),
      column(2, plotOutput("top4_plot", height = "150px")),
      column(2, plotOutput("top5_plot", height = "150px"))
    ),

    p("The five catalogue photos most similar to the selected photo based on PCA distance."),
    tableOutput("top5"),

    h4("Prediction"),
    textOutput("result")
  ),


  # ==========================================================
  # TAB 2. Explain PCA
  # ==========================================================

  tabPanel(
    "Explain PCA",

    tags$div(id = "pca_top"),

    h3("Explain PCA"),
    p("This tab explains the photo and K selected in Shoe Search."),

    fluidRow(
      column(
        12,

        tags$a(
          href = "#pca_step1",
          class = "btn btn-default",
          "1 Average"
        ),

        tags$a(
          href = "#pca_step2",
          class = "btn btn-default",
          "2 Difference"
        ),

        tags$a(
          href = "#pca_step3",
          class = "btn btn-default",
          "3 Score Map"
        ),

        tags$a(
          href = "#pca_step4",
          class = "btn btn-default",
          "4 Eigenshoes"
        ),

        tags$a(
          href = "#pca_step5",
          class = "btn btn-default",
          "5 Scores"
        ),

        tags$a(
          href = "#pca_step6",
          class = "btn btn-default",
          "6 Rebuild"
        )
      )
    ),


    h3(id = "pca_step1", "Step 1. Average shoe"),

    p("The average of all 2,000 catalogue photos."),

    plotOutput(
      "pca_average_plot",
      height = "250px"
    ),

    tags$a(
      href = "#pca_top",
      "Back to top"
    ),


    h3(id = "pca_step2", "Step 2. Photo - average"),

    p("Shows where the selected photo is brighter or darker than the average shoe."),

    fluidRow(

      column(
        6,
        h4("Selected photo"),
        plotOutput(
          "pca_input_plot",
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
      href = "#pca_top",
      "Back to top"
    ),


    h3(id = "pca_step3", "Step 3. Score map"),

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
      href = "#pca_top",
      "Back to top"
    ),


    h3(id = "pca_step4", "Step 4. Eigenshoes"),

    p("The first principal component directions shown as 64 x 64 images, with the variance explained by each PC."),

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
      href = "#pca_top",
      "Back to top"
    ),


    h3(id = "pca_step5", "Step 5. Scores"),

    p("The first K PC scores of the selected photo."),

    tableOutput("score_table"),

    tags$a(
      href = "#pca_top",
      "Back to top"
    ),


    h3(id = "pca_step6", "Step 6. Rebuild"),

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
      href = "#pca_top",
      "Back to top"
    )
  ),


  # ==========================================================
  # TAB 3. Our Shoes
  # ==========================================================

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

    fluidRow(

      column(
        4,
        h4("Afina - Shoes 1"),
        imageOutput("our4", width = "250px", height = "250px")
      ),

      column(
        4,
        h4("Afina - Shoes 2"),
        imageOutput("our5", width = "250px", height = "250px")
      ),

      column(
        4,
        h4("Afina - Boots 3"),
        imageOutput("our6", width = "250px", height = "250px")
      )
    ),

    fluidRow(

      column(
        4,
        h4("Gayeon - Slippers 2"),
        imageOutput("our7", width = "250px", height = "250px")
      ),

      column(
        4,
        h4("Gayeon - Slippers 1"),
        imageOutput("our8", width = "250px", height = "250px")
      ),

      column(
        4,
        h4("Gayeon - Shoes 3"),
        imageOutput("our9", width = "250px", height = "250px")
      )
    ),

    hr(),

    h3("Find a Match"),

    selectInput(
      "our_file",
      "Choose a shoe",
      choices = setNames(
        our_shoes$file,
        c(
          "Yunseo - Shoes 1",
          "Yunseo - Sandals 2",
          "Yunseo - Slippers 3",
          "Afina - Shoes 1",
          "Afina - Shoes 2",
          "Afina - Boots 3",
          "Gayeon - Slippers 2",
          "Gayeon - Slippers 1",
          "Gayeon - Shoes 3"
        )
      )
    ),

    sliderInput(
      "our_K",
      "Number of PCs (K)",
      1, 100, 20
    ),

    h3("Best Match"),

    fluidRow(

      column(
        6,
        h4("Our shoe"),
        plotOutput(
          "our_input_plot",
          height = "250px"
        )
      ),

      column(
        6,
        h4("Closest catalogue photo"),
        plotOutput(
          "our_best_plot",
          height = "250px"
        )
      )
    ),

    h4("Top 5"),

    fluidRow(
      column(2, plotOutput("our_top1_plot", height = "150px")),
      column(2, plotOutput("our_top2_plot", height = "150px")),
      column(2, plotOutput("our_top3_plot", height = "150px")),
      column(2, plotOutput("our_top4_plot", height = "150px")),
      column(2, plotOutput("our_top5_plot", height = "150px"))
    ),

    tableOutput("our_top5"),

    h4("Prediction"),

    textOutput("our_result")
  ),


  # ==========================================================
  # TAB 4. Data 1
  # ==========================================================

  tabPanel(
    "Data 1",

    tags$div(id = "data_top"),

    h3("Question"),
    p("How does bike usage change over time?"),

    hr(),

    h4("Go to"),

    fluidRow(
      column(
        3,
        tags$a(
          href = "#data_overview",
          class = "btn btn-default",
          "Overview"
        )
      ),

      column(
        3,
        tags$a(
          href = "#data_hourly",
          class = "btn btn-default",
          "Hourly pattern"
        )
      ),

      column(
        3,
        tags$a(
          href = "#data_monthly",
          class = "btn btn-default",
          "Daily pattern"
        )
      ),

      column(
        3,
        tags$a(
          href = "#data_compare",
          class = "btn btn-default",
          "Compare dates"
        )
      )
    ),

    hr(),


    h3(id = "data_overview", "Overview"),

    fluidRow(

      column(
        3,
        selectInput(
          "year",
          "Year",
          choices = c(
            "All",
            sort(unique(bike$year))
          )
        )
      ),

      column(
        3,
        selectInput(
          "month",
          "Month",
          choices = c(
            "All",
            1:12
          )
        )
      ),

      column(
        3,
        selectInput(
          "weekday",
          "Weekday",
          choices = c(
            "All",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
            "Sunday"
          )
        )
      ),

      column(
        3,
        selectInput(
          "hour",
          "Hour",
          choices = c(
            "All",
            0:23
          )
        )
      )
    ),

    h3("Results"),

    fluidRow(

      column(
        3,
        h4("Total rentals"),
        textOutput("total_rentals")
      ),

      column(
        3,
        h4("Average daily rentals"),
        textOutput("avg_daily")
      ),

      column(
        3,
        h4("Average duration"),
        textOutput("avg_duration")
      ),

      column(
        3,
        h4("Average distance"),
        textOutput("avg_distance")
      )
    ),

    tags$a(
      href = "#data_top",
      "Back to top"
    ),

    hr(),
    
    h3("Bike usage by month"),
    
    plotOutput(
      "monthly_plot",
      height = "350px"
    ),
    
    tags$a(
      href = "#data_top",
      "Back to top"
    ),
    
    hr(),


    h3(id = "data_hourly", "Bike usage by hour"),

    plotOutput(
      "usage_plot",
      height = "350px"
    ),

    tags$a(
      href = "#data_top",
      "Back to top"
    ),

    hr(),


    div(id = "data_monthly"),

    conditionalPanel(
      condition = "input.year != 'All' && input.month != 'All'",

      h3("Daily rentals in selected month"),

      plotOutput(
        "monthly_daily_plot",
        height = "350px"
      ),

      tags$a(
        href = "#data_top",
        "Back to top"
      ),

      hr()
    ),


    h3(id = "data_compare", "Compare specific dates"),

    fluidRow(

      column(
        6,
        dateInput(
          "compare_date1",
          "Compare date 1",
          value = as.Date("2025-01-01"),
          min = min(bike$date),
          max = max(bike$date)
        )
      ),

      column(
        6,
        dateInput(
          "compare_date2",
          "Compare date 2",
          value = as.Date("2025-01-02"),
          min = min(bike$date),
          max = max(bike$date)
        )
      )
    ),

    plotOutput(
      "compare_dates_plot",
      height = "350px"
    ),

    tags$a(
      href = "#data_top",
      "Back to top"
    ),

    hr(),

    h4("Why it matters"),

    p(
      "Understanding when bike usage increases or decreases helps us identify important temporal patterns in bike demand."
    ),

    h4("Next analysis"),

    p(
      "This helps us find time periods with unusual or noticeable changes in bike usage, which we can examine more closely later."
    )
  ),


  # ==========================================================
  # TAB 5. AI
  # ==========================================================

  tabPanel(
    "AI",

    h4("Ethics statement"),

    p(
      "We understand the rules on data and AI use for this project, including following the license and privacy rules",
      "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
      "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
      "and we agree to receive an F in this course if we have not."
    ),

    p("Signed: TODO (every member)"),

    h4("What AI did and what we did"),
    tableOutput("ai_table"),

    h4("Reflection"),
    tableOutput("ai_reflection")
  )
)


# ============================================================
# 6. Server
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # TAB 1 + TAB 2 shared selected photo
  # ==========================================================

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


  # ==========================================================
  # TAB 1. Shoe Search
  # ==========================================================

  top5_result <- reactive({

    target <- selected_photo()

    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[, 1:input$K, drop = FALSE]
    )

    catalogue_score <- pc$x[, 1:input$K, drop = FALSE]

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

    if (input$src == "Catalogue") {
      distance[input$cat_id] <- Inf
    }

    top5_id <- order(distance)[1:5]

    data.frame(
      id = top5_id,
      category = shoes_meta$category[
        match(top5_id, shoes_meta$id)
      ],
      distance = round(distance[top5_id], 4)
    )
  })


  output$input_plot <- renderPlot({
    plot_shoe(
      selected_photo(),
      "Input"
    )
  })


  output$average_plot <- renderPlot({
    plot_shoe(
      pc$center,
      "Average"
    )
  })


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


  output$best_plot <- renderPlot({

    best_id <- top5_result()$id[1]

    plot_shoe(
      X[best_id, ],
      paste("ID", best_id)
    )
  })


  output$top1_plot <- renderPlot({
    id <- top5_result()$id[1]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$top2_plot <- renderPlot({
    id <- top5_result()$id[2]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$top3_plot <- renderPlot({
    id <- top5_result()$id[3]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$top4_plot <- renderPlot({
    id <- top5_result()$id[4]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$top5_plot <- renderPlot({
    id <- top5_result()$id[5]
    plot_shoe(X[id, ], paste("ID", id))
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

    } else if (input$src == "Self-check") {

      answer <- selfcheck_labels$category[
        match(input$sc_file, selfcheck_labels$photo)
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


  # ==========================================================
  # TAB 2. Explain PCA
  # ==========================================================

  output$pca_average_plot <- renderPlot({

    plot_shoe(
      pc$center,
      "Average shoe"
    )
  })


  output$pca_input_plot <- renderPlot({

    plot_shoe(
      selected_photo(),
      "Selected photo"
    )
  })


  output$difference_plot <- renderPlot({

    difference <- selected_photo() - pc$center

    plot_signed_image(
      difference,
      "Photo - average"
    )
  })


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


  # ==========================================================
  # TAB 3. Our Shoes
  # ==========================================================

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
      alt = "Yunseo Sandals"
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
      alt = "Yunseo Slippers"
    )
  }, deleteFile = FALSE)


  output$our4 <- renderImage({
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "Afina_Shoes_1.jpg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Afina Shoes 1"
    )
  }, deleteFile = FALSE)


  output$our5 <- renderImage({
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "Afina_Shoes_2.jpg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Afina Shoes 2"
    )
  }, deleteFile = FALSE)


  output$our6 <- renderImage({
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "Afina_Boots_3.jpg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Afina Boots 3"
    )
  }, deleteFile = FALSE)


  output$our7 <- renderImage({
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "gayeon_Slippers_2.jpeg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Gayeon Slippers 2"
    )
  }, deleteFile = FALSE)


  output$our8 <- renderImage({
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "gayeon_Slippers_1.jpeg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Gayeon Slippers 1"
    )
  }, deleteFile = FALSE)


  output$our9 <- renderImage({
    list(
      src = normalizePath(
        file.path(
          "our_shoes",
          "gayeon_Shoes_3.jpeg"
        )
      ),
      contentType = "image/jpeg",
      width = 250,
      height = 250,
      alt = "Gayeon Shoes 3"
    )
  }, deleteFile = FALSE)


  our_selected_photo <- reactive({

    read_shoe(
      file.path(
        "our_shoes",
        input$our_file
      )
    )
  })


  our_correct_category <- reactive({

    our_shoes$category[
      match(
        input$our_file,
        our_shoes$file
      )
    ]
  })


  our_top5_result <- reactive({

    target <- our_selected_photo()

    target_score <- as.numeric(
      (target - pc$center) %*%
        pc$rotation[
          , 1:input$our_K,
          drop = FALSE
        ]
    )

    catalogue_score <- pc$x[
      , 1:input$our_K,
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


  output$our_input_plot <- renderPlot({

    plot_shoe(
      our_selected_photo(),
      our_correct_category()
    )
  })


  output$our_best_plot <- renderPlot({

    best_id <- our_top5_result()$id[1]

    plot_shoe(
      X[best_id, ],
      paste("ID", best_id)
    )
  })


  output$our_top1_plot <- renderPlot({
    id <- our_top5_result()$id[1]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$our_top2_plot <- renderPlot({
    id <- our_top5_result()$id[2]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$our_top3_plot <- renderPlot({
    id <- our_top5_result()$id[3]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$our_top4_plot <- renderPlot({
    id <- our_top5_result()$id[4]
    plot_shoe(X[id, ], paste("ID", id))
  })

  output$our_top5_plot <- renderPlot({
    id <- our_top5_result()$id[5]
    plot_shoe(X[id, ], paste("ID", id))
  })


  output$our_top5 <- renderTable({
    our_top5_result()
  })


  output$our_result <- renderText({

    result <- our_top5_result()

    prediction <- names(
      which.max(
        table(result$category)
      )
    )

    answer <- our_correct_category()

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


  # ==========================================================
  # TAB 4. Data 1
  # ==========================================================

  filtered_data <- reactive({

    dat <- bike

    if (input$year != "All") {
      dat <- dat |>
        filter(year == as.integer(input$year))
    }

    if (input$month != "All") {
      dat <- dat |>
        filter(month == as.integer(input$month))
    }

    if (input$weekday != "All") {
      dat <- dat |>
        filter(weekday == input$weekday)
    }

    if (input$hour != "All") {
      dat <- dat |>
        filter(hour == as.integer(input$hour))
    }

    dat
  })


  output$total_rentals <- renderText({

    format(
      sum(
        filtered_data()$rental_count,
        na.rm = TRUE
      ),
      big.mark = ","
    )
  })


  output$avg_daily <- renderText({

    daily <- filtered_data() |>
      group_by(date) |>
      summarise(
        daily_rentals = sum(
          rental_count,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    format(
      round(
        mean(daily$daily_rentals)
      ),
      big.mark = ","
    )
  })


  output$avg_duration <- renderText({

    dat <- filtered_data()

    round(
      sum(
        dat$total_duration,
        na.rm = TRUE
      ) /
        sum(
          dat$rental_count,
          na.rm = TRUE
        ),
      1
    )
  })


  output$avg_distance <- renderText({

    dat <- filtered_data()

    round(
      sum(
        dat$total_distance,
        na.rm = TRUE
      ) /
        sum(
          dat$rental_count,
          na.rm = TRUE
        ),
      1
    )
  })
  
  output$monthly_plot <- renderPlot({
    
    month_data <- bike
    
    if (input$year != "All") {
      month_data <- month_data |>
        filter(year == as.integer(input$year))
    }
    
    if (input$weekday != "All") {
      month_data <- month_data |>
        filter(weekday == input$weekday)
    }
    
    if (input$hour != "All") {
      month_data <- month_data |>
        filter(hour == as.integer(input$hour))
    }
    
    month_data <- month_data |>
      group_by(month) |>
      summarise(
        rentals = sum(
          rental_count,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    ggplot(
      month_data,
      aes(
        x = month,
        y = rentals
      )
    ) +
      geom_line() +
      geom_point() +
      labs(
        x = "Month",
        y = "Number of rentals"
      ) +
      scale_x_continuous(
        breaks = 1:12
      ) +
      scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      )
  })

  output$usage_plot <- renderPlot({

    plot_data <- filtered_data() |>
      group_by(hour) |>
      summarise(
        rentals = sum(
          rental_count,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    peak_data <- plot_data |>
      arrange(desc(rentals)) |>
      slice_head(n = 2)

    ggplot(
      plot_data,
      aes(
        x = hour,
        y = rentals
      )
    ) +
      geom_line() +
      geom_point() +

      geom_point(
        data = peak_data,
        color = "red",
        size = 4
      ) +

      geom_text(
        data = peak_data,
        aes(label = paste0(hour, ":00")),
        vjust = -1,
        size = 4
      ) +

      labs(
        x = "Hour",
        y = "Number of rentals"
      ) +

      scale_x_continuous(
        breaks = 0:23
      ) +

      scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      )
  })


  output$compare_dates_plot <- renderPlot({

    compare_data <- bike |>
      filter(
        date == input$compare_date1 |
          date == input$compare_date2
      ) |>
      group_by(date, hour) |>
      summarise(
        rentals = sum(
          rental_count,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    compare_data$date <- as.factor(compare_data$date)

    ggplot(
      compare_data,
      aes(
        x = hour,
        y = rentals,
        group = date,
        linetype = date
      )
    ) +
      geom_line() +
      geom_point() +
      labs(
        x = "Hour",
        y = "Number of rentals",
        linetype = "Date"
      ) +
      scale_x_continuous(
        breaks = 0:23
      ) +
      scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      )
  })


  output$monthly_daily_plot <- renderPlot({

    req(
      input$year != "All",
      input$month != "All"
    )

    month_data <- bike |>
      filter(
        year == as.integer(input$year),
        month == as.integer(input$month)
      ) |>
      group_by(date) |>
      summarise(
        rentals = sum(
          rental_count,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    ggplot(
      month_data,
      aes(
        x = date,
        y = rentals
      )
    ) +
      geom_line() +
      geom_point() +
      labs(
        x = "Date",
        y = "Number of rentals"
      ) +
      scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      )
  })


  # ==========================================================
  # TAB 5. AI
  # ==========================================================

  output$ai_table <- renderTable(
    data.frame(
      Tab = c(
        "Shoe Search",
        "Explain PCA",
        "Our Shoes",
        "Data 1"
      ),

      `What AI did` = c(
        "TODO",
        "TODO",
        "TODO",
        "TODO"
      ),

      `What we did` = c(
        "TODO",
        "TODO",
        "TODO",
        "TODO"
      ),

      check.names = FALSE
    )
  )


  output$ai_reflection <- renderTable(
    data.frame(
      Member = c(
        "TODO",
        "TODO",
        "TODO",
        "TODO",
        "TODO"
      ),

      Reflection = c(
        "TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
        "TODO",
        "TODO",
        "TODO",
        "TODO"
      )
    )
  )
}


shinyApp(ui, server)
