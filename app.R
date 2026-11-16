# In order to be able to open the images
# install.packages("magick")

# Project 2 template: the tabs and the inputs only. Everything else, and the look of the app, is yours.
library(shiny)

# Example: read one photo as 4,096 numbers (64 x 64 grayscale, row by row, 0 = black to 255 = white)
library(magick)
# img <- image_read("shoes/0001.png")
# x <- as.numeric(t(matrix(as.integer(image_data(img, channels = "gray")), 64, 64)))\

# beginning of our code for tab1
get_photo_number <- function(path)
{
  img <- image_read(path)

  img <- image_convert(img, colorspace = "gray")
  img <- image_resize(img, "64x64")
  img <- image_extent(img, "64x64", color = "white") #in case the photo is not a squere

  result <- as.numeric(as.integer(image_data(img, channels = "gray")))
  result
}

csv <- read.csv("shoes_meta.csv")
labels <- read.csv("selfcheck/labels.csv")
number <- formatC(csv$id, width = 4, flag = 0)
files <- paste0("shoes/", number, ".png")

mX <- matrix(0, nrow = 2000, ncol = 4096)
for (i in 1:2000)
{
  mX[i, ] <- get_photo_number(files[i]) #[i, ] return all the row of i, so 4096 number
}

pca <- prcomp(mX, center = TRUE, scale. = FALSE)

draw <- function(photo)
{
  photo <- pmin(pmax(photo, 0), 255)
  result <- matrix(photo, 64, 64) #picture 64*64
  plot(as.raster(result / 255))
}

draw_color <- function(values, name = "")
{
  lim <- max(abs(values))
  
  colors <- colorRampPalette(
    c("#2166ac", "white", "#b2182b")
  )(101)
  
  result <- matrix(
    colors[round(50 * values / lim) + 51],
    64,
    64
  )
  
  par(
    mar = c(0.5, 0.5, 2.5, 0.5),
    cex.main = 1.5
  )
  
  plot(as.raster(result))
  title(name)
}

shoe_search <- function(photo, K, id_excluded)
{
  scores_photo <- as.numeric((photo - pca$center) %*% pca$rotation[, 1:K])

  distance <- numeric(2000)
  for (i in 1:2000)
  {
    if (i == id_excluded)
    {
      distance[i] <- Inf
    }
    else
    {
      shoes_score <- pca$x[i, 1:K]
      difference <- shoes_score - scores_photo
      distance[i] <-sqrt(sum(difference^2))
    }
  }


  top5 <- order(distance)[1:5]

  top5_better <- data.frame(id = csv$id[top5],
    category = csv$category[top5],
    distance = round(distance[top5], 1))

  final <- table(top5_better$category)
  final_prediction <- names(which.max(final))

  result <- list(scores = scores_photo,
    top5 = top5,
    table = top5_better,
    prediction = final_prediction)
  return(result)
}

#end of the code pfor tab1

# beginning of our code for tab3 (Our Shoes)
# our photos are in the folder our_shoes/, named Name_Category_N.jpg (ex: yunseo_Sandals_2.jpeg)
get_our_photo_number <- function(path, mirror)
{
  img <- image_read(path)
  
  img <- image_resize(img, "64x64")
  
  img <- image_convert(img, colorspace = "gray")
  img <- image_level(img, white_point = 75) # the grey floor becomes white, like the catalogue background
  
  if (mirror == TRUE)
  {
    img <- image_flop(img) # left <-> right, so the toe points to the left like the catalogue
  }
  
  img <- image_extent(img, "64x64", color = "white")
  
  result <- as.numeric(as.integer(image_data(img, channels = "gray")))
  result
}

our_files <- list.files("our_shoes")
n_our <- length(our_files)
our_name <- character(n_our)
our_category <- character(n_our)
our_mX <- matrix(0, nrow = n_our, ncol = 4096)
our_mX_mirror <- matrix(0, nrow = n_our, ncol = 4096)
for (i in 1:n_our)
{
  parts <- strsplit(our_files[i], "_")[[1]] # "yunseo_Sandals_2.jpeg" -> "yunseo" "Sandals" "2.jpeg"
  our_name[i] <- parts[1]
  our_category[i] <- parts[2]
  path <- paste0("our_shoes/", our_files[i])
  our_mX[i, ] <- get_our_photo_number(path, FALSE)
  our_mX_mirror[i, ] <- get_our_photo_number(path, TRUE)
  
  gc()
}
# end of the code for tab3

# beginning of our code for tab4 (Data 1)
bike <- read.csv("bike_summary_2020_2025.csv")
bike$date <- as.Date(bike$date) # "2025-01-01" becomes a real date (for the compare dates part)
bike$month_name <- month.abb[bike$month] # 1 -> "Jan", 2 -> "Feb", ...
bike$commute <- bike$hour %in% c(7, 8, 9, 17, 18, 19) # TRUE if the hour is a commute hour
week_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# keep only the rows chosen with the filters
bike_filter <- function(years, months, days, hours)
{
  keep <- bike$year >= years[1] & bike$year <= years[2] &
  bike$month_name %in% months &
  bike$weekday %in% days &
  bike$hour >= hours[1] & bike$hour <= hours[2]
  data <- bike[keep, ]
  return(data)
}

# the numbers for a group of rows
bike_summary <- function(data)
{
  days <- length(unique(data$date))
  rentals <- sum(data$rental_count)
  daily <- rentals / days
  duration <- sum(data$total_duration) / rentals # minutes per ride
  distance <- sum(data$total_distance) / rentals / 1000 # km per ride
  commute <- 100 * sum(data$rental_count[data$commute]) / rentals

  result <- data.frame(days = days, rentals = rentals, daily = round(daily),
    duration = round(duration, 1), distance = round(distance, 2),
    commute = round(commute, 1))
  return(result)
}

# one line of bike_summary for each year (or month, weekday, hour)
bike_by_group <- function(data, group)
{
  values <- data[[group]] # data[["year"]] is the same as data$year
  if (group == "weekday")
  {
    groups <- week_days[week_days %in% values] # Monday to Sunday, not alphabetical
  }
  else
  {
    groups <- sort(unique(values))
  }

  result <- data.frame()
  for (i in 1:length(groups))
  {
    part <- data[values == groups[i], ]
    line <- bike_summary(part)
    line$group <- groups[i]
    if (group == "month")
    {
      line$group <- month.abb[groups[i]]
    }
    result <- rbind(result, line)
  }
  return(result)
}

# total of rentals for each value (ex: column = "hour", values = 0:23 -> 24 totals)
sum_by <- function(data, column, values)
{
  total <- numeric(length(values))
  for (i in 1:length(values))
  {
    total[i] <- sum(data$rental_count[data[[column]] == values[i]])
  }
  return(total)
}

# end of the code for tab4

ui <- navbarPage("TinyData",

  # ---------------- Tab 1 : Shoe Search ----------------
  tabPanel("Shoe Search",
    h2("Shoe Search"),
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload"), inline = TRUE),
        # only show the input that matches the choice above
        conditionalPanel("input.src == 'Catalogue'",
          numericInput("cat_id", "Catalogue id", 1, 1, 2000)),
        conditionalPanel("input.src == 'Self-check'",
          selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
        conditionalPanel("input.src == 'Upload'",
          fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png"))),
        sliderInput("K", "Number of PCs (K)", 1, 100, 20)
      ),
      mainPanel(
        width = 9,
        
        h3("Best Match"),
        plotOutput(
          "four_photos",
          height = "300px"
        ),
        
        hr(),
        
        h3("Top 5"),
        plotOutput(
          "top5_photos",
          height = "220px"
        ),
        tableOutput("top5"),
        
        hr(),
        
        h3("Prediction"),
        h4(textOutput("prediction")),
        
        tags$div(
          style = "height: 50px;"
        )
      )
    )
  ),

  # ---------------- Tab 2 : Explain PCA ----------------
  tabPanel(
    "Explain PCA",
    
    tags$div(id = "pca-top"),
    
    h2("Explain PCA"),
    
    p(
      "This tab shows how PCA represents the selected shoe and compares it with the catalogue."
    ),
    
    sliderInput(
      "K_tab2",
      "Number of PCs (K)",
      1, 100, 20
    ),
    
    tags$div(
      style = "margin-top: 10px; margin-bottom: 25px;",
      
      tags$a(
        "Step 1",
        href = "#step1",
        class = "btn btn-default",
        style = "margin-right: 8px;"
      ),
      
      tags$a(
        "Step 2",
        href = "#step2",
        class = "btn btn-default",
        style = "margin-right: 8px;"
      ),
      
      tags$a(
        "Step 3",
        href = "#step3",
        class = "btn btn-default",
        style = "margin-right: 8px;"
      ),
      
      tags$a(
        "Step 4",
        href = "#step4",
        class = "btn btn-default",
        style = "margin-right: 8px;"
      ),
      
      tags$a(
        "Step 5",
        href = "#step5",
        class = "btn btn-default",
        style = "margin-right: 8px;"
      ),
      
      tags$a(
        "Step 6",
        href = "#step6",
        class = "btn btn-default",
        style = "margin-right: 8px;"
      ),
      
      tags$a(
        "More",
        href = "#more",
        class = "btn btn-default",
      )
    ),
    
    fluidRow(
      column(6,
             tags$div(id = "step1"),
        h4("Step 1. Average shoe"), p("x̄: the average image of all 2,000 catalogue shoes."),
        plotOutput("step_1", height = "260px")),
      column(6,
             tags$div(id = "step2"),
        h4("Step 2. Photo − average"), p("x − x̄: the difference between the selected photo and the average shoe. Blue areas are darker than average, and red areas are brighter."),
        plotOutput("step_2", height = "260px"))
    ),
    hr(),
    tags$div(id = "step3"),
    h4("Step 3. Score map"), p("Catalogue shoes are positioned by their PC1 and PC2 scores, with the selected photo highlighted."),
    plotOutput("step_3", height = "450px"), # left: full map, right: zoom on the 30 nearest points
    hr(),
    tags$div(id = "step4"),
    h4("Step 4. Eigenshoes"),
    p("PC1 to PC10 as pictures: the loadings of each PC drawn as a 64 × 64 image (red = positive, blue = negative), with the share of variance it explains."),
    plotOutput("step_4", height = "350px"),
    hr(),
    tags$div(id = "step5"),
    h4("Step 5. Scores"), p("The selected shoe and its best match are compared using their first K PCA scores."),
    plotOutput("step_5", height = "300px"),
    div(style = "overflow-x: auto;", tableOutput("step_5_table")), # scroll if K is big
    hr(),
    tags$div(id = "step6"),
    h4("Step 6. Rebuild"), p("The selected shoe is reconstructed with different numbers of principal components."),
    plotOutput("step_6", height = "220px"),
    hr(),
    tags$div(id = "more"),
    h4("More"),
    p("The scree plot shows how much variance is explained by each principal component."),
    plotOutput("more", height = "350px"),
    
    tags$a(
      "Go to top",
      href = "#pca-top",
      style = "
    color: #337ab7;
    font-weight: 600;
    display: inline-block;
    margin-top: 20px;
    margin-bottom: 30px;
  "
    )
  ),

  # ---------------- Tab 3 : Our Shoes ----------------
  tabPanel(
    "Our Shoes",
    tags$div(id = "our-top"),
    h2("Our Shoes"),
    p("Can our app recognize real shoes it has never seen? Each member photographed 3 of their own shoes."),
    h4("All our shoes (as the app sees them: grayscale, 64 × 64)"),
    plotOutput("our_gallery", height = "450px"),
    hr(),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        selectInput(
          "our_pick",
          "Choose a shoe",
          our_files
        ),
        
        checkboxInput(
          "our_mirror",
          "Mirror the photos (toe to the left, like the catalogue)",
          FALSE
        ),
        
        sliderInput(
          "our_K",
          "Number of PCs (K)",
          1, 100, 20
        )
      ),
      
      mainPanel(
        width = 9,
        
        h3("Best Match"),
        
        fluidRow(
          column(
            3,
            h4("Our photo"),
            plotOutput(
              "our_photo",
              height = "200px"
            )
          ),
          
          column(
            3,
            h4("Best match"),
            plotOutput(
              "our_best",
              height = "200px"
            )
          )
        ),
        
        hr(),
        
        h3("Top 5"),
        
        plotOutput(
          "our_top5_photos",
          height = "180px"
        ),
        
        tableOutput("our_top5"),
        
        hr(),
        
        h3("Prediction"),
        
        h4(
          textOutput("our_prediction")
        ),
        
        tags$div(
          style = "height: 50px;"
        )
      )
    ),
    
    hr(),
    
    h3("How many of our shoes did the app get right?"),
    h4(textOutput("our_score")),
    tableOutput("our_all"),
    
    tags$a(
      "Go to top",
      href = "#our-top",
      style = "
    color: #337ab7;
    font-weight: 600;
    display: inline-block;
    margin-top: 20px;
    margin-bottom: 30px;
  "
    )
  ),

  # ---------------- Tab 4 : Data 1 ----------------
  tabPanel("Data 1",
    h2("Data 1"),
    tags$div(id = "data-top"),
    h3("How does Ddareungyi usage change over time?"),
    p("Seoul public bike rentals, 2020-2025, one line per hour for all of Seoul."),
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("d1_year", "Years", 2020, 2025, c(2020, 2025), step = 1, sep = ""),
        checkboxGroupInput(
          "d1_month",
          "Months",
          month.abb,
          month.abb,
          inline = TRUE
        ),
        
        fluidRow(
          column(
            6,
            actionButton(
              "month_all",
              "Select all",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              "month_none",
              "Clear all",
              width = "100%"
            )
          )
        ),
        
        br(),
        
        checkboxGroupInput(
          "d1_day",
          "Weekdays",
          week_days,
          week_days,
          inline = TRUE
        ),
        
        fluidRow(
          column(
            6,
            actionButton(
              "day_all",
              "Select all",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              "day_none",
              "Clear all",
              width = "100%"
            )
          )
        ),
        sliderInput("d1_hour", "Hours", 0, 23, c(0, 23), step = 1),
        radioButtons("d1_group",
          "Group by",
          c("Year" = "year", "Month" = "month", "Weekday" = "weekday", "Hour" = "hour")),
        radioButtons("d1_measure", "Show in graph", c("Average daily rentals" = "daily",
            "Average duration (min)" = "duration",
            "Average distance (km)" = "distance",
            "Commute-hour share (%)" = "commute")),
        helpText("Commute hours: 07-09h and 17-19h.")
      ),
      mainPanel(width = 9,
        h4("Result for the selection"),
        tableOutput("d1_total"),
        plotOutput("d1_plot", height = "320px"),
        tableOutput("d1_table")
      )
    ),
    hr(),
    h3("Compare two dates"),
    p("Rentals hour by hour for two days you choose (for example a weekday and a holiday)."),
    fluidRow(
      column(6,
        dateInput("compare_date1", "Date 1", value = "2025-01-01", min = "2020-01-01", max = "2025-12-31")),
      column(6,
        dateInput("compare_date2", "Date 2", value = "2025-01-02", min = "2020-01-01", max = "2025-12-31"))
    ),
    plotOutput("compare_dates_plot", height = "350px"),
    hr(),
    h4("Why it matters"),
    p("Rentals almost doubled from 2020 to 2023 (23.7 M to 44.9 M), while the average ride became shorter (about 31 to 21 minutes)",
      "and a larger share of rides happened in commute hours (37 % to 42 %). Ddareungyi is moving from a leisure ride",
      "to a short everyday trip, which matters for where and when bikes need to be available."),
    h4("How it helps our next analysis"),
    p("Usage depends strongly on the hour, weekday and season. When we link the bike data to other data",
      "(air quality, satellite images or news), we should compare the same hour, weekday and month,",
      "so these patterns are not mistaken for the effect we want to measure."),
    tags$a(
      "Go to top",
      href = "#data-top",
      style = "
    color: #337ab7;
    font-weight: 600;
    display: inline-block;
    margin-top: 20px;
    margin-bottom: 30px;
  "
    )
  ),

  # ---------------- Tab 5 : AI ----------------
  tabPanel("AI",
    h2("AI"),
    h4("Ethics statement"),
    p("We understand the rules on data and AI use for this project, including following the license and privacy rules",
      "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
      "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
      "and we agree to receive an F in this course if we have not."),
    p("Signed: Alex Arnaud BARRIOL, Gayeon Seo, Yunseo Yang, Khairullina Afina"),
    h4("What AI did and what we did"),
    tableOutput("ai_table"),
    h4("Reflection"),
    tableOutput("ai_reflection")
  )
)

server <- function(input, output, session) {
  # output$result <- renderText("TODO")

  input_chosen <- reactive(
    {
      if (input$src == "Catalogue")
      {
        photo <- mX[input$cat_id, ]
        id_excluded <- input$cat_id
        true_category <- csv$category[input$cat_id]
      }
      else if (input$src == "Self-check")
      {
        photo <- get_photo_number(paste0("selfcheck/", input$sc_file))
        id_excluded <- 0
        true_category <- labels$category[labels$photo == input$sc_file]
      }
      else
      {
        req(input$photo)
        photo <- get_photo_number(input$photo$datapath)
        id_excluded <- 0
        true_category <- "Unknown"
      }
      list(photo = photo, id_excluded = id_excluded, true_category = true_category)
    }
  )

  search <- reactive(
    {
      chosen <- input_chosen()
      photo <- chosen$photo
      id_excluded <- chosen$id_excluded
      result <- shoe_search(photo, input$K, id_excluded)
      result
    }
  )
  output$four_photos <- renderPlot(
    {
      chosen <- input_chosen()
      result <- search()
      best <- result$top5[1]
      scores <- result$scores
      rebuild <- pca$center + pca$rotation[, 1:input$K, drop = FALSE] %*% scores # drop = FALSE : works also for K = 1

      par(
        mfrow = c(1, 4),
        mar = c(1, 1, 4, 1)
      )
      
      draw(chosen$photo)
      title(
        "Your photo",
        cex.main = 1.6
      )
      
      draw(mX[best, ])
      title(
        paste(
          "Closest: ID",
          csv$id[best],
          "-",
          csv$category[best]
        ),
        cex.main = 1.6
      )
      
      draw(as.numeric(rebuild))
      title(
        paste("Rebuilt with K =", input$K),
        cex.main = 1.6
      )
      
      draw(pca$center)
      title(
        "Average shoe",
        cex.main = 1.6
      )
    }
  )

  output$top5 <- renderTable(
    {
      result <- search()
      result$table
    }
  )

  output$top5_photos <- renderPlot(
    {
      result <- search()
      
      par(
        mfrow = c(1, 5),
        mar = c(1, 1, 4, 1)
      )
      
      for (i in result$top5)
      {
        draw(mX[i, ])
        
        title(
          paste("ID", csv$id[i], "-", csv$category[i]),
          cex.main = 1.6
        )
      }
    }
  )


  output$prediction <- renderText(
    {
      result <- search()
      
      prediction <- result$prediction
      chosen <- input_chosen()
      answer <- chosen$true_category
      
      if (answer == "Unknown")
      {
        paste(
          "Prediction:", prediction,
          "| Answer: Unknown"
        )
      }
      else
      {
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
      }
    }
  )

  #tab2 by us

  output$step_1 <- renderPlot(
    {
      draw(pca$center)
    }
  )

  output$step_2 <- renderPlot(
    {
      photo_chosen <- input_chosen()
      
      difference <- photo_chosen$photo - pca$center
      
      par(
        mfrow = c(1, 2),
        mar = c(1, 1, 4, 1)
      )
      
      # original photo
      draw(photo_chosen$photo)
      title(
        "Original photo",
        cex.main = 1.3
      )
      
      # photo - average
      max_difference <- max(abs(difference))
      
      colors <- colorRampPalette(
        c(
          "#3B6EDC",
          "white",
          "#E45756"
        )
      )(101)
      
      color_index <- round(
        (difference + max_difference) /
          (2 * max_difference) * 100
      ) + 1
      
      difference_image <- matrix(
        colors[color_index],
        64,
        64
      )
      
      plot(
        as.raster(difference_image)
      )
      
      title(
        "Photo - average",
        cex.main = 1.3
      )
    }
  )

  output$step_3 <- renderPlot(
    {
      pc1 <- pca$x[, 1]
      pc2 <- pca$x[, 2]
      
      category_colors <- c(
        "Boots"    = "#F4A3A8",  # pastel pink
        "Sandals"  = "#A8C66C",  # pastel green
        "Shoes"    = "#74C9D4",  # pastel cyan
        "Slippers" = "#C9A0F5"   # pastel purple
      )
      
      point_colors <- category_colors[csv$category]
      
      photo_chosen <- input_chosen()
      
      result <- photo_chosen$photo - pca$center
      pc1_photo_chosen <- sum(result * pca$rotation[, 1])
      pc2_photo_chosen <- sum(result * pca$rotation[, 2])
      
      par(mfrow = c(1, 2)) # 2 graphs side by side
      
      # graph 1 : all the 2000 shoes
      plot(
        pc1,
        pc2,
        col = point_colors,
        pch = 16,
        cex = 0.6,
        main = "PC1 - PC2 score map",
        xlab = "PC1",
        ylab = "PC2"
      )
      
      points(
        pc1_photo_chosen,
        pc2_photo_chosen,
        pch = 21,
        cex = 2.5,
        lwd = 3,
        col = "navy",
        bg = "white"
      )
      
      text(
        pc1_photo_chosen,
        pc2_photo_chosen,
        "Selected",
        pos = 4,
        col = "navy"
      )
      
      legend(
        "topright",
        legend = names(category_colors),
        col = category_colors,
        pch = 16
      )
      
      # graph 2 : zoom on the 30 shoes closest to our photo (on PC1 and PC2 only)
      distance <- sqrt(
        (pc1 - pc1_photo_chosen)^2 +
          (pc2 - pc2_photo_chosen)^2
      )
      
      near <- order(distance)[1:30]
      
      plot(
        pc1[near],
        pc2[near],
        col = point_colors[near],
        pch = 16,
        cex = 1.5,
        xlim = range(c(pc1[near], pc1_photo_chosen)),
        ylim = range(c(pc2[near], pc2_photo_chosen)),
        xlab = "PC1",
        ylab = "PC2",
        main = "30 nearest points"
      )
      
      points(
        pc1_photo_chosen,
        pc2_photo_chosen,
        pch = 21,
        cex = 2.5,
        lwd = 3,
        col = "navy",
        bg = "white"
      )
      
      text(
        pc1_photo_chosen,
        pc2_photo_chosen,
        "Selected",
        pos = 4,
        col = "navy"
      )
    }
  )

  output$step_4 <- renderPlot(
    {
      variance <- summary(pca)$importance[2, ]
      
      par(mfrow = c(2, 5)) #10 first PC, on 2 rows of 5
      
      for (i in 1:10)
      {
        draw_color(
          pca$rotation[, i],
          paste0(
            "PC",
            i,
            " (",
            round(100 * variance[i], 1),
            "%)"
          )
        )
      }
    }
  )

  output$step_5 <- renderPlot(
    {
      photo_chosen <- input_chosen()
      
      result <- shoe_search(
        photo_chosen$photo,
        input$K_tab2,
        photo_chosen$id_excluded
      )
      
      scores <- result$scores
      best <- result$top5[1]
      
      scores_best <- pca$x[
        best,
        1:input$K_tab2
      ]
      
      nb <- min(
        input$K_tab2,
        20
      )
      
      both <- rbind(
        scores[1:nb],
        scores_best[1:nb]
      )
      
      barplot(
        both,
        beside = TRUE,
        names.arg = paste0(
          "PC",
          1:nb
        ),
        las = 2,
        col = c(
          "blue",
          "orange"
        ),
        ylab = "score",
        legend.text = c(
          "Your photo",
          "Best match"
        )
      )
      
      abline(h = 0)
      
      title(
        paste(
          "Distance using all K =",
          input$K_tab2,
          "scores :",
          result$table$distance[1]
        )
      )
    }
  )

  output$step_5_table <- renderTable(
    {
      photo_chosen <- input_chosen()
      
      result <- shoe_search(
        photo_chosen$photo,
        input$K_tab2,
        photo_chosen$id_excluded
      )
      
      scores <- round(
        result$scores,
        1
      )
      
      score_table <- data.frame(
        t(scores)
      )
      
      names(score_table) <- paste0(
        "PC",
        1:input$K_tab2
      )
      
      score_table
    }
  )

  output$step_6 <- renderPlot(
    {
      photo_chosen <- input_chosen()
      result <- shoe_search(photo_chosen$photo, 100, 0)
      scores <- result$scores
      par(mfrow = c(1, 5))
      list_K <- c(2, 10, 20, 50, 100)
      for (i in list_K)
      {
        rebuild <- pca$center + pca$rotation[, 1:i] %*% scores[1:i]
        draw(as.numeric(rebuild))
        title(
          paste("K =", i),
          cex.main = 1.6
        )
      }
    }
  )

  output$more <- renderPlot(
    {
      factoextra::fviz_eig(pca, ncp = 30) # I tried 100 but way too much
    }
  )


  #end tab2

  #tab3 by us

  output$our_gallery <- renderPlot(
    {
      par(mfrow = c(ceiling(n_our / 6), 6))
      for (i in 1:n_our)
      {
        if (input$our_mirror == TRUE)
        {
          draw(our_mX_mirror[i, ])
        }
        else
        {
          draw(our_mX[i, ])
        }
        title(
          paste(our_name[i], ":", our_category[i]),
          cex.main = 1.6
        )
      }
    }
  )

  our_chosen <- reactive(
    {
      i <- which(our_files == input$our_pick)
      if (input$our_mirror == TRUE)
      {
        photo <- our_mX_mirror[i, ]
      }
      else
      {
        photo <- our_mX[i, ]
      }
      list(photo = photo, true_category = our_category[i])
    }
  )

  our_search <- reactive(
    {
      chosen <- our_chosen()
      result <- shoe_search(chosen$photo, input$our_K, 0)
      result
    }
  )

  output$our_photo <- renderPlot(
    {
      chosen <- our_chosen()
      draw(chosen$photo)
    }
  )

  output$our_best <- renderPlot(
    {
      result <- our_search()
      best <- result$top5[1]
      
      draw(mX[best, ])
      
      title(
        paste(
          "ID",
          csv$id[best],
          "-",
          csv$category[best]
        ),
        cex.main = 1.0
      )
    }
  )

  output$our_top5_photos <- renderPlot(
    {
      result <- our_search()
      par(mfrow = c(1, 5))
      for (i in result$top5)
      {
        draw(mX[i, ])
        title(
          paste("ID", csv$id[i], "-", csv$category[i]),
          cex.main = 1.6
        )
      }
    }
  )

  output$our_top5 <- renderTable(
    {
      result <- our_search()
      result$table
    }
  )

  output$our_prediction <- renderText(
    {
      result <- our_search()
      chosen <- our_chosen()
      
      prediction <- result$prediction
      answer <- chosen$true_category
      
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
    }
  )

  # all our shoes at once, with the same K and the same mirror choice
  our_all_table <- reactive(
    {
      prediction <- character(n_our)
      right_wrong <- character(n_our)
      for (i in 1:n_our)
      {
        if (input$our_mirror == TRUE)
        {
          photo <- our_mX_mirror[i, ]
        }
        else
        {
          photo <- our_mX[i, ]
        }
        result <- shoe_search(photo, input$our_K, 0)
        prediction[i] <- result$prediction
        if (prediction[i] == our_category[i])
        {
          right_wrong[i] <- "Correct"
        }
        else
        {
          right_wrong[i] <- "Incorrect"
        }
      }
      data.frame(photo = our_files,
        true = our_category,
        prediction = prediction,
        result = right_wrong)
    }
  )

  output$our_all <- renderTable(
    {
      our_all_table()
    }
  )

  output$our_score <- renderText(
    {
      table_all <- our_all_table()
      nb_right <- sum(table_all$result == "Correct")
      paste(
        nb_right,
        "out of",
        n_our,
        "shoes were correctly classified with K =",
        input$our_K
      )
    }
  )

  #end tab3

  #tab4 by us
  
  observeEvent(
    input$month_all,
    {
      updateCheckboxGroupInput(
        session,
        "d1_month",
        selected = month.abb
      )
    }
  )
  
  observeEvent(
    input$month_none,
    {
      updateCheckboxGroupInput(
        session,
        "d1_month",
        selected = character(0)
      )
    }
  )
  
  observeEvent(
    input$day_all,
    {
      updateCheckboxGroupInput(
        session,
        "d1_day",
        selected = week_days
      )
    }
  )
  
  observeEvent(
    input$day_none,
    {
      updateCheckboxGroupInput(
        session,
        "d1_day",
        selected = character(0)
      )
    }
  )

  d1_data <- reactive(
    {
      req(input$d1_month, input$d1_day) # at least one month and one weekday must be checked
      data <- bike_filter(input$d1_year, input$d1_month, input$d1_day, input$d1_hour)
      data
    }
  )

  output$d1_total <- renderTable(
    {
      data <- d1_data()
      result <- bike_summary(data)
      names(result) <- c("Days", "Total rentals", "Avg daily rentals", "Avg duration (min)",
        "Avg distance (km)", "Commute-hour share (%)")
      result
    }
  )

  output$d1_plot <- renderPlot(
    {
      data <- d1_data()
      result <- bike_by_group(data, input$d1_group)
      values <- result[[input$d1_measure]] # result[["daily"]] is the same as result$daily
      barplot(values, names.arg = result$group, col = "steelblue",
        main = paste(input$d1_measure, "by", input$d1_group))
    }
  )

  output$d1_table <- renderTable(
    {
      data <- d1_data()
      result <- bike_by_group(data, input$d1_group)
      result <- result[, c("group", "days", "rentals", "daily", "duration", "distance", "commute")]
      result
    }
  )

  output$compare_dates_plot <- renderPlot(
    {
      data1 <- bike[bike$date == input$compare_date1, ]
      data2 <- bike[bike$date == input$compare_date2, ]
      total1 <- sum_by(data1, "hour", 0:23)
      total2 <- sum_by(data2, "hour", 0:23)
      plot(0:23, total1, type = "b", lty = 1, col = "blue", ylim = range(c(total1, total2)),
        xlab = "Hour", ylab = "Number of rentals")
      lines(0:23, total2, type = "b", lty = 2, col = "orange")
      legend("topleft",
        legend = c(as.character(input$compare_date1), as.character(input$compare_date2)),
        lty = c(1, 2), col = c("blue", "orange"))
    }
  )

  #end tab4



  # AI tab: fill in the two tables. Keep the columns.
  output$ai_table <- renderTable(data.frame(
    Tab = c(
      "Shoe Search",
      "Explain PCA",
      "Our Shoes",
      "Data 1"
    ),
    
    `What AI did` = c(
      "AI helped us review and debug the Shiny code, improve the layout, and identify minor errors in the shoe search functions.",
      "AI helped us review the PCA explanations and visualizations, including the score map, eigenshoes, PCA scores, and reconstruction plots.",
      "AI helped us review the image preprocessing and matching code, fix errors, and improve how the matching and prediction results were displayed.",
      "AI helped us review the filtering and visualization code, find minor errors, and improve the wording used to explain the descriptive results."
    ),
    
    `What we did` = c(
      "We implemented and tested the shoe search with catalogue, self-check, and uploaded photos, and decided how the results should be presented.",
      "We selected the PCA steps to show, tested the visualizations with real inputs, and checked whether the explanations matched the actual results.",
      "We photographed and labelled our own shoes, tested the matching results, adjusted the preprocessing, and evaluated the predictions.",
      "We selected the research question and variables, prepared and checked the bike dataset, chose the filters and comparisons, and interpreted the results."
    ),
    
    check.names = FALSE
  ))
  output$ai_reflection <- renderTable(data.frame(
    Member = c(
      "Alex Arnaud BARRIOL",
      "Gayeon Seo",
      "Yunseo Yang",
      "Khairullina, Afina"
    ),
    
    Reflection = c(
      "I used AI to structure my thinking, I explained my plans and asked Claude to criticise it, so I could improve it. I also used it to do some code reviews and to understand the server part of Shiny. It was not always right, he made some critics that were not justified, and it tends to complexify a lot of things.

But it was really useful to question my way of thinking, to review the code and understand some errors and test some edge cases.",
      
      "AI helped me find minor errors and typos that were difficult to spot. When we made the summary file of the bike usage data, AI also showed us how to check for changes we should not have made, such as adding 9 hours to every timestamp because of the difference between KST and UTC. However, AI also made mistakes. When it explained the scatter plot (score map) in the 'Explain PCA' tab, it told me that the best match can look farther away in the graph than it really is, but it was actually the opposite. This taught me that AI explanations can sound convincing even when they are wrong, so I now check its answers against the actual graph or data before I use them.",
      
      "I used AI mostly to review our Shiny app and to find errors in the code. It also helped me when I was not sure how to organize the layout or explain some of the PCA results.
      
      But AI was not always correct. Sometimes it misunderstood the code we already had or suggested changes that were more complicated than necessary. So I had to run the app many times and check the actual results myself. I think AI was useful for finding problems and giving ideas, but I learned that I still need to understand the code and check whether its suggestions really work.",
      
      "Claude wrote most of the code for my version of the three shoe tabs. My part was to run the app on my laptop, take photos of my own shoes and test them, fix the folders and files when it did not start, and put everything on my GitHub branch.

The AI made mistakes that showed up only when I ran the app with the real photos. The catalogue shoes came out sideways and the titles above the pictures were missing, even though the code looked fine. So I learned that I can’t trust code just because it looks finished. I have to run it, look at the result myself and be able to explain every step."
    )
  ))
}

shinyApp(ui, server)
