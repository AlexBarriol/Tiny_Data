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
  colors <- colorRampPalette(c("#2166ac", "white", "#b2182b"))(101)
  result <- matrix(colors[round(50 * values / lim) + 51], 64, 64)
  par(mar = c(0.5, 0.5, 2.5, 0.5), cex.main = 1.5)
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
  
  top5_better <- data.frame(id = csv$id[top5], category = csv$category[top5], distance = round(distance[top5], 1))
  
  final <- table(top5_better$category)
  final_prediction <- names(which.max(final))
  
  result <- list(scores = scores_photo, top5 = top5, table = top5_better, prediction = final_prediction)
  return(result)
}

#end of the code pfor tab1

# beginning of our code for tab3 (Our Shoes)
# our photos are in the folder our_shoes/, named Name_Category_N.jpg (ex: yunseo_Sandals_2.jpeg)
get_our_photo_number <- function(path, mirror)
{
  img <- image_read(path)
  img <- image_convert(img, colorspace = "gray")
  img <- image_level(img, white_point = 75) # the grey floor becomes white, like the catalogue background
  if (mirror == TRUE)
  {
    img <- image_flop(img) # left <-> right, so the toe points to the left like the catalogue
  }
  img <- image_resize(img, "64x64")
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
}
# end of the code for tab3

# beginning of our code for tab4 (Data 1)
bike <- read.csv("bike_summary_2020_2025.csv")
bike$date <- as.Date(bike$date) # "2025-01-01" becomes a real date (for the compare dates part)
bike$month_name <- month.abb[bike$month]                 # 1 -> "Jan", 2 -> "Feb", ...
bike$commute <- bike$hour %in% c(7, 8, 9, 17, 18, 19)   # TRUE if the hour is a commute hour
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
  duration <- sum(data$total_duration) / rentals            # minutes per ride
  distance <- sum(data$total_distance) / rentals / 1000     # km per ride
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
                            mainPanel(width = 9,
                                      h4("Best match"),
                                      plotOutput("four_photos", height = "260px"), # your photo, best match, average shoe, rebuilt : in one line
                                      h3(textOutput("prediction")),
                                      hr(),
                                      h4("Top 5"),
                                      plotOutput("top5_photos", height = "200px"),
                                      tableOutput("top5")
                            )
                          )
                 ),
                 
                 # ---------------- Tab 2 : Explain PCA ----------------
                 tabPanel("Explain PCA",
                          p("For the photo chosen in Shoe Search, show how PCA finds the match."),
                          sliderInput("K_tab2", "Number of PCs (K), the same as in Shoe Search", 1, 100, 20),
                          fluidRow(
                            column(6,
                                   h4("Step 1. Average shoe"), p("x̅, the mean of the 2,000 photos."),
                                   plotOutput("step_1", height = "260px")),
                            column(6,
                                   h4("Step 2. Photo − average"), p("x − x̅, where the photo is brighter or darker than the average shoe."),
                                   plotOutput("step_2", height = "260px"))
                          ),
                          hr(),
                          h4("Step 3. Score map"), p("All catalogue photos on PC1 and PC2, colored by category, with your photo marked."),
                          plotOutput("step_3", height = "450px"),  # left: full map, right: zoom on the 30 nearest points
                          hr(),
                          h4("Step 4. Eigenshoes"), p("PC1 to PC10 as pictures: the loadings of each PC drawn as a 64 × 64 image (red = positive, blue = negative), with the share of variance it explains."),,
                          hr(),
                          h4("Step 5. Scores"), p("The photo’s first K PC scores, the numbers used to find the match, next to the best match’s scores."),
                          plotOutput("step_5", height = "300px"),
                          div(style = "overflow-x: auto;", tableOutput("step_5_table")), # scroll if K is big
                          hr(),
                          h4("Step 6. Rebuild"), p("The photo rebuilt from the average shoe and the first K eigenshoes, for several K."),
                          plotOutput("step_6", height = "220px"),
                          hr(),
                          h4("More"), p("How much of the variance each PC explains (scree plot, like in the course)."),
                          plotOutput("more", height = "350px")
                 ),
                 
                 # ---------------- Tab 3 : Our Shoes ----------------
                 tabPanel("Our Shoes",
                          p("Can our app recognize real shoes it has never seen? Each member photographed 3 of their own shoes."),
                          h4("All our shoes (as the app sees them: grayscale, 64 × 64)"),
                          plotOutput("our_gallery", height = "450px"),
                          hr(),
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectInput("our_pick", "Choose a shoe", our_files),
                                         checkboxInput("our_mirror", "Mirror the photos (toe to the left, like the catalogue)", FALSE),
                                         sliderInput("our_K", "Number of PCs (K)", 1, 100, 20)
                            ),
                            mainPanel(width = 9,
                                      fluidRow(
                                        column(3, h4("Our photo"),  plotOutput("our_photo", height = "200px")),
                                        column(3, h4("Best match"), plotOutput("our_best",  height = "200px"))
                                      ),
                                      h3(textOutput("our_prediction")),
                                      hr(),
                                      h4("Top 5"),
                                      plotOutput("our_top5_photos", height = "180px"),
                                      tableOutput("our_top5")
                            )
                          ),
                          hr(),
                          h4("How many of our shoes did the app get right?"),
                          h3(textOutput("our_score")),
                          tableOutput("our_all")
                 ),
                 
                 # ---------------- Tab 4 : Data 1 ----------------
                 tabPanel("Data 1",
                          h3("How does Ttareungyi usage change over time?"),
                          p("Seoul public bike rentals, 2020-2025, one line per hour for all of Seoul."),
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         sliderInput("d1_year", "Years", 2020, 2025, c(2020, 2025), step = 1, sep = ""),
                                         checkboxGroupInput("d1_month", "Months", month.abb, month.abb, inline = TRUE),
                                         checkboxGroupInput("d1_day", "Weekdays", week_days, week_days, inline = TRUE),
                                         sliderInput("d1_hour", "Hours", 0, 23, c(0, 23), step = 1),
                                         radioButtons("d1_group", "Group by", c("Year" = "year", "Month" = "month", "Weekday" = "weekday", "Hour" = "hour")),
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
                            column(6, dateInput("compare_date1", "Date 1", value = "2025-01-01", min = "2020-01-01", max = "2025-12-31")),
                            column(6, dateInput("compare_date2", "Date 2", value = "2025-01-02", min = "2020-01-01", max = "2025-12-31"))
                          ),
                          plotOutput("compare_dates_plot", height = "350px"),
                          hr(),
                          h4("Why it matters"),
                          p("Rentals almost doubled from 2020 to 2023 (23.7 M to 44.9 M), while the average ride became shorter (about 31 to 21 minutes)",
                            "and a larger share of rides happened in commute hours (37 % to 42 %). Ttareungyi is moving from a leisure ride",
                            "to a short everyday trip, which matters for where and when bikes need to be available."),
                          h4("How it helps our next analysis"),
                          p("Usage depends strongly on the hour, weekday and season. When we link the bike data to other data",
                            "(air quality, satellite images or news), we should compare the same hour, weekday and month,",
                            "so these patterns are not mistaken for the effect we want to measure.")
                 ),
                 
                 # ---------------- Tab 5 : AI ----------------
                 tabPanel("AI",
                          h4("Ethics statement"),
                          p("We understand the rules on data and AI use for this project, including following the license and privacy rules",
                            "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
                            "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
                            "and we agree to receive an F in this course if we have not."),
                          p("Signed: TODO (every member)"),
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
      rebuild <- pca$center + pca$rotation[, 1:input$K, drop = FALSE] %*% scores  # drop = FALSE : works also for K = 1
      
      par(mfrow = c(1, 4)) # 4 photos in one line
      draw(chosen$photo)
      title("Your photo")
      draw(mX[best, ])
      title(paste("Closest : id", csv$id[best]))
      draw(pca$center)
      title("Average shoe")
      draw(as.numeric(rebuild))
      title(paste("Rebuilt with K =", input$K))
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
      par(mfrow = c(1, 5))
      for (i in result$top5)
      {
        draw(mX[i, ])
        title(paste("id", csv$id[i], ":", csv$category[i]))
      }
    }
  )
  
  
  output$prediction <- renderText(
    {
      result <- search()
      prediction <- result$prediction
      chosen <- input_chosen()
      true_category <- chosen$true_category
      if (true_category == prediction)
      {
        paste("Prediction : ", prediction, " RIGHT")
      }
      else if (true_category == "Unknown")
      {
        paste("Prediction : ", prediction, " Unknown")
      }
      else
      {
        paste("Prediction : ", prediction, " Wrong")
      }
    }
  )
  
  #tab2 by us
  
  # the 2 sliders K (tab1) and K_tab2 (tab2) always have the same value
  observeEvent(input$K, { updateSliderInput(session, "K_tab2", value = input$K) })
  observeEvent(input$K_tab2, { updateSliderInput(session, "K", value = input$K_tab2) })
  
  output$step_1 <- renderPlot(
    {
      draw(pca$center)
    }
  )
  
  output$step_2 <- renderPlot(
    {
      photo_chosen <- input_chosen()
      result = photo_chosen$photo - pca$center + 128
      draw(result)
    }
  )
  
  output$step_3 <- renderPlot(
    {
      pc1 <- pca$x[, 1]
      pc2 <- pca$x[, 2]
      list_category <- as.numeric(factor(csv$category))
      photo_chosen <- input_chosen()
      
      result = photo_chosen$photo - pca$center
      pc1_photo_chosen <- sum(result * pca$rotation[, 1])
      pc2_photo_chosen <- sum(result * pca$rotation[, 2])
      
      par(mfrow = c(1, 2)) # 2 graphs side by side
      
      # graph 1 : all the 2000 shoes
      plot(pc1, pc2, col = list_category, pch = 16, cex = 0.5, main = "PC1 - PC2 score map")
      points(pc1_photo_chosen, pc2_photo_chosen, pch = 21, cex = 2.5, lwd = 3, col = "navy", bg = "white")
      text(pc1_photo_chosen, pc2_photo_chosen, "Selected", pos = 4, col = "navy")
      legend("topright", legend = levels(factor(csv$category)), col = 1:4, pch = 16)
      
      # graph 2 : zoom on the 30 shoes closest to our photo (on PC1 and PC2 only)
      distance <- sqrt((pc1 - pc1_photo_chosen)^2 + (pc2 - pc2_photo_chosen)^2)
      near <- order(distance)[1:30]
      plot(pc1[near], pc2[near], col = list_category[near], pch = 19, cex = 1.5,
           xlim = range(c(pc1[near], pc1_photo_chosen)), ylim = range(c(pc2[near], pc2_photo_chosen)),
           xlab = "pc1", ylab = "pc2", main = "30 nearest points")
      points(pc1_photo_chosen, pc2_photo_chosen, pch = 21, cex = 2.5, lwd = 3, col = "navy", bg = "white")
      text(pc1_photo_chosen, pc2_photo_chosen, "Selected", pos = 4, col = "navy")
    }
  )
  
  output$step_4 <- renderPlot(
    {
            variance <- summary(pca)$importance[2, ]
      par(mfrow = c(2, 5)) #10 first PC, on 2 rows of 5
      for (i in 1:10)
      {
        draw_color(pca$rotation[, i], paste0("PC", i, " (", round(100 * variance[i], 1), "%)"))
      }
      }
    }
  )
  
  output$step_5 <- renderPlot(
    {
      result <- search()
      scores <- result$scores                        # the K scores of our photo
      best <- result$top5[1]
      scores_best <- pca$x[best, 1:input$K]          # the K scores of the best match
      nb <- min(input$K, 20)                         # we show only the first 20 PCs, otherwise too small
      both <- rbind(scores[1:nb], scores_best[1:nb]) # line 1 = our photo, line 2 = best match
      barplot(both, beside = TRUE, names.arg = paste0("PC", 1:nb), las = 2,
              col = c("blue", "orange"), ylab = "score",
              legend.text = c("Your photo", "Best match"))
      abline(h = 0)
      title(paste("Distance using all K =", input$K, "scores :", result$table$distance[1]))
    }
  )
  
  output$step_5_table <- renderTable(
    {
      result <- search()
      scores <- round(result$scores, 1)
      score_table <- data.frame(t(scores)) # t() : one line with K columns instead of one column
      names(score_table) <- paste0("PC", 1:input$K)
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
        title(paste("nb K : ", i))
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
        title(paste(our_name[i], ":", our_category[i]))
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
    }
  )
  
  output$our_top5_photos <- renderPlot(
    {
      result <- our_search()
      par(mfrow = c(1, 5))
      for (i in result$top5)
      {
        draw(mX[i, ])
        title(paste("id", csv$id[i], ":", csv$category[i]))
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
      if (chosen$true_category == result$prediction)
      {
        paste("Prediction : ", result$prediction, " RIGHT")
      }
      else
      {
        paste("Prediction : ", result$prediction, " Wrong (true : ", chosen$true_category, ")")
      }
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
          right_wrong[i] <- "RIGHT"
        }
        else
        {
          right_wrong[i] <- "Wrong"
        }
      }
      data.frame(photo = our_files, true = our_category, prediction = prediction, result = right_wrong)
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
      nb_right <- sum(table_all$result == "RIGHT")
      paste(nb_right, "out of", n_our, "shoes are right with K =", input$our_K)
    }
  )
  
  #end tab3
  
  #tab4 by us
  
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
      legend("topleft", legend = c(as.character(input$compare_date1), as.character(input$compare_date2)),
             lty = c(1, 2), col = c("blue", "orange"))
    }
  )
  
  #end tab4
  
  
  
  # AI tab: fill in the two tables. Keep the columns.
  output$ai_table <- renderTable(data.frame(
    Tab           = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
    `What AI did` = c("TODO", "TODO", "TODO", "TODO"),
    `What we did` = c("TODO", "TODO", "TODO", "TODO"),
    check.names = FALSE))
  output$ai_reflection <- renderTable(data.frame(
    Member     = c("TODO", "TODO", "TODO", "TODO", "TODO"),
    Reflection = c("TODO: one or two paragraphs: what AI helped with, where it failed, what you learned.",
                   "TODO", "TODO", "TODO", "TODO")))
}

shinyApp(ui, server)
