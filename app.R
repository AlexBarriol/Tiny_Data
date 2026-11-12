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

ui <- navbarPage("TinyData",
  tabPanel("Shoe Search",
    radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload"), inline = TRUE),
    numericInput("cat_id", "Catalogue id", 1, 1, 2000),
    selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4)),
    fileInput("photo", "Upload", accept = c(".jpg", ".jpeg", ".png")),
    sliderInput("K", "Number of PCs (K)", 1, 100, 20),
    # textOutput("result")   # TODO: best match, top 5, prediction, right or wrong
    plotOutput("closest_photo"),
    plotOutput("photo_chosen"),
    plotOutput("average_photo"),
    plotOutput("photo_rebuild"),
    tableOutput("top5"),
    textOutput("prediction")
  ),
  tabPanel("Explain PCA",
    p("For the photo chosen in Shoe Search, show how PCA finds the match."),
    h4("Step 1. Average shoe"), p("x̅, the mean of the 2,000 photos."),
    plotOutput("step_1"),
    h4("Step 2. Photo − average"), p("x − x̅, where the photo is brighter or darker than the average shoe."),
    plotOutput("step_2"),
    h4("Step 3. Score map"), p("All catalogue photos on PC1 and PC2, colored by category, with your photo marked."),
    plotOutput("step_3"),
    h4("Step 4. Eigenshoes"), p("Show PC1, PC2, … as pictures (their loadings drawn as 64 × 64 images), so we can see what shape each PC captures, and how much variance each explains."),
    plotOutput("step_4"),
    h4("Step 5. Scores"), p("The photo’s first K PC scores, the numbers used to find the match."),
    plotOutput("step_5"),
    h4("Step 6. Rebuild"), p("The photo rebuilt from the average shoe and the first K eigenshoes, for several K."),
    plotOutput("step_6"),
    h4("More"), p("Anything else you want to show or explain related to it."),
    plotOutput("more")
  ),
  tabPanel("Our Shoes",
    p("Can your app recognize real shoes it has never seen?"),
    p("Each member photographs 3 different shoes of their own (shoe only, plain background), e.g., YourName_Sandals_1.jpg."),
    p("Show all of them with name and category (3 × n photos for n members)."),
    p("Choose a shoe with a widget; each choice shows its best match, its top 5 and whether it is right.")
  ),
  tabPanel("Data 1",
    p("One Project 1 dataset, descriptive results only (filter, sort, count, mean, median, rate, group summary)."),
    p("It should include:"),
    tags$ul(
      tags$li("the question you are trying to answer;"),
      tags$li("the inputs that show it (e.g., the filter, grouping or time range you choose);"),
      tags$li("the result in numbers;"),
      tags$li("why it matters;"),
      tags$li("how this finding helps your next analysis of this dataset."))
  ),
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
  output$photo_chosen <- renderPlot(
    {
      chosen <- input_chosen()
      draw(chosen$photo)
    }
  )
  output$average_photo <- renderPlot({draw(pca$center)})
  
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
  output$closest_photo <- renderPlot(
    {
      result <- search()
      best <- result$top5[1]
      draw(mX[best, ])
    }
  )
  
  output$top5 <- renderTable(
    {
      result <- search()
      result$table
    }
  )
  
  output$photo_rebuild <- renderPlot(
    {
      result <- search()
      scores <- result$scores
      rebuild <- pca$center + pca$rotation[, 1:input$K] %*% scores
      draw(as.numeric(rebuild))
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
      plot(pc1, pc2, col = list_category, cex = 0.5)
      
      result = photo_chosen$photo - pca$center
      pc1_photo_chosen <- sum(result * pca$rotation[, 1])
      pc2_photo_chosen <- sum(result * pca$rotation[, 2])
      points(pc1_photo_chosen, pc2_photo_chosen, pch = 4, cex = 2, lwd = 6)
    }
  )
  
  output$step_4 <- renderPlot(
    {
      variance <- summary(pca)$importance[2, ]
      photo_chosen <- input_chosen()
      result <- shoe_search(photo_chosen$photo, 10, 0)
      scores <- result$scores
      par(mfrow = c(1, 10)) #10 first PC
      for (i in 1:10)
      {
        i_photo_pc <- pca$center + scores[i] * pca$rotation[, i]
        draw(i_photo_pc)
        title(paste("PC", i, " : variance : ", variance[i]
                    ))
      }
    }
  )
  
  output$step_5 <- renderPlot(
    {
      result <- search()
      scores <- result$scores
      plot(scores)
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
