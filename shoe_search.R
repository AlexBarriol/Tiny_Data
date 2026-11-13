# shoe_search.R
# Shoe Search tab: UI (shoe_search_tab) and server (shoe_search_server).
# Needs pca_functions.R to be sourced first.

# ============================================================
# UI
# ============================================================
shoe_search_tab <- tabPanel("Shoe Search",
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("src", "Photo", c("Catalogue", "Self-check", "Upload")),
      conditionalPanel("input.src == 'Catalogue'",
        numericInput("cat_id", "Catalogue id", 1, 1, 2000)),
      conditionalPanel("input.src == 'Self-check'",
        selectInput("sc_file", "Self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
      conditionalPanel("input.src == 'Upload'",
        fileInput("photo", "Upload (JPG or PNG)", accept = c(".jpg", ".jpeg", ".png"))),
      sliderInput("K", "Number of PCs (K)", 1, 100, 23)
    ),
    mainPanel(width = 9,
      h4("Best match"),
      fluidRow(
        column(4, plotOutput("p_query", height = 230)),
        column(4, plotOutput("p_best",  height = 230)),
        column(4, plotOutput("p_mean",  height = 230))
      ),
      fluidRow(
        column(4, plotOutput("p_rebuild", height = 230)),
        column(8, uiOutput("best_info"))
      ),
      h4("Top 5"),
      plotOutput("p_top5", height = 190),
      tableOutput("top5_table"),
      h4("Prediction"),
      uiOutput("prediction")
    )
  )
)

# ============================================================
# SERVER
# ============================================================
# Call it inside server(): chosen <- shoe_search_server(input, output, session)
# It returns the two reactives, so the Explain PCA tab can use the same photo:
#   chosen$sel()  -> the chosen photo
#   chosen$res()  -> its matches
shoe_search_server <- function(input, output, session) {

  # The chosen photo: x (4096 numbers), id (only for catalogue), truth (true category or NA)
  sel <- reactive({
    if (input$src == "Catalogue") {
      id <- input$cat_id
      req(!is.na(id), id >= 1, id <= 2000)
      id <- as.integer(id)
      list(x = X[id, ], id = id, truth = as.character(meta$category[id]),
           label = paste("Catalogue", id))
    } else if (input$src == "Self-check") {
      f <- input$sc_file
      truth <- sc_cat[match(sub("\\.png$", "", f), sc_key)]
      list(x = read_gray(file.path("selfcheck", f)), id = NULL, truth = truth, label = f)
    } else {
      req(input$photo)
      list(x = read_gray(input$photo$datapath), id = NULL, truth = NA_character_,
           label = input$photo$name)
    }
  })

  # Matches for the chosen photo and K (recomputed only when photo or K changes)
  res <- reactive({
    s <- sel()
    find_matches(s$x, input$K, s$id)
  })

  output$p_query   <- renderPlot(show_img(sel()$x, paste("Your photo:", sel()$label)))
  output$p_best    <- renderPlot(show_img(X[res()$idx[1], ],
                                          paste0("Best match: id ", res()$idx[1])))
  output$p_mean    <- renderPlot(show_img(mean_shoe, "Average shoe"))
  output$p_rebuild <- renderPlot(show_img(rebuild(res()$score, input$K),
                                          paste("Photo rebuilt with K =", input$K)))

  output$best_info <- renderUI({
    r <- res()
    tagList(
      p(strong("Closest photo: "), "id ", r$idx[1], ", category ", r$cats[1]),
      p(strong("Distance: "), round(r$dist[1], 3))
    )
  })

  output$p_top5 <- renderPlot({
    r <- res()
    par(mfrow = c(1, 5))
    for (i in 1:5) show_img(X[r$idx[i], ],
                            sprintf("#%d  id %d\n%s  d=%.2f", i, r$idx[i], r$cats[i], r$dist[i]))
  })

  output$top5_table <- renderTable({
    r <- res()
    data.frame(Rank = 1:5, id = r$idx, Category = r$cats, Distance = round(r$dist, 3))
  })

  output$prediction <- renderUI({
    r <- res(); truth <- sel()$truth
    verdict <- if (is.na(truth)) "true category unknown for an uploaded photo"
               else if (r$pred == truth) paste0("RIGHT (true category: ", truth, ")")
               else paste0("WRONG (true category: ", truth, ")")
    p(strong("Predicted category: "), r$pred, " | ", verdict)
  })

  list(sel = sel, res = res)
}
