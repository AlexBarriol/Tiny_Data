# our_shoes.R
# Our Shoes tab: UI (our_shoes_tab) and server (our_shoes_server).
# Needs pca_functions.R to be sourced first.
#
# Put the team's photos into the folder our_shoes/ (next to app.R).
# File names: Name_Category_Number.jpg, for example Afina_Sandals_1.jpg.
# Category must be one of: Boots, Sandals, Shoes, Slippers. The true category is read from the file name.

own_files <- sort(list.files("our_shoes", pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE))
own_parts <- strsplit(sub("\\.[^.]+$", "", own_files), "_")           # "Afina_Sandals_1" -> Afina, Sandals, 1
own_name  <- vapply(own_parts, function(p) p[1], character(1))
own_cat   <- vapply(own_parts, function(p) p[2], character(1))

# Read every photo ONCE when the app starts, with the same function as the catalogue photos.
own_X <- if (length(own_files) > 0) t(sapply(file.path("our_shoes", own_files), read_gray)) else NULL

no_photos_msg <- "No photos yet. Put them into the folder our_shoes/ (for example Afina_Sandals_1.jpg) and restart the app."

# ============================================================
# UI
# ============================================================
our_shoes_tab <- tabPanel("Our Shoes",
  p("Can the app recognize real shoes it has never seen? These are our own photos,",
    "shown the way the app sees them (64 × 64, gray)."),

  h4("All our shoes"),
  plotOutput("own_gallery", height = 480),

  h4("Choose a shoe"),
  selectInput("own_file", "Our shoe", own_files),

  h4("Best match"),
  fluidRow(
    column(3, plotOutput("own_query", height = 230)),
    column(3, plotOutput("own_best",  height = 230)),
    column(6, uiOutput("own_info"))
  ),

  h4("Top 5"),
  plotOutput("own_top5", height = 190),
  tableOutput("own_table")
)

# ============================================================
# SERVER
# ============================================================
# Call it inside server(): our_shoes_server(input, output, session)
our_shoes_server <- function(input, output, session) {

  # The chosen shoe (row i of own_X) and its matches. K comes from the slider in Shoe Search.
  own <- reactive({
    validate(need(length(own_files) > 0, no_photos_msg))
    req(input$own_file)
    i <- match(input$own_file, own_files)
    list(i = i, r = find_matches(own_X[i, ], input$K))
  })

  # All photos: one column per member (3 photos each), with name and category
  output$own_gallery <- renderPlot({
    validate(need(length(own_files) > 0, no_photos_msg))
    n <- length(own_files)
    par(mfcol = c(3, ceiling(n / 3)))
    for (i in 1:n) show_img(own_X[i, ], paste0(own_name[i], ": ", own_cat[i]))
  })

  output$own_query <- renderPlot(show_img(own_X[own()$i, ], paste("Our shoe:", own_name[own()$i])))
  output$own_best  <- renderPlot(show_img(X[own()$r$idx[1], ],
                                          paste0("Best match: id ", own()$r$idx[1])))

  output$own_info <- renderUI({
    o <- own(); r <- o$r; truth <- own_cat[o$i]
    verdict <- if (is.na(truth)) "true category unknown (check the file name)"
               else if (r$pred == truth) paste0("RIGHT (true category: ", truth, ")")
               else paste0("WRONG (true category: ", truth, ")")
    tagList(
      p(strong("Closest photo: "), "id ", r$idx[1], ", category ", r$cats[1]),
      p(strong("Distance: "), round(r$dist[1], 3)),
      p(strong("Predicted category: "), r$pred, " | ", verdict),
      p("K = ", input$K, " (change it in Shoe Search)")
    )
  })

  output$own_top5 <- renderPlot({
    r <- own()$r
    par(mfrow = c(1, 5))
    for (i in 1:5) show_img(X[r$idx[i], ],
                            sprintf("#%d  id %d\n%s  d=%.2f", i, r$idx[i], r$cats[i], r$dist[i]))
  })

  output$own_table <- renderTable({
    r <- own()$r
    data.frame(Rank = 1:5, id = r$idx, Category = r$cats, Distance = round(r$dist, 3))
  })
}
