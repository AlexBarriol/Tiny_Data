# explain_pca.R
# Explain PCA tab: UI (explain_pca_tab) and server (explain_pca_server).
# For the photo chosen in Shoe Search, it shows step by step how PCA finds the match.
# Needs pca_functions.R to be sourced first.

# Share of the total variance explained by each PC.
# pca$sdev holds ALL PCs (not only the first 100), so the sum is the total variance.
var_expl <- pca$sdev^2 / sum(pca$sdev^2)

# One color per category (used in the score map)
cat_cols <- c(Boots = "#1b9e77", Sandals = "#d95f02", Shoes = "#7570b3", Slippers = "#e7298a")

# Draw 4,096 numbers that can be negative (a difference or an eigenshoe) as a 64 x 64 picture:
# blue = below zero, white = zero, red = above zero.
show_diff <- function(v, main = "") {
  lim <- max(abs(v))
  if (lim == 0) lim <- 1
  pal <- colorRampPalette(c("blue", "white", "red"))(201)
  idx <- round(v / lim * 100) + 101                      # -lim..lim  ->  color 1..201
  par(mar = c(0, 0, 3, 0))
  plot(as.raster(matrix(pal[idx], 64, 64, byrow = TRUE)), interpolate = FALSE)
  title(main = main)
}

# ============================================================
# UI
# ============================================================
explain_pca_tab <- tabPanel("Explain PCA",
  p("For the photo chosen in Shoe Search, this tab shows how PCA finds the match, step by step."),
  uiOutput("ex_chosen"),

  h4("Step 1. Average shoe"),
  p("x̅, the mean of the 2,000 photos. PCA describes every photo by how it differs from this picture."),
  plotOutput("ex_mean", height = 220, width = 220),

  h4("Step 2. Photo − average"),
  p("x − x̅. Red: the photo is brighter than the average shoe there. Blue: it is darker."),
  plotOutput("ex_diff", height = 220, width = 660),

  h4("Step 3. Score map"),
  p("All catalogue photos on PC1 and PC2, colored by category. The star is your photo, the circles are its top 5.",
    "The match uses the first K PCs, not only these two, so the top 5 are not always the nearest points on this map."),
  plotOutput("ex_map", height = 450),

  h4("Step 4. Eigenshoes"),
  p("Each PC is a picture (its loadings drawn as 64 × 64). Red and blue areas are where this PC changes a photo the most.",
    "The percentage is the share of the total variance this PC explains."),
  plotOutput("ex_eigen", height = 170),
  plotOutput("ex_var", height = 260),

  h4("Step 5. Scores"),
  p("The photo's first K scores: how much of each eigenshoe is in the photo. These K numbers are used to find the match,",
    "instead of the 4,096 pixels. The best match is the catalogue photo whose scores are closest."),
  plotOutput("ex_scores", height = 280),
  verbatimTextOutput("ex_scores_txt"),

  h4("Step 6. Rebuild"),
  p("The photo rebuilt from the average shoe and the first K eigenshoes. More PCs give back more detail."),
  plotOutput("ex_rebuild", height = 170),

  h4("More: does the prediction depend on K?"),
  p("The predicted category for every K from 1 to 100. The dashed line is the K chosen in Shoe Search."),
  plotOutput("ex_more", height = 240)
)

# ============================================================
# SERVER
# ============================================================
# Call it inside server(): explain_pca_server(input, output, session, chosen)
# `chosen` is what shoe_search_server() returns: chosen$sel() and chosen$res().
explain_pca_server <- function(input, output, session, chosen) {
  sel <- chosen$sel     # the chosen photo
  res <- chosen$res     # its matches for the chosen K

  output$ex_chosen <- renderUI(p(strong("Chosen photo: "), sel()$label, " | K = ", input$K))

  # Step 1
  output$ex_mean <- renderPlot(show_img(mean_shoe, "Average shoe"))

  # Step 2
  output$ex_diff <- renderPlot({
    x <- sel()$x
    par(mfrow = c(1, 3))
    show_img(x, "Photo x")
    show_img(mean_shoe, "Average shoe")
    show_diff(x - mean_shoe, "Photo - average")
  })

  # Step 3
  output$ex_map <- renderPlot({
    r  <- res()
    s2 <- as.numeric((sel()$x - mean_shoe) %*% eigen_[, 1:2])      # the photo's PC1 and PC2 scores
    par(mar = c(4, 4, 2, 1))
    plot(scores[, 1], scores[, 2], pch = 16, cex = 0.7,
         col  = adjustcolor(cat_cols[as.character(meta$category)], 0.5),
         xlab = sprintf("PC1 (%.1f%% of variance)", 100 * var_expl[1]),
         ylab = sprintf("PC2 (%.1f%% of variance)", 100 * var_expl[2]),
         main = "Catalogue photos on PC1 and PC2")
    points(scores[r$idx, 1], scores[r$idx, 2], pch = 1, cex = 2, lwd = 2)   # top 5
    points(s2[1], s2[2], pch = 8, cex = 2.5, lwd = 3)                       # your photo
    legend("topright", bty = "n",
           legend = c(names(cat_cols), "Your photo", "Top 5"),
           col    = c(cat_cols, "black", "black"),
           pch    = c(16, 16, 16, 16, 8, 1))
  })

  # Step 4
  output$ex_eigen <- renderPlot({
    par(mfrow = c(1, 8))
    for (k in 1:8) show_diff(eigen_[, k], sprintf("PC%d (%.1f%%)", k, 100 * var_expl[k]))
  })

  output$ex_var <- renderPlot({
    K   <- input$K
    cum <- 100 * cumsum(var_expl)[1:100]
    par(mar = c(4, 4, 2, 1))
    plot(1:100, cum, type = "l", lwd = 2, ylim = c(0, 100),
         xlab = "Number of PCs (K)", ylab = "Variance explained (%)",
         main = sprintf("The first %d PCs explain %.1f%% of the variance", K, cum[K]))
    abline(v = K, lty = 2)
    points(K, cum[K], pch = 16, cex = 1.5)
  })

  # Step 5
  output$ex_scores <- renderPlot({
    r <- res(); K <- input$K
    both <- rbind(r$score, scores[r$idx[1], 1:K])        # row 1 = your photo, row 2 = best match
    par(mar = c(4, 4, 2, 1))
    barplot(both, beside = TRUE, names.arg = 1:K, col = c("grey20", "orange"), border = NA,
            xlab = "PC", ylab = "Score", main = paste("Scores on the first", K, "PCs"))
    legend("topright", bty = "n", fill = c("grey20", "orange"),
           legend = c("Your photo", paste("Best match: id", r$idx[1])))
  })

  output$ex_scores_txt <- renderPrint(round(res()$score, 2))

  # Step 6
  output$ex_rebuild <- renderPlot({
    x  <- sel()$x
    s  <- as.numeric((x - mean_shoe) %*% eigen_)                   # all 100 scores of the photo
    Ks <- sort(unique(c(1, 5, 10, 20, 50, 100, input$K)))          # several K, and the chosen K
    par(mfrow = c(1, length(Ks) + 1))
    show_img(x, "Original")
    for (k in Ks) show_img(rebuild(s, k), paste("K =", k))
  })

  # More: the prediction for every K (recomputed only when the photo changes, not when K changes)
  pred_all <- reactive({
    s <- sel()
    sapply(1:100, function(k) find_matches(s$x, k, s$id)$pred)
  })

  output$ex_more <- renderPlot({
    pred  <- pred_all()
    truth <- sel()$truth
    col   <- if (is.na(truth)) "grey40" else ifelse(pred == truth, "forestgreen", "red")
    par(mar = c(4, 6, 2, 1))
    plot(1:100, match(pred, names(cat_cols)), pch = 15, col = col,
         ylim = c(0.5, 4.5), yaxt = "n", xlab = "Number of PCs (K)", ylab = "",
         main = if (is.na(truth)) "Predicted category for every K"
                else paste0("Predicted category for every K (green = right, red = wrong; true: ", truth, ")"))
    axis(2, at = 1:4, labels = names(cat_cols), las = 1)
    abline(v = input$K, lty = 2)
  })
}
