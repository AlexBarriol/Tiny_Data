# pca_functions.R
# Data, PCA and helper functions shared by all tabs. Runs ONCE when the app starts.
# Run the app from the folder that contains shoes/, shoes_meta.csv and selfcheck/.

# ============================================================
# 1. LOAD DATA AND COMPUTE PCA
# ============================================================

# Read one photo as 4,096 numbers between 0 (black) and 1 (white), row by row.
# The same function is used for the catalogue, self-check and uploads,
# so every photo gets exactly the same format.
read_gray <- function(path) {
  img <- image_read(path)
  img <- image_orient(img)                                # phone photos: turn upright (uses the rotation saved in the file)
  img <- image_background(img, "white", flatten = TRUE)  # PNG with transparency -> white
  img <- image_convert(img, colorspace = "gray")
  img <- image_resize(img, "64x64!")                      # no-op for catalogue photos
  as.numeric(as.integer(image_data(img, channels = "gray"))) / 255
}

meta <- read.csv("shoes_meta.csv")
meta <- meta[match(1:2000, meta$id), ]                    # row i = shoe with id i

X <- t(sapply(1:2000, function(i) read_gray(sprintf("shoes/%04d.png", i))))  # 2000 x 4096

pca       <- prcomp(X, center = TRUE, scale. = FALSE, rank. = 100)  # keep first 100 PCs
mean_shoe <- pca$center      # average shoe, 4096 numbers
scores    <- pca$x           # catalogue scores, 2000 x 100
eigen_    <- pca$rotation    # eigenshoes (loadings), 4096 x 100

# Self-check labels. ASSUMPTION: first column = file name, second = category. Check your labels.csv!
sc_labels <- read.csv("selfcheck/labels.csv")
sc_key    <- sub("\\.png$", "", basename(as.character(sc_labels[[1]])))
sc_cat    <- as.character(sc_labels[[2]])

# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

# Project a photo x onto the first K PCs, find the 5 closest catalogue photos.
find_matches <- function(x, K, exclude_id = NULL) {
  s <- as.numeric((x - mean_shoe) %*% eigen_[, 1:K, drop = FALSE])      # the photo's K scores
  d <- sqrt(rowSums(sweep(scores[, 1:K, drop = FALSE], 2, s)^2))        # Euclidean distance to all 2000
  if (!is.null(exclude_id)) d[exclude_id] <- Inf                        # leave the photo itself out
  o    <- order(d)[1:5]
  cats <- as.character(meta$category[o])
  tab  <- table(cats)
  tied <- names(tab)[tab == max(tab)]
  pred <- cats[cats %in% tied][1]    # most common category; a tie goes to the closest photo
  list(score = s, idx = o, dist = d[o], cats = cats, pred = pred)
}

# Rebuild a photo from the average shoe and the first K eigenshoes.
rebuild <- function(score, K) mean_shoe + as.numeric(eigen_[, 1:K, drop = FALSE] %*% score[1:K])

# Draw a 64 x 64 photo. Small white dot in the top-right corner (project requirement).
show_img <- function(x, main = "", dot = TRUE) {
  par(mar = c(0, 0, 3, 0))                         # room for a title of up to two lines
  plot(as.raster(matrix(pmin(pmax(x, 0), 1), 64, 64, byrow = TRUE)), interpolate = FALSE)
  title(main = main)                               # plot() of a picture ignores main=, so the title is added here
  if (dot) points(59, 59, pch = 21, bg = "white", col = "grey30", cex = 1.5)
}
