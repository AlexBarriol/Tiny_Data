# app.R
# Project 2: main file. It only loads the other files and puts the tabs together.
# Run the app from the folder that contains shoes/, shoes_meta.csv and selfcheck/.
library(shiny)
library(magick)

source("pca_functions.R", local = TRUE)   # data, PCA, helper functions (must be first)
source("shoe_search.R",   local = TRUE)   # shoe_search_tab, shoe_search_server
source("explain_pca.R",   local = TRUE)   # explain_pca_tab, explain_pca_server
source("our_shoes.R",     local = TRUE)   # our_shoes_tab, our_shoes_server
source("data1.R",         local = TRUE)   # data1_tab
source("ai.R",            local = TRUE)   # ai_tab, ai_server

ui <- navbarPage("TODO: team name",
  shoe_search_tab,
  explain_pca_tab,
  our_shoes_tab,
  data1_tab,
  ai_tab
)

server <- function(input, output, session) {
  # chosen$sel() = the chosen photo, chosen$res() = its matches (for the Explain PCA tab)
  chosen <- shoe_search_server(input, output, session)
  explain_pca_server(input, output, session, chosen)
  our_shoes_server(input, output, session)
  ai_server(input, output, session)
}

shinyApp(ui, server)
