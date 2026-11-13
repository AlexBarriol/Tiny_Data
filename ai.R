# ai.R
# AI tab: UI (ai_tab) and server (ai_server). Same format as the template. Fill in the TODOs.

ai_tab <- tabPanel("AI",
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

# Call it inside server(): ai_server(input, output, session)
ai_server <- function(input, output, session) {
  # Fill in the two tables. Keep the columns.
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
