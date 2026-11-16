bike <- read.csv("data/bike_summary_2020_2025.csv")
bike$date <- as.Date(bike$date)
bike$weekday <- factor(bike$weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bike$commute <- bike$hour %in% c(7:9, 17:19)

d1_groups <- c("Year" = "year", "Month" = "month", "Weekday" = "weekday", "Hour" = "hour")
d1_measures <- c("Average daily rentals" = "daily",
                 "Average duration (min)" = "duration",
                 "Average distance (km)" = "distance",
                 "Commute-hour share (%)" = "commute")

d1_summ <- function(d) data.frame(
  days     = length(unique(d$date)),
  rentals  = sum(d$rental_count),
  daily    = sum(d$rental_count) / length(unique(d$date)),
  duration = sum(d$total_duration) / sum(d$rental_count),
  distance = sum(d$total_distance) / sum(d$rental_count) / 1000,
  commute  = 100 * sum(d$rental_count[d$commute]) / sum(d$rental_count)
)

d1_num <- function(x, digits = 0) format(round(x, digits), big.mark = ",", nsmall = digits)

data1_ui <- tabPanel("Data 1",
                     h3("How does Ttareungyi usage change over time?"),
                     p("Seoul public bike rentals, 2020-2025, summarised by date and hour."),
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput("d1_year", "Years", 2020, 2025, c(2020, 2025), step = 1, sep = ""),
                         checkboxGroupInput("d1_month", "Months", month.abb, month.abb, inline = TRUE),
                         checkboxGroupInput("d1_day", "Weekdays", levels(bike$weekday), levels(bike$weekday), inline = TRUE),
                         sliderInput("d1_hour", "Hours", 0, 23, c(0, 23), step = 1),
                         radioButtons("d1_group", "Group by", d1_groups),
                         radioButtons("d1_measure", "Show in graph", d1_measures),
                         helpText("Commute hours: 07-09h and 17-19h.")
                       ),
                       mainPanel(
                         uiOutput("d1_cards"),
                         plotOutput("d1_plot", height = "320px"),
                         tableOutput("d1_table")
                       )
                     ),
                     h4("Why it matters"),
                     p("Rentals almost doubled from 2020 to 2023, while the average ride became shorter (about 31 to 21 minutes)",
                       "and a larger share of rides happened in commute hours. Ttareungyi is moving from a leisure ride",
                       "to a short everyday trip, which matters for where and when bikes need to be available."),
                     h4("How it helps our next analysis"),
                     p("Usage depends strongly on the hour, weekday and season. When we link the bike data to other data",
                       "(air quality, satellite images or news), we should compare the same hour, weekday and month,",
                       "so these patterns are not mistaken for the effect we want to measure.")
)

data1_server <- function(input, output) {
  d1_data <- reactive({
    validate(need(length(input$d1_month) > 0, "Choose at least one month."),
             need(length(input$d1_day) > 0, "Choose at least one weekday."))
    d <- bike[bike$year >= input$d1_year[1] & bike$year <= input$d1_year[2] &
                month.abb[bike$month] %in% input$d1_month &
                bike$weekday %in% input$d1_day &
                bike$hour >= input$d1_hour[1] & bike$hour <= input$d1_hour[2], ]
    validate(need(nrow(d) > 0, "No rentals for this selection."))
    d
  })
  
  d1_table <- reactive({
    d <- d1_data()
    g <- d[[input$d1_group]]
    if (input$d1_group == "month") g <- factor(month.abb[g], month.abb)
    parts <- split(d, g, drop = TRUE)
    cbind(group = names(parts), do.call(rbind, lapply(parts, d1_summ)))
  })
  
  output$d1_cards <- renderUI({
    d <- d1_data()
    s <- d1_summ(d)
    vals <- c("Total rentals"        = d1_num(s$rentals),
              "Avg daily rentals"    = d1_num(s$daily),
              "Median daily rentals" = d1_num(median(tapply(d$rental_count, d$date, sum))),
              "Avg duration"         = paste(d1_num(s$duration, 1), "min"),
              "Avg distance"         = paste(d1_num(s$distance, 2), "km"),
              "Commute-hour share"   = paste0(d1_num(s$commute, 1), "%"))
    fluidRow(lapply(names(vals), function(n)
      column(2, div(class = "well", style = "text-align:center; padding:8px; height:80px;",
                    div(n), strong(style = "font-size:18px;", vals[[n]])))))
  })
  
  output$d1_plot <- renderPlot({
    t <- d1_table()
    par(mar = c(4, 6, 3, 1))
    barplot(t[[input$d1_measure]], names.arg = t$group, col = "#2c7fb8", border = NA, las = 1,
            main = paste(names(d1_measures)[d1_measures == input$d1_measure], "by",
                         names(d1_groups)[d1_groups == input$d1_group]),
            cex.main = 1.4)
  })
  
  output$d1_table <- renderTable({
    t <- d1_table()
    data.frame(check.names = FALSE,
               Group                      = t$group,
               Days                       = t$days,
               `Total rentals`            = d1_num(t$rentals),
               `Avg daily rentals`        = d1_num(t$daily),
               `Avg duration (min)`       = d1_num(t$duration, 1),
               `Avg distance (km)`        = d1_num(t$distance, 2),
               `Commute-hour share (%)`   = d1_num(t$commute, 1))
  }, align = "lrrrrrr")
}