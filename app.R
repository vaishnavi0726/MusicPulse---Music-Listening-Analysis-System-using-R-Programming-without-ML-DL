# MusicPulse - Final Shiny App - S5 S6 S7

library(shiny)
source("modules/helpers.R")
music <- load_music()

ui <- fluidPage(
  titlePanel("MusicPulse - Music Listening Analysis"),
  tabsetPanel(
    tabPanel("Music Data", tableOutput("music_table")),
    
    tabPanel("1. Extract", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("col_name", "Column:", choices = colnames(music), selected = "Song_Name"),
                 actionButton("extract_btn", "Extract")
               ),
               mainPanel(verbatimTextOutput("vector_output"))
             )
    ),
    
    tabPanel("2. Sequence",
             sidebarLayout(
               sidebarPanel(
                 numericInput("start", "Start:", 1), numericInput("end", "End:", 10),
                 numericInput("step", "Step:", 2), actionButton("seq_btn", "Generate")
               ),
               mainPanel(verbatimTextOutput("seq_output"))
             )
    ),
    
    tabPanel("3. Combine",
             sidebarLayout(
               sidebarPanel(
                 selectInput("vec1", "Month 1:", choices = unique(music$Month), selected = "January", multiple = TRUE),
                 selectInput("vec2", "Month 2:", choices = unique(music$Month), selected = "February", multiple = TRUE),
                 actionButton("combine_btn", "Combine")
               ),
               mainPanel(verbatimTextOutput("combine_output"), verbatimTextOutput("unique_output"))
             )
    ),
    
    tabPanel("4. Monthly Listening",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("months", "Select Months:", choices = unique(music$Month), selected = unique(music$Month)[1:3]),
                 actionButton("monthly_btn", "Show")
               ),
               mainPanel(
                 verbatimTextOutput("monthly_text"),
                 plotOutput("monthly_plot")
               )
             )
    ),
    
    tabPanel("5. Average Rating",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("songs", "Select Song IDs:", choices = music$Song_ID, selected = music$Song_ID[1:3]),
                 actionButton("avg_btn", "Calculate Average")
               ),
               mainPanel(
                 verbatimTextOutput("avg_text"),
                 h4("Ratings:"),
                 verbatimTextOutput("avg_ratings")
               )
             )
    ),
    
    tabPanel("6. Summary",
             h3("Project Summary"),
             verbatimTextOutput("summary_output"),
             h4("Concepts Used (for Viva):"),
             p("seq(), c(), %in%, mean(), indexing [1:3], tail(), unique(), tolower(), trimws(), is.na(), load_music() validation")
    )
  )
)

server <- function(input, output) {
  output$music_table <- renderTable({ music })
  
  # 1 Extract
  extracted <- eventReactive(input$extract_btn, { music[[input$col_name]] })
  output$vector_output <- renderPrint({
    vec <- extracted(); print(vec)
    cat("\nLength:", length(vec), "\nFirst 3:", paste(vec[1:3], collapse=", "))
  })
  
  # 2 Sequence
  seq_result <- eventReactive(input$seq_btn, {
    s <- input$start; e <- input$end; st <- input$step
    if ((s < e && st < 0) || (s > e && st > 0)) return("Wrong step direction! Positive if start<end, negative if start>end")
    seq(s, e, st)
  })
  output$seq_output <- renderPrint({ seq_result() })
  
  # 3 Combine
  combined <- eventReactive(input$combine_btn, { c(input$vec1, input$vec2) })
  output$combine_output <- renderPrint({ cat("Combined c():\n"); print(combined()) })
  output$unique_output <- renderPrint({
    filtered <- music[music$Month %in% combined(), ]
    cat("Unique Genres:\n"); print(unique(filtered$Genre))
  })
  
  # 4 Monthly - with tolower() validation like console
  monthly_data <- eventReactive(input$monthly_btn, {
    sel <- input$months
    sel_lower <- tolower(trimws(sel))
    months_lower <- tolower(music$Month)
    # Check not found
    not_found <- sel[!sel_lower %in% months_lower]
    filtered <- music[months_lower %in% sel_lower, ]
    list(data = filtered, not_found = not_found)
  })
  output$monthly_text <- renderPrint({
    res <- monthly_data()
    if (length(res$not_found) > 0) cat("Not found in data:", paste(res$not_found, collapse=", "), "\n")
    if (nrow(res$data) == 0) { cat("No matching months found.\n"); return() }
    agg <- aggregate(Listening_Count ~ Month, data = res$data, sum)
    print(agg)
    cat("\nTotal:", sum(agg$Listening_Count), "\n")
  })
  output$monthly_plot <- renderPlot({
    res <- monthly_data()
    if (nrow(res$data) == 0) return()
    agg <- aggregate(Listening_Count ~ Month, data = res$data, sum)
    barplot(agg$Listening_Count, names.arg = agg$Month, col = "skyblue",
            main = "Listening Count by Month", ylab = "Count")
  })
  
  # 5 Average
  avg_data <- eventReactive(input$avg_btn, {
    ids <- suppressWarnings(as.numeric(input$songs))
    if (any(is.na(ids))) return(list(error = "Please enter valid Song_IDs"))
    not_found <- ids[!ids %in% music$Song_ID]
    filtered <- music[music$Song_ID %in% ids, ]
    list(data = filtered, not_found = not_found, ids = ids, error = NULL)
  })
  output$avg_text <- renderPrint({
    res <- avg_data()
    if (!is.null(res$error)) { cat(res$error, "\n"); return() }
    if (length(res$not_found) > 0) cat("Song_ID not found:", paste(res$not_found, collapse=", "), "\n")
    if (nrow(res$data) == 0) { cat("No valid Song_IDs found.\n"); return() }
    cat("Average Rating:", mean(res$data$Rating), "\n")
    cat("Count:", nrow(res$data), "\n")
  })
  output$avg_ratings <- renderPrint({
    res <- avg_data()
    if (!is.null(res$error) || nrow(res$data)==0) return()
    print(res$data[, c("Song_ID", "Song_Name", "Rating")])
  })
  
  # 6 Summary
  output$summary_output <- renderPrint({
    cat("Total Songs:", nrow(music), "\n")
    cat("Total Listening:", sum(music$Listening_Count), "\n")
    cat("Avg Rating:", mean(music$Rating), "\n")
    cat("Months:", paste(unique(music$Month), collapse=", "), "\n")
  })
}

shinyApp(ui, server)