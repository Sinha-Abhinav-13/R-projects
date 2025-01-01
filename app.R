library(shiny)
library(httr)
library(jsonlite)
fetch <- function(query) {
  url <- sprintf("https://books.google.com/ngrams/json?content=%s&year_start=1800&year_end=2016&corpus=26&smoothing=3", query)
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(data)
}


ui <- fluidPage(
  titlePanel("FREQUENCY OF A WORD USED THROUGHOUT HISTORY"),
  sidebarLayout(
    sidebarPanel(
      textInput("ngram", "Enter some text", "Shiny"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("ngram_plot")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$submit, {
    query <- input$ngram
    ngram_data <- fetch(query)
    
    
    print("Full ngram_data structure:")
    print(str(ngram_data))
    
    
    if (nrow(ngram_data) == 0 || 
        is.null(ngram_data$timeseries) || 
        length(ngram_data$timeseries[[1]]) == 0) {
      print("No valid timeseries data found.")
      return()
    }
    

    timeseries_data <- ngram_data$timeseries[[1]]
    

    if (!is.numeric(timeseries_data) || length(timeseries_data) == 0) {
      print("No valid numeric timeseries data found.")
      return()
    }
    
    years <- seq(1800, 1800 + length(timeseries_data) - 1)

    ngram_df <- data.frame(
      Year = years,
      Frequency = timeseries_data
    )

    output$ngram_plot <- renderPlot({
      plot(ngram_df$Year, ngram_df$Frequency, type = "l", 
           main = paste("Historical Usage of:", query), 
           xlab = "Year", 
           ylab = "Frequency", 
           col = "blue", 
           lwd = 2)
      grid()
    })
  })
}
shinyApp(ui = ui, server = server)