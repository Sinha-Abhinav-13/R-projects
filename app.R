# Install and load required packages
install.packages("shiny")
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Rock-Paper-Scissors Game"),
  sidebarLayout(
    sidebarPanel(
      selectInput("user_choice", "Choose:", choices = c("Rock", "Paper", "Scissors")),
      actionButton("play", "Play"),
      hr(),
      h4("Your Choice:"),
      textOutput("user_choice_display"),
      h4("Computer's Choice:"),
      textOutput("computer_choice_display")
    ),
    mainPanel(
      h4("Result:"),
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$play, {
    user_choice <- input$user_choice
    computer_choice <- sample(c("Rock", "Paper", "Scissors"), 1)
    
    output$user_choice_display <- renderText({ user_choice })
    output$computer_choice_display <- renderText({ computer_choice })
    
    result <- switch(user_choice,
                     "Rock" = switch(computer_choice, "Rock" = "It's a tie!", "Paper" = "You lose!", "Scissors" = "You win!"),
                     "Paper" = switch(computer_choice, "Rock" = "You win!", "Paper" = "It's a tie!", "Scissors" = "You lose!"),
                     "Scissors" = switch(computer_choice, "Rock" = "You lose!", "Paper" = "You win!", "Scissors" = "It's a tie!")
    )
    
    output$result <- renderText({ result })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
