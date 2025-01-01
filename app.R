# Install and load required packages
install.packages(c("shiny", "ggplot2", "dplyr", "DT", "shinydashboard"))
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Personal Fitness Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Workout Log", tabName = "log", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Input Workout", status = "primary", solidHeader = TRUE,
                    textInput("workout_name", "Workout Name", value = ""),
                    selectInput("category", "Category", choices = c("Cardio", "Strength", "Flexibility", "Balance")),
                    numericInput("duration", "Duration (minutes)", value = 30),
                    dateInput("date", "Date", value = Sys.Date()),
                    actionButton("add_workout", "Add Workout")
                ),
                box(title = "Progress Overview", status = "info", solidHeader = TRUE,
                    plotOutput("progress_plot")
                )
              )
      ),
      tabItem(tabName = "log",
              fluidRow(
                box(title = "Workout Log", status = "info", solidHeader = TRUE,
                    dataTableOutput("workout_table")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  workout_data <- reactiveVal(data.frame(
    Workout = character(),
    Category = character(),
    Duration = numeric(),
    Date = as.Date(character())
  ))
  
  observeEvent(input$add_workout, {
    new_workout <- data.frame(
      Workout = input$workout_name,
      Category = input$category,
      Duration = input$duration,
      Date = input$date
    )
    workout_data(rbind(workout_data(), new_workout))
  })
  
  output$workout_table <- renderDataTable({
    datatable(workout_data())
  })
  
  output$progress_plot <- renderPlot({
    ggplot(workout_data(), aes(x = Date, y = Duration, color = Workout, shape = Category)) +
      geom_line() +
      geom_point() +
      labs(title = "Workout Progress Over Time", x = "Date", y = "Duration (minutes)") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
