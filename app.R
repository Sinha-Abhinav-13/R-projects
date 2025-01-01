# Install and load required packages
install.packages("shiny")
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Simple Unit Converter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("conversion_type", "Select Conversion Type:",
                  choices = c("Length", "Weight", "Temperature")),
      conditionalPanel(
        condition = "input.conversion_type == 'Length'",
        numericInput("length_value", "Enter value to convert:", value = 1),
        selectInput("length_unit_from", "From:", choices = c("Meters", "Kilometers", "Miles")),
        selectInput("length_unit_to", "To:", choices = c("Meters", "Kilometers", "Miles"))
      ),
      conditionalPanel(
        condition = "input.conversion_type == 'Weight'",
        numericInput("weight_value", "Enter value to convert:", value = 1),
        selectInput("weight_unit_from", "From:", choices = c("Kilograms", "Grams", "Pounds")),
        selectInput("weight_unit_to", "To:", choices = c("Kilograms", "Grams", "Pounds"))
      ),
      conditionalPanel(
        condition = "input.conversion_type == 'Temperature'",
        numericInput("temp_value", "Enter value to convert:", value = 1),
        selectInput("temp_unit_from", "From:", choices = c("Celsius", "Fahrenheit", "Kelvin")),
        selectInput("temp_unit_to", "To:", choices = c("Celsius", "Fahrenheit", "Kelvin"))
      ),
      actionButton("convert", "Convert")
    ),
    mainPanel(
      h3("Converted Value:"),
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  convert_length <- function(value, from, to) {
    conversion_factors <- c(Meters = 1, Kilometers = 0.001, Miles = 0.000621371)
    value * conversion_factors[to] / conversion_factors[from]
  }
  
  convert_weight <- function(value, from, to) {
    conversion_factors <- c(Kilograms = 1, Grams = 1000, Pounds = 2.20462)
    value * conversion_factors[to] / conversion_factors[from]
  }
  
  convert_temperature <- function(value, from, to) {
    if (from == to) return(value)
    switch(from,
           Celsius = switch(to,
                            Fahrenheit = value * 9/5 + 32,
                            Kelvin = value + 273.15),
           Fahrenheit = switch(to,
                               Celsius = (value - 32) * 5/9,
                               Kelvin = (value - 32) * 5/9 + 273.15),
           Kelvin = switch(to,
                           Celsius = value - 273.15,
                           Fahrenheit = (value - 273.15) * 9/5 + 32)
    )
  }
  
  output$result <- renderText({
    req(input$convert)
    if (input$conversion_type == "Length") {
      result <- convert_length(input$length_value, input$length_unit_from, input$length_unit_to)
    } else if (input$conversion_type == "Weight") {
      result <- convert_weight(input$weight_value, input$weight_unit_from, input$weight_unit_to)
    } else if (input$conversion_type == "Temperature") {
      result <- convert_temperature(input$temp_value, input$temp_unit_from, input$temp_unit_to)
    }
    paste(result)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
