library(shiny)

# Function to load game state from file
load_state <- function() {
  if (file.exists("game_state.rds")) {
    readRDS("game_state.rds")
  } else {
    list(board = matrix(0, nrow = 6, ncol = 7), current_player = 1, score1 = 0, score2 = 0)
  }
}

# Function to save game state to file
save_state <- function(state) {
  saveRDS(state, "game_state.rds")
}

ui <- fluidPage(
  titlePanel("Connect 4"),
  sidebarLayout(
    sidebarPanel(
      actionButton("reset", "Reset Game"),
      br(),
      h3("SCORES"),
      textOutput("score1"),
      textOutput("score2")
    ),
    mainPanel(
      tags$style(type="text/css", "
        .grid { display: inline-block; border: 5px solid #333; padding: 10px; background: #2c3e50; box-shadow: 0px 5px 20px rgba(0, 0, 0, 0.5); border-radius: 10px; }
        .slot { width: 50px; height: 50px; border-radius: 50%; display: inline-block; margin: 1px; }
        .red { background-color: red; }
        .yellow { background-color: yellow; }
        .white { background-color: white; }
        .fall { animation: fall 0.5s; }
        @keyframes fall {
          from { transform: translateY(-50px); }
          to { transform: translateY(0); }
        }
      "),
      div(class = "grid", uiOutput("board"))
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveVal(load_state())
  
  check_win <- function(board) {
    for (i in 1:6) {
      for (j in 1:7) {
        if (board[i, j] != 0) {
          if (j <= 4 && all(board[i, j:(j + 3)] == board[i, j])) return(TRUE)
          if (i <= 3 && all(board[i:(i + 3), j] == board[i, j])) return(TRUE)
          if (i <= 3 && j <= 4 && all(board[cbind(i:(i + 3), j:(j + 3))] == board[i, j])) return(TRUE)
          if (i >= 4 && j <= 4 && all(board[cbind(i:(i - 3), j:(j + 3))] == board[i, j])) return(TRUE)
        }
      }
    }
    return(FALSE)
  }
  
  output$board <- renderUI({
    board_matrix <- state()$board
    lapply(1:6, function(row) {
      fluidRow(
        lapply(1:7, function(col) {
          slot_class <- ifelse(board_matrix[row, col] == 0, "white", ifelse(board_matrix[row, col] == 1, "red", "yellow"))
          button_id <- paste0("button_", row, "_", col)
          actionButton(button_id, "", width = "50px", height = "50px",
                       class = paste("slot", slot_class, sep = " "),
                       style = ifelse(input[[button_id]] == 0, "fall", ""))
        })
      )
    })
  })
  
  observe({
    lapply(1:6, function(row) {
      lapply(1:7, function(col) {
        button_id <- paste0("button_", row, "_", col)
        observeEvent(input[[button_id]], {
          s <- state()
          if (s$board[row, col] == 0) {
            for (r in 6:1) {
              if (s$board[r, col] == 0) {
                s$board[r, col] <- s$current_player
                break
              }
            }
            if (check_win(s$board)) {
              if (s$current_player == 1) {
                s$score1 <- s$score1 + 1
              } else {
                s$score2 <- s$score2 + 1
              }
              showModal(modalDialog(
                title = "Game Over",
                paste("Player", s$current_player, "wins!"),
                easyClose = TRUE,
                footer = NULL
              ))
              s$board <- matrix(0, nrow = 6, ncol = 7)
            } else {
              s$current_player <- ifelse(s$current_player == 1, 2, 1)
            }
            state(s)
            save_state(s)
          }
        })
      })
    })
  })
  
  observeEvent(input$reset, {
    s <- list(board = matrix(0, nrow = 6, ncol = 7), current_player = 1, score1 = 0, score2 = 0)
    state(s)
    save_state(s)
  })
  
  output$score1 <- renderText({ paste("Player 1: ", state()$score1) })
  output$score2 <- renderText({ paste("Player 2: ", state()$score2) })
}

shinyApp(ui = ui, server = server)
