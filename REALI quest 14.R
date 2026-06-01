install.packages("fGarch")
install.packages("shinyjs")
install.packages("plotly")
install.packages("ggplot2")
install.packages("shiny")

library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(fGarch)


set.seed(123)
pop_data <- data.frame(x = rsnorm(1000, mean = 8.5, sd = 1.5, xi = 0.6))

interval_labels <- c(
  "2 til 3",
  "3 til 4",
  "4 til 5",
  "5 til 6",
  "6 til 7",
  "7 til 8",
  "8 til 9",
  "9 til 10",
  "10 til 11",
  "11 til 12"
)

interval_bounds <- list(
  c(2, 3),
  c(3, 4),
  c(4, 5),
  c(5, 6),
  c(6, 7),
  c(7, 8),
  c(8, 9),
  c(9, 10),
  c(10, 11),
  c(11, 12)
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Spørsmål 14: Gjennomsnitt og intervall"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Spørsmål:"),
      p("Her er fordelingen av flere målinger gjort av en populasjon. Et utvalg på 10 tilfeldig trukkede verdier tas fra populasjonen og utvalgsgjennomsnittet blir regnet ut. Hvilket intervall er mest sannsynlig å inkludere utvalgsgjennomsnittet?"),
      actionButton("submit", "Sjekk svar"),
      textOutput("feedback"),
      br(),
      actionButton("next", "Neste spørsmål", style = "display:none;")
    ),
    
    mainPanel(
      plotlyOutput("distPlot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  # start på 8–9 → det er det 7. intervallet
  current_interval <- reactiveVal(7)
  
  output$distPlot <- renderPlotly({
    idx <- current_interval()
    chosen <- interval_bounds[[idx]]
    
    breaks_seq <- seq(
      floor(min(pop_data$x)),
      ceiling(max(pop_data$x)),
      by = 0.2
    )
    
    hist_data <- hist(
      pop_data$x,
      breaks = breaks_seq,
      plot = FALSE
    )
    
    df_hist <- data.frame(
      x = hist_data$mids,
      y = hist_data$counts,
      color = ifelse(hist_data$mids >= chosen[1] & hist_data$mids < chosen[2],
                     "#4CAF50",
                     "black")
    )
    
    g <- ggplot(df_hist, aes(x = x, y = y, fill = color)) +
      geom_col(color = "white") +
      scale_fill_identity() +
      scale_x_continuous(
        breaks = seq(floor(min(pop_data$x)), ceiling(max(pop_data$x)), by = 1)
      ) +
      labs(x = "Måling", y = "Frekvens") +
      theme_minimal()
    
    ggplotly(g, source = "dist", tooltip = NULL) |>
      config(displayModeBar = FALSE)
  })
  
  observeEvent(event_data("plotly_click", source = "dist"), {
    click <- event_data("plotly_click", source = "dist")
    if (is.null(click)) return()
    
    x_clicked <- click$x
    
    # finn hvilket intervall klikket faller i
    idx <- which(sapply(interval_bounds, function(b) x_clicked >= b[1] & x_clicked < b[2]))
    
    if (length(idx) == 0) return()
    
    current_interval(idx)
  })
  
  observeEvent(input$submit, {
    idx <- current_interval()
    chosen_label <- interval_labels[idx]
    
    if (chosen_label == "8 til 9") {
      output$feedback <- renderText("✅ Riktig! Utvalgets gjennomsnitt ligger mest sannsynlig mellom 8 og 9.")
      shinyjs::show("next")
    } else {
      output$feedback <- renderText("❌ Feil. Riktig svar er mellom 8 og 9.")
      shinyjs::show("next")
    }
  })
}

shinyApp(ui = ui, server = server)

