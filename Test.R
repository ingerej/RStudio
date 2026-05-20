install.packages("shiny")
library(shiny)


#Trying to understand a little, so here is a game finding out if one have written a word begining on E. 
ui <- fluidPage(
  titlePanel("Start på E-spill"),
  textInput("ord", "Skriv et ord som starter på E:"),
  textOutput("resultat")
)

server <- function(input, output, session) {
  output$resultat <- renderText({
    req(input$ord)
    
    if (toupper(substr(input$ord, 1, 1)) == "E") {
      "RIKTIG! Ordet starter på E."
    } else {
      "Feil! Ordet starter ikke på E."
    }
  })
}

shinyApp(ui, server)

#Now math
#oppsummert: her er output. Først; hva skal stå øverst
ui <- fluidPage(
  titlePanel("Hvor smart er du?"),
  textOutput("Oppgave"),
  numericInput("svar", "Skriv svaret:", value = NULL),
  textOutput("Tilbakemelding"),
  actionButton("ny", "Nytt mattestykke")
)

server <- function(input, output, session) {
  
  # To reaktive tall som kan endres
  tall1 <- reactiveVal(sample(1:10, 1))
  tall2 <- reactiveVal(sample(1:10, 1))
  
  # Når brukeren trykker "ny"
  observeEvent(input$ny, {
    tall1(sample(10:100, 1))
    tall2(sample(10:100, 1))
  })
  
  # Oppgavetekst
  output$Oppgave <- renderText({
    paste("Hva er", tall1(), "+", tall2(), "?")
  })
  
  # Tilbakemelding
  output$Tilbakemelding <- renderText({
    req(input$svar)
    if (input$svar == tall1() + tall2()) {
      "Riktig!"
    } else {
      "Feil, prøv igjen..."
    }
  })
}

shinyApp(ui, server)


#okei, Git saves changes i script, so that you can see earlier versions, and go back if you make mistakes. Moreover, others can worsk on the same codeset.
install.packages("usethis")
library(usethis)
edit_git_config()

use_git()