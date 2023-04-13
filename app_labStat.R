library(shiny)

ui <- fluidPage(
  selectInput("gerät", "Gerät", choices = unique(combined.data.kc$Gerät)),
  selectInput("methodenbezeichnung", "Analysen Methode", choices = NULL),
  selectInput("year", "Jahr", choices = NULL),
  tableOutput("data")
)

server <- function(input, output, session) {
  gerät <- reactive({
    filter(combined.data.kc, Gerät == input$gerät)
  })
  observeEvent(gerät(), {
    choices <- unique(gerät()$Bezeichnung)
    updateSelectInput(inputId = "methodenbezeichnung", choices = choices) 
  })
  
  methode <- reactive({
    req(input$methodenbezeichnung)
    filter(gerät(), Bezeichnung == input$methodenbezeichnung)
  })
  observeEvent(methode(), {
    choices <- unique(methode()$Year)
    updateSelectInput(inputId = "year", choices = choices)
  })
  
  output$data <- renderTable({
    req(input$year)
    methode() %>% 
      filter(Year == input$year) %>% 
      #select(Taxpkt., g_Auftragg., f_Geschl.) |> 
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
        .by = Quarter
      )
  })
}



shinyApp(ui, server)