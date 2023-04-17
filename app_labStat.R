library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(flextable)
library(shinyjs)

ui <- fluidPage(
  
  #themeSelector(),
  list(tags$head(HTML('<href="logo_pos.png",
                                   type="png" />'))),
  div(style="padding: 10px 10px; width: '100%'",
      titlePanel(
        title="", windowTitle="Statistiken Klinische Chemie"
      )
  ),
  navbarPage(
    title = div(" ", img(src="logo_pos.png", 
                         height = 28, 
                         width = 130, 
                         style = "margin:1px 3px")
    ), 
    theme = shinytheme("readable"), 
    collapsible = TRUE,
    fluid = TRUE,
    tabPanel("KPI Klinische Chemie", "Analysen",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gerät", "Gerät", choices = unique(combined.data.kc$Gerät)),
                 selectInput("methodenbezeichnung", "Analysen Methode", choices = NULL),
                 selectInput("year", "Jahr", choices = NULL)
               ),
               mainPanel(
                 fluidRow(
                   column(12, tableOutput("quarterly")),
                   # column(6, tableOutput("yearly"))
                 ),
                 fluidRow(
                   # column(6, tableOutput("quarterly")),
                   column(12, tableOutput("yearly"))
                 ),
                 
               )
             )
    ),
    tabPanel("Plot", "Verlauf",
             fluidRow(
               column(4, plotOutput("plot1")),
               column(4, plotOutput("plot2")),
               column(4, plotOutput("plot3"))
             )
    )
  )
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
  
  output$quarterly <- renderTable(hover = TRUE,
                                  {
                                    req(input$year, input$methodenbezeichnung)
                                    methode() %>% 
                                      filter(Year == input$year) %>% 
                                      group_by(Quarter) %>% 
                                      summarize(
                                        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
                                        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
                                        Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE)
                                      ) 
                                  })
  
  output$yearly <- renderTable({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      #group_by(Year) %>% 
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
        .by = Year
      )
  })
  
  output$plot1 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      group_by(Year) %>% 
      summarize(TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE)) %>% 
      ggplot(aes(x = Year, y = TxpUmsatz_KC)) + 
      geom_line()
  })
  
  output$plot2 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      group_by(Year) %>% 
      summarize(Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE)) %>% 
      ggplot(aes(x = Year, y = Anz_Aufträge)) + 
      geom_line()
  })
  
  output$plot3 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      group_by(Year) %>% 
      summarize(Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE)) |> 
      ggplot(aes(x = Year, y = Anz_Fälle)) +
      geom_line()
  })
}

shinyApp(ui, server)
