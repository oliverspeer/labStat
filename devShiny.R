library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyjs)
library(DT)

ui <- fluidPage(
  
  #themeSelector(),
  
  navbarPage(
    title = div(" ", img(src="logo_pos.png", 
                         height = 28, 
                         width = 130, 
                         style = "margin:1px 3px")
    ), 
    theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,
    tabPanel("KPI Klinische Chemie", "Analysen",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gerät", "Arbeitsplatz", choices = unique(combined.data.kc$Gerät)),
                 selectInput("methodenbezeichnung", "Analysen Methode", choices = NULL),
                 selectInput("year", "Jahr", choices = NULL)
                          ),
               mainPanel(
                 fluidRow(
                   column(12, DTOutput("yearlyAP"))
                 ),
                 fluidRow(
                   column(12, DTOutput("yearly"))
                 ),
                 fluidRow(
                  column(6, plotOutput("plot2")), 
                  column(6, DTOutput("quarterly"))
                 )
                 
               )
             )
    ),
    tabPanel("Plot", "Verlauf",
             fluidRow(
               column(12, plotOutput("plot1")),
               #column(12, plotOutput("plot2"))
               #column(12, plotOutput("plot3"))
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
    freezeReactiveValue(input, "methodenbezeichnung")
    updateSelectInput(inputId = "methodenbezeichnung", choices = choices) 
  })
  
  methode <- reactive({
    req(input$methodenbezeichnung)
    filter(gerät(), Bezeichnung == input$methodenbezeichnung)
  })
  observeEvent(methode(), {
    choices <- unique(methode()$Year)
    freezeReactiveValue(input, "year")
    updateSelectInput(inputId = "year", choices = choices)
  })
  
  output$quarterly <- renderDT({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      filter(Year == input$year) %>% 
      group_by(Quarter) %>% 
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE)
      )%>%
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 4, 
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE), 
                    caption = paste("Quartals-Zahlen ", input$year, " pro Methode: ", input$methodenbezeichnung),
                    rownames = FALSE) 
  })
  
  calculate_relative_difference <- function(df, variable) {
    df %>%
      arrange(Year) %>%
      mutate(
        !!paste0("%Delta ", variable) :=
          round((!!sym(variable) - lag(!!sym(variable))) / lag(!!sym(variable)) * 100, 2),
        Year = as.character(Year)
      ) |> arrange(desc(Year))
  }
  
  output$yearly <- renderDT({
    req(input$year, input$methodenbezeichnung)
    methode() %>%  
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
        .by = Year
      ) %>% 
      calculate_relative_difference("Anz_Aufträge")%>%
      mutate(across(.cols = is.numeric, .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )%>%
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 2, 
                                   lengthChange = FALSE,
                                   searching = FALSE), 
                    caption = paste("Jährlicher Umsatz pro Methode: ", input$methodenbezeichnung),
                    rownames = FALSE)
    
  })
  
  output$yearlyAP <- renderDT({
    req(input$year, input$gerät)
    gerät() %>%  
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        #Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
        .by = Year
      ) %>% 
      calculate_relative_difference("Anz_Aufträge")%>%
      mutate(across(.cols = is.numeric, .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )%>%
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 2, 
                                   lengthChange = FALSE,
                                   searching = FALSE), 
                    caption = paste("Jährlicher Umsatz pro Arbeitsplatz: ", input$gerät),
                    rownames = FALSE)
    
  })
  
  output$plot1 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      #group_by(Year) %>% 
      summarize(TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
                .by = c(Quarter, Year)) %>% 
      ggplot(aes(x = paste0(Year, " Q", Quarter), y = TxpUmsatz_KC)) + 
      geom_bar(stat = "identity") +
      labs(x = "Quartal", y = "Taxpunkte", 
           title = paste("Taxpunkt-Umsatz je Quartal:", input$methodenbezeichnung)
      )
  })
  
  output$plot2 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      group_by(Quarter = quarter(Datum), Year = year(Datum))  %>% 
      summarize(Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE)) %>% 
      ggplot(aes(x = Quarter, y = Anz_Aufträge, group = Year, fill = as.factor(Year))) + 
      geom_col(position = "dodge") +
      #geom_line() +
      #geom_point()+
      labs(x = "Quartal", y = "Anzahl Aufträge", 
           title = paste("Auftragszahlen je Quartal:", input$methodenbezeichnung)
      )+
      guides(fill = guide_legend(title = NULL))
  })
  
  output$sidebar_plot <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() %>% 
      group_by(Quarter = quarter(Datum), Year = year(Datum))  %>% 
      summarize(Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE)) %>% 
      ggplot(aes(x = Quarter, y = Anz_Aufträge, group = Year, fill = as.factor(Year))) + 
      geom_col(position = "dodge") +
      #geom_line() +
      #geom_point()+
      labs(x = "Quartal", y = "Anzahl Aufträge", 
           title = paste("Auftragszahlen je Quartal:", input$methodenbezeichnung)
      )+
      guides(fill = guide_legend(title = NULL))
  })
}

shinyApp(ui, server)
