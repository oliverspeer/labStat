library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(memoise)
library(cachem)
library(data.table) 

# set working directory ----------------------------------------------------
setwd("C:/R_local/labStat")


# import rds data ---------------------------------------------------------


combined.data.kc <- readRDS("Combined_Data_KC.rds")




# shiny app ----------------------------------------------------------------

shinyOptions(cache = cache_mem(max_size = 5000e6))

ui <- fluidPage(
  
  #themeSelector(),
  
  navbarPage(
    title = div(img(src="logo_pos.png",  
                         height = 28, 
                         width = 130, 
                         style = "margin:1px 3px", "  Klinische Chemie ")
    ), 
    theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,

# Taxpunktumsatz tabPanel  ------------------------------------------------

    tabPanel("Taxpunk-Umsatz", "Analysen",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gerät", "Arbeitsplatz", 
                             choices = sort(unique(combined.data.kc$Gerät))),
                 selectInput("methodenbezeichnung", "Analysen Methode", 
                             choices = NULL),
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
                  column(6, withSpinner(plotOutput("plot2"),type = 2, color = "red", color.background = "white",  size = 2, hide.ui = FALSE)), 
                  column(6, DTOutput("quarterly"))
                 )
                 
               )
             )
    ),

# Quartals-Plot -----------------------------------------------------------

    tabPanel("Plot", "Verlauf",
             fluidRow(
               column(12, plotOutput("plot1")),
               #column(12, plotOutput("plot2"))
               #column(12, plotOutput("plot3"))
             )
          ),

# Costs for salary, reagents, contracts, services tabPanel ---------------------------------------------

    tabPanel("Lohn- und Sach-Kosten", "jährliche Lohnkosten, jährliche Sachkosten (inkl Unterauftrag & Services & Reagenzien), quart Reagenzien-Kosten",
            
            fluidRow(
              column(6, DTOutput("yearlysalary"))
            ),
            fluidRow(
              column(6, DTOutput("yearlycosts"))
                    ),
            fluidRow(
              column(6, withSpinner(DTOutput("yearlyrevenue"),type = 2, color = "red", color.background = "white",  size = 2, hide.ui = FALSE))
            ),
            fluidRow(
              column(6, DTOutput("quarterlyReagents"))
                    ),
           #column(12, plotOutput("plot2"))
           #column(12, plotOutput("plot3"))
         )
      )
    )
 

server <- function(input, output, session) {
  session$cache <- cache_mem(max_size = 4000e6)
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
    arrange(gerät(), input$methodenbezeichnung)
      }) |> bindCache(gerät(), input$methodenbezeichnung, cache = "session")
  observeEvent(methode(), {
    choices <- unique(methode()$Year)
    freezeReactiveValue(input, "year")
    updateSelectInput(inputId = "year", choices = choices)
  })
  
  reagents <- reactive({
    filter(reag.costs, Year == input$year)
  }) |> bindCache(reag.costs, input$year, cache = "session")

  cons.costs  <- reactive({
    filter(bookings, Year == input$year)
  }) |> bindCache(bookings, input$year, cache = "session")
  
  HR.costs  <- reactive({
    filter(salary, Year == input$year)
  }) |> bindCache(salary, input$year, cache = "session")
  
  revenue  <- reactive({
    filter(combined.data.kc, Year == input$year)
  }) |> bindCache(combined.data.kc, input$year, cache = "session")
  
  
  output$quarterly <- renderDT({
    req(input$year, input$methodenbezeichnung)
    methode() |> 
      filter(Year == input$year, 
             Bezeichnung == input$methodenbezeichnung
             ) |> 
        summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        # Anz_Auftraege_m = length(Taxpkt., na.rm = TRUE),
        .by = Quarter
      )|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 4, 
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE), 
                    caption = paste("Quartals-Zahlen ", input$year, " pro Methode: ", input$methodenbezeichnung),
                    rownames = FALSE) 
  }) 
  
  calculate_relative_difference <- function(df, variable) {
    df |>
      arrange(Year) |>
      mutate(
        !!paste0("%Delta ", variable) :=
          round((!!sym(variable) - lag(!!sym(variable))) / lag(!!sym(variable)) * 100, 2),
        Year = as.character(Year)
      ) |> arrange(desc(Year))
  }
  calculate_relative_differencem <- memoise(calculate_relative_difference, 
                                            cache = session$cache)
  
  
  output$yearly <- renderDT({
    req(input$year, input$methodenbezeichnung)
    methode()  |>  
      filter(Bezeichnung == input$methodenbezeichnung) |>
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
        .by = Year
      ) |> 
      calculate_relative_differencem("Anz_Aufträge")|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 2, 
                                   lengthChange = FALSE,
                                   searching = FALSE), 
                    caption = paste("Jährlicher Umsatz pro Methode: ", input$methodenbezeichnung),
                    rownames = FALSE)
    
  }) 
  
  output$yearlyAP <- renderDT({
    req(input$year, input$gerät)
    gerät() |>  
      summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        #Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
        .by = Year
      ) |> 
      calculate_relative_difference("Anz_Aufträge")|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 2, 
                                   lengthChange = FALSE,
                                   searching = FALSE), 
                    caption = paste("Jährlicher Umsatz pro Arbeitsplatz: ", input$gerät),
                    rownames = FALSE)
    
  }) |> bindEvent(input$gerät)
  
  output$plot1 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() |> 
       
      summarize(TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
                .by = c(Quarter, Year)) |> 
      ggplot(aes(x = paste0(Year, " Q", Quarter), y = TxpUmsatz_KC)) + 
      geom_bar(stat = "identity") +
      labs(x = "Quartal", y = "Taxpunkte", 
           title = paste("Taxpunkt-Umsatz je Quartal:", input$gerät)
      )
  }) #|> bindCache(input$year, input$methodenbezeichnung, methode(), cache = "session")
  
  output$plot2 <- renderPlot({
    req(input$year, input$methodenbezeichnung)
    methode() |> 
      filter(Bezeichnung == input$methodenbezeichnung) |>
      #group_by(Quarter = quarter(Datum), Year = year(Datum))  |> 
      summarize(Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
                .by = c(Quarter, Year)) |> 
      ggplot(aes(x = Quarter, y = Anz_Aufträge, group = Year, fill = as.factor(Year))) + 
      geom_col(position = "dodge") +
      #geom_line() +
      #geom_point()+
      labs(x = "Quartal", y = "Anzahl Aufträge", 
           title = paste("Quartalszahlen:", input$methodenbezeichnung)
      )+
      guides(fill = guide_legend(title = NULL))
  })|> bindCache(input$year, input$methodenbezeichnung, methode(), cache = "session") 
  
# #  output$sidebar_plot <- renderPlot({
#     req(input$year, input$methodenbezeichnung)
#     methode() |> 
#       group_by(Quarter = quarter(Datum), Year = year(Datum))  |> 
#       summarize(Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE)) |> 
#       ggplot(aes(x = Quarter, y = Anz_Aufträge, group = Year, fill = as.factor(Year))) + 
#       geom_col(position = "dodge") +
#       #geom_line() +
#       #geom_point()+
#       labs(x = "Quartal", y = "Anzahl Aufträge", 
#            title = paste("Auftragszahlen je Quartal:", input$methodenbezeichnung)
#       )+
#       guides(fill = guide_legend(title = NULL))
#   })|> bindCache(input$year, input$methodenbezeichnung, methode())
  
  # output of quaterly reagents costs
  output$quarterlyReagents <- renderDT({
    req(input$year)
    reagents() |> 
      filter(Year == input$year) |> 
      #group_by(Quarter) |> 
      summarize(
        Reagenzienkosten = sum(Wwgesamtbrutto, na.rm = TRUE),
        .by = Quarter  )|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 4, 
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE), 
                    caption = paste("Quartals-Zahlen ", input$year),
                    rownames = FALSE) 
 })
  
  # output of yearly salary
  output$yearlysalary <- renderDT({
    req(input$year)
    HR.costs() |> 
      filter(Year == input$year) |> 
      #group_by(Quarter) |> 
      summarize(
        Lohnkosten = sum(Ges.Lohn, na.rm = TRUE),
        .by = Year
      )|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 4, 
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE), 
                    #caption = paste("Jährliche Ausgaben ", input$year),
                    rownames = FALSE) 
    
  })
  
  # output of yearly costs
  output$yearlycosts <- renderDT({
    req(input$year)
    cons.costs() |> 
      filter(Year == input$year) |> 
      #group_by(Quarter) |> 
      summarize(
        Sachkosten = sum(WertBWähr, na.rm = TRUE),
        .by = Year
      )|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 4, 
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE), 
                    #caption = paste("Jährliche Ausgaben ", input$year),
                    rownames = FALSE) 
                   
  })
  
  # output of yearly revenues
  output$yearlyrevenue <- renderDT({
    req(input$year)
    revenue() |> 
      filter(Year == input$year) |> 
      #group_by(Quarter) |> 
      summarize(
        Umsatz = sum(Taxpkt., na.rm = TRUE),
        .by = Year
      )|>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")
      )
      )|>
      DT::datatable(options = list(hover = TRUE, 
                                   pageLength = 4, 
                                   lengthChange = FALSE,
                                   searching = FALSE,
                                   paging = FALSE), 
                    caption = paste("Umsatz ", input$year),
                    rownames = FALSE) 
    
  })
  
}

shinyApp(ui, server)
