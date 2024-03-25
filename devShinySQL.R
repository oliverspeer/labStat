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
library(DBI)
library(RSQLite)
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}
library(rstudioapi)

# set working directory ----------------------------------------------------
setwd("/home/olli/R_local/labStat")


# import rds data ---------------------------------------------------------


combined.data.kc <- readRDS("Combined_Data_KC.rds")
combined.data.kc <- as.data.table(combined.data.kc)



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
                 uiOutput("geräteChoices"),
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
    )))


# server -------------------------------------------------------------------
server <- function(input, output, session) {
  session$cache <- cache_mem(max_size = 4000e6)
  db <- dbConnect( SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )
  sql.gerät <- "SELECT DISTINCT Gerät FROM MethodData"
  gerät_choices <- dbGetQuery(db, sql.gerät)$Gerät
  gerät <- reactive({
    dbGetQuery(db, sql)
  }) 
  
  output$geräteChoices <- renderUI({
    selectInput("gerät", "Arbeitsplatz", choices = gerät_choices)
  })
  
  
  # Observe changes in gerät() and update methodenbezeichnung
  observeEvent(input$gerät, {
    # SQL to get unique `Bezeichnung` for the selected `Gerät`.
    sql.bez <- sprintf("SELECT DISTINCT Bezeichnung FROM MethodData WHERE Gerät = '%s'", input$gerät)
    # Fetch the unique `Bezeichnung` values.
    bezeichnung_choices <- dbGetQuery(db, sql.bez)$Bezeichnung
    freezeReactiveValue(input, "methodenbezeichnung")
    updateSelectInput(inputId = "methodenbezeichnung", choices = bezeichnung_choices) 
  }) 
  
  # Reactive expression for `methode`, getting data based on selected `Bezeichnung`.
  methode <- reactive({
    req(input$methodenbezeichnung)  # Ensure that a method description is selected
    
    # Prepare a SQL query to select data based on `Bezeichnung`
    # sql <- sprintf("SELECT DISTINCT m.Jahr
    #                 FROM MeasurementData m
    #                 Join MethodData d ON m.Bezeichnung = d.Bezeichnung
    #                 WHERE d.Bezeichnung = '%s'", input$methodenbezeichnung)
    # 
    # Execute the SQL query
    # dbGetQuery(db, sql)
  }) |> bindCache(input$methodenbezeichnung, cache = "session")
  
  # Cache the reactive for performance
  # observe({ methode() }) |> 
  #   bindCache(input$methodenbezeichnung, session = session)
  
  # Observe changes in `methode` and update years accordingly.
   observeEvent(input$methodenbezeichnung, {
  #   # Prepare a SQL query to get unique `Year` for the selected `Bezeichnung`.
  #   sql <- sprintf("SELECT DISTINCT Jahr FROM MethodData WHERE Bezeichnung = '%s'", input$methodenbezeichnung)
  #   
  # Fetch the unique `Year` values.
     year_choices <- dbGetQuery(db, sql)$Jahr
     
     # Freeze and update the select input for years.
     freezeReactiveValue(input, "year")
     updateSelectInput(session, "year", choices = year_choices)
   })
  
  

  
  
  output$quarterly <- renderDT({
    req(input$year, input$methodenbezeichnung)
    methode() |> 
      filter(Year == input$year, 
             Bezeichnung == input$methodenbezeichnung
             ) |> 
        summarize(
        TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
        Anz_Aufträge = n_distinct(a_Tagesnummer, na.rm = TRUE),
        #Anz_Fälle = n_distinct(b_Fallnummer, na.rm = TRUE),
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
  

}

shinyApp(ui, server)
