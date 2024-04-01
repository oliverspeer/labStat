# library(shiny)
# library(tidyverse)
# library(ggplot2)
# library(plotly)
# library(shinythemes)
# library(shinyjs)
# library(shinycssloaders)
# library(DT)
# library(memoise)
# library(cachem)
# library(data.table)
# library(DBI)
# library(RSQLite)
# if (!requireNamespace("rstudioapi", quietly = TRUE)) {
#   install.packages("rstudioapi")
# }
# library(rstudioapi)

# Vector of libraries
packages <- c(
              "shiny", 
              "shinythemes", 
              "shinyjs", 
              "shinycssloaders", 
              "DT", 
              "memoise", 
              "readxl", 
              "data.table", 
              "tidyverse", 
              "zoo", 
              "plotly", 
              "lubridate", 
              "DBI", 
              "RSQLite", 
              "refineR", 
              "rstudioapi", 
              "flextable", 
              "cachem", 
              "scales"
              )

# Loop to check if libraries are installed and install them if not and load them
for (package in packages) {
  if (! require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# connect to database ------------------------------------------------------
# Function to detect the operating system and return the corresponding database path
getDatabasePath <- function() {
  # Detect operating system
  os <- Sys.info()["sysname"]
  
  # Set the path based on the operating system
  if (os == "Linux") {
    # Path for Ubuntu
    path <- "/home/olli/R_local/labStat/ClinicalChemistry_test.db"
  } else if (os == "Windows") {
    # Path for Windows
    path <- "C:/R_local/labStat/ClinicalChemistry_test.db"
  } else {
    stop("Operating system not supported")
  }
  
  return(path)
}

# set database directory
db.wd <- getDatabasePath()

# Connect to the database
db <- dbConnect(SQLite(), dbname = db.wd)



# set working directory ----------------------------------------------------
# Use rstudioapi to get the path of the current project
project_directory <- rstudioapi::getActiveProject()

# if running in an RStudio project, set the working directory to the project directory
# If not running in an RStudio project, print a message
if (!is.null(project_directory)) {
  setwd(project_directory)
} else {
  print("This R session is not running within an RStudio Project.")
}






# shiny app ----------------------------------------------------------------

# Define UI------------------------------------------------------------------
ui <- fluidPage(
  
  navbarPage(
    title = div(img(src="logo_pos.png",  
                    height = 28, 
                    width = 130, 
                    style = "margin:1px 3px", "  Klinische Chemie ")
    ), 
    theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,
    
    sidebarLayout(
      sidebarPanel(
  # titlePanel("Clinical Chemistry Analysis"),
  selectInput("device", "Wähle den Arbeitsplatz", choices = NULL),
  selectInput("method", "Wähle die Methode", choices = NULL),
  # selectInput("year", "Wähle das Jahr", choices = NULL)
  ),
      
      mainPanel(
        fluidRow(
          column(12, DTOutput("yearlyDevice"))
        ),
        fluidRow(
          column(12, DTOutput("yearlyMethod"))
        ),
      )
  
)))

# Define server logic---------------------------------------------------------------------

server <- function(input, output, session) {
  # Update Device choices based on the database
  updateSelectInput(session, "device",
                    choices = dbGetQuery(db, "SELECT DISTINCT Gerät FROM MethodData 
                                         WHERE Gerät IS NOT NULL 
                                         ORDER BY Gerät ASC"))
  
  # Update Method choices based on selected Device
  observeEvent(input$device, {
    req(input$device) # Require a device to be selected
    methods <- dbGetQuery(db, sprintf("SELECT DISTINCT Bezeichnung FROM MethodData WHERE Gerät = '%s'", input$device))
    updateSelectInput(session, "method", choices = methods)
  })
  
  # Update Year choices based on selected Method
   observeEvent(input$method, {
     req(input$method) # Require a method to be selected
    #years <- dbGetQuery(db, sprintf("SELECT DISTINCT strftime('%%Y', Datum) as Year FROM MeasurementData WHERE Bezeichnung = '%s'", input$method))
     # years <- dbGetQuery(db, sprintf("SELECT DISTINCT Jahr FROM MethodData WHERE Bezeichnung = '%s'", input$method))
     # updateSelectInput(session, "year", choices = years)
   })
   
    # Generate table with yearly data for the selected device
  output$yearlyDevice <- renderDT({
    req(input$device) # Require a device to be selected
    query <- sprintf("SELECT strftime('%%Y', Datum) as Year, 
                       SUM(Txp) as 'Txp Umsatz', 
                       COUNT (DISTINCT Tagesnummer) as 'Anzahl Aufträge',
                       COUNT (DISTINCT Fallnummer) as 'Anzahl Fälle'
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                      JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                      WHERE MethodData.Gerät = '%s' 
                      GROUP BY Year
                      ORDER BY Year ASC", input$device)
    data.device <- dbGetQuery(db, query)
    
    # Initialize new column
    data.device$'Delta Aufträge' <- NA
    
    # Convert the data to a data frame and calculate the year-over-year percentage change
    if(nrow(data.device) > 1)   {
      data.device$`Delta Aufträge`[-1] <- percent(diff(data.device$`Anzahl Aufträge`)/head(data.device$`Anzahl Aufträge`, -1)) 
      # data.device$`Delta Aufträge [%]` <- round(data.device$`Delta Aufträge [%]` * 100, 2)
    }
    
    # render the data table
    datatable(data.device, options = list(
                            hover = TRUE, 
                            pageLength = 10, 
                            lengthChange = TRUE,
                            searching = FALSE,
                            ordering = TRUE,
                            order = list(0, "desc")
                                           ),
              caption = paste("Jährlicher Umsatz pro Gerät/AP: ", input$device),
              rownames = FALSE)
    
  })
    
    

 # Generate table with yearly data for the selected method
  output$yearlyMethod <- renderDT({
    req(input$method) # Require a method to be selected
    query <- sprintf("SELECT strftime('%%Y', Datum) as Year, 
                       SUM(Txp) as 'Txp Umsatz', 
                       COUNT (DISTINCT Tagesnummer) as 'Anzahl Aufträge',
                       COUNT (DISTINCT Fallnummer) as 'Anzahl Fälle'
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                      WHERE MeasurementData.Bezeichnung = '%s' 
                      GROUP BY Year
                      ORDER BY Year ASC", input$method
                     
                     )
    data.method <- dbGetQuery(db, query)
    
    # initialize a new column
    data.method$'Delta Aufträge' <- NA
    
    # Convert the data to a data frame and calculate the year-over-year percentage change
    if(nrow(data.method) > 1)   {
      data.method$`Delta Aufträge`[-1] <- percent(diff(data.method$`Anzahl Aufträge`)/head(data.method$`Anzahl Aufträge`, -1))
      
      
    }
    
    # render the data table
    datatable(data.method, options = list(
                            hover = TRUE, 
                            pageLength = 10, 
                            lengthChange = TRUE,
                            searching = FALSE,
                            ordering = TRUE,
                            order = list(0, "desc")
                            ), 
              caption = paste("Jährlicher Umsatz pro Methode: ", input$method),
              rownames = FALSE)
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
