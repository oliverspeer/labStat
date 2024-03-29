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
packages <- c("shiny", "shinythemes", "shinyjs", "shinycssloaders", "DT", "memoise", 
              "readxl", "data.table", "tidyverse", "zoo", "plotly", "lubridate", 
              "DBI", "RSQLite", "refineR", "rstudioapi", "flextable", "cachem", "rstudioapi")

# Loop to check if libraries are installed and install them if not and load them
for (package in packages) {
  if (!(package %in% installed.packages())) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# connect to database ------------------------------------------------------
# Function to detect the operating system and return the corresponding database path
getDatabasePath <- function() {
  # Detect operating system
  os <- Sys.info()["sysname"]
  
  # Set the path based on the operating system
  if (os == "Linux") {
    # Path for Linux (Ubuntu)
    path <- "/home/olli/R_local/labStat/ClinicalChemistry_test.db"
  } else if (os == "Windows") {
    # Path for Windows
    # Adjust the path as necessary for your Windows setup
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

# List the tables in the database
# dbListTables(con)



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

# Define UI
ui <- fluidPage(
  titlePanel("Clinical Chemistry Analysis"),
  selectInput("device", "Choose Device", choices = NULL),
  selectInput("method", "Choose Method", choices = NULL),
  selectInput("year", "Choose Year", choices = NULL),
  tableOutput("analysisTable")
)

# Define server logic
server <- function(input, output, session) {
  # Update Device choices based on the database
  updateSelectInput(session, "device",
                    choices = dbGetQuery(db, "SELECT DISTINCT Gerät FROM MethodData"))
  
  # Update Method choices based on selected Device
  observeEvent(input$device, {
    req(input$device) # Require a device to be selected
    methods <- dbGetQuery(db, sprintf("SELECT DISTINCT Bezeichnung FROM MethodData WHERE Gerät = '%s'", input$device))
    updateSelectInput(session, "method", choices = methods)
  })
  
  # Update Year choices based on selected Method
  observeEvent(input$method, {
    req(input$method) # Require a method to be selected
    years <- dbGetQuery(db, sprintf("SELECT DISTINCT strftime('%%Y', Datum) as Year FROM MeasurementData WHERE Bezeichnung = '%s'", input$method))
    updateSelectInput(session, "year", choices = years)
  })
  
  # Generate analysis table based on selections
  output$analysisTable <- renderTable({
    req(input$year) # Require a year to be selected
    query <- sprintf("SELECT strftime('%%Y', Datum) as Year, COUNT(*) as NumberOfAnalysis, SUM(Txp) as TotalTxp
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Bezeichnung = TarifData.Bezeichnung
                      WHERE MeasurementData.Bezeichnung = '%s' AND strftime('%%Y', Datum) = '%s'
                      GROUP BY Year", input$method, input$year)
    dbGetQuery(db, query)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
