# prepare libraries ---------------------------------------------------------
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
              "scales",
              "dtplyr",
              "shinydashboard"
              )

# Loop to check if libraries are installed and install them if not and load them
for (package in packages) {
  if (! require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# setting ggplot theme------------------------------------------------------
theme_set(
  theme_grey() +
  theme( text = element_text(size = 14),
         axis.title = element_text(size = 16),
         axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
         axis.text.y = element_text(size = 14))
                      )

# connect to database ------------------------------------------------------
# Function to detect the operating system and return the corresponding database path
getDatabasePath <- function() {
  # Detect operating system
  os <- Sys.info()["sysname"]
  
  # Set the path based on the operating system
  if (os == "Linux") {
    # Path for Ubuntu
    path <- "/home/olli/R_local/labStat/ClinicalChemistry_1.db"
  } else if (os == "Windows") {
    # Path for Windows
    path <- "C:/R_local/labStat/ClinicalChemistry_1.db"
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

# setup functions ------------------------------------------------------------
fun.labels <- function(values, data.range = NULL) {
  if(is.null(data.range)) {
    data.range <- range(values, na.rm = TRUE)
  }
  max.value <- max(abs(data.range))
  
  if (max.value >= 1e6) {
    return(number(values / 1e6, accuracy = 0.1, suffix = " M"))
  } else if (max.value >= 1e3) {
    return(number(values / 1e3, accuracy = 0.1, suffix = " k"))
  } else {
    return(number(values))
  }
}




# shiny app ----------------------------------------------------------------
shinyOptions(cache = cache_mem(max_size = 5000e6))
# Define UI------------------------------------------------------------------
ui <- fluidPage(
  
  navbarPage(
    title = div(img(src="logo_pos.png",  
                    height = 28, 
                    width = 130, 
                    style = "margin:1px 3px", "  Klinische Chemie ")
    ), 
    # theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,
  
  
  # tabPanel-----------------------------------------------------------------------
   tabPanel("Jährlicher Umsatz in Grafiken -> hier Auswahl des Gerätes / Arbeitsplatz.", 
            selectInput("device", "Wähle den Arbeitsplatz", choices = NULL),
        fluidRow(
          column(12, withSpinner( plotOutput("yearlyDevicePlot", height = "300px"),
                                 type = 2, 
                                 color ="red", 
                                 color.background = "white",
                                 size = 2, 
                                 hide.ui = FALSE)) # ,
          # column(6, plotOutput("yearlyMethodPlot", height = "300px"))
        ),
        fluidRow(
         column(12, plotOutput("yearlyMethodPlot", height = "400px"))
        ), 

  ),
  
  # tabPanel-----------------------------------------------------------------------
  tabPanel("Gesamtumsatz",
           # sidebarPanel(
           # titlePanel("Clinical Chemistry Analysis"),
           
           
           # selectInput("year", "Wähle das Jahr", choices = NULL),
           
           # mainPanel(
           
           fluidRow(
             column(6, plotOutput("globalChart", height = "350px")),
             column(6, plotOutput("globalChartRel", height = "350px"))
           )),
  
  # tabPanel-----------------------------------------------------------------------
    tabPanel("Jährlicher Umsatz in Tabellen",
        fluidRow(
          column(12, DTOutput("yearlyDevice"))
        ),
             selectInput("method", "Wähle die Methode", choices = NULL),
        fluidRow(
          column(12, withSpinner(DTOutput("yearlyMethod"), 
                                 type = 2, 
                                 color ="red", 
                                 color.background = "white",
                                 size = 2, 
                                 hide.ui = FALSE))
        )),
  # tabPanel-----------------------------------------------------------------------
    tabPanel("Umsatz pro Analyt",
      fluidRow(
        column(12, withSpinner(plotOutput("analytePlot", height = "400px"),
                               type = 2, 
                               color ="red", 
                               color.background = "white",
                               size = 2, 
                               hide.ui = FALSE))
      ),
      # fluidRow(
      #   column(12, DTOutput("analyteTable"))
      # )
      )
   # ------------------------------------------------------------------------------   
    )
      # )
  
     # )
   )


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
    methods <- dbGetQuery(db, sprintf("SELECT DISTINCT MeasurementData.Bezeichnung 
                                      FROM MeasurementData 
                                      JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                                      WHERE MethodData.Gerät = '%s'
                                      ORDER BY MeasurementData.Bezeichnung ASC", input$device))
    updateSelectInput(session, "method", choices = methods)
  })
  
  # Update Year choices based on selected Method
   # observeEvent(input$method, {
   #   req(input$method) # Require a method to be selected
   #  #years <- dbGetQuery(db, sprintf("SELECT DISTINCT strftime('%%Y', Datum) as Year FROM MeasurementData WHERE Bezeichnung = '%s'", input$method))
   #   # years <- dbGetQuery(db, sprintf("SELECT DISTINCT Jahr FROM MethodData WHERE Bezeichnung = '%s'", input$method))
   #   # updateSelectInput(session, "year", choices = years)
   # })
   
# Generate table with yearly data for the selected device-------------------------------
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
    
    data.device <- data.device |>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")))
    # render the data table
    datatable(data.device, options = list(
                            hover = TRUE, 
                            pageLength =2, 
                            lengthChange = TRUE,
                            searching = FALSE,
                            ordering = TRUE,
                            order = list(0, "desc")
                                           ),
              caption = paste("Jährlicher Umsatz pro Gerät/AP: ", input$device),
              rownames = FALSE)
    
  })

  # all  devices over years ---------------------------------------------------
  
    query.all <- sprintf("SELECT MeasurementData.Jahr AS Year, MethodData.Gerät AS Device,
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  GROUP BY MeasurementData.Jahr, MethodData.Gerät
                  ORDER BY MeasurementData.Jahr ASC, MethodData.Gerät ASC")
  
    data.all <- dbGetQuery(db, query.all)
  
  output$globalChart <- renderPlot({  
        data.range <- range(data.all$'Txp Umsatz', na.rm = TRUE)
  
  
  # ggplot stacked bar chart
    ggplot(data.all, aes(x = Year, y = `Txp Umsatz`, fill = Device)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = " ", y = "Txp Umsatz", title = "Jährlicher Txp Umsatz") +
     # theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(labels = function(values) fun.labels(values, data.range))
  
  })  
  
  output$globalChartRel <- renderPlot({  
    
    
    # ggplot stacked bar chart
    ggplot(data.all, aes(x = Year, y = `Txp Umsatz`, fill = Device)) +
      geom_bar(stat = "identity", position = "fill") +
      labs(x = " ", y = "relativer Umsatz", title = "Jährlicher Umsatz, relativ pro Gerät") +
      #theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
      scale_fill_brewer(palette = "Set1") 
    
  })
  
# Generate plot with yearly data for the selected device-------------------------------
   output$yearlyDevicePlot <- renderPlot({
     req(input$device) 
     query <- sprintf("SELECT MeasurementData.Jahr AS Year, 
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge',
                       COUNT(DISTINCT Fallnummer) AS 'Anzahl Fälle'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s'
                  GROUP BY MeasurementData.Jahr
                  ORDER BY MeasurementData.Jahr ASC", input$device)
     data.device.y <- dbGetQuery(db, query)
     
     # Initialize new column
     data.device.y$'Delta Aufträge' <- 0
     setDT(data.device.y)
     
     # calculate the year-over-year percentage change
     if(nrow(data.device.y) >1) {
       data.device.y[, `Delta Aufträge` := ( `Anzahl Aufträge` - shift(`Anzahl Aufträge`) )/shift(`Anzahl Aufträge`)*100]
       
       # identify the last complete year
       lastCompleteYear <-  max(data.device.y$Year[data.device.y$Year < year(today())])
       
       # calculate the year fraction
       yearFraction <- yday(today()) / yday(as.Date(paste(year(today()), "12-31", sep = "-")))
       
       # Identify and adjust counts for the current year
       data.device.y <- data.device.y %>%
         lazy_dt(immutable = FALSE) %>%
         mutate(`Txp Umsatz*` = ifelse(Year == year(today()), `Txp Umsatz` / yearFraction, `Txp Umsatz`),
                `Anzahl Aufträge*` = ifelse(Year == year(today()) , `Anzahl Aufträge` / yearFraction, `Anzahl Aufträge`),
                `Anzahl Fälle*` = ifelse(Year == year(today()), `Anzahl Fälle` / yearFraction, `Anzahl Fälle`)) |> as.data.table()
       
     }
     
     data.range <- range(data.device.y$'Txp Umsatz*', na.rm = TRUE)
     
     # Plot the data
     p <- ggplot(data.device.y, aes(x = Year)) +
       geom_point(aes(y = `Txp Umsatz*`), size = 5, shape = 21,  fill = "red", color = "darkgreen", stroke = 0.8, alpha = 0.8) +
       geom_smooth(aes(y = `Txp Umsatz*`, group = 1),
                   method = 'lm',
                   formula = y ~ x,
                   se = TRUE,
                   color = 'red') +
       # geom_line(aes(y = `Txp Umsatz*`, group = 1), color = 'red') +
       labs(size = 25, x = " ", y = "Txp Umsatz", title = paste(input$device, "Jährlicher Txp-Umsatz" )) +
       # theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
       scale_y_continuous(labels = function(values) fun.labels(values, data.range)) +
       scale_x_continuous(breaks = unique(data.device.y$Year))
    
     
     p
       
   }) 
  
   
# Generate plot with yearly Txp & Count data per method for the selected device-------------------------------
   output$yearlyMethodPlot <- renderPlot({
     req(input$device) 
     # query for the last complete year (lcy)
     query.lcy <- sprintf("SELECT DISTINCT MeasurementData.Jahr AS Years 
                       FROM MeasurementData
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s'", input$device)
     
     lcy <- dbGetQuery(db, query.lcy)
     lcy <- max(lcy$Years[lcy$Years < year(today())]) 
     
     
     query <- sprintf("SELECT MeasurementData.Jahr AS Year, MeasurementData.Bezeichnung AS Analyt,
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s' 
                  AND MeasurementData.Jahr =  '%s' 
                  GROUP BY MeasurementData.Jahr, MeasurementData.Methode
                  ORDER BY MeasurementData.Jahr ASC, MeasurementData.Methode ASC", input$device, lcy)
     
     data.device.a <- dbGetQuery(db, query)
     
     # calculate 'Txp' as percentage of total
     data.device.a$'[%] Txp\n Umsatz' <- data.device.a$'Txp Umsatz' / sum(data.device.a$'Txp Umsatz') * 100
     
     # calculate ranges and factors for axis scaling
     data.range <- range(data.device.a$'Txp Umsatz', na.rm = TRUE)
     scaling.factor <- max(abs(data.range))/max(data.device.a$'Anzahl Aufträge', na.rm = TRUE)
     
     
     # Plot the data
     ggplot(data.device.a, aes(x = Analyt)) +
       geom_col(aes(y = `Txp Umsatz`), fill = "red", color = "darkgreen", alpha = 0.8) +
       geom_point(aes(y = scaling.factor*`Anzahl Aufträge`, size = `[%] Txp\n Umsatz`), 
                  shape = 21, 
                  fill = "lightblue", 
                  color = "navy", 
                  stroke = 0.8, 
                  alpha = 0.8
                  ) + 
       labs(x = " ", y = "Txp Umsatz", title = paste( input$device, ": Umsatz und Produktionszahlen", lcy," pro Analyt ")) +
       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
       scale_y_continuous(labels = function(values) fun.labels(values, data.range), 
                          #label_number(big.mark = "'", decimal.mark = '.'),  
                          sec.axis = sec_axis(~. /scaling.factor, name = "Anzahl Aufträge")) # +
     # scale_x_continuous(breaks = unique(data.device.y$Year))

        }) 
  
  
  # plot the yearly data for the selected method----------------------------------------------------------------
  output$analytePlot <- renderPlot({
    req(input$device) 
    
  query <- sprintf("SELECT MeasurementData.Jahr AS Year, MeasurementData.Bezeichnung AS Analyt,
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s' 
                  GROUP BY MeasurementData.Jahr, MeasurementData.Methode
                  ORDER BY MeasurementData.Jahr ASC, MeasurementData.Methode ASC", input$device)
  
  data.method <- dbGetQuery(db, query)
  if(nrow(data.method) > 1)   {
    
    data.method <- data.method %>%
      group_by(Analyt) %>%
      mutate(
        # Calculate scaling factor for each Analyt
        scaling.factor = max(`Txp Umsatz`, na.rm = TRUE) / max(`Anzahl Aufträge`, na.rm = TRUE)
      )
  }
  
  # facet the data
  ggplot(data.method, aes(x = Year)) +
    # geom_point(aes(y = `Txp Umsatz`, group = Analyt), size = 1, fill = "red", color = "darkred", alpha = 0.8) +
    # geom_line(aes(y = `Txp Umsatz`, group = Analyt), color = "red", linetype = "dashed") +
    geom_point(aes(y = scaling.factor * `Anzahl Aufträge`, group = Analyt),
               shape = 21,
               size = 1,
               fill = "lightblue",
               color = "navy",
               stroke = 0.8,
               alpha = 0.8) +
    geom_line(aes(y = scaling.factor * `Anzahl Aufträge`, group = Analyt), color = "blue", linetype = "dashed") +
    labs(x = " ", y = "Txp Umsatz", title = paste(input$device,"Jährlicher Umsatz pro Analyt")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
    scale_y_continuous(labels = function(values) fun.labels(values, data.range), 
                       #label_number(big.mark = "'", decimal.mark = '.'), 
                       sec.axis = sec_axis(~. /scaling.factor, name = "Anzahl Aufträge")
    ) +
    facet_wrap(~Analyt, scales = "free_y")
  
})

 # Generate table with yearly data for the selected method-----------------------------------------------------
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
    
    data.method <- data.method |>
      mutate(across(.cols = where(is.numeric), .fns = ~ format(., decimal.mark = ".", big.mark = "'")))
    # render the data table
    datatable(data.method, options = list(
                            hover = TRUE, 
                            pageLength = 5, 
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
