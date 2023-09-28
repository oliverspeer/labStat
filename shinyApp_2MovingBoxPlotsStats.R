# Load the required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(memoise)
library(cachem)
library(grid)

# set working directory ----------------------------------------------------
setwd("C:/R_local/labStat")


# import rds data ---------------------------------------------------------

combined.data.kc <- readRDS("Combined_Data_KC.rds")

# shiny app ----------------------------------------------------------------
# user interface -----------------------------------------------------------

shinyOptions(cache = cache_mem(max_size = 5000e6))

ui <- fluidPage(
  
  titlePanel("Analyte Moving Average and Boxplot - Clinical Chemistry ZLM"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("analyte1", "Choose the first Analyte:", 
                  choices = sort(unique(combined.data.kc$Bezeichnung))),
      selectInput("analyte2", "Choose the second Analyte:", 
                  choices = sort(unique(combined.data.kc$Bezeichnung))),
      downloadButton('downloadPDF', 'Download PDF')
    ),
    
    mainPanel(
      plotOutput("boxplots"),
      tableOutput("quartileTable1"),
      tableOutput("quartileTable2"),
      
      uiOutput("dynamicRatioTitel"),
      tableOutput("quartileRatios")
              )
                  )
                )

# server ------------------------------------------------------------

server <- function(input, output, session) {
  session$cache <- cache_mem(max_size = 4000e6)
  
   # Create a function to filter data based on selected analyte
  
    generate_filtered_data <- function(analyte) {
      filtered_data <- combined.data.kc  |> 
        filter(Bezeichnung == analyte) |> 
        arrange(Datum)
      filtered_data$Year <- as.integer(filtered_data$Year)
      return(filtered_data)
                                                  }
  
    
    # filter data reactively after choosing the analyte
    data1 <- reactive({
      generate_filtered_data(input$analyte1)
            })
    data2 <- reactive({generate_filtered_data(input$analyte2)})
    
    
    # Create a function to compute the quartiles
    calculate_quartiles <- function(data, analyte) {
      quartiles <- data  |> 
        group_by(Year) |> 
        summarise(
          `2.5th Quantile` = round(quantile(Werte, 0.025, na.rm = TRUE), 2),
          `25th Quartile` = round(quantile(Werte, 0.25, na.rm = TRUE), 2),
          `50th Quartile (Median)` = round(quantile(Werte, 0.5, na.rm = TRUE), 2),
          `75th Quartile` = round(quantile(Werte, 0.75, na.rm = TRUE), 2),
          `97.5th Quantile` = round(quantile(Werte, 0.975, na.rm = TRUE), 2),
          Count = n()
          
                  )
      quartiles$Analyte <- analyte
      
      # move the column "Analyte" to the first position
      quartiles <- quartiles[,c("Analyte", setdiff(names(quartiles), "Analyte"))]
      return(quartiles)
                                                      }
    
    # Create a reactive expression for the ratio data
    ratio_data <- reactive({
      df1 <- data1()
      df2 <- data2()
      
      common_data <- inner_join(
                                df1, df2, 
                                by = c("a_Tagesnummer", "Year"), 
                                suffix = c(".df1", ".df2"), 
                                multiple = "any"
                                )
      
      common_data$Ratio <- common_data$Werte.df1 / common_data$Werte.df2
      return(common_data)
                            })
    
    # Create a function to compute the quartiles for the ratio data
    calculate_ratio_quartiles <- function(data) {
      quartiles <- data  |> 
        group_by(Year) |> 
        summarise(
          `2.5th Quantile` = round(quantile(Ratio, 0.025, na.rm = TRUE), 2),
          `25th Quartile` = round(quantile(Ratio, 0.25, na.rm = TRUE), 2),
          `50th Quartile (Median)` = round(quantile(Ratio, 0.5, na.rm = TRUE), 2),
          `75th Quartile` = round(quantile(Ratio, 0.75, na.rm = TRUE),2),
          `97.5th Quantile` = round(quantile(Ratio, 0.975, na.rm = TRUE), 2),
          CountQ = n()
          
                  )
      return(quartiles)
                                                  }
    
  #boxplots as output from data 1 & data2, with log10 scale, showing plots in one row  
  output$boxplots <- renderPlot({
    
      plot1 <- ggplot(data1(), aes(x = factor(Year), y = Werte)) +
        geom_boxplot() +
        scale_y_log10() +
        # geom_text(data = Count, aes(x = factor(Year), y = Inf, label = paste("n =", Count)), vjust = 2)
        labs(
          title = paste("Boxplot of", input$analyte1, "by Year"),
          x = "Year",
          y = "Values (log10)"
              ) +
        theme_minimal()
    
      plot2 <- ggplot(data2(), aes(x = factor(Year), y = Werte)) +
        geom_boxplot() +
        scale_y_log10() +
        #geom_text(aes(label = paste("n =", Count())), vjust = 2) +
        labs(
          title = paste("Boxplot of", input$analyte2, "by Year"),
          x = "Year",
          y = "Values (log10)"
              ) +
        theme_minimal()
    
      grid.arrange(plot1, plot2, nrow = 1)
                                })
  
  #creating a table with the quartiles over the years as output
  output$quartileTable1 <- renderTable({
    
    stats1 <- calculate_quartiles(data1(), input$analyte1)
   # stats2 <- calculate_quartiles(data2(), input$analyte2)
    
    
    quartileTable1 <- stats1
    return(quartileTable1)
   
  })
  
  output$quartileTable2 <- renderTable({
    
    
    # stats1 <- calculate_quartiles(data1(), input$analyte1)
    stats2 <- calculate_quartiles(data2(), input$analyte2)
    
    
    
    quartileTable2 <- stats2
    return(quartileTable2)
    
  })
  
  # Create a new output for the ratio quartile table
  output$dynamicRatioTitel <- renderUI({
    paste("ratio", input$analyte1, "/", input$analyte2)
  })
  output$quartileRatios <- renderTable({
    ratio_stats <- calculate_ratio_quartiles(ratio_data())
    return(ratio_stats)
  })
  
  output$downloadPDF <- downloadHandler(
    filename = function() { paste('Analysis-', Sys.Date(), '.pdf', sep='') },
    content = function(file) {
      pdf(file, width = 10, height = 8)
      
      plot1 <- ggplot(data1(), aes(x = factor(Year), y = Werte)) +
        geom_boxplot() +
        scale_y_log10() +
       # geom_text(aes(label = paste("n =", Count)), vjust = 2) +
        labs(
          title = paste("Boxplot of", input$analyte1, "by Year"),
          x = "Year",
          y = "Values (log10)"
        ) +
        theme_minimal()
      
      plot2 <- ggplot(data2(), aes(x = factor(Year), y = Werte)) +
        geom_boxplot() +
        scale_y_log10() +
        labs(
          title = paste("Boxplot of", input$analyte2, "by Year"),
          x = "Year",
          y = "Values (log10)"
        ) +
        theme_minimal()
      
      print(plot1)
      print(plot2)
      
      stats1 <- calculate_quartiles(data1(), input$analyte1)
      stats2 <- calculate_quartiles(data2(), input$analyte2)
      ratio_stats <- calculate_ratio_quartiles(ratio_data())
     
       grid.newpage()
      grid.table(stats1)
      
        grid.newpage()
      grid.table(stats2)
     
       grid.newpage()
       grid.text(paste("RATIO", input$analyte1, "/", input$analyte2), y = 0.8, just = c(0.5, 0.8),
                 gp=gpar(fontsize=12))
      #grid.text("ratio FLK/FLL", just = "top")
      grid.table(ratio_stats)
      
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
