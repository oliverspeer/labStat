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
  "openxlsx2"
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




query <- sprintf("SELECT strftime('%%Y', Datum) as Year, 
                       SUM(Txp) as 'Txp Umsatz', 
                       COUNT (DISTINCT Tagesnummer) as 'Anzahl Aufträge',
                       COUNT (DISTINCT Fallnummer) as 'Anzahl Fälle'
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Bezeichnung = TarifData.Bezeichnung
                      JOIN MethodData ON MeasurementData.Bezeichnung = MethodData.Bezeichnung
                        AND MethodData.Methode = TarifData.Methode
                      WHERE MethodData.Gerät = '%s' 
                      GROUP BY Year", 'AU5800')
data.dvice <- dbGetQuery(db, query)

query1 <- sprintf("SELECT 
    strftime('%%Y', Datum) AS Year, 
    COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge', 
    COUNT(DISTINCT Fallnummer) AS 'Anzahl Fälle',
    SUM(Txp) AS 'Txp Umsatz'
FROM 
    MeasurementData m
JOIN 
    MethodData md ON m.Bezeichnung = md.Bezeichnung
JOIN 
    TarifData t ON m.Bezeichnung = t.Bezeichnung
WHERE 
    md.Gerät = '%s' 
GROUP BY 
    Year
", 'AU5800')
data.dvice1 <- dbGetQuery(db, query1)

query2 <- sprintf(
"SELECT 
    strftime('%%Y', Datum) AS Year, 
    SUM(Txp) AS 'Txp Umsatz',
    COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge', 
    COUNT(DISTINCT Fallnummer) AS 'Anzahl Fälle'
    
FROM 
    MeasurementData a
JOIN 
    TarifData c ON a.Bezeichnung = c.Bezeichnung
JOIN 
    MethodData b ON a.Bezeichnung = b.Bezeichnung

WHERE 
    b.Gerät = '%s' 
GROUP BY 
    Year
", 'AU5800')
data.dvice2 <- dbGetQuery(db, query2)

query3 <- sprintf("SELECT strftime('%%Y', Datum) as Year, 
                       SUM(Txp) as 'Txp Umsatz', 
                       COUNT (DISTINCT Tagesnummer) as 'Anzahl Aufträge',
                       COUNT (DISTINCT Fallnummer) as 'Anzahl Fälle'
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                      JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                        
                      WHERE MethodData.Gerät = '%s' 
                      GROUP BY Year", 'AU5800')
data.dvice3 <- dbGetQuery(db, query3)

query4 <- sprintf("SELECT strftime('%%Y', Datum) as Year, 
                       SUM(Txp) as 'Txp Umsatz', 
                       COUNT (DISTINCT Tagesnummer) as 'Anzahl Aufträge',
                       COUNT (DISTINCT Fallnummer) as 'Anzahl Fälle'
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                      JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                        AND MethodData.Methode = TarifData.Methode
                      WHERE MethodData.Gerät = '%s' 
                      GROUP BY Year", 'DxI')
data.dvice4 <- dbGetQuery(db, query4)

# retrieve MethodData: Bezeichnung, Methode, Gerät----------------------------------
query.met.bez.ger <- "SELECT DISTINCT Bezeichnung, Methode, Gerät 
                      FROM MethodData 
                      ORDER BY Bezeichnung ASC"
data.met.bez.ger <- dbGetQuery(db, query.met.bez.ger)

# save as excel
write_xlsx(data.met.bez.ger, "dataMethGerät.xlsx", row.names = TRUE)



# retrieve MeasurementData: Bezeichnung, Methode----------------------------------
query.met.bez.meas <- "SELECT DISTINCT Bezeichnung, Methode 
                      FROM MeasurementData 
                      ORDER BY Bezeichnung ASC"
data.met.bez.meas <- dbGetQuery(db, query.met.bez.meas)

# save as excel
write_xlsx(data.met.bez.meas, "dataMethBez.xlsx", row.names = TRUE)


# retrieve TarifData: Bezeichnung, Methode----------------------------------
query.met.bez.tar <- "SELECT DISTINCT Bezeichnung, Methode 
                      FROM TarifData 
                      ORDER BY Bezeichnung ASC"
data.met.bez.tar <- dbGetQuery(db, query.met.bez.tar)
# save as excel
write.csv2(data.met.bez.tar, "dataTarifBez.csv", row.names = TRUE)


# remove all rows from data.met.bez.tar that are present in data.met.bez.meas by Methode
data.met.bez.tar.diff <- data.met.bez.tar[!data.met.bez.tar$Methode %in% data.met.bez.meas$Methode,]
# save as excel
write.csv2(data.met.bez.tar.diff, "dataTarifBezDiff.csv", row.names = TRUE)



query5 <- sprintf("SELECT DISTINCT MeasurementData.Methode, MeasurementData.Bezeichnung 
                      FROM MeasurementData
                        JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                        WHERE MethodData.Gerät = '%s' ", 'AU5800')
data.dvice5 <- dbGetQuery(con, query5)
write.csv2(data.dvice5, "AUQ1_24.csv", row.names = TRUE)

# query for quarter--------------------------------------------------------------
query <- sprintf("SELECT MeasurementData.Jahr AS Year, 
                       Quartal AS Quarter, 
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge',
                       COUNT(DISTINCT Fallnummer) AS 'Anzahl Fälle'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s' 
                  GROUP BY MeasurementData.Jahr, Quartal
                  ORDER BY MeasurementData.Jahr ASC, Quartal ASC", "HPLC")
data.device.Q <- dbGetQuery(db, query)

# Initialize new column
data.device.Q$'Delta Aufträge' <- 0
setDT(data.device.Q)

# calculate the year-over-year percentage change
if(nrow(data.device.Q) >1) {
  data.device.Q[, `Delta Aufträge` := (shift(`Anzahl Aufträge`) -`Anzahl Aufträge`)/shift(`Anzahl Aufträge`)*100]
  data.device.Q[, YearQuarter := paste(Year, Quarter, sep = "-Q")]
}



# # Convert the data to a data frame and calculate the year-over-year percentage change
# if(nrow(data.device.Q) > 1)   {
#   data.device.Q$`Delta Aufträge`[-1] <- diff(data.device.Q$`Anzahl Aufträge`)/head(data.device.Q$`Anzahl Aufträge`, -1)
#   # data.device$`Delta Aufträge [%]` <- round(data.device$`Delta Aufträge [%]` * 100, 2)
# 
#   data.device.Q$YearQuarter <- with(data.device.Q, paste(Year, Quarter, sep = "-Q"))
#   data.device.Q$YearQuarter <- factor(data.device.Q$YearQuarter, levels = unique(data.device.Q$YearQuarter))
# 
# }

#   # Plot the data
ggplot(data.device.Q, aes(x = YearQuarter)) +
  # geom_line(aes(y = `Txp Umsatz`, group = 1), color = 'blue') +
  geom_point(aes(y = `Txp Umsatz`, group = 1), color = 'red') +
  geom_smooth(aes(y = `Txp Umsatz`, group = 1), method = 'loess', span = 0.2, se = TRUE, color = 'red') +
  # geom_line(aes(y = `Delta Aufträge`, group = 1), color = 'red') +
  # scale_y_continuous(sec.axis = sec_axis(~ . *1000000, name = "Delta Aufträge [%]", labels = percent_format())) +
  labs(x = "Jahr", y = "Txp Umsatz", title = "jährlicher Txp Umsatz") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
  scale_y_continuous(labels = label_number(big.mark = "'", decimal.mark = '.'))


# query for year--------------------------------------------------------------
query.lcy <- sprintf("SELECT MAX(MeasurementData.Jahr) AS lcYear 
                       FROM MeasurementData
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s'", "DxI")
lcy <- dbGetQuery(db, query.lcy)
(lcy <- lcy$lcYear)

query <- sprintf("SELECT MeasurementData.Jahr AS Year, 
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge',
                       COUNT(DISTINCT Fallnummer) AS 'Anzahl Fälle'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s' 
                  GROUP BY MeasurementData.Jahr
                  ORDER BY MeasurementData.Jahr ASC", "HPLC")
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


   # Plot the data
ggplot(data.device.y, aes(x = Year)) +
  geom_point(aes(y = `Txp Umsatz*`, group = 1), color = 'red') +
  geom_smooth(aes(y = `Txp Umsatz*`, group = 1), 
              method = 'loess',
              formula = y ~ x,
              se = TRUE, 
              color = 'red') +
  labs(x = "Jahr", y = "Txp Umsatz", title = paste("Jährlicher Umsatz pro Gerät/AP: ", "HPLC")) +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
  scale_y_continuous(labels = label_number(big.mark = "'", decimal.mark = '.')) +
  scale_x_continuous(breaks = unique(data.device.y$Year))


# query for year and analyte --------------------------------------------------------------
fun.labels <- function(values, data.range = NULL) {
  if(is.null(data.range)) {
    data.range <- range(values, na.rm = TRUE)
  }
  max.value <- max(abs(data.range))

  if (max.value >= 1e6) {
  return(number(values / 1e6, accuracy = 1, suffix = " M"))
  } else if (max.value >= 1e3) {
  return(number(values / 1e3, accuracy = 1, suffix = " k"))
  } else {
  return(number(values))
  }
}

query.lcy <- sprintf("SELECT DISTINCT MeasurementData.Jahr AS Years 
                       FROM MeasurementData
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s'", "HPLC")
(lcy <- dbGetQuery(db, query.lcy))
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
                  ORDER BY MeasurementData.Jahr ASC, MeasurementData.Methode ASC", "HPLC", lcy)

data.device.a <- dbGetQuery(db, query)

# calculate 'Txp' as percentage of total
data.device.a$'Txp [%]' <- data.device.a$'Txp Umsatz' / sum(data.device.a$'Txp Umsatz') * 100

data.range <- range(data.device.a$'Txp Umsatz', na.rm = TRUE)
scaling.factor <- max(abs(data.range))/max(data.device.a$'Anzahl Aufträge', na.rm = TRUE)

# Plot the data
ggplot(data.device.a, aes(x = Analyt)) +
  geom_col(aes(y = `Txp Umsatz`), fill = "red", color = "darkred", alpha = 0.8) +
  geom_point(aes(y = scaling.factor*`Anzahl Aufträge`, size = `Txp [%]`), 
             shape = 21, 
             fill = "lightblue", 
             color = "navy", 
             stroke = 0.8, 
             alpha = 0.8) + 
  # scale_color_manual(values = c("red", "blue"), breaks = c("Txp Umsatz", "Anzahl Aufträge")) +
  labs(x = " ", y = "Txp Umsatz", title = paste("Jährlicher Umsatz pro Analyt: ", "DxI")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
  scale_y_continuous(labels = function(values) fun.labels(values, data.range), 
                     #label_number(big.mark = "'", decimal.mark = '.'), 
                     sec.axis = sec_axis(~. /scaling.factor, name = "Anzahl Aufträge")) # +
  # scale_x_continuous(breaks = unique(data.device.y$Year))


# plot yearly method data --------------------------------------------------------------
query <- sprintf("SELECT strftime('%%Y', Datum) as Year, 
                       SUM(Txp) as 'Txp Umsatz', 
                       COUNT (DISTINCT Tagesnummer) as 'Anzahl Aufträge',
                       COUNT (DISTINCT Fallnummer) as 'Anzahl Fälle'
                      FROM MeasurementData
                      JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                      WHERE MeasurementData.Bezeichnung = '%s' 
                      GROUP BY Year
                      ORDER BY Year ASC", "Vitamin A")

data.method <- dbGetQuery(db, query)

# initialize a new column
data.method$'Delta Aufträge' <- NA

# Convert the data to a data frame and calculate the year-over-year percentage change
if(nrow(data.method) > 1)   {
  data.method$`Delta Aufträge`[-1] <- percent(diff(data.method$`Anzahl Aufträge`)/head(data.method$`Anzahl Aufträge`, -1))
                              }

  data.range <- range(data.method$'Txp Umsatz', na.rm = TRUE)
  scaling.factor <- max(abs(data.range))/max(data.method$'Anzahl Aufträge', na.rm = TRUE)

# Plot the data
ggplot(data.method, aes(x = Year)) +
  geom_point(aes(y = `Txp Umsatz`), size = 1, fill = "red", color = "darkred", alpha = 0.8) +
  geom_line(aes(y = `Txp Umsatz`, group = 1), color = "red", linetype = "dashed") +
  geom_point(aes(y = scaling.factor*`Anzahl Aufträge`), 
             shape = 21,
             size = 1,
             fill = "lightblue", 
             color = "navy", 
             stroke = 0.8, 
             alpha = 0.8) + 
  geom_line(aes(y = scaling.factor*`Anzahl Aufträge`, group = 2), color = "blue", linetype = "dashed") +
  labs(x = " ", y = "Txp Umsatz", title = paste("Jährlicher Umsatz pro Analyt: ", "*")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
  scale_y_continuous(labels = function(values) fun.labels(values, data.range), 
                     #label_number(big.mark = "'", decimal.mark = '.'), 
                     sec.axis = sec_axis(~. /scaling.factor, name = "Anzahl Aufträge"))

# all methods per device over years ---------------------------------------------------
query <- sprintf("SELECT MeasurementData.Jahr AS Year, MeasurementData.Bezeichnung AS Analyt,
                       SUM(Txp) AS 'Txp Umsatz', 
                       COUNT(DISTINCT Tagesnummer) AS 'Anzahl Aufträge'
                  FROM MeasurementData
                  JOIN TarifData ON MeasurementData.Methode = TarifData.Methode
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s' 
                  GROUP BY MeasurementData.Jahr, MeasurementData.Methode
                  ORDER BY MeasurementData.Jahr ASC, MeasurementData.Methode ASC", "EP_IFE")

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
  labs(x = " ", y = "Txp Umsatz", title = paste("Jährlicher Umsatz pro Analyt: ", "*")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))  +
  scale_y_continuous(labels = function(values) fun.labels(values, data.range), 
                     #label_number(big.mark = "'", decimal.mark = '.'), 
                    sec.axis = sec_axis(~. /scaling.factor, name = "Anzahl Aufträge")
                     ) +
  facet_wrap(~Analyt, scales = "free_y")

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

data.range <- range(data.all$'Txp Umsatz', na.rm = TRUE)


# ggplot stacked bar chart
ggplot(data.all, aes(x = Year, y = `Txp Umsatz`, fill = Device)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = " ", y = "Txp Umsatz", title = "Jährlicher Umsatz pro Gerät") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(values) fun.labels(values, data.range))

ggplot(data.all, aes(x = Year, y = `Txp Umsatz`, fill = Device)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = " ", y = "relativer Txp Umsatz", title = "Jährlicher Umsatz pro Gerät") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) +
  scale_fill_brewer(palette = "Set3") 



# ---------------------------------------------------------------------------------
query <- sprintf("SELECT DISTINCT Jahr
                       FROM MeasurementData
                  JOIN MethodData ON MeasurementData.Methode = MethodData.Methode
                  WHERE MethodData.Gerät = '%s'", "Nephelometrie")
(lcy <- dbGetQuery(db, query))
