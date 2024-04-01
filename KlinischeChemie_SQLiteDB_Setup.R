# preparing libraries
# library(data.table)
# library(tidyverse)
# library(readxl)
# library(nephro)
# library(DBI)
# library(RSQLite)
# library(openxlsx)

# Vector of library names
libraries <- c(
               "data.table", 
               "tidyverse", 
               "readxl", 
               "nephro", 
               "DBI", 
               "RSQLite", 
               "openxlsx2", 
               "rstudioapi"
               )


# Loop through the vector, check if each library is installed, and load it
for(lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}




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


# setwd("C:/R_local/labStat")
# setwd(getwd())


# connect to database ------------------------------------------------------
# Function to detect the operating system 
# and return the corresponding database path

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
con <- dbConnect(SQLite(), dbname = db.wd)



# reading Customer data from excel file ---------------------------------------------
DT.customer <- read_excel("ZLM-Auftraggeber-20231110.xlsx")

# creating a data table
DT.customer <- data.table(DT.customer)

# rename column VAXKUERZEL to KundenID
setnames(DT.customer, "VAXKUERZEL", "KundenID")

# import into SQLite db
# Create a new SQLite database / open connection to the database
con <- dbConnect( SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )
dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")

# create new tables in the database
dbExecute(con, "
CREATE TABLE CustomerData (
           KundenID TEXT PRIMARY KEY,
           INSTITUTION	TEXT,
           NUMMER	INTEGER,
           ADRESSGRUPPE	TEXT,
           ZEILE6	TEXT,
           ZEILE5	TEXT,
           ZEILE1	TEXT,
           ZEILE2	TEXT,
           ZEILE3	TEXT,
           ZEILE4	TEXT,
           TELEFON	TEXT,
           TELEFAX	TEXT
) ")

# Insert data from DT.tidy.AU into the measurement.data table in the SQLite database
dbWriteTable(con, "CustomerData", DT.customer, append = TRUE, row.names = FALSE)

(dbGetQuery(con, "SELECT * FROM CustomerData LIMIT 5"))

# for deletion of a table
# dbExecute(con, "DROP TABLE IF EXISTS MeasurementData")

# close the connection
# dbDisconnect(con)

# import tariff data ------------------------------------------------------

# import multiple csv files into a single data.table
DT.tarifZLM <- rbindlist(lapply(list.files(pattern = "UmsatzStatistikQ.*.csv"), fread))

# DT.tarifZLM <- read_excel("UmsatzStatistik.xlsx")
DT.tarifZLM <- data.table(DT.tarifZLM)

DT.tarifZLM <- unique(DT.tarifZLM, by = c("Methode", "Taxpkt."))
 
setnames(DT.tarifZLM ,c("Taxpkt.", "Tarif"), c("Txp", "EAL_Position"))

DT.tarifZLM <- DT.tarifZLM[
  !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
][, Datum := ymd(substr(Tagesnummer, 1, 10))
][, Jahr := year(Datum)]

 for (col in c("Tagesnummer", "R.stelle", "z.Gunsten", "Kunde", "Preis", "Datum")) {
   set(DT.tarifZLM, j = col, value = NULL)
 } |> setDT()


# import into SQLite db
# Create a new SQLite database / open connection to the database
con <- dbConnect(SQLite(), 
                 dbname = paste0(getActiveProject(),
                                 "/ClinicalChemistry_test.db"))

# create new tables in the database
dbExecute(con, "
CREATE TABLE TarifData (
           TarifID	INTEGER	PRIMARY KEY AUTOINCREMENT,
           Methode	INTEGER,
           Bezeichnung	TEXT,
           Jahr	INTEGER,
           EAL_Position	NUMERIC,
           Txp	NUMERIC,
           FOREIGN KEY(Methode) REFERENCES MethodData(Methode) ON UPDATE CASCADE,
           FOREIGN KEY(EAL_Position) REFERENCES EALData(EAL_Position) ON UPDATE CASCADE
          ) ")

# Insert data from TarifData into the measurement.data table in the SQLite database
dbWriteTable(con, "TarifData", DT.tarifZLM, append = TRUE, row.names = FALSE)

(dbGetQuery(con, "SELECT * FROM TarifData LIMIT 5"))

# import eAL data
DT.tarif <- read_excel("EAL.xlsx")
setDT(DT.tarif)


# import into SQLite db
# Create a new SQLite database / open connection to the database
con <- dbConnect(SQLite(), dbname = paste0(getActiveProject(),"/ClinicalChemistry_test.db"))

# create new tables in the database
dbExecute(con, "
CREATE TABLE EALData (
           EAL_Position	REAL	PRIMARY KEY,
           Txp	REAL,
           EAL_Bezeichnung TEXT,
           Chemie TEXT,
           Hämatologie	TEXT,
           Immunologie	TEXT,
           Genetik	TEXT, 
           Mikrobiologie	TEXT,
           Spezialanalyse	TEXT,
           Basisanalyse	TEXT

) ")

# Insert data from TarifData into the measurement.data table in the SQLite database
dbWriteTable(con, "EALData", DT.tarif, append = TRUE, row.names = FALSE)

(dbGetQuery(con, "SELECT * FROM EALData LIMIT 5"))
# dbExecute(con, "ALTER TABLE TarifData RENAME TO EALData")
# dbExecute(con, "DROP TABLE IF EXISTS TarifData")

# import method data ------------------------------------------------------

DT.method <- read_excel("MethodenKatalogKC.xlsx")
DT.method <- data.table(DT.method)



# import units and reference intervals ----------------------------------------
DT.unitsRI <- fread("Einheiten&RefIntrvle.csv")

setnames(DT.unitsRI, c("NUMMER", 
                       "NAME", 
                       "REF_UNTEN M", 
                       "REF_OBEN M", 
                       "REF_UNTEN W", 
                       "REF_OBEN W"), 
                     c("Methode", 
                       "Bezeichnung",
                       "REF_L_M",
                       "REF_H_M",
                       "REF_L_W",
                       "REF_H_W"
                       ))

DT.unitsRI <- left_join(DT.unitsRI, DT.method, by = "Methode")

#delete column Bezeichnung.y
DT.unitsRI <- DT.unitsRI[ , !"Bezeichnung.y", with = F]
setnames(DT.unitsRI, "Bezeichnung.x", "Bezeichnung")

# import into SQLite db
# Create a new SQLite database / open connection to the database
con <- dbConnect(SQLite(), dbname = paste0(getwd(),"/ClinicalChemistry_test.db"))

# create new tables in the database
dbExecute(con, "
CREATE TABLE MethodData (
           Methode	INTEGER	PRIMARY KEY,
           CODE	TEXT,
           Bezeichnung TEXT,
           EINHEIT TEXT,
           REF_L_M	NUMERIC,
           REF_H_M	NUMERIC,
           REF_L_W	NUMERIC,
           REF_H_W	NUMERIC,
           ABTEILUNG	TEXT,
           Gerät	TEXT
) ")

# Insert data from TarifData into the measurement.data table in the SQLite database
dbWriteTable(con, "MethodData", DT.unitsRI, append = TRUE, row.names = FALSE)
# dbWriteTable(con, "MethodData", DT.unitsRI, append = FALSE, row.names = FALSE, overwrite = TRUE)

(dbGetQuery(con, "SELECT * FROM MethodData LIMIT 5"))

# dbExecute(con, "DROP TABLE IF EXISTS MethodData")



# DT.unitsRI <- left_join(DT.unitsRI, DT.tarifZLM, by = "Methode")

# import reagent costs ----------------------------------------------------

DT.reag.costs <- read_excel("Bestellliste.xlsx")
DT.reag.costs <- data.table(DT.reag.costs)
for (col in c("Name2", "PRZ FERTIG", "Statustext", "Bestellmenge", "Offene Liefermenge", "Lager")) {
  set(DT.reag.costs, j = col, value = NULL)
}

# read excel from file for bookings and salary ----------------------------------
DT.bookings  <- read_excel("Kopie von 2022 Q1-2023 Q1 KST S0210 S0212  S0410.xlsx")
DT.bookings  <- data.table(DT.bookings)
DT.salary <- read_excel("Lohn_os_KCH.xlsx")
DT.salary <- data.table(DT.salary)

# define functions --------------------------------------------------------
# function to read multiple files ---------------------
fun.read.multi.excel.data <- function(file.pattern, dt.name) {
  # project_directory <- rstudioapi::getActiveProject()
  data.path <- getActiveProject()
  files <- list.files(data.path, pattern = file.pattern)
  all.data <- list()
  
  # Loop through all files
  for (file.name in files) {
    full.path <- file.path(data.path, file.name)
    
    # Read the header
    head <- read_xlsx(full.path, rows = 1:2, colNames = FALSE)
    
    
    # Detect NA cells in the second row  , replace them with letters 
    head[2, which(is.na(head[2, ]))] <- as.list(letters[1:length(which(is.na(head[2, ])))])
    
    # Combine the first and second row to create the column names
    head <- apply(head, 2, function(x) paste(rev(x), collapse = "_"))
    
    
    # Read the data while skipping the first two rows and using the combined column names
    dt <- read_xlsx(full.path, start_row = 2)
    colnames(dt) <- head
    
    
    
    # Removing "a_", "b_", ..., "z_" from the column names
    colnames(dt) <- gsub("^[a-z]_", "", colnames(dt))
    
    # Ensure the "Probennummer" column exists even if it was not in the source file,
    # filling with NA for rows if it was added manually
    if (!"Probennummer" %in% colnames(dt)) {
      dt <- mutate(dt, Probennummer = NA)
    }
    
    # Convert the data.frame to a data.table
    setDT(dt)  
    
    
    # Remove specific columns to anonymize the data
    dt[, c("Name", "Vorname") := NULL]
    
    # Define and exclude certain columns from conversion to numeric
    exclude.cols <- c("Tagesnummer", "Geb.datum", "Geschl.", "Auftragg.")
    include.cols <- setdiff(names(dt), exclude.cols)
    
    
    # Convert included columns to numeric
    for (col in include.cols) {
      dt[, (col) := as.numeric(get(col))]
    }
    
    dt[, Geb.datum := ymd(Geb.datum)]
    
    # Store the processed data table in a list
    all.data[[file.name]] <- dt
  }
  
  # Combine all processed data.tables
  dt.name <- rbindlist(all.data, use.names = TRUE, fill = TRUE)
  
  # new column `sex` with 0 if Dt.wide.pth$f_Geschl. == "F" and 1 if Dt.wide.pth$f_Geschl. == "M"
  dt.name$sex <- ifelse(dt.name$Geschl. == "F", 0, 1)
  
  # calculate the age from dt.name$Datum and dt.name$e_Geb.datum
  dt.name <- dt.name[
    !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
  ][, Datum := ymd(substr(Tagesnummer, 1, 10))
  ]
  
  dt.name[, Datum := ymd(Datum)
  ][, Geb.datum := ymd(Geb.datum)
  ][, Alter := round(interval(start = Geb.datum, end = Datum) / years(1), 2)] 
  
  
  
  
  return(dt.name)
}


# function to tidy up data --------------------------------------
fun.write.tidy.data<- function(data, dt_name) {

 # prepare  id.cols that are not to be melted
 id.cols <- names(data)[!grepl("_\\d+", names(data))]

 # Melt the data.table
 DT.m1 = melt(
  data,
  id.vars = id.cols,
  variable.name = "Bezeichnung_Methode",
  value.name = "Werte",
  na.rm = TRUE
 )

 # Split 'Bezeichnung_Methode' into two columns 'Bezeichnung' and 'Methode'
 DT.m1[, c("Bezeichnung", "Methode") := tstrsplit(Bezeichnung_Methode, "_", fixed = TRUE)
      ][, Bezeichnung_Methode := NULL][, Methode := as.numeric(Methode)]

 dt_name <- setDT(DT.m1)


# change the column names to be SQL compatible
 setnames(dt_name, old = c("Geb.datum", "Geschl.", "Auftragg."), new = c("DOB", "Geschlecht", "KundenID"))

# change DOB and Datum to numeric
# dt_name[, DOB := as.numeric(DOB)]
# dt_name[, Datum := as.numeric(Datum)]

# extract year, quarter, month, week, day from Datum
 dt_name[, Jahr := year(Datum)
        ][, Quartal := quarter(Datum)
          ][, Monat := month(Datum)
            ][, Woche := week(Datum)
              ][, Tag := day(Datum)
                ][, DOB := as.character(DOB)
                  ][, Datum := as.character(Datum)
                    ]



return(dt_name)
}




# AU data (read excel, tidy up data)  -------------------------------------
AU.data <- fun.read.multi.excel.data("BlutAU", "AU.data")

# eGFR calculation
creatinin <- AU.data$Creatinin_14581 / 88.42
sex <- AU.data$sex
age <- AU.data$Alter

library(nephro)

AU.data$`GFR(CKD-EPI)_22125` <- round(CKDEpi_RF.creat(creatinin, sex, age), 2)

# tidy up the data
DT.tidy.AU <- fun.write.tidy.data(AU.data, 
                                  #DT.tarif, 
                                  "DT.tidy.AU")

# Create a new SQLite database / open connection to the database
con <- dbConnect(SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )

# create new tables in the database
dbExecute(con, "
CREATE TABLE IF NOT EXISTS MeasurementData (
          Tagesnummer TEXT,
          Probennummer INTEGER DEFAULT NULL,
          Fallnummer INTEGER,
          DOB DATE,
          Geschlecht TEXT,
          KundenID TEXT,
          sex INTEGER,
          Datum DATE,
          Jahr INTEGER,
          Quartal INTEGER,
          Monat INTEGER,
          Woche INTEGER,
          Tag INTEGER,
          \"Alter\" NUMERIC,
          Werte REAL,
          Bezeichnung TEXT, 
          Methode INTEGER,
          WerteID INTEGER PRIMARY KEY AUTOINCREMENT,
          FOREIGN KEY(Methode) REFERENCES MethodData(Methode) ON UPDATE CASCADE,
          FOREIGN KEY(KundenID) REFERENCES CustomerData(KundenID) ON UPDATE CASCADE
          
) ")


# inserted a new column into the table
# dbExecute(con, "ALTER TABLE MeasurementData ADD COLUMN Probennummer INTEGER DEFAULT NULL")

# Insert data from DT.tidy.AU into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.AU, append = TRUE, row.names = FALSE)

# controlle if the data was inserted
(dbGetQuery(con, "PRAGMA table_info(MeasurementData)"))
(dbGetQuery(con, "SELECT * FROM MeasurementData LIMIT 5"))

# for deletion of a table
# dbExecute(con, "DROP TABLE IF EXISTS MeasurementData")

# close the connection
dbDisconnect(con)

# DxI data (read excel, tidy up data)  -------------------------------------
dxi.data <- fun.read.multi.excel.data("BlutDxI", "dxi.data")
DT.tidy.dxi <- fun.write.tidy.data(dxi.data, 
                                   #DT.tarif, 
                                   "DT.tidy.dxi")

# Create a new SQLite database / open connection to the database
con <- dbConnect(SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )

# Insert data from DT.tidy.dxi into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.dxi, append = TRUE, row.names = FALSE)

# BNII Data (read excel, tidy up data) ------------------------------------
# bn.data <- fun.read.excel.data("BNII", "bn.data")

bn.data <- fun.read.multi.excel.data("BNII", "bn.data")
DT.tidy.bn <- fun.write.tidy.data(bn.data, 
                                  #DT.tarif, 
                                  "DT.tidy.bn")

# Insert data from DT.tidy.AU into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.bn, append = TRUE, row.names = FALSE)

# EP & IFE data (read excel, tidy up data) --------------------------------------------------
# Define the path and file name
file.pattern <- "EPIFE"
data.path <- getActiveProject()
files <- list.files(data.path, pattern = file.pattern)
all.data <- list()

for (file.name in files) {
  full.path <- file.path(data.path, file.name)
  
  # Read the header
  head <- read_xlsx(full.path, rows = 1:2, colNames = FALSE)
  
  # Detect NA cells in the second row  , replace them with letters 
  head[2, which(is.na(head[2, ]))] <- as.list(letters[1:length(which(is.na(head[2, ])))])
  
  # Combine the first and second row to create the column names
  head <- apply(head, 2, function(x) paste(rev(x), collapse = "_"))
  
  
  
  # Read the data while skipping the first two rows and using the combined column names
  dt <- read_xlsx(full.path, start_row = 2)
  colnames(dt) <- head
  
  # Removing "a_", "b_", ..., "z_" from the column names
  colnames(dt) <- gsub("^[a-z]_", "", colnames(dt))
  
  # Ensure the "Probennummer" column exists even if it was not in the source file,
  # filling with NA for rows if it was added manually
  if (!"Probennummer" %in% colnames(dt)) {
    dt <- mutate(dt, Probennummer = NA)
  }
  
  # Convert the data.frame to a data.table
  setDT(dt) 
  
  # Remove specific columns
  dt[, c("Name", "Vorname") := NULL]
  
 
  
  # Define the new order of columns
   new.order <- names(dt)
   new.order <- c(new.order[1:6], 
                  " M-Protein (densitometrisch)_37158", 
                  new.order[8:which(new.order == " M-Protein (densitometrisch)_37158") - 1])
   
   # Rearrange the columns
   setcolorder(dt, new.order)
  
  
  # Define a function to change strings to 1
  apply_logic_to_columns <- function(dt) {
    # Get the column names except the first five
    cols_to_modify <- names(dt)[8:ncol(dt)]
    
    # Loop through the columns and apply the logic
    for (col in cols_to_modify) {
      set(dt, j = col, value = ifelse(!grepl("SISTIERT", dt[[col]], fixed = TRUE) & !is.na(dt[[col]]), 1, NA_integer_))
    }
  }
  
  # Apply the function to your data.table
  apply_logic_to_columns(dt)
  
  
  # Define and exclude certain columns from conversion to numeric
  exclude.cols <- c("Tagesnummer", "Geb.datum", "Geschl.", "Auftragg.")
  include.cols <- setdiff(names(dt), exclude.cols)
  
  # Convert included columns to numeric
  for (col in include.cols) {
    dt[, (col) := as.numeric(get(col))]
  }
  
  # set Geb.datum to date format
  dt[, Geb.datum := ymd(Geb.datum)]
  
 
  
  # Store the processed data table in a list
  all.data[[file.name]] <- dt
}

# Combine all processed data.tables
ep.data <- rbindlist(all.data, use.names = TRUE, fill = TRUE)

# new column `sex` with 0 if Dt.wide.pth$f_Geschl. == "F" and 1 if Dt.wide.pth$f_Geschl. == "M"
ep.data$sex <- ifelse(ep.data$Geschl. == "F", 0, 1)

# calculate the age from dt.name$Datum and dt.name$e_Geb.datum
ep.data <- ep.data[
  !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
][, Datum := ymd(substr(Tagesnummer, 1, 10))
]

ep.data[, Datum := ymd(Datum)
][, Geb.datum := ymd(Geb.datum)
][, Alter := round(interval(start = Geb.datum, end = Datum) / years(1), 2)]



DT.tidy.ep <- fun.write.tidy.data(ep.data, 
                                  # DT.tarif, 
                                  "DT.tidy.ep")

# Connect to the SQLite database
con <- dbConnect( SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )

# Insert data from DT.tidy.AU into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.ep, append = TRUE, row.names = FALSE)

# HPLC Data (read excel, tidy up data) ------------------------------------
hplc.data <- fun.read.multi.excel.data("HPLC", "hplc.data")
DT.tidy.hplc <- fun.write.tidy.data(hplc.data, 
                                    # DT.tarif, 
                                    "DT.tidy.hplc")

# Insert data from DT.tidy.AU into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.hplc, append = TRUE, row.names = FALSE)


# LcMSMS Data (read excel, tidy up data) ------------------------------------
lcms.data <- fun.read.multi.excel.data("MSMS", "lcms.data")
DT.tidy.lcms <- fun.write.tidy.data(lcms.data, 
                                    # DT.tarif, 
                                    "DT.tidy.lcms")

# Insert data from DT.tidy.AU into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.lcms, append = TRUE, row.names = FALSE)

# VH4 Data (read excel, tidy up data) ------------------------------------
# Define the path and file name
file.pattern <- "VH4"
data.path <- getActiveProject()
files <- list.files(data.path, pattern = file.pattern)
all.data <- list()

for (file.name in files) {
  full.path <- file.path(data.path, file.name)
  
  # Read the header
  head <- read_xlsx(full.path, rows = 1:2, colNames = FALSE)
  
  # Detect NA cells in the second row  , replace them with letters 
  head[2, which(is.na(head[2, ]))] <- as.list(letters[1:length(which(is.na(head[2, ])))])
  
  # Combine the first and second row to create the column names
  head <- apply(head, 2, function(x) paste(rev(x), collapse = "_"))
  
  
  
  # Read the data while skipping the first two rows and using the combined column names
  dt <- read_xlsx(full.path, start_row = 2)
  colnames(dt) <- head
  
  
  # Removing "a_", "b_", ..., "z_" from the column names
  colnames(dt) <- gsub("^[a-z]_", "", colnames(dt))
  
  # Ensure the "Probennummer" column exists even if it was not in the source file,
  # filling with NA for rows if it was added manually
  if (!"Probennummer" %in% colnames(dt)) {
    dt <- mutate(dt, Probennummer = NA)
  }
  
  setDT(dt)
  
  # Remove specific columns
  dt[, c("Name", "Vorname") := NULL]
  
  
  chr.col <- c("IgG-oligoklonale Banden_25329", 
               "Oligoklonale IgG Banden_11475", 
               "Oligoklonale IgG Banden_102", 
               "IEF (Oligoklonale Banden)_21618")
  # Loop through each column name in chr.col and convert to character
  for (col in chr.col) {
    dt[, (col) := as.character(get(col))]
  }
  
  
  # Define a function to change strings to 1
  apply_logic_to_columns <- function(dt) {
    # Get the column names except the first five
    cols_to_modify <- names(dt)[c(18:21, 26)]
    
    # Loop through the columns and apply the logic
    for (col in cols_to_modify) {
      set(dt, j = col, value = ifelse(!grepl("SISTIERT", dt[[col]], fixed = TRUE) & !is.na(dt[[col]]), 1, NA_integer_))
    }
  }
  
  # Apply the function to your data.table
  apply_logic_to_columns(dt)
  
  
  # Define and exclude certain columns from conversion to numeric
  exclude.cols <- c("Tagesnummer", "Geb.datum", "Geschl.", "Auftragg.")
  include.cols <- setdiff(names(dt), exclude.cols)
  
  # Convert included columns to numeric
  for (col in include.cols) {
    dt[, (col) := as.numeric(get(col))]
  }
  
  dt[, Geb.datum := ymd(Geb.datum)]
  
  # Store the processed data table in a list
  all.data[[file.name]] <- dt
}

# Combine all processed data.tables
vh.data <- rbindlist(all.data, use.names = TRUE, fill = TRUE)

# new column `sex` with 0 if Dt.wide.pth$f_Geschl. == "F" and 1 if Dt.wide.pth$f_Geschl. == "M"
vh.data$sex <- ifelse(vh.data$Geschl. == "F", 0, 1)

# calculate the age from dt.name$Datum and dt.name$e_Geb.datum
vh.data <- vh.data[
  !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
                  ][, Datum := ymd(substr(Tagesnummer, 1, 10))
                   ]

vh.data[, Datum := ymd(Datum)
       ][, Geb.datum := ymd(Geb.datum)
        ][, Alter := round(interval(start = Geb.datum, end = Datum) / years(1), 2)]


DT.tidy.vh <- fun.write.tidy.data(vh.data, 
                                  # DT.tarif, 
                                  "DT.tidy.vh")

# Insert data from DT.tidy.vh into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.vh, append = TRUE, row.names = FALSE)



# Testing the db----------------------------------------------------------------
con <- dbConnect( SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )
dbListTables(con)
(dbGetQuery(con, "PRAGMA table_info(MeasurementData)"))
(dbGetQuery(con, "SELECT * FROM MeasurementData LIMIT 5"))
query.result <- dbGetQuery(
  con, 
  "SELECT COUNT(*) AS count FROM MeasurementData WHERE Bezeichnung = 'Aripiprazol'"
  )
print(query.result)

(DT.tidy.lcms[Bezeichnung == "Aripiprazol", .N])

# check and document the min_max_dates into ERD.xlsx----------------------------
# query presence of device data
# and query for time stamps in device data
con <- dbConnect( SQLite(), dbname = paste0( getActiveProject(), "/ClinicalChemistry_test.db") )
query <- "SELECT m.Gerät, 
          MIN(a.Datum) AS MinDate, 
          MAX(a.Datum) AS MaxDate 
          FROM MeasurementData a JOIN MethodData m 
          ON a.Methode = m.Methode GROUP BY m.Gerät;"

min_max_dates <- dbGetQuery(con, query)
min_max_dates$MinDate <- as.Date(min_max_dates$MinDate)
min_max_dates$MaxDate <- as.Date(min_max_dates$MaxDate)


# write the result into a specific sheet of a specific xlsx file
library(openxlsx)


filepath <- paste0( getActiveProject(), "/ERD.xlsx")

# Load the Excel file)
wb <- loadWorkbook(filepath)

# Specify the sheet name where you want to write the data. 
# If the sheet does not exist, it will be created.
sheet_name <- "Tabelle3"

# Write query execution date & time starting at a specific cell
writeData(wb, sheet = sheet_name, x = paste("query time:", Sys.time()), startRow = 23, startCol = 2 )

# Write min_max_dates starting at a specific cell
writeData(wb, sheet = sheet_name, min_max_dates, startRow = 26, startCol = 2, colNames = TRUE )

# Save the workbook (this overwrites the existing Excel file with the new data added)
saveWorkbook(wb, filepath, overwrite = T)

# close the connection
dbDisconnect(con)



# introducing age groups -------------------------------------------------
# DT.kc.blood <- DT.kc.blood |> 
#   mutate(Altersgruppe = case_when(
#     Alter <= 1 ~ 'Säugling(u1a)',
#     Alter > 1 & Alter <= 2 ~ 'Baby(1-2a)',
#     Alter > 2 & Alter <= 7 ~ 'Kleinkind(2-7a)',
#     Alter > 7 & Alter <= 12 ~ 'Kind(7-12a)',
#     Alter > 12 & Alter <= 18 ~ 'Jugendlich(12-18a)',
#     Alter > 18 & Alter <= 25 ~ 'JungeErwachsene(18-25a)',
#     Alter > 25 & Alter <= 60 ~ 'Erwachsene(25-60a)',
#     Alter > 60 & Alter <= 75 ~ 'jungeSenioren(60-75a)',
#     Alter > 75  ~ 'Senioren(ü75a)'
#   ))
# setDT(DT.kc.blood)


