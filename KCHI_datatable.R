# preparing libraries
library(data.table)
library(tidyverse)
library(readxl)

# set working directory ----------------------------------------------------
setwd("C:/R_local/labStat")


# reading data from excel file ---------------------------------------------
DT.customer <- read_excel("ZLM-Auftraggeber-20231110.xlsx")

# creating a data table
DT.customer <- data.table(DT.customer)

# rename column VAXKUERZEL to g_Auftragg
setnames(DT.customer, "VAXKUERZEL", "g_Auftragg.")

# import tariff data ------------------------------------------------------

DT.tarif <- read_excel("UmsatzStatistik.xlsx")
DT.tarif <- data.table(DT.tarif)
for (col in c("R.stelle", "z.Gunsten", "Kunde", "Preis")) {
  set(DT.tarif, j = col, value = NULL)
}
DT.tarif <- unique(DT.tarif, by = c("Methode", "Taxpkt."))

# import method data ------------------------------------------------------

DT.method <- read_excel("MethodenKatalogKC.xlsx")
DT.method <- data.table(DT.method)

# import units and reference intervals ----------------------------------------
DT.unitsRI <- fread("Einheiten&RefIntrvle.csv")
setnames(DT.unitsRI, c("NUMMER", "NAME"), c("Methode", "Bezeichnung"))

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
# function to read a single file --------------------------------------
fun.read.excel.data <- function(file.pattern, dt.name) {
  # Define the path and file name
  data.path <- "C:/R_local/labStat"
  files <- list.files(data.path)
  file.name <- files[grep(file.pattern, files)]
  full.path <- file.path(data.path, file.name)
 
  # Read the header 
  head1 <- read_excel(full.path, col_names = TRUE) |> names()
  head2 <-
    read_excel(full.path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
  head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
  head <- paste0(head2, sep = "_", head1)
  
  # Read the data
  dt.name <- read_excel(full.path, skip = 2, col_names = head)
  setDT(dt.name)
  for (col in c("c_Name", "d_Vorname")) {
    set(dt.name, j = col, value = NULL)
  }
  
  
  
  # Define the columns to exclude
  exclude.cols <- c("a_Tagesnummer", "e_Geb.datum", "f_Geschl.", "g_Auftragg.")
  
  # Get the column names to include
  include.cols <- setdiff(names(dt.name), exclude.cols)
  
  # Iterate over the columns and convert to numeric
  for (col in include.cols) {
    # Use as.numeric on the column data, not the column name
    dt.name[, (col) := as.numeric(get(col))]
  }
  
return(dt.name)
  
}

# function to read multiple files --------------------------------------
fun.read.multi.excel.data <- function(file.pattern, dt.name) {
  data.path <- "C:/R_local/labStat"
  files <- list.files(data.path, pattern = file.pattern)
  all.data <- list()
  
  for (file.name in files) {
    full.path <- file.path(data.path, file.name)
    
    # Read the header
    head1 <- readxl::read_excel(full.path, col_names = TRUE) |> names()
    head2 <- readxl::read_excel(full.path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
    head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
    head <- paste0(head2, sep = "_", head1)
    
    # Read the data
    dt <- readxl::read_excel(full.path, skip = 2, col_names = head)
    setDT(dt)
    
    # Remove specific columns
    dt[, c("c_Name", "d_Vorname") := NULL]
    
    # Define and exclude certain columns from conversion to numeric
    exclude.cols <- c("a_Tagesnummer", "e_Geb.datum", "f_Geschl.", "g_Auftragg.")
    include.cols <- setdiff(names(dt), exclude.cols)
    
    # Convert included columns to numeric
    for (col in include.cols) {
      dt[, (col) := as.numeric(get(col))]
    }
    
    # Store the processed data table in a list
    all.data[[file.name]] <- dt
  }
  
  # Combine all processed data.tables
  dt.name <- rbindlist(all.data, use.names = TRUE, fill = TRUE)
  return(dt.name)
}

# function to tidy up data --------------------------------------
fun.write.tidy.data<- function(data, tarif, dt_name) {
data <- data[
  !grepl("Tagesnummer", a_Tagesnummer) & !is.na(a_Tagesnummer)
  ][, Datum := ymd(substr(a_Tagesnummer, 1, 10))
    ]

# Define the new order of columns
new.order <- names(data)
new.order <- c(new.order[1:5], 
               "Datum", 
               new.order[7:which(new.order == "Datum") - 1])

# Rearrange the columns
setcolorder(data, new.order)

# copy column names from AU.data$a_Tagesnummer to AU.data$Datum to id.cols
id.cols <- names(data)[1:6]

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

# calculate the age from DT.m1$Datum and DT.m1$Geb.datum
DT.m1[, Datum := ymd(Datum)
][, Geb.datum := ymd(e_Geb.datum)][
  , Alter := as.numeric(difftime(Datum, ymd(e_Geb.datum), units = "weeks"))/52.25]

# Merge the data.tables DT.m1 and tarif.scales on 'Methode'

dt_name <-
  left_join(
    DT.m1,
    tarif,
    by = c("Methode"),
    keep = FALSE,
    multiple = "any"
  )
setDT(dt_name)

dt_name[, c("Bezeichnung.y", "Tagesnummer") := NULL]
setnames(dt_name, "Bezeichnung.x", "Bezeichnung")
return(dt_name)
}




# AU data (read excel, tidy up data)  -------------------------------------
AU.data <- fun.read.multi.excel.data("BlutAU", "AU.data")
DT.tidy.AU <- fun.write.tidy.data(AU.data, DT.tarif, "DT.tidy.AU")

# DxI data (read excel, tidy up data)  -------------------------------------
dxi.data <- fun.read.multi.excel.data("BlutDxI", "dxi.data")
DT.tidy.dxi <- fun.write.tidy.data(dxi.data, DT.tarif, "DT.tidy.dxi")

# BNII Data (read excel, tidy up data) ------------------------------------
# bn.data <- fun.read.excel.data("BNII", "bn.data")

bn.data <- fun.read.multi.excel.data("BNII", "bn.data")
DT.tidy.bn <- fun.write.tidy.data(bn.data, DT.tarif, "DT.tidy.bn")

# EP & IFE data (read excel, tidy up data) --------------------------------------------------
# Define the path and file name
file.pattern <- "EPIFE"
data.path <- "C:/R_local/labStat"
files <- list.files(data.path, pattern = file.pattern)
all.data <- list()

for (file.name in files) {
  full.path <- file.path(data.path, file.name)
  
  # Read the header
  head1 <- readxl::read_excel(full.path, col_names = TRUE) |> names()
  head2 <- readxl::read_excel(full.path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
  head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
  head <- paste0(head2, sep = "_", head1)
  
  # Read the data
  dt <- readxl::read_excel(full.path, skip = 2, col_names = head)
  setDT(dt)
  
  # Remove specific columns
  dt[, c("c_Name", "d_Vorname") := NULL]
  
  
  # Define the new order of columns
  new.order <- names(dt)
  new.order <- c(new.order[1:5], 
                 "M-Protein (densitometrisch)_37158", 
                 new.order[7:which(new.order == "M-Protein (densitometrisch)_37158") - 1])
  
  # Rearrange the columns
  setcolorder(dt, new.order)
  
  
  # Define a function to change strings to 1
  apply_logic_to_columns <- function(dt) {
    # Get the column names except the first five
    cols_to_modify <- names(dt)[7:ncol(dt)]
    
    # Loop through the columns and apply the logic
    for (col in cols_to_modify) {
      set(dt, j = col, value = ifelse(!grepl("SISTIERT", dt[[col]], fixed = TRUE) & !is.na(dt[[col]]), 1, NA_integer_))
    }
  }
  
  # Apply the function to your data.table
  apply_logic_to_columns(dt)
  
  
  # Define and exclude certain columns from conversion to numeric
  exclude.cols <- c("a_Tagesnummer", "e_Geb.datum", "f_Geschl.", "g_Auftragg.")
  include.cols <- setdiff(names(dt), exclude.cols)
  
  # Convert included columns to numeric
  for (col in include.cols) {
    dt[, (col) := as.numeric(get(col))]
  }
  
  # Store the processed data table in a list
  all.data[[file.name]] <- dt
}

# Combine all processed data.tables
ep.data <- rbindlist(all.data, use.names = TRUE, fill = TRUE)

DT.tidy.ep <- fun.write.tidy.data(ep.data, DT.tarif, "DT.tidy.ep")






# HPLC Data (read excel, tidy up data) ------------------------------------
hplc.data <- fun.read.multi.excel.data("HPLC", "hplc.data")
DT.tidy.hplc <- fun.write.tidy.data(hplc.data, DT.tarif, "DT.tidy.hplc")


# LcMSMS Data (read excel, tidy up data) ------------------------------------
lcms.data <- fun.read.multi.excel.data("LcMSMS", "lcms.data")
DT.tidy.lcms <- fun.write.tidy.data(lcms.data, DT.tarif, "DT.tidy.lcms")

# VH4 Data (read excel, tidy up data) ------------------------------------
# Define the path and file name
file.pattern <- "VH4"
data.path <- "C:/R_local/labStat"
files <- list.files(data.path, pattern = file.pattern)
all.data <- list()

for (file.name in files) {
  full.path <- file.path(data.path, file.name)
  
  # Read the header
  head1 <- readxl::read_excel(full.path, col_names = TRUE) |> names()
  head2 <- readxl::read_excel(full.path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
  head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
  head <- paste0(head2, sep = "_", head1)
  
  # Read the data
  dt <- readxl::read_excel(full.path, skip = 2, col_names = head)
  setDT(dt)
  
  # Remove specific columns
  dt[, c("c_Name", "d_Vorname") := NULL]
  
  
  chr.col <- c("IgG-oligoklonale Banden_25329", 
               "Oligoklonale IgG Banden_11475", 
               "Oligoklonale IgG Banden_102", 
               "IEF (Oligoklonale Banden)_21618")
  # Loop through each column name in chr.col and convert to character
  for (col in chr.col) {
    vh.data[, (col) := as.character(get(col))]
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
  exclude.cols <- c("a_Tagesnummer", "e_Geb.datum", "f_Geschl.", "g_Auftragg.")
  include.cols <- setdiff(names(dt), exclude.cols)
  
  # Convert included columns to numeric
  for (col in include.cols) {
    dt[, (col) := as.numeric(get(col))]
  }
  
  # Store the processed data table in a list
  all.data[[file.name]] <- dt
}

# Combine all processed data.tables
vh.data <- rbindlist(all.data, use.names = TRUE, fill = TRUE)

DT.tidy.vh <- fun.write.tidy.data(vh.data, DT.tarif, "DT.tidy.vh")

# combine data, write csv -------------------------------------------------
DT.kc.blood <- rbindlist(list(
  DT.tidy.AU,
  DT.tidy.dxi,
  DT.tidy.bn,
  DT.tidy.ep,
  DT.tidy.hplc,
  DT.tidy.lcms,
  DT.tidy.vh
), use.names = TRUE, idcol = "file")

# add instrument type by comparing character string "Bezeichnung" and 
# numeric values "Methode" and left join 
DT.kc.blood <-
  left_join(
    DT.kc.blood,
    DT.method,
    by = c("Bezeichnung", "Methode"),
    keep = FALSE,
    multiple = "any"
  )

# adding units and reference intervals by comparing character string "Bezeichnung" and 
# numeric values "Methode" and left join 
DT.kc.blood <-
  left_join(
    DT.kc.blood,
    DT.unitsRI,
    by = c("Bezeichnung", "Methode"),
    keep = FALSE,
    multiple = "any"
  )

# adding customer names and addresses by comparing character string "g_Auftragg" 
DT.kc.blood <-
  left_join(
    DT.kc.blood,
    DT.customer,
    by = c("g_Auftragg."),
    keep = FALSE,
    multiple = "any"
  )

# introducing age groups -------------------------------------------------
DT.kc.blood <- DT.kc.blood |> 
  mutate(Altersgruppe = case_when(
    Alter <= 1 ~ 'Säugling(u1a)',
    Alter > 1 & Alter <= 2 ~ 'Baby(1-2a)',
    Alter > 2 & Alter <= 7 ~ 'Kleinkind(2-7a)',
    Alter > 7 & Alter <= 12 ~ 'Kind(7-12a)',
    Alter > 12 & Alter <= 18 ~ 'Jugendlich(12-18a)',
    Alter > 18 & Alter <= 25 ~ 'JungeErwachsene(18-25a)',
    Alter > 25 & Alter <= 60 ~ 'Erwachsene(25-60a)',
    Alter > 60 & Alter <= 75 ~ 'jungeSenioren(60-75a)',
    Alter > 75  ~ 'Senioren(ü75a)'
  ))
setDT(DT.kc.blood)

# save old data ----------------------------------------------------
# Load the existing RDS file
existing.data <- readRDS("Combined_Data_KC.rds")

# Append the new data to the existing data
combined.data <- rbind(existing.data, DT.kc.blood)

setDT(combined.data)

# check for duplicate rows in combined.data
# combined.data.dups <- combined.data[duplicated(combined.data)]
# length(combined.data.dups)

# Save the updated data to the RDS file
#saveRDS(DT.kc.blood, "Combined_Data_KC.rds")
saveRDS(combined.data, "Combined_Data_KC.rds")



# append DT.kc.blood to existing file
fwrite(DT.kc.blood, file = "Combined_Data_KC.csv", append = TRUE)

# write combined.data.kc to new file
#write_excel_csv(combined.data.kc, "Combined_Data_KC.csv", append = FALSE)
#saveRDS(combined.data.kc, "Combined_Data_KC.rds")
