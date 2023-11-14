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

#----------------------------------------------------------------------------

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
AU.data <- fun.read.excel.data("BlutAU", "AU.data")
DT.tidy.AU <- fun.write.tidy.data(AU.data, DT.tarif, "DT.tidy.AU")

# DxI data (read excel, tidy up data)  -------------------------------------
dxi.data <- fun.read.excel.data("BlutDxI", "dxi.data")
DT.tidy.dxi <- fun.write.tidy.data(AU.data, DT.tarif, "DT.tidy.dxi")

# import EP & IFE data--------------------------------------------------
files <- list.files("/home/olli/R_local/labStat")
data_path <- "/home/olli/R_local/labStat"
file_pattern <- "EPIFE"
file_name <- files[grep(file_pattern, files)]
full_path <- file.path(data_path, file_name)

head1 <- read_excel(full_path, col_names = TRUE) |>  names()

head2 <- read_excel(full_path, skip = 1, col_names = TRUE, .name_repair = "minimal")  |>  names()

head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")

head <- paste0(head2, sep = "_", head1)
DT.ep.data <- read_excel(full_path, skip = 2, col_names = head)
DT.ep.data <- data.table(DT.ep.data)
for (col in c("c_Name", "d_Vorname")) {
  set(DT.ep.data, j = col, value = NULL)
}

# count all values in DT.ep.data$Immunfixation with data.table
DT.ep.data[, IFix := ifelse(!grepl("SISTIERT", 
                                         Immunfixation_12722 , 
                                         fixed = TRUE) & !is.na(Immunfixation_12722), 1, NA_integer_)]










# Combine the columns into one vector
combined_vector <- unlist(DT.ep.data[,c(6:17)])

# Get unique values
unique_values <- unique(combined_vector)

# View the unique values
print(unique_values)


#count all values but not "SISTIERT" and NA in DT.ep.data$Immunfixation with data.table
count.Ifix <- DT.ep.data[Immunfixation_12722 != "SISTIERT" & Immunfixation_12722 != "NA", .N, by = Immunfixation_12722]
count.Ifix



