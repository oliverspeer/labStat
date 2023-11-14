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
  data.path <- "/home/olli/R_local/labStat"
  files <- list.files(data.path)
  file.name <- files[grep(file.pattern, files)]
  full.path <- file.path(data.path, file.name)
 
  # Read the header 
  head1 <- read_excel(full.path, col_names = TRUE) |> names()
  head2 <-
    read_excel(full.path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
  head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
  head <- paste0(head2, sep = "_", head1)
  
  # Define the column types
  col.count <- length(head)
  col.types <-
    c(rep(c(
      "text", "numeric", "skip", "skip", "date", "text", "text"
    ), 1), rep("numeric", col.count - 7))
  
  # Read the data
  dt.name <- read_excel(full.path, 
                     skip = 2, 
                     col_names = head, 
                     col_types = col.types)
  
  # Convert to data.table and return
  return(as.data.table(dt.name))
  
}

# AU data (read excel, tidy up data)  -------------------------------------
fun.read.excel.data("BlutAU", "AU.data")

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



# 
# 
# # define all columns as.character except b_Fallnummer and e_Geb.datum
# for (col in names(DT.ep.data)) {
#   if (col !="b_Fallnummer" & col !="e_Geb.datum") {
#     set(DT.ep.data, j = col, value = as.character(DT.ep.data[[col]]))
#   }
# }
# # in DT.ep.data from DT.ep.data$Immunfixation to last column change "s.unten". "folgt", "s. unten", "negative", "fraglich", "s.u.", " Nachweis von Chylomikronen", "keine Chylomikronen" to 1
# DT.ep.data <- DT.ep.data |> 
#   mutate(across(where(is.character), ~str_replace_all(., c(
#     "s.unten" = "1", 
#     "folgt" = "1", 
#     "s. unten" ="1", 
#     "negative" = "1",
#     "positiv" = "1",
#     "fraglich" = "1", 
#     "s.u." = "1", 
#     " Nachweis von Chylomikronen" = "1", 
#     "keine Chylomikronen" = "1"))))
# 
# # define all columns as.numeric except b_Fallnummer and e_Geb.datum
# for (col in names(DT.ep.data)) {
#   if (col !="b_Fallnummer" & col !="e_Geb.datum") {
#     set(DT.ep.data, j = col, value = as.numeric(DT.ep.data[[col]]))
#   }
# }
# 
