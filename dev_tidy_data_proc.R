# prepare libraries -------------------------------------------------------

library(tidyverse)
library(parallel)
library(robslopes)
library(readxl)



# import tariff data ------------------------------------------------------

tarif.scales <- read_excel(
  "UmsatzStatistik.xlsx",
  col_types = c(
    "text",
    "text",
    "skip",
    "skip",
    "skip",
    "skip",
    "numeric",
    "skip",
    "numeric"
  )
)



# import method data ------------------------------------------------------

methoden.kc <- read_excel("MethodenKatalogKC.xlsx",
                          col_types = c("text", "numeric", "text"))


# import reagent costs ----------------------------------------------------

reag.costs <- read_excel("Bestellliste.xlsx", col_types = c("text", 
                                        "date", 
                                        "text",
                                        "skip",
                                        "skip",
                                        "skip",
                                        "skip",
                                        "text",
                                        "skip",
                                        "skip",
                                        "skip",
                                        "numeric",
                                        "numeric"))
reag.costs$Year_Month <- format(ymd(reag.costs$Datum), "%Y-%m")
reag.costs <- reag.costs |>  
  mutate(Quarter = quarter(Datum), Year = year(Datum)) |> 
  mutate(across(where(is.numeric), ~round(., 1)))


# define functions --------------------------------------------------------
fun.read.excel.data <- function(file_pattern, df_name) {
  data_path <- "C:\\R_local\\labStat\\"
  files <- list.files(data_path)
  file_name <- files[grep(file_pattern, files)]
  full_path <- file.path(data_path, file_name)
  
  head1 <- read_excel(full_path, col_names = TRUE) |> names()
  head2 <-
    read_excel(full_path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
  head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
  head <- paste0(head2, sep = "_", head1)
  
  col.count <- ncol(read_excel(full_path, n_max = 1))
  col.types <-
    c(rep(c(
      "text", "numeric", "skip", "skip", "date", "text", "text"
    ), 1), rep("numeric", col.count - 7))
  
  assign(
    df_name,
    read_excel(
      full_path,
      skip = 2,
      col_names = head,
      col_types = col.types
    ),
    envir = .GlobalEnv
  )
 
}

fun.process.data <- function(data, tarif) {
  analy.pivot <- data %>% pivot_longer(
    cols = !(a_Tagesnummer:Datum),
    names_to = c("Bezeichnung", "Methode"),
    names_sep = "_",
    values_to = "Werte",
    values_drop_na = TRUE
  )
  
  tidy.data <-
    left_join(
      analy.pivot,
      tarif,
      by = c("Methode"),
      keep = FALSE,
      multiple = "any"
    )
  tidy.data <- subset(tidy.data, select = -c(Tagesnummer))
  tidy.data$Alter <-
    year(tidy.data$Datum) - year(tidy.data$e_Geb.datum) -
    (
      month(tidy.data$Datum) < month(tidy.data$e_Geb.datum) |
        (
          month(tidy.data$Datum) == month(tidy.data$e_Geb.datum) &
            day(tidy.data$Datum) < day(tidy.data$e_Geb.datum)
        )
    )
  tidy.data <- tidy.data |> relocate(Alter, .before = f_Geschl.)
  tidy.data$Year_Month <- format(ymd(tidy.data$Datum), "%Y-%m")
  tidy.data <-
    tidy.data %>% mutate(Quarter = quarter(Datum), Year = year(Datum))
  return(tidy.data)
}

fun.write.tidy.data <- function(data, tarif, df_name) {
  data <- data |>  filter(!grepl("Tagesnummer", a_Tagesnummer)) |>
    filter(!is.na(a_Tagesnummer))
  data$Datum <- ymd(substr(data$a_Tagesnummer, 1, 10))
  data <- data |>  relocate(Datum, .before = 6)
  tidy.data <- fun.process.data(data, tarif)
  assign(df_name, tidy.data, envir = .GlobalEnv)
  saveRDS(tidy.data, paste0(df_name, ".rds"))
}



# AU data (read excel, tidy up data)  -------------------------------------
fun.read.excel.data("BlutAU", "AU.data")
fun.write.tidy.data(AU.data, tarif.scales, "tidy.AU.data")



# DxI data (read excel, tidy up data) -------------------------------------
fun.read.excel.data("BlutDxI", "dxi.data")
fun.write.tidy.data(dxi.data, tarif.scales, "tidy.dxi.data")



# BNII Data (read excel, tidy up data) ------------------------------------
fun.read.excel.data("BNII", "bn.data")
fun.write.tidy.data(bn.data, tarif.scales, "tidy.bn.data")


# EP Data (read excel, tidy up data) ------------------------------------
files <- list.files("C:\\R_local\\labStat\\")
data_path <- "C:\\R_local\\labStat\\"

# find the file that has the material and machine in its name

file_pattern <- "EPIFE"


file_name <- files[grep(file_pattern, files)]
full_path <- file.path(data_path, file_name)

head1 <- read_excel(full_path, col_names = TRUE) |>  names()
head2 <- read_excel(full_path, skip = 1, col_names = TRUE, .name_repair = "minimal")  |>  names()
head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")

head <- paste0(head2, sep = "_", head1)
ep.data <- read_excel(full_path, skip = 2, col_names = head, col_types =
                               c(
                                 "text",
                                 "numeric",
                                 "skip",
                                 "skip",
                                 "date",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text",
                                 "text"
                               ))


ep.data <- ep.data  |>
  mutate_at(vars(!a_Tagesnummer:g_Auftragg.),
            ~ as.numeric(str_replace_all(
              .,
              c(
                "s.unten" = "1",
                "folgt" = "1",
                "s. unten" = "1",
                "negative" = "1",
                "fraglich" = "1",
                "s.u." = "1",
                " Nachweis von Chylomikronen" = "1",
                "keine Chylomikronen" = "1"
              )
            ))) |> as.data.frame()

fun.write.tidy.data(ep.data, tarif.scales, "tidy.ep.data")



# HPLC Data (read excel, tidy up data) ------------------------------------
fun.read.excel.data("HPLC", "hplc.data")
fun.write.tidy.data(hplc.data, tarif.scales, "tidy.hplc.data")


# LcMSMS Data (read excel, tidy up data) ------------------------------------
fun.read.excel.data("LcMSMS", "lcms.data")
fun.write.tidy.data(lcms.data, tarif.scales, "tidy.lcms.data")


# VH4 Data (read excel, tidy up data) ------------------------------------
data_path <- "C:\\R_local\\labStat\\"
file_pattern <- "VH4"
files <- list.files(data_path)
file_name <- files[grep(file_pattern, files)]
full_path <- file.path(data_path, file_name)

head1 <- read_excel(full_path, col_names = TRUE) |> names()
head2 <-
  read_excel(full_path, skip = 1, col_names = TRUE, .name_repair = "minimal") |> names()
head2[1:7] <- c("a", "b", "c", "d", "e", "f", "g")
head <- paste0(head2, sep = "_", head1)

col.count <- ncol(read_excel(full_path, n_max = 1))
col.types <- c(rep(c("text", "numeric", "skip", "skip", "date", "text", "text"), 1), 
               rep(c("numeric","skip", "text", "skip"), c(col.count-16, 1, 1, 1)), 
               rep("numeric", 6))
vh.data <- read_excel(full_path, skip = 2, col_names = head, col_types =
                        col.types
)

vh.data <- vh.data |> mutate_at(vars(!a_Tagesnummer:g_Auftragg.),
                                  ~ ifelse(is.character(.) & . != "SISTIERT", "1", .)) |> 
  mutate_at(vars(!a_Tagesnummer:g_Auftragg.), as.numeric)  |> 
  as.data.frame()



fun.write.tidy.data(vh.data, tarif.scales, "tidy.vh.data")



# combine data, write csv -------------------------------------------------

combined.data.kc <- bind_rows(
  tidy.AU.data,
  tidy.dxi.data,
  tidy.ep.data,
  tidy.bn.data,
  tidy.hplc.data,
  tidy.lcms.data,
  tidy.vh.data,
)

combined.data.kc$Methode <-
  as.numeric(combined.data.kc$Methode)

combined.data.kc <-
  left_join(
    combined.data.kc,
    methoden.kc,
    by = c("Bezeichnung", "Methode"),
    keep = FALSE,
    multiple = "any"
  )

# append combined.data.kc to existing file
# write_excel_csv(combined.data.kc, "Combined_Data_KC.csv", append = TRUE)

# write combined.data.kc to new file
write_excel_csv(combined.data.kc, "Combined_Data_KC.csv", append = FALSE)
```


