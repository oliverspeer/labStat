library(lubridate)
library(dplyr)



# convert date column to month-year format
tidy.dxi.data$Year_Month <- format(ymd(tidy.dxi.data$Datum), "%Y-%m")




Monate <- format(ymd(tidy.dxi.data$Datum), "%Y-%m")



# count unique variables per month


dxi.sum.data <- tidy.dxi.data |> 
  group_by(Year_Month) |> 
  summarize(DxI_TxpUmsatz = sum(!is.na(Taxpkt.)),
            DxI_Aufträge = n_distinct(a_Tagesnummer),
            Anz_Patienten = n_distinct(b_Fallnummer)
            )

dxi.sum.data |> flextable() |> 
  theme_vanilla() |> 
  bg(bg = "grey", part = "header") |> 
  set_table_properties(width = 0.8, layout = "autofit") |> 
  colformat_date(fmt_date = "%d/%m/%Y")

Analysen.count <- global.data |> group_by(Datum) |>
  summarise(
    Natrium.c = sum(!is.na(Natrium)),
    Kalium.c = sum(!is.na(Kalium)),
    Calcium.c = sum(!is.na(Calcium)),
    Chlorid.c = sum(!is.na(Chlorid)),
    Aufträge = sum(!is.na(Tagesnummer))
  )