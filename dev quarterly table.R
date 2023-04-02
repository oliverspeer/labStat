library(dplyr)
library(stringr)

combined.cases <- bind_rows(
  tidy.dxi.data,
  tidy.bn.data
)
#as data frame
sum.cases <- combined.cases |> 
  #group_by(Qtr_Year, Year) |> 
  filter(Year == 2022) |>
  summarize(TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
            Anz_Auftraege = n_distinct(a_Tagesnummer, na.rm = TRUE),
            Anz_Faelle = n_distinct(b_Fallnummer, na.rm = TRUE),
            Auftragstaxen = n_distinct(b_Fallnummer, na.rm = TRUE)*10,
            .by = Quarter
                    ) |> as.data.frame()

sum.cases.t <- sum.cases |> t() |> as.data.frame()


#as tibble
sum.cases.tib <- combined.cases |> 
  #group_by(Qtr_Year, Year) |> 
  filter(Year == 2022) |>
  summarize(TxpUmsatz_KC = sum(Taxpkt., na.rm = TRUE),
            Anz_Auftraege = n_distinct(a_Tagesnummer, na.rm = TRUE),
            Anz_Faelle = n_distinct(b_Fallnummer, na.rm = TRUE),
            Auftragstaxen = n_distinct(b_Fallnummer, na.rm = TRUE)*10,
            .by = Quarter
             ) |> as_tibble()

sum.cases.tib.t <- sum.cases.tib |> t() |> as_tibble()


sum.cases.t$Q_Zahlen <- row.names(sum.cases.t)
names(sum.cases.t) <- sum.cases.t[1,]

sum.cases.t |> 
  flextable() |>  
  theme_vanilla() |> 
  bg(bg = "grey", part = "header") |> 
  align(align = "center", part = "all")|>
  set_table_properties(width = 0.8, layout = "autofit") |> 
  colformat_date(fmt_date = "%m/%Y") 


sum.cases.piv <- sum.cases |> 
  rownames_to_column() |> 
  pivot_longer(!rowname, names_to = "Quartal", values_to = "col2") %>% 
  pivot_wider(names_from = "rowname", values_from = "col2")
names(sum.cases) <- sum.cases[1,] 
sum.cases <- sum.cases[-1,]


sum.cases.piv <- sum.cases |> 
  #rownames_to_column() |> 
  pivot_longer(names_to = "Quartal", values_to = "col2") %>% 
  pivot_wider(names_from = "rowname", values_from = "col2")



sum.case |>
  flextable() |>  
  theme_vanilla() |> 
  bg(bg = "grey", part = "header") |> 
  align(align = "center", part = "all")|>
  set_table_properties(width = 0.8, layout = "autofit")
