library(dplyr)
library(tidyr)

# Add a column for quarters
tidy.bn.data <- tidy.bn.data |> 
  mutate(Quarter = quarter(Datum))

tidy.dxi.data <- tidy.dxi.data |> 
  mutate(Quarter = quarter(Datum))

# Summarize by quarter and b_Fallnummer
summary_table <- bind_rows(tidy.bn.data, tidy.dxi.data) |> 
  group_by(Quarter, b_Fallnummer) |> 
  summarize(count = n(), Taxpkt_sum = sum(Taxpkt., na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(Quarter)  |> 
  summarize(unique_b_Fallnummer = n_distinct(b_Fallnummer, na.rm = TRUE), 
            Taxpkt_sum = sum(Taxpkt_sum, na.rm = TRUE))  |> 
  pivot_wider(names_from = Quarter, values_from = c(unique_b_Fallnummer, Taxpkt_sum), 
              names_prefix = "Q")

# View the summary table
summary_table
