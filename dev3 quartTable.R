library(dplyr)
library(tidyr)
library(lubridate)

tidy.bn.data <- tidy.bn.data %>% 
  mutate(Quarter = quarter(Datum), Year = year(Datum)) %>% 
  select(b_Fallnummer, Quarter, Taxpkt.)

tidy.dxi.data <- tidy.dxi.data %>% 
  mutate(Quarter = quarter(Datum), Year = year(Datum)) %>% 
  select(b_Fallnummer, Quarter, Taxpkt.)

df.txp <- bind_rows(tidy.bn.data, tidy.dxi.data) |> 
  summarize(Taxpkt_sum = sum(Taxpkt., na.rm = TRUE), 
            unique_b_Fallnummer = n_distinct(b_Fallnummer, na.rm = TRUE),
            .by = Quarter) |> 
  pivot_wider(names_from = Quarter, values_from = c(Taxpkt_sum, unique_b_Fallnummer))


# Add quarter and year columns to tidy.bn.data
tidy.bn.data <- tidy.bn.data %>%
  mutate(Qtr_Year = paste0("Q", quarter(Datum), "_", year(Datum)))

# Add quarter and year columns to tidy.dxi.data
tidy.dxi.data <- tidy.dxi.data %>%
  mutate(Qtr_Year = paste0("Q", quarter(Datum), "_", year(Datum)))

# Combine data frames
combined_data <- bind_rows(tidy.bn.data, tidy.dxi.data)

# Group by quarter and year, count unique b_Fallnummer, and sum Taxpkt.
result <- combined_data %>%
  group_by(Qtr_Year) %>%
  summarize(Count = n_distinct(b_Fallnummer, na.rm = TRUE), Taxpkt_Sum = sum(Taxpkt., na.rm = TRUE)) %>%
  ungroup()

# Pivot wider to get desired format
result <- result %>%
  pivot_wider(names_from = Qtr_Year, values_from = c("Count", "Taxpkt_Sum"))

# Rename columns
colnames(result)[-1] <- paste0("Q", substr(colnames(result)[-1], 1, 2), "_", substr(colnames(result)[-1], 4, 7))
