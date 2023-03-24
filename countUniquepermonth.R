library(lubridate)
library(dplyr)

# create a sample data frame
df <- data.frame(
  id = c(1, 2, 3, 4, 5),
  date = c("2022-01-05", "2022-02-07", "2022-01-12", "2022-03-21", "2022-02-15"),
  variable = c("A", "B", "C", "A", "B")
)

# convert date column to month-year format
df$month_year <- format(ymd(df$date), "%Y-%m")

# count unique variables per month
df %>%
  group_by(month_year) %>%
  summarize(count = n_distinct(variable))

