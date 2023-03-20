library(readxl)
global.data <- read_xlsx("C:\\R_local\\labStat\\globaldata.xlsx")
# elektrolyte <- read_xlsx("C:\\R_local\\labStat\\MethodenStatistik_elektrol.xlsx")

global.data$Datum <- ymd(substr(global.data$Tagesnummer, 1, 10))
global.data.sort <- global.data |> group_by(Datum) |>
  summarise(
    NaCount = sum(!is.na(Natrium)),
    CRPCount = sum(!is.na(CRP)),
    CRPhs = sum(!is.na(`CRP (hoch sensitiv)`))
  )

ggplot(data = global.data.sort) + 
  geom_point(mapping =  aes(Datum, CRPCount)) +
    geom_point(mapping = aes(Datum, NaCount))
  