library(readxl)
global.data <- read_excel("globaldata.xlsx", col_types = 
                            c("text", "skip", "skip", "skip", "skip", "date", "text", "text", 
                              "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
# elektrolyte <- read_xlsx("C:\\R_local\\labStat\\MethodenStatistik_elektrol.xlsx")


global.data$Datum <- ymd(substr(global.data$Tagesnummer, 1, 10))

Analysen <- global.data |> pivot_longer(cols = Natrium:Calprotectin, names_to = "Parameter", values_to = "Werte")


global.data.sort <- global.data |> group_by(Datum) |>
  summarise(
    Natrium.c = sum(!is.na(Natrium)),
    Kalium.c = sum(!is.na(Kalium)),
    CRP.c = sum(!is.na(CRP)),
    Albumin = sum(!is.na(Albumin_chem)),
    ALT.c = sum(!is.na(ALT)),
    AST.c = sum(!is.na(AST)),
    Bili = sum(!is.na(Bilirubin_ges)),
    Calcium.c = sum(!is.na(Calcium)),
    Calprotectin.c = sum(!is.na(Calprotectin)),
  )


Analysen.sort <- global.data.sort |> pivot_longer(cols = Natrium.c:Calprotectin.c, names_to = "Parameter", values_to = "Anzahl")

# keine sehr einprägsame Darstellung, informativer ist das Histogramm unten
ggplot(data = Analysen.sort) + 
  geom_point(aes(x = Datum, y = Anzahl)) +
  facet_wrap(~ Parameter, scales = "free_y")

#ggplot vom data.frame "Analysen". Kein "summarise" nötig?

ggplot(data = Analysen, aes(x = Datum, fill = Geschl.)) + 
  geom_histogram(binwidth = 0.5, na.rm = TRUE) +
  facet_wrap(~ Parameter)


ggplot(Analysen, aes(x = Datum, fill = Auftragg.)) + #wegen sehr vieler Auftragg.
  geom_bar() +                                      # nicht sehr sinnvoll
  facet_wrap(~ Parameter)


  