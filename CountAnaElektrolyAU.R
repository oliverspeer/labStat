library(readxl)
global.data <- read_excel("globaldata.xlsx", col_types = 
                            c("text", "skip", "skip", "skip", "date", "text", "text", 
                              "numeric", "numeric","numeric","numeric","numeric","numeric"))
# elektrolyte <- read_xlsx("C:\\R_local\\labStat\\MethodenStatistik_elektrol.xlsx")


global.data$Datum[2] <- ymd(substr(global.data$Tagesnummer, 1, 10))

Analysen <- global.data |> pivot_longer(
  cols = !(a_Tagesnummer:Datum), 
  names_to = c("Bezeichnung","Methode"),
  names_sep = "_",
  values_to = "Werte")

analy.tarif <-
  left_join(
    DxI1.qm.data,
    DxI2.qm.data,
    by = c("Datum", "Level", "Parameter", "Zielwert", "SD", "Proben-ID"),
    multiple = "any",
    na_matches = "never"
  )



Analysen.count <- global.data |> group_by(Datum) |>
  summarise(
    Natrium.c = sum(!is.na(Natrium)),
    Kalium.c = sum(!is.na(Kalium)),
    Calcium.c = sum(!is.na(Calcium)),
    Chlorid.c = sum(!is.na(Chlorid)),
    Aufträge = sum(!is.na(Tagesnummer))
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

ggplot(Analysen, aes(x=Datum)) +
  geom_histogram(Tagesnummer)
  