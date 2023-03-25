library(readxl)


#wie fürs QC gelernt:
Analysen.count <- global.data |> group_by(Datum) |>
  summarise(
    Natrium.c = sum(!is.na(Natrium)),
    Kalium.c = sum(!is.na(Kalium)),
    Calcium.c = sum(!is.na(Calcium)),
    Chlorid.c = sum(!is.na(Chlorid)),
    Aufträge = sum(!is.na(Tagesnummer))
  )

#andere Alternative
dxi.sum.data <- tidy.dxi.data |> 
  group_by(Year_Month, Methode) |> 
  summarize(DxI_TxpUmsatz = sum(!is.na(Taxpkt.)),
            DxI_Aufträge = n_distinct(a_Tagesnummer),
            Anz_Patienten = n_distinct(b_Fallnummer),
            Natrium.c = sum(!is.na(Natrium)),
            Kalium.c = sum(!is.na(Kalium)),
            Calcium.c = sum(!is.na(Calcium)),
            Chlorid.c = sum(!is.na(Chlorid))
  )

#alternative with dplyr
dxi.sum.data <- tidy.dxi.data |>
  summarize(
    n = n(),
    DxI_TxpUmsatz = sum(!is.na(Taxpkt.)),
    DxI_Aufträge = n_distinct(a_Tagesnummer),
    Anz_Patienten = n_distinct(b_Fallnummer),
    hsTroponin_I = sum(!is.na("hs Troponin I (DxI)")),
    PCT = sum(!is.na("Procalcitonin quant. (PCT)")),
    Cortisol.c = sum(!is.na("Cortisol")),
    CEA.c = sum(!is.na("CEA")),
    .by = c(Year_Month, Methode)
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
  