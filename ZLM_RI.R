ZLM_RI <- function(data, analyte, NBootstrap = 0, seed = 8173){
  df.filt <- data[data$Bezeichnung == analyte,]
  RI.data <- dplyr::pull(df.filt, Werte) |> na.omit()
  RI <- refineR::findRI(RI.data, NBootstrap = NBootstrap, seed = seed)
  refineR::print.RWDRI(RI, RIperc = c(0.025, 0.975), CIprop = 0.95, pointEst = "fullDataEst")
  refineR::plot.RWDRI(RI, showPathol = TRUE, showValue = TRUE, CIprop = 0.95, pointEst = "fullDataEst", showCI = TRUE)
  assign(paste(analyte, "RI_data", sep = "_"), RI, envir = .GlobalEnv)
}





