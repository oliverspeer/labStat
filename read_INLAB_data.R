read_INLAB_data <- function(file.pattern, dir.path = NULL) {
  # Helper function to determine the data path
  get_data_path <- function(dir.path) {
    if (is.null(dir.path)) {
      if (rstudioapi::isAvailable() && !is.null(rstudioapi::getActiveProject())) {
        return(rstudioapi::getActiveProject())
      } else {
        return(getwd())
      }
    } else {
      return(dir.path)
    }
  }
  
  # Determine the data path
  data.path <- get_data_path(dir.path)
  
  # Get the list of files
  files <- list.files(data.path, pattern = file.pattern)
  if (length(files) == 0) stop("No files found matching the pattern")
  
  all.data <- list()
  
  # Function to read and process each file
  process_file <- function(full.path) {
    # Read the header
    head <- readxl::read_xlsx(full.path, range = readxl::cell_rows(1:2), col_names = FALSE)
    
    # Detect NA cells in the second row, replace them with letters 
    head[2, which(is.na(head[2, ]))] <- as.list(letters[1:length(which(is.na(head[2, ])))])
    
    # Combine the first and second row to create the column names
    col_names <- apply(head, 2, function(x) paste(rev(x), collapse = "_"))
    
    # Read the data while skipping the first two rows and using the combined column names
    dt <- readxl::read_xlsx(full.path, skip = 2)
    data.table::setnames(dt, col_names)
    
    # Remove prefixes from column names
    data.table::setnames(dt, gsub("^[a-z]_", "", names(dt)))
    
    # Ensure the "Probennummer" column exists
    if (!"Probennummer" %in% names(dt)) {
      dt <- dplyr::mutate(dt, Probennummer = NA)
    }
    
    # Convert the data.frame to a data.table
    data.table::setDT(dt)  
    
    # Remove specific columns to anonymize the data
    dt[, c("Name", "Vorname") := NULL]
    
    # Define and exclude certain columns from conversion to numeric
    exclude.cols <- c("Tagesnummer", "Geb.datum", "Geschl.", "Auftragg.")
    include.cols <- setdiff(names(dt), exclude.cols)
    
    # Convert included columns to numeric
    dt[, (include.cols) := lapply(.SD, as.numeric), .SDcols = include.cols]
    
    dt[, Geb.datum := lubridate::ymd(Geb.datum)]
    
    return(dt)
    
  }
  
  # Process all files
  all.data <- lapply(files, function(file.name) process_file(file.path(data.path, file.name)))
  # return(all.data)
  # next
  # Combine all processed data.tables
  dt.name <- data.table::rbindlist(all.data, use.names = TRUE, fill = TRUE)
  
  # New column `sex` with 0 if dt.name$Geschl. == "F" and 1 if dt.name$Geschl. == "M"
  dt.name <- dt.name[, sex := ifelse(Geschl. == "F", 0, 1)]
  
  # Calculate the age from dt.name$Datum and dt.name$Geb.datum
  dt.name <- dt.name[
    !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
  ][, Datum := lubridate::ymd(substr(Tagesnummer, 1, 10))
  ][, Datum := lubridate::ymd(Datum)
  ][, Geb.datum := lubridate::ymd(Geb.datum)
  ][, Alter := round(lubridate::interval(start = Geb.datum, end = Datum) / lubridate::years(1), 2)]
  
  # Prepare id.cols that are not to be melted
  id.cols <- names(dt.name)[!grepl("_\\d+", names(dt.name))]
  
  # Melt the data.table
  DT.m1 <- data.table::melt(
    dt.name,
    id.vars = id.cols,
    variable.name = "Bezeichnung_Methode",
    value.name = "Werte",
    na.rm = TRUE
  )
  
  
  # Split 'Bezeichnung_Methode' into two columns 'Bezeichnung' and 'Methode'
  DT.m1[, c("Bezeichnung", "Methode") := data.table::tstrsplit(Bezeichnung_Methode, "_", fixed = TRUE)
                 ][, Bezeichnung_Methode := NULL
                   ][, Methode := as.numeric(Methode)]
  
  
  dt.name <- data.table::setDT(DT.m1)
  
  # Change the column names to be SQL compatible
  data.table::setnames(dt.name, old = c("Geb.datum", "Geschl.", "Auftragg."), new = c("DOB", "Geschlecht", "KundenID"))
  
  # Extract year, quarter, month, week, day from Datum
  dt.name <- dt.name[, `:=`(
    Jahr = lubridate::year(Datum),
    Quartal = lubridate::quarter(Datum),
    Monat = lubridate::month(Datum),
    Woche = lubridate::week(Datum),
    Tag = lubridate::day(Datum),
    DOB = as.character(DOB),
    Datum = as.character(Datum)
  )]
  
  return(dt.name)
  
}

