################################################
#########    ICARTT PACKAGE FOR R   ############
##### Rebuilt Spring 2024 by Alex Altiere ######
####### Based on package by Zoey Werbin ########
################################################


checkFFI <- function(input) {
  FFI <- scan(input, nlines = 1, sep = ",", quiet = T)[2]
  if(FFI != "1001") {
    cat("Sorry, this package only supports time-series data files (FFI = 1001). Input data may be multidimensional (FFI = 2110 or FFI = 2310).")
    return(FALSE)
  } else return(TRUE)
}

#' Parse ICARTT header
#'
#' Parses header of ICARTT file, with field descriptions according to official ICARTT Data Format guidelines (https://www-air.larc.nasa.gov/missions/etc/IcarttDataFormat.htm)
#'
#' @param input Path to the input .ict file
#' @param verbose Defaults to TRUE; print all fields with description. If verbose = FALSE, output excludes full variable list, 'normal comments' section, and data file details.
#' @param raw Defaults to FALSE. If true, print header as seen in ICARTT file, without field descriptions.
#' @return A character vector
#' @export
#'
ict.header <- function(input, verbose = TRUE, raw = FALSE) ({
  headerlength <- scan(input, nlines = 1, sep = ",", quiet = T)[1]
  metadata <- scan(input, nlines = headerlength, what = character(), sep="\t", quiet = T)

  if(raw) return(metadata)

  header <- paste("Header Length:", headerlength, "lines")
  FFI <- paste("File format Index:", unlist(strsplit(metadata[1], ", "))[2])

  if (!checkFFI(input)) return

  filename <- paste("File name:", input)
  PI <- paste("Principal Investigator:", metadata[2])
  org_info <- paste("Affiliation:", metadata[3])
  data_info <- paste("Data source description:", metadata[4])
  mission_info <- paste("Mission name or acronym:", metadata[5])
  file_volume <- paste0("File volume: ", unlist(strsplit(metadata[6], ", "))[1], " File volumes per day: ", unlist(strsplit(metadata[6], ","))[2])
  date_begin <- paste("Data begin on:", paste(unlist(strsplit(metadata[7], ", "))[1:3], collapse= "-"))
  date_revise <- paste("Data revised on:", paste0(unlist(strsplit(metadata[7], ", "))[4:6], collapse= "-"))
  interval <- paste("Data interval (s):", metadata[8])
  indep_var <- paste("Independent variable:", metadata[9])
  n_dep_var <- as.numeric(metadata[10])
  dep_var <- paste("Number of dependent variables:", n_dep_var)
  scale <- paste("Scale factors:",  metadata[11])
  missing_val <- paste("Missing data indicators: ",  metadata[12])
  var_names <- c("Variable names and units: ", paste(metadata[13:(12+n_dep_var)], collapse=",  "))
  n_sc_line <- 13+n_dep_var
  n_sc <- as.numeric(metadata[n_sc_line])
  sc <- paste("Special comments:", ifelse(n_sc==0, "N/A", metadata[(n_sc_line+1):(n_sc_line+n_sc)]))
  n_nc_line <- n_sc_line+n_sc+1
  n_nc <- as.numeric(metadata[n_nc_line])
  nc <- metadata[(n_nc_line+1):(n_nc_line+n_nc-1)]
  PI_contact <- metadata[grep("PI_CONTACT_INFO", metadata)]
  revision <- metadata[grep("REVISION", metadata)]


  if(verbose) {
    return(
      c(filename, header, FFI, PI, org_info, data_info, mission_info, file_volume, date_begin, date_revise, interval, indep_var, dep_var, scale, missing_val, var_names, sc, nc)
    )
  } else {
    return(
      c(filename, header, PI, org_info, data_info, mission_info, date_begin, date_revise, interval, indep_var, dep_var, sc, PI_contact, revision)
    )
  }
})

#ict.header(input)
#' Print variable names
#'
#' Prints variable names (independent and dependent)
#'
#' @param input Path to the input .ict file
#' @param units Defaults to FALSE; print all variables without units. If units = TRUE, variable is followed by unit.
#' @return A character vector
#' @export
#'
ict.varnames <- function(input, units = FALSE) ({

  headerlength <- scan(input, nlines = 1, sep = ",", quiet = T)[1]
  metadata <- scan(input, nlines = headerlength, what = character(), sep="\t", quiet = T, strip.white = T)

  # check that data is one-dimensional, time-series
  FFI <- unlist(strsplit(metadata[1], ", "))[2]
  if(FFI != "1001") {
    cat("Sorry, this package only supports time-series data files (FFI = 1001), not multidimensional data files (FFI = 2110 or FFI = 2310).")
    return()
  }

  n_dep_var <- as.numeric(metadata[10])

  if(units) {
    with_units <- c(metadata[9], metadata[13:(12+n_dep_var)])
  } else {
    no_units <- unlist(strsplit(metadata[headerlength], ", "))
  }
})



#' Import ICARTT data to dataframe
#'
#' Reads data from ICARTT file into dataframe, and discards header information.
#' @param input Path to the input .ict file
#' @return A dataframe
#' @export

read.ict <- function(input) {
  
  # Read the first line as a character string
  first_line <- readLines(input, n = 1)
  
  # Debugging: Print the first line to understand its content
  print(first_line)
  
  # Check for comma or tab and add a comma after the first two characters if missing
  if (!grepl("[,\t]", first_line)) {
    # Add a comma after the first two characters
    first_line <- paste0(substr(first_line, 1, 2), ",", substr(first_line, 3, nchar(first_line)))
  }
  
  # Split the first line on comma or tab
  header_info <- strsplit(first_line, "[,\t]")[[1]]
  
  # Ensure header_info contains at least two values
  if (length(header_info) >= 2) {
    # Trim whitespace from elements
    header_info <- trimws(header_info)
    # Get header length (the first element)
    headerlength <- as.integer(header_info[1]) - 1
    # Get FFI (the second element)
    FFI <- as.character(header_info[2])
  } else {
    stop("The first line of the input file does not contain enough values. Expected at least two values.")
  }
  
  # Debugging: Print the headerlength and FFI values
  print(paste("headerlength:", headerlength))
  print(paste("FFI:", FFI))
  
  # Check if FFI is time-series data (1001)
  if (FFI == "1001") {
    
    # Determine the separator used in the file
    sep <- ifelse(grepl("\t", first_line), "\t", ",")
    
    # Import ICARTT data, without metadata
    dataset <- read.table(input, skip = headerlength, header = TRUE, sep = sep)
    
    return(dataset)
    
  } else {
    cat("Sorry, this package only supports time-series data files (FFI = 1001), not multidimensional data files (FFI = 2110 or FFI = 2310).")
  }
}

#' Convert to CSV
#'
#' Converts an ICARTT file to a comma-separated values file, printing the new .csv file path and file size.
#'
#' @param input Path to the input .ict file
#' @param outfile Defaults to NULL: output file is created in the working directory with the same base name as the input file.
#' @return CSV file is saved in same directory unless outfile is specified
#' @export
#'
ict.to.csv <- function(input, outfile = NULL) {
  
  # Read the first line as a character string
  first_line <- readLines(input, n = 1)
  
  # Debugging: Print the first line to understand its content
  print(first_line)
  
  # Check for comma or tab and add a comma after the first two characters if missing
  if (!grepl("[,\t]", first_line)) {
    # Add a comma after the first two characters
    first_line <- paste0(substr(first_line, 1, 2), ",", substr(first_line, 3, nchar(first_line)))
  }
  
  # Split the first line on comma or tab
  header_info <- strsplit(first_line, "[,\t]")[[1]]
  
  # Ensure header_info contains at least two values
  if (length(header_info) >= 2) {
    # Trim whitespace from elements
    header_info <- trimws(header_info)
    # Get header length (the first element)
    headerlength <- as.integer(header_info[1]) - 1
    # Get FFI (the second element)
    FFI <- as.character(header_info[2])
  } else {
    stop("The first line of the input file does not contain enough values. Expected at least two values.")
  }
  
  # Debugging: Print the headerlength and FFI values
  print(paste("headerlength:", headerlength))
  print(paste("FFI:", FFI))
  
  # Check if FFI is time-series data (1001)
  if (FFI == "1001") {
    
    # Determine the separator used in the file
    sep <- ifelse(grepl("\t", first_line), "\t", ",")
    
    # Import ICARTT data, without metadata
    dataset <- read.table(input, skip = headerlength, header = TRUE, sep = sep)
    
    # Write as CSV file, and print new file path
    inputname <- gsub('.{4}$', '', input)
    filepath <- ifelse(is.null(outfile), paste0(inputname, ".csv"), outfile)
    write.csv(dataset, file = filepath, row.names = FALSE)
    filesize <- file.info(filepath)[1,1] / 1024
    cat("New file: ", filepath, "\nSize: ", filesize, "KB\n")
    
  } else {
    cat("Sorry, this package only supports time-series data files (FFI = 1001), not multidimensional data files (FFI = 2110 or FFI = 2310).")
  }
}


#' Convert to JSON
#'
#' This function converts an ICARTT file to a JSON file, printing the new .json file path and file size and (optionally) saving the metadata as a .txt .csv or .json. JSON format can be validated at: https://jsonlint.com/
#'
#' @param input Path to the input .ict file
#' @param outfile Defaults to NULL: output file is created in the working directory with the same base name as the input file.
#' @return JSON file is saved in same directory
#' @export
ict.to.json <- function(input, outfile=NULL) {

  # require(jsonlite)

  # get length of header (minus last line, which has variable names)
  headerlength <- (scan(input, nlines = 1, sep = ",", quiet = T)[1] - 1)

  # check that data is one-dimensional, time-series
  FFI <- scan(input, nlines = 1, sep = ",", quiet = T)[2]
  if(FFI == "1001") {

    # import ICARTT data, without metadata
    dataset <- read.table(input, skip = headerlength, header = TRUE, sep=",")

    exportJson <- jsonlite::toJSON(dataset)
    inputname <- gsub('.{4}$', '', input)
    #filepath = paste0(normalizePath(dirname(inputname)),"/", basename(inputname), ".JSON")
    filepath <- ifelse(is.null(outfile), paste0(inputname, ".JSON"), outfile)
    write(exportJson, filepath)
    filesize <- file.info(filepath)[1,1]/1024
    cat("New file: ", filepath, "\nSize: ",filesize,"KB\n")

  } else {
    cat("Sorry, this package only supports time-series data files (FFI = 1001), not multidimensional data files (FFI = 2110 or FFI = 2310).")
  }
}


#' Plot flight path
#'
#' Plots simple flight path if latitude and longitude columns are present
#'
#' @param input Path to the input .ict file
#' @param lat Specify latitude column; if empty, will search for column called "latitude" or including "lat"
#' @param lon Specify longitude column; if empty, will search for column called "longitude" or including "lon", "long", or "lng"
#' @return A Plotly-geo object
#' @export
#'
#'
ict.flightpath <- function(input, lat = NULL, lon = NULL) {
  # get length of header (minus last line, which has variable names)
  headerlength <- (scan(input, nlines = 1, sep = ",", quiet = T)[1] - 1)

  # check that data is one-dimensional, time-series
  FFI <- scan(input, nlines = 1, sep = ",", quiet = T)[2]
  if(FFI == "1001") {

    # import ICARTT data, without metadata
    headerlength <- (scan(input, nlines = 1, sep = ",", quiet = T)[1] - 1)
    dataset <- read.table(input, skip = headerlength, header = TRUE, sep=",")
    dataset$LATITUDE <- dataset$LAT
    # attempt to find Lat/Long
    if(is.null(lat)){
      lat <- colnames(dataset)[grep("latitude|\\blat\\b", colnames(dataset), ignore.case=TRUE)][1]
    }
    if(nchar(lat) == 0){
      return("Please specify latitude column; could not find column labeled 'Lat' or 'Latitude'")
    }

    if(is.null(lon)){
      lon <- colnames(dataset)[grep("longitude|\\blon\\b|\\blong\\b", colnames(dataset), ignore.case=TRUE)][1]
    }
    if(nchar(lon) == 0){
      return("Please specify longitude column; could not find column labeled 'Lon', 'Long', or 'Longitude'")
    }

    lat <- dataset[ , colnames(dataset) %in% lat]
    lon <- dataset[ , colnames(dataset) %in% lon]


    library(plotly)
    geo <- list(
      #scope = 'north america',
      projection = list(type = 'azimuthal equal area',
                        rotation = list(lon = median(lon, na.rm=TRUE))),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray80"),
      countrywidth = 0.5,
      subunitwidth = 0.5,
      lataxis = list(
        range = c(min(lat, na.rm=TRUE), max(lat, na.rm=TRUE)) # change based on min/max
      ),
      lonaxis = list(
        range = c(min(lon, na.rm=TRUE), max(lon, na.rm=TRUE)) # change based on min/max
      )
    )

    p <- plot_geo(locationmode = 'USA-states',color = I("black")) %>%
      add_markers(
        data = dataset, x = ~lon, y = ~lat, alpha = 0.5
      ) %>%
      layout(
        geo = list(
          scope = 'world',
          projection = list(type = 'azimuthal equal area',
                            rotation = list(lon = median(lon, na.rm=TRUE))),
          showland = TRUE,
          showlakes = TRUE,
          showcountries = TRUE,
          showstates = TRUE,
          landcolor = toRGB("gray95"),
          subunitcolor = toRGB("gray85"),
          countrycolor = toRGB("gray80"),
          countrywidth = 0.5,
          subunitwidth = 0.5,
          lataxis = list(
            range = c(min(lat, na.rm=TRUE)-3, max(lat, na.rm=TRUE)+3) # change based on min/max
          ),
          lonaxis = list(
            range = c(min(lon, na.rm=TRUE)-3, max(lon, na.rm=TRUE)+3) # change based on min/max
          )
        ),
        showlegend = FALSE
      )
    p
  } else {
    cat("Sorry, this package only supports time-series data files (FFI = 1001), not multidimensional data files (FFI = 2110 or FFI = 2310).")
  }
}




#' Get variable values
#'
#' Get values for select variables
#'
#' @param input Path to the input .ict file
#' @param variable Variable (or vector of variables)
#' @return Values of variable in data frame
#' @export
ict.var <- function(input, variable) ({
  df <- ICARTT.to.df(input)
  subset(df, select = variable)
})
