# Function to import questionnaire

FM_questionnaire_import <- function(directory, file, sheet_data, range_data, col_names){
  
  # Extract country name
  
  country <- toupper(sub("\\_.*", "", file))
  
  # Import data from Review sheet
  
  data <- suppressMessages(read_excel(enc2native(paste0(directory, file)), sheet = sheet_data, range = range_data, col_types = "text"))
  
  data$country <- toupper(country)
  
  data <- data %>%
    select(country, everything())
  
  names(data) <- col_names
  
  data$`Working domain` <- rep(c("Aquaculture", "Marine Coastal Fishing", "Marine Deep-Sea Fishing", "Inland Waters Fishing", "Marine Fishing, nei", "Subsistence", "Unspecified", "Processing"), each = 12)
  data$`Working Status` <- rep(rep(c("Full time", "Part time", "Occasional", "Status Unspecified"), each = 3), 8)
  data$Sex <- rep(rep(c("M", "F", "U"), 4), 8)
  
  return(data)
  
}

# Function to turn FM questionnaire in wide form to long form

FM_wide_to_long <- function(year_start, year_end, data){
  
  for (i in year_start:year_end) {
    
    data <- data %>%
      unite(!!as.name(i), starts_with(toString(i)), sep = "_", remove = TRUE, na.rm = FALSE)
    
  }
  
  data_long <- data %>%
    pivot_longer(as.character(year_start):as.character(year_end), names_to = "Year", values_to = "Value") %>%
    mutate(Value = gsub("NA", "", Value))%>%
    separate(col = Value, into = c("Value", "Flag"), sep = "_")
  
  data_long$Value[data_long$Value == ""] <- NA
  data_long$Flag[data_long$Flag == ""] <- NA
  
  data_long <- data_long %>%
    filter(!is.na(Value) | !is.na(Flag))
  
  wrong_values <- slice(data_long, which(is.na(as.numeric(as.character(data_long$Value))))) %>%
    filter(!is.na(Value))
  
  if (nrow(wrong_values) > 0) {
    print(wrong_values)
    stop("Some values are not numerical and risk being lost (see above).")
  }
  
  data_long$Year <- as.integer(data_long$Year)
  data_long$Value <- as.numeric(data_long$Value)
  data_long$Value <- as.integer(round(data_long$Value, digits = 0))
  data_long$Country <- gsub("/", "-", data_long$Country)
  data_long$Flag <- toupper(data_long$Flag)
  
  return(data_long)
  
}

# Function to check that files are in Excel format

check_excel <- function(directory){
  
  files <- list.files(path = directory, full.names = FALSE, recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE)
  extensions <- excel_format(files)
  wrong_extensions <- files[is.na(extensions)]
  
  if (length(wrong_extensions) > 0) {
    
    stop(paste(length(wrong_extensions), "file(s) with incorrect extensions were found in", directory, ":", paste(wrong_extensions, collapse = ", ")))
    
  } else if (length(wrong_extensions) == 0){
    
    message(paste("All files in", directory, "have correct extensions."))
    
  }
  
}

# Function to ensure received questionnaires are matched by sent questionnaires

FM_questionnaire_match <-  function(received, sent){
  
  countries_received <- toupper(sub("\\_.*", "", received))
  countries_sent <- toupper(sub("\\_.*", "", sent))
  nomatch <- c()
  
  for (i in countries_received) {
    
    if(!(i %in% countries_sent)){
      
      nomatch <- c(nomatch, i)
      
    }
    
  }
  
  if (length(nomatch) > 0) {
    stop(paste("No corresponding sent questionnaire found for:", paste(nomatch, collapse = ", ")))
  } else if(length(nomatch) == 0){
    message("All received questionnaires are matched by sent questionnaires.")
  }
  
}

# Function to check that the data received is properly structured

FM_check_questionnaire_structure <- function(year_start, year_end, files, directory, range_data){
  
  issues <- c()
  
  for (i in files) {
    
    country <- sub("\\_.*", "", i)
    data <- suppressMessages(read_excel(enc2native(paste0(directory, i)), sheet = "Sect1 FishStat-FM", range = range_data, col_types = "text"))
    
    if (!(names(data)[1] == "Working domain" | names(data)[1] == "Domaine de travail" | names(data)[1] == "Dominio operativo")) {
      
      issues <- c(issues, (paste0("- Apparent issue with ", i, ": wrong name for first column.")))
      
    } else if (!(names(data)[ncol(data)-1] == as.name(year_end))) {
      
      issues <- c(issues, (paste0("- Apparent issue with ", i, "second to last column does not correspond to ", year_end)))
      
    } else if (!(tail(data[[3]], 1) == "U" | tail(data[[3]], 1) == "D")) {
      
      issues <- c(issues, (paste0("- Apparent issue with ", i, ": last row of third column has an irregular value.")))
      
    }
    
  }
  
  if (length(issues) > 0) {
    
    stop(paste(issues, collapse = "\n"))
    
  } else if (length(issues) == 0) {
    
    message(paste("No data structure issue was detected in the", directory, "questionnaires."))
    
  }
  
}

# Function to identify countries that submitted multiple questionnaires

duplicated_questionnaires <- function(files, directory){
  
  country_list <- sub("\\_.*", "", files)
  duplicated_questionnaires <- country_list[duplicated(country_list)] 
  
  if (length(duplicated_questionnaires) > 0) {
   
    stop(paste("Countries that submitted multiple questionnaires:", paste(duplicated_questionnaires, collapse = ", ")))
     
  } else{message("No duplicated questionnaires detected in ", directory, ".")}
  
}

# Function to check if questionnaire return is blank

FM_check_blank <- function(received, sent){
  
  blank_questionnaires <- c()
  
  for (i in unique(received$Country)) {
    
    if(identical(filter(received, Country == i), filter(sent, Country == i))){
      
      blank_questionnaires <- c(blank_questionnaires, i)
      
    }
    
  }
  
  if (length(blank_questionnaires) > 0) {
    
    stop(paste("Countries that returned blank questionnaires: ", paste(blank_questionnaires, collapse = ", ")))
  
    } else{message("No country returned a blank questionnaire.")}
  
  
}

# Function to check that flags are in list of official flags

FM_check_flags <- function(data, ref_flags){
  
  wrong_flags <- data %>%
    filter(!(Flag %in% ref_flags))
  
  if (nrow(wrong_flags) > 0) {
    print(wrong_flags, n = 1000)
    stop(paste(nrow(wrong_flags), "entries have non-official flags (see above)."))
  } else if (nrow(wrong_flags) == 0) {
    message("The flags in the data correspond to official flags.")
  }
  
}

# Function to create a rainbow chart

rainbow_chart <- function(data, country, sector, title, year_start, year_end){
  
  data <- data  %>%
    mutate(working_domain_short = case_when(
      `Working domain` == "Marine Coastal Fishing" ~ "Coastal",
      `Working domain` == "Marine Deep-Sea Fishing" ~ "Deep-Sea",
      `Working domain` == "Marine Fishing, nei" ~ "NEI",
      TRUE ~ `Working domain`
    )) %>%
    mutate(subseries = ifelse(`Working domain` %in% c("Marine Coastal Fishing", "Marine Deep-Sea Fishing", "Marine Fishing, nei"),
                              paste(working_domain_short, `Working Status`, Sex, sep = " | "),
                              paste(`Working Status`, Sex, sep = " | ")
    )) %>%
    mutate(alpha = case_when(Flag == "E"  ~ 1,
                             is.na(Flag) | Flag == "B" | Flag == "T" | Flag == "R" | Flag == "Q" | Flag == "I" | Flag == "P"  ~ 0.35))
  
  print(
    ggplot(data, aes(x = Year, y = Value, fill = subseries, alpha = alpha)) +
        geom_bar(stat="identity", colour="white") +
        labs(title = title, subtitle = paste(country, "|", sector), y = "Employment (people)", caption = "Transparent bars indicate official data or alternative sources. Solid bars indicate estimates.") +
        guides(alpha = "none") +
        scale_alpha_identity() +
        scale_fill_discrete(name = "Subseries") +
        scale_y_continuous(labels = addUnits) + 
        scale_x_continuous(breaks = integer_breaks(), minor_breaks = seq(start_year, end_year, 1)) +
        theme(aspect.ratio = 3/4, axis.title.x = element_blank()) +
        coord_cartesian(xlim = c(year_start, year_end))
  )
  
}

# Function for more user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, 1), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, 1), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, 1), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, 1), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

# Function to get integer axis values on plots.

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Check country mapping of consolidated data

country_mapping_check <- function(consolidated_long, ref_names){
  
  data <- consolidated_long %>%
    mutate(Country = toupper(Country)) %>%
    mutate(Country = gsub("BONAIRE-S.EUSTATIUS-SABA", "BONAIRE/S.EUSTATIUS/SABA", Country)) %>%
    mutate(Country = gsub("SAINT VINCENT-GRENADINES", "SAINT VINCENT/GRENADINES", Country)) %>%
    left_join(ref_names, by = c("Country" = "CountryUpper"))
  
  if (nrow(data[is.na(data$Name_En),]) > 0) {
    print(data[is.na(data$Name_En),])
    stop(paste("Missing country mapping for:", toString(unique(data[is.na(data$Name_En),]$Country))))
  } else if (nrow(data[is.na(data$Name_En),]) == 0) {
    message(paste("All country names in", substitute(consolidated_long),"could be mapped with the reference list."))
  }
  
}