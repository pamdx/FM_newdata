### FM DATA COMPARISON AND CONSOLIDATION

# README #

# Files must be in Excel format.
# File names must start with the official country name before a "_" (underscore).
# All received questionnaires must be standardized (i.e. the data must have the same structure as the sent questionnaires).

rm(list = ls()) # Clear environment

pkgs <- c("dplyr", "readxl", "readr", "tidyr", "tibble", "ggplot2", "stringr", "lubridate", "compareDF", "tools", "rmarkdown", "kableExtra")
#lapply(pkgs, require, character.only = TRUE)

library(groundhog)
groundhog.library(pkgs, '2022-03-01')

source("functions.R")

## PARAMETERS

path_sent <- "./inputs/sent/"
path_received <- "./inputs/received/"
path_comparisons <- "./outputs/comparisons/"

start_year <- 1995
end_year <- 2021 # MAKE SURE TO UPDATE

data_sheet <- "Sect1 FishStat-FM"
data_range <- "A3:BE99" # MAKE SURE TO UPDATE

official_flags <- c(NA, "B", "E", "I", "M", "P", "Q", "T") # List of official flags for data validation

country_names <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_M49.csv") %>%
  select(Name_En) %>%
  mutate(CountryUpper = toupper(Name_En))

files_sent <- list.files(path = path_sent, pattern = "*.xls*", full.names = FALSE, recursive = FALSE, ignore.case = TRUE) # List sent questionnaires
files_received <- list.files(path = path_received, pattern = "*.xls*", full.names = FALSE, recursive = FALSE, ignore.case = TRUE) # List received questionnaires

## INPUT VALIDATION

# Sent data

check_excel(path_sent) # Check that all files are in Excel format
FM_check_questionnaire_structure(start_year, end_year, files_sent, path_sent, data_range) # Check structure of sent questionnaires
duplicated_questionnaires(files_sent, path_sent) # Ensure there is only one per country

# Received data

check_excel(path_received) # Check that all files are in Excel format
FM_questionnaire_match(files_received, files_sent) # Check that received questionnaires have a corresponding sent questionnaire
FM_check_questionnaire_structure(start_year, end_year, files_received, path_received, data_range) # Check structure of received questionnaires
duplicated_questionnaires(files_received, path_received) # Check for multiple submissions by single country

## GENERATE EMPTY CONSOLIDATED TABLES

consolidated_sent <- tibble(
  Country = character(), 
  `Working domain` = character(), 
  `Working Status` = character(), 
  Sex = character())

for (i in start_year:end_year) {
  consolidated_sent[as.character(i)] <- ""
  consolidated_sent[paste0(i, "E")] <- ""
}

consolidated_received <- consolidated_sent

## CONSOLIDATE QUESTIONNAIRES

# Sent

for (i in files_sent) {
  data <- FM_questionnaire_import(path_sent, i, data_sheet, data_range, names(consolidated_sent))
  consolidated_sent <- consolidated_sent %>%
    add_row(data)
}

consolidated_sent_long <- FM_wide_to_long(start_year, end_year, consolidated_sent) %>%
  arrange(Country, `Working domain`, Year, `Working Status`, Sex)

# Received

dates_received <- tibble(country = character(), date = character())

for (i in files_received) {
  dates_received <- dates_received %>%
    bind_rows(c(country = toupper(sub("\\_.*", "", i)), date = str_split(i, "_")[[1]][4]))
  data <- FM_questionnaire_import(path_received, i, data_sheet, data_range, names(consolidated_received))
  consolidated_received <- consolidated_received %>%
    add_row(data)
}

consolidated_received_long <- FM_wide_to_long(start_year, end_year, consolidated_received) %>%
  arrange(Country, `Working domain`, Year, `Working Status`, Sex)

## ADDITIONAL DATA VALIDATION

FM_check_blank(consolidated_received_long, consolidated_sent_long) # Check for countries that returned a blank (not modified) questionnaire
FM_check_flags(consolidated_received_long, official_flags) # Check that flags are in list of official flags
country_mapping_check(consolidated_received_long, country_names) # Check validity of country names for received data
country_mapping_check(consolidated_sent_long, country_names) # Check validity of country names for sent data

## COMPARE RECEIVED VS SENT DATA

for (i in unique(consolidated_received_long$Country)) {
  
  filtered_received = filter(consolidated_received_long, Country == i)
  filtered_sent = filter(consolidated_sent_long, Country == i)
  
  rmarkdown::render("report.Rmd", output_file = paste0(path_comparisons, i, "_", start_year,"-", end_year,".html"))
  
}

## EXPORT CONSOLIDATED RECEIVED DATA

consolidated_received_long_names <- consolidated_received_long %>%
  mutate(Country = gsub("BONAIRE-S.EUSTATIUS-SABA", "BONAIRE/S.EUSTATIUS/SABA", Country)) %>%
  mutate(Country = gsub("SAINT VINCENT-GRENADINES", "SAINT VINCENT/GRENADINES", Country)) %>%
  left_join(country_names, by = c("Country" = "CountryUpper")) %>%
  select(-Country) %>%
  rename(geographic_area = Name_En, OC3 = `Working domain`, working_time = `Working Status`, sex = Sex, year = Year, value = Value, flag = Flag) %>%
  mutate(comment = as.character(NA)) %>%
  mutate(OC2 = case_when(
    OC3 == "Aquaculture" ~ "Aquaculture",
    OC3 == "Marine Coastal Fishing" ~ "Marine fishing",
    OC3 == "Marine Deep-Sea Fishing" ~ "Marine fishing",
    OC3 == "Marine Fishing, nei" ~ "Marine fishing",
    OC3 == "Inland Waters Fishing" ~ "Inland fishing",
    OC3 == "Subsistence" ~ "Subsistence",
    OC3 == "Processing" ~ "Processing",
    OC3 == "Unspecified" ~ "Unspecified"
  )) %>%
  select(geographic_area, OC2, everything())

consolidated_sent_long_names <- consolidated_sent_long %>%
  mutate(Country = gsub("BONAIRE-S.EUSTATIUS-SABA", "BONAIRE/S.EUSTATIUS/SABA", Country)) %>%
  mutate(Country = gsub("SAINT VINCENT-GRENADINES", "SAINT VINCENT/GRENADINES", Country)) %>%
  left_join(country_names, by = c("Country" = "CountryUpper")) %>%
  select(-Country) %>%
  rename(geographic_area = Name_En, OC3 = `Working domain`, working_time = `Working Status`, sex = Sex, year = Year, value = Value, flag = Flag) %>%
  mutate(comment = as.character(NA)) %>%
  mutate(OC2 = case_when(
    OC3 == "Aquaculture" ~ "Aquaculture",
    OC3 == "Marine Coastal Fishing" ~ "Marine fishing",
    OC3 == "Marine Deep-Sea Fishing" ~ "Marine fishing",
    OC3 == "Marine Fishing, nei" ~ "Marine fishing",
    OC3 == "Inland Waters Fishing" ~ "Inland fishing",
    OC3 == "Subsistence" ~ "Subsistence",
    OC3 == "Processing" ~ "Processing",
    OC3 == "Unspecified" ~ "Unspecified"
  )) %>%
  select(geographic_area, OC2, everything())

countries_received <- unique(consolidated_received_long_names$geographic_area)
countries_not_received <- setdiff(consolidated_sent_long_names$geographic_area, consolidated_received_long_names$geographic_area)

length(countries_received)
length(countries_not_received)

data_export <- consolidated_received_long_names %>%
  bind_rows(filter(consolidated_sent_long_names, geographic_area %in% countries_not_received)) %>%
  arrange(geographic_area)

saveRDS(data_export, "./outputs/FM_DB.rds")
