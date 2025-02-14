### FM DATA COMPARISON AND CONSOLIDATION

# README #

# Files must be in Excel format.
# File names must start with the official country name before a "_" (underscore).
# All received questionnaires must be standardized (i.e. the data must have the same structure as the sent questionnaires).

rm(list=ls()) # clear R environment

pkgs <- c("dplyr", "readxl", "readr", "tidyr", "tibble", "ggplot2", "ggtext", "stringr", "lubridate", "compareDF", "tools", "rmarkdown", "kableExtra")
lapply(pkgs, require, character.only = TRUE)

# library(groundhog)
# groundhog.library(pkgs, '2022-03-01')

source("./parameters/parameters_FM_newdata.R")
source(path_functions)

## INPUT VALIDATION

# Received data

check_excel(files_received, path_received) # Check that all files are in Excel format
FM_questionnaire_match(files_received, files_sent) # Check that received questionnaires have a corresponding sent questionnaire
duplicated_questionnaires(files_received, path_received) # Check for multiple submissions by single country
# FM_check_questionnaire_structure(start_year, end_year, files_received, path_received, data_sheet, data_range) # Check structure of received questionnaires

## CREATE OR CALL IN SENT DATA

# source("./modules/sent_compile.R")
consolidated_sent_long <- readRDS("./inputs/consolidated_sent_long.RDS")

## GENERATE EMPTY CONSOLIDATED TABLES

consolidated_received <- tibble(
  Country = character(), 
  `Working domain` = character(), 
  `Working Status` = character(), 
  Sex = character())

for (i in start_year:end_year) {
  consolidated_received[as.character(i)] <- ""
  consolidated_received[paste0(i, "E")] <- ""
}


## CONSOLIDATE QUESTIONNAIRES

# Received

dates_received <- tibble(country = character(), date = character())

for (i in files_received) {
  dates_received <- dates_received %>%
    bind_rows(c(country = toupper(sub("\\_.*", "", i)), date = str_split(i, "_")[[1]][4]))
  data_questionnaire_received <- FM_questionnaire_import(path_received, i, data_sheet, data_range, names(consolidated_received))
  consolidated_received <- consolidated_received %>%
    add_row(data_questionnaire_received)
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

   rmarkdown::render(path_report_newdata, output_file = paste0(i, "_", start_year,"-",  end_year, "_", dates_received$date[dates_received$country == i], ".html"), output_dir = path_comparisons)

  }

## EXPORT CONSOLIDATED RECEIVED DATA
consolidated_received_long_names <- format_export(consolidated_received_long)

consolidated_sent_long_names <- format_export(consolidated_sent_long)

countries_received <- unique(consolidated_received_long_names$geographic_area)
countries_not_received <- setdiff(consolidated_sent_long_names$geographic_area, consolidated_received_long_names$geographic_area)

length(countries_received)
length(countries_not_received)

newdata_export <- consolidated_received_long_names %>%
  bind_rows(filter(consolidated_sent_long_names, geographic_area %in% countries_not_received)) %>%
  arrange(geographic_area)

saveRDS(newdata_export, paste0(path_export, "FM_DB.rds"))
