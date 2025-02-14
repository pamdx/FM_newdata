# Sent module
## since it takes long to run, only done once in the beginning.

# Checks

# check_excel(files_sent, path_sent) # Check that all files are in Excel format
# duplicated_questionnaires(files_sent, path_sent) # Ensure there is only one per country
# FM_check_questionnaire_structure(start_year, end_year, files_sent, path_sent, data_sheet, data_range) # Check structure of sent questionnaires


# Create empty table

consolidated_sent <- tibble(
  Country = character(), 
  `Working domain` = character(), 
  `Working Status` = character(), 
  Sex = character())

for (i in start_year:end_year) {
  consolidated_sent[as.character(i)] <- ""
  consolidated_sent[paste0(i, "E")] <- ""
}

# Fill table

for (i in files_sent) {
  data_questionnaire_sent <- FM_questionnaire_import(path_sent, i, data_sheet, data_range, names(consolidated_sent))
  consolidated_sent <- consolidated_sent %>%
    add_row(data_questionnaire_sent)
}

consolidated_sent_long <- FM_wide_to_long(start_year, end_year, consolidated_sent) %>%
  arrange(Country, `Working domain`, Year, `Working Status`, Sex)

saveRDS(consolidated_sent_long, "./inputs/consolidated_sent_long.RDS")
