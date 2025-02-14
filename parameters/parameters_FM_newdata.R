## PARAMETERS

path_sent <- "./inputs/sent/"
path_received <- "./inputs/received/"
path_comparisons <- "./outputs/comparisons/"
path_export <- "./outputs/database/"

path_report_newdata <- "./modules/report_FM_newdata.Rmd"
path_functions <- "./modules/functions_FM_newdata.R"

start_year <- 1995
end_year <- 2023 # MAKE SURE TO UPDATE

data_sheet <- 5
data_range <- "A3:BI99" # MAKE SURE TO UPDATE

official_flags <- c("A", "B", "E", "I", "M", "P", "Q", "X", "K") # List of official flags for data validation

country_names <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_M49.csv") %>%
  select(Name_En) %>%
  mutate(CountryUpper = toupper(Name_En))

files_sent <- list.files(path = path_sent, full.names = FALSE, recursive = FALSE, ignore.case = TRUE) # List sent questionnaires
files_received <- list.files(path = path_received, full.names = FALSE, recursive = FALSE, ignore.case = TRUE) # List received questionnaires