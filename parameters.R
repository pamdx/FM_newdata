## PARAMETERS

path_sent <- "./inputs/sent/"
path_received <- "./inputs/received/"
path_comparisons <- "./outputs/comparisons/"
path_export <- "./outputs/"

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