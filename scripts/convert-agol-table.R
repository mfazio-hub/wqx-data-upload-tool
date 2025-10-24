library(tidyverse)
library(arcgis)
library(lubridate)

# fetch lists and field maps from config file
cfg <- yaml::read_yaml("configs/config.yaml")
lookup <- read_csv("configs/analyte_lookup.csv", show_col_types = FALSE) %>%
  mutate(
    analyte_key = str_to_upper(str_trim(analyte_key)),
    canonical_name = if_else(
      is.na(canonical_name) | canonical_name == "",
      analyte_key,
      canonical_name
    ),
    result_sample_fraction = if_else(
      is.na(result_sample_fraction),
      NA_character_,
      result_sample_fraction
    )
  )

# normalize config objects
cfg$drop_list <- as.character(cfg$drop_list)
cfg$params_list <- as.character(cfg$params_list)


# Log into AGOL ------------------------------------------------------------
my_token <- tryCatch(
  auth_user(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = arc_host(),
    expiration = 120
  ),
  error = function(e) stop("ArcGIS auth failed: ", e$message)
)

table_url <- Sys.getenv("ARCGIS_URL")

# Load AGOL and WQX Template -----------------------------------------------
template <- readxl::read_xlsx("data/WQX_SRC_env_template.xlsx", sheet = 5)
wq_table <- tryCatch(
  arc_read(table_url, token = my_token),
  error = function(e) stop("Failed to read AGOL table: ", e$message)
)

# Begin dataframe conversion and cleaning -----------------------------------

# Enter start date to filter data
start_date <- as_date("2024-09-01")

# pivot data to wide table and update columns
wide_wq_table <- wq_table %>%
  filter(as_date(Sample_Date) >= start_date) %>% # Filter only desired dates
  select(-any_of(cfg$drop_list)) %>% # drop columns from drop list
  pivot_longer(
    cols = all_of(cfg$params_list), # pivot all columns in params_list to be characteristic names
    names_to = "Characteristic Name",
    values_to = "Result Value"
  ) %>%
  mutate(
    `Characteristic Name` = str_to_upper(str_trim(`Characteristic Name`))
  ) %>%
  left_join(lookup, by = c("Characteristic Name" = "analyte_key")) %>%
  mutate(
    `Result Value` = readr::parse_number(as.character(`Result Value`)),
    Site = paste0("SRC-", Site, "-22"),
    `Activity Start Date` = as_date(Sample_Date), # split to date only
    `Activity Start Time` = format(Sample_Date, "%H:%M:%S") # split to time only
  ) %>%
  drop_na(`Result Value`) %>% # Remove any row that does not have a result value
  select(-c(Sample_Date, `Characteristic Name`)) # drop unused columns

# TODO: Assign values to template table -------------------------------------

# Rename based on YAML config
wqx_table <- long_wq_table %>%
  rename(!!!setNames(names(cfg$column_map), unlist(cfg$column_map)))

# TODO Upload to WQX portal using cqx library -------------------------------
