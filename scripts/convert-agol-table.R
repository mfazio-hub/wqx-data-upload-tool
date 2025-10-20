library(tidyverse)
library(arcgis)
library(lubridate)

# fetch lists and field maps from config file
cfg <- yaml::read_yaml("configs/config.yaml")

# normalize config objects
cfg$drop_list <- as.character(cfg$drop_list)
cfg$params_list <- as.character(cfg$params_list)
cfg$unit_map <- unlist(cfg$unit_map)
cfg$param_name_map <- unlist(cfg$param_name_map)

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

# Convert data to long form and update columns
long_wq_table <- wq_table %>%
  filter(as_date(Sample_Date) >= start_date) %>% # Filter only desired dates
  select(-any_of(cfg$drop_list)) %>% # drop columns from drop list
  pivot_longer(
    cols = all_of(cfg$params_list), # pivot all columns in params_list to be characteristic names
    names_to = "Characteristic Name",
    values_to = "Result Value"
  ) %>%
  mutate(
    `Result Value` = readr::parse_number(as.character(`Result Value`)),
    Result_MeasureUnit = recode(
      `Characteristic Name`,
      !!!cfg$unit_map,
      .default = NA_character_
    ),
    .after = `Result Value`,
    `Activity Start Date` = as_date(Sample_Date), # split to date only
    `Activity Start Time` = format(Sample_Date, "%H:%M:%S") # split to time only
  ) %>%
  select(-Sample_Date) # drop OG date column after using it

# TODO: Assign values to template table -------------------------------------

mapped_df <- long_wq_table %>% # rename/match columns according to field_map
  rename(!!!set_names(cfg$field_map, names(cfg$field_map))) %>% # add fixed fields (each as a new column)
  mutate(across(everything(), as.character)) %>% # ensure string-safe glue ops
  mutate(!!!cfg$fixed_fields) %>%
  mutate(
    # add computed fields using glue templates
    !!!imap(cfg$computed_fields, ~ glue_data(long_wq_table, .x))
  )

# ensure all template columns exist
filled_template <- template %>%
  select(names(template)) %>% # ensure consistent col order
  bind_cols(mapped_df) %>% # combine mapped data
  select(names(template)) # keep only valid template columns

glimpse(filled_template)

# TODO Upload to WQX portal using cqx library -------------------------------
