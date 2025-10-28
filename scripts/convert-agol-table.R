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

wqx_table <- wide_wq_table %>%
  transmute(
    `Project ID` = "SRC-SRSWQ",
    `Monitoring Location ID` = wide_wq_table$Site,
    #`Activity ID (CHILD-subset)` = NULL,                       # removes column
    #`Activity ID User Supplied (PARENTs)` = NULL,              # removes column
    `Activity Type` = wide_wq_table$Activity_type,
    `Activity Media Name` = "Water",
    `Activity Start Date` = wide_wq_table$`Activity Start Date`,
    `Activity Start Time` = wide_wq_table$`Activity Start Time`,
    `Activity Start Time Zone` = "CST",
    `Activity Depth/Height Measure` = wide_wq_table$Water_Depth,
    `Activity Depth/Height Unit` = "m",
    `Sample Collection Method ID` = "UWF-S",
    #`Sample Collection Method Context` = NULL,
    `Sample Collection Equipment Name` = wide_wq_table$sample_equipment_name,
    `Sample Collection Equipment Comment` = wide_wq_table$sample_collection_comment,
    `Characteristic Name` = wide_wq_table$canonical_name,
    #`Characteristic Name User Supplied` = NULL,
    `Method Speciation` = wide_wq_table$method_speciation,
    `Result Detection Condition` = case_when(
      wide_wq_table$`Result Value` == 0 ~ "Not Detected",
      wide_wq_table$`Result Value` <
        wide_wq_table$result_detection_limit ~ "Present Below Quantification Limit",
      TRUE ~ NA_character_
    ),
    `Result Value` = wide_wq_table$`Result Value`,
    `Result Unit` = wide_wq_table$unit,
    `Result Measure Qualifier` = ifelse(
      `Result Detection Condition` == "Not Detected",
      "UQ",
      "Q"
    ),
    `Result Sample Fraction` = wide_wq_table$result_sample_fraction,
    `Result Status ID` = "Accepted",
    `Result Value Type` = "Actual",
    `Result Analytical Method ID` = wide_wq_table$analytical_method_id,
    `Result Analytical Method Context` = wide_wq_table$result_analytical_method_ID
    #`Analysis Start Date` = NULL,
    #`Result Detection/Quantitation Limit Type` = NULL,
    #`Result Detection/Quantitation Limit Measure` = NULL,
    #`Result Detection/Quantitation Limit Unit` = NULL,
    #`Result Comment` = NULL,
    #`Result Sampling Point Type` = NULL,
    #`Result Sampling Point Name` = NULL,
    #`Result Sampling Point Place In Series` = NULL,
    #`Result Depth/Altitude Reference Point` = NULL,
    #`Result Depth/Height Measure` = NULL,
    #`Result Depth/Height Unit` = NULL,
    #`Laboratory Name` = NULL,
    #`Activity Group Type` = NULL,
    #`Activity Group ID` = NULL,
    #`Activity Group Name` = NULL
  )
