library(tidyverse)
library(arcgis)


# Log into AGOL ------------------------------------------------------------
my_token <- auth_user(
  username = Sys.getenv("ARCGIS_USER"),
  password = Sys.getenv("ARCGIS_PASSWORD"),
  host = arc_host(),
  expiration = 120
)

table_url <- "https://services.arcgis.com/Eg4L1xEv2R3abuQd/arcgis/rest/services/SRCWQM_Update_20250922/FeatureServer/3"
connection <- arc_open(table_url, token = my_token)

# Load AGOL and WQX Template -----------------------------------------------
template <- readxl::read_xlsx("data/WQX_SRC_env_template.xlsx", sheet = 5)
wq_table <- arc_read(table_url, token = my_token)

# Set correct time zone
wq_table <- wq_table %>% mutate(Sample_Date = as_datetime(Sample_Date))

# Enter Start Date to Filter Data
start_date <- as_date("2024-09-01")

# fetch lists from config file
config <- jsonlite::fromJSON("configs/config.json")
drop_list <- config$drop_list
params_list <- config$params_list
unit_map <- config$unit_mappings


# convert data to long form and update columns
long <- wq_table %>%
  filter(as_date(Sample_Date) >= start_date) %>% # Filter only desired dates
  select(!any_of(drop_list)) %>% # drop columns from drop list
  pivot_longer(
    # Pivot table to characteristic name and result value
    cols = all_of(params_list),
    names_to = "Characteristic Name",
    values_to = "Result Value"
  ) %>%
  mutate(
    Result_MeasureUnit = recode(`Characteristic Name`, !!!unit_map),
    .after = `Result Value`,
    `Activity Start Date` = as_date(Sample_Date), # split to date only
    `Activity Start Time` = format(Sample_Date, "%H:%M:%S"), # split to time only
    `Sample_Date` = NULL # drop OG date column
  )

# TODO: Assign values to template table

# TODO Investigate uploading straight to portal using cqx library
