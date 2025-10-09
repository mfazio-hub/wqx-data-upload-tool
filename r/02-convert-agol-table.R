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

# list of variable names to drop
drop_list <- c(
  "Rel_Depth",
  "last_edited_date",
  "last_edited_user",
  "Parent_GUID",
  "GlobalID_1",
  "isLive",
  "OBJECTID",
  "Weather",
  "Wind",
  "Wind_Direction",
  "Cloud_Cover",
  "Rainfall_24hr",
  "Secchi",
  "CreationDate",
  "Creator",
  "EditDate",
  "Editor"
)

# list of water parameter columns to add to characteristic name column during pivot
params_list <- c(
  "Water_Temp",
  "DO_Percent",
  "DO_Concentration",
  "Conductivity",
  "Salinity",
  "pH",
  "Turbidity",
  "Chl_a",
  "NOx",
  "NO2",
  "NH4",
  "DIP",
  "LA",
  "Color",
  "TSS",
  "Entero",
  "TKN",
  "TN",
  "TP"
)

# convert data to long form and update columns
long <- wq_table %>%
  filter(as_date(Sample_Date) >= start_date) %>% # Filter only desired dates
  select(!any_of(drop_list)) %>% # drop columns from drop list
  pivot_longer(
    # Pivot table, all parameter columns to be listed under "charateristic name" with associated values stored in "result value"
    cols = all_of(params_list),
    names_to = "Characteristic Name",
    values_to = "Result Value"
  ) %>%
  mutate(
    Result_MeasureUnit = case_when(
      #Create column to house proper units
      `Characteristic Name` == "Water_Temp" ~ "deg C",
      `Characteristic Name` == "DO_Percent" ~ "%",
      `Characteristic Name` == "DO_Concentration" ~ "mg/L",
      `Characteristic Name` == "Conductivity" ~ "µS/cm",
      `Characteristic Name` == "Salinity" ~ "PSU",
      `Characteristic Name` == "Turbidity" ~ "FNU",
      `Characteristic Name` == "Secchi" ~ "m",
      `Characteristic Name` == "Chl_a" ~ "ug/L",
      `Characteristic Name` == "NOx" ~ "ug/L",
      `Characteristic Name` == "NO2" ~ "ug/L",
      `Characteristic Name` == "NH4" ~ "ug/L",
      `Characteristic Name` == "DIP" ~ "mg/L",
      `Characteristic Name` == "Color" ~ "PCU",
      `Characteristic Name` == "TSS" ~ "mg/L",
      `Characteristic Name` == "Entero" ~ "MPN/100 mL",
      `Characteristic Name` == "TP" ~ "mg/L",
      `Characteristic Name` == "TN" ~ "mg/L",
      `Characteristic Name` == "LA" ~ "Kd",
      `Characteristic Name` == "pH" ~ "",
      TRUE ~ NA_character_ # adds NA to all other values
    ),
    .after = `Result Value`,
    `Activity Start Date` = as_date(Sample_Date), # split to date only
    `Activity Start Time` = format(Sample_Date, "%H:%M:%S"), # split to time only
    `Sample_Date` = NULL #drop OG date column
  )
