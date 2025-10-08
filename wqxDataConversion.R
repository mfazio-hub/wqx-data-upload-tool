library(tidyverse)

# Load Template
template <- readxl::read_xlsx("WQX_SRC_env_template.xlsx", sheet = 5)

# Load data to be uploaded
data <- readxl::read_xls("C:\\Users\\michaelf\\OneDrive - Santa Rosa County, FL\\Projects\\Santa Rosa Sound Water Quality\\Data\\WQX\\WQX_SRC_ENV_Y3.xls")

# list of variable names for WQX format



long <- data %>%
  select(!(c(Rel_Depth, last_edited_date, last_edited_user, Parent_GUID, GlobalID_1, isLive, OBJECTID, Weather, Wind, Wind_Direction, Cloud_Cover, Rainfall_24hr ))) %>%
  relocate(Water_Depth, .before = Site) %>%
  pivot_longer(cols = "Water_Temp":"TN", names_to = "Characteristic Name", values_to = "Result Value") %>%
  mutate(Result_MeasureUnit = 
    case_when( 
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
    TRUE ~ NA_character_
    ), .after = `Result Value`)



null_characteristics <- long %>%
  group_by(`Sample_Date`, `Characteristic Name`) %>%
  summarise(null_count = sum(is.na(`Result Value`)), .groups = "drop")

null_sites <- long %>%
  group_by(`Sample_Date`, `Site`) %>%
  summarise(null_count = sum(is.na(`Result Value`)), .groups = "drop")

july <- long %>%
  select(`Sample_Date`)

