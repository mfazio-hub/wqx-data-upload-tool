library(tidyverse)
library(arcgis)

st <- Sys.time()

# Load AGOL Data ----------------------------------------------------------------------

# Log into AGOL
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

# Access Table
wq_table <- tryCatch(
  arc_read(table_url, token = my_token),
  error = function(e) stop("Failed to read AGOL table: ", e$message)
)

# Load Local Data ---------------------------------------------------------------------
dn <- readxl::read_xlsx("data\\UWF_yr3_for_SRC.xlsx", sheet = 1)
tkntp <- readxl::read_xlsx("data\\UWF_yr3_for_SRC.xlsx", sheet = 2) %>%
  select(-c("ID", "Layer")) #TODO ADD DATE BACK TO DROP
tntp <- readxl::read_xlsx("data\\UWF_yr3_for_SRC.xlsx", sheet = 3) %>%
  select(-c("ID", "Layer")) #TODO: ADD DATE BACK TO DROP
ent <- readxl::read_xlsx("data\\UWF_yr3_for_SRC.xlsx", sheet = 4) %>%
  select(-c("ID", "Layer", "Date"))
tss <- readxl::read_xlsx("data\\UWF_yr3_for_SRC.xlsx", sheet = 5) %>%
  select(-c("site", "Layer", "date"))

# Load config and for field mappings ---------------------------------------------------
cfg <- yaml::read_yaml("configs/config.yaml")

lookup_table <- read_csv(
  "configs/analyte_lookup.csv",
  show_col_types = FALSE
) %>%
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

# Select just site - date - depth fields from AGOL table and create key for table join
depth <- wq_table %>%
  select(all_of(c('Rel_Depth', 'Site', 'Sample_Date', 'Water_Depth'))) %>%
  mutate(
    Sample_Time = format(Sample_Date, "%H:%M:%S"),
    Sample_Date = as_date(Sample_Date),
    Rel_Depth = case_when(
      Rel_Depth == "Bottom" ~ "B",
      Rel_Depth == "Surface" ~ "S"
    ),
    DEP_JOIN = paste(Site, Sample_Date, Rel_Depth, sep = "_")
  )

# Join all dataframes to one table
join_table <- dn %>%
  full_join(tkntp, join_by(DN_JOIN == TKNTP_JOIN)) %>%
  full_join(tntp, join_by(DN_JOIN == TNTP_JOIN)) %>%
  full_join(ent, join_by(DN_JOIN == ENT_JOIN)) %>%
  full_join(tss, join_by(DN_JOIN == TSS_JOIN)) %>%
  mutate(
    JOIN = paste(ID, Date.x, Layer, sep = "_")
  ) %>%
  left_join(depth, join_by(JOIN == DEP_JOIN))


anl_dates <-
  readxl::read_xlsx("data\\uwf_yr3_analysis_dates.xlsx") %>%
  mutate(
    join_key = case_when(
      Analyte == "Chla" ~ "chla",
      Analyte == "NO2-" ~ "no2",
      Analyte == "NH4+" ~ "nh4",
      Analyte == "NO3-" ~ "nox",
      Analyte == "DIP" ~ "dip",
      Analyte == "Color" ~ "color",
      Analyte == "TSS" ~ "tss",
      Analyte == "TN" ~ "tn",
      Analyte == "TP" ~ "tp",
      Analyte == "TKN" ~ "tkn",
      Analyte == "Entero" ~ "entero",
      TRUE ~ NA_character_
    ),
    ANL_JOIN = if_else(
      join_key %in% c("tss", "tkn", "tp"),
      true = paste(Station, `sample date`, Layer, join_key, sep = "_"),
      false = paste(`sample date`, join_key, sep = "_")
    )
  )


# pivot characteristics
pivot_table <- join_table %>%
  select(-c(Rel_Depth, Site, Date.x)) %>%
  pivot_longer(
    cols = any_of(cfg$params_list),
    names_to = "analyte",
    values_to = "result_value"
  ) %>%
  mutate(
    p_map = case_when(
      analyte == "Entero" ~ "entero",
      analyte == "chl a µg/L" ~ "chla",
      analyte == "NO3-+NO2- µgN/L" ~ "nox",
      analyte == "NO2- µgN/L" ~ "no2",
      analyte == "NH4+ µgN/L" ~ "nh4",
      analyte == "DIP ugP/L" ~ "dip",
      analyte == "TN_mgL" ~ "tn",
      analyte == "TKN_mgL" ~ "tkn",
      analyte == "TP_mgPL.x" ~ "tp",
      analyte == "TP_mgPL.y" ~ "tp",
      analyte == "Color PCU" ~ "color",
      analyte == "TSS mg/L" ~ "tss",
      TRUE ~ NA_character_
    ),
    JOIN = paste(JOIN, p_map, sep = "_"),
    ANL_JOIN = if_else(
      p_map %in% c("tss", "tkn", "tp"),
      true = paste(JOIN),
      false = paste(Sample_Date, p_map, sep = "_")
    )
  ) %>%
  select(c(
    ID,
    Sample_Date,
    Sample_Time,
    Layer,
    Water_Depth,
    analyte,
    result_value,
    JOIN,
    ANL_JOIN
  ))

qc_table <- join_table %>%
  pivot_longer(
    cols = any_of(cfg$qc_names_list),
    names_to = "QC_Name",
    values_to = "QC_Value"
  ) %>%
  mutate(
    p_map = case_when(
      QC_Name == "QC" ~ "entero",
      QC_Name == "Chla QC" ~ "chla",
      QC_Name == "NO3+NO2 QC" ~ "nox",
      QC_Name == "NO2 QC" ~ "no2",
      QC_Name == "NH4 QC" ~ "nh4",
      QC_Name == "DIP QC" ~ "dip",
      QC_Name == "TKN_QC" ~ "tkn",
      QC_Name == "TN_QC" ~ "tn",
      QC_Name == "TP_QC.x" ~ "tp.x",
      QC_Name == "TP_QC.y" ~ "tp.y",
      TRUE ~ NA_character_
    ),
    JOIN = paste(JOIN, p_map, sep = "_")
  ) %>%
  select(c(QC_Name, QC_Value, JOIN))


final_table <- pivot_table %>%
  left_join(qc_table, by = "JOIN") %>%
  mutate(
    analyte = recode(
      analyte,
      !!!cfg$param_name_map,
      .default = NA_character_
    ),
    ID = paste("SRC", ID, "22", sep = "-"),
  ) %>%
  drop_na(result_value) %>%
  left_join(lookup_table, by = join_by(analyte == canonical_name)) %>%
  left_join(anl_dates, by = "ANL_JOIN", relationship = "many-to-many") %>%
  relocate(
    ID,
    Sample_Date,
    Sample_Time,
    Layer.x,
    Water_Depth,
    Activity_type,
    analyte,
    method_speciation,
    result_value,
    unit,
    sample_equipment_name,
    QC_Value,
    result_detection_type,
    result_detection_limit,
    analytical_method_id,
    result_sample_fraction,
    `date run`,
    analyte,
    `sample date`,
    Station,
    Layer.y,
    join_key
  ) %>%
  select(-c(analyte_key, JOIN, QC_Name))

final_table %>% writexl::write_xlsx("data/srswq_yr3_lab_results.xlsx")

et <- Sys.time()
print(paste(
  " Finished! Total runtime: ",
  round(difftime(et, st, units = "secs"), 3),
  "s"
))
