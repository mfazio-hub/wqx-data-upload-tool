source("scripts/lab-analysis-data-pivot.R")

analysis_NA <- final_table %>%
  filter(
    is.na(Water_Depth)
  )

depth %>%
  filter(
    Site == "AI44",
    Rel_Depth == "B"
  ) %>%
  mutate(
    Water_Depth = as.numeric(Water_Depth)
  ) %>%
  pull(Water_Depth) %>%
  mean(na.rm = TRUE)


sums <- analysis_NA %>%
  group_by(Sample_Date, analyte) %>%
  summarize(total_na = n())


analysis_NA %>% writexl::write_xlsx("data/analysis_NAs.xlsx")
