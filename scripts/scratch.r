source("scripts/lab-analysis-data-pivot.R")

analysis_NA <- final_table %>%
  filter(
    is.na(`date run`)
  )

unique(analysis_NA$ID)
unique(analysis_NA$Sample_Date)
unique(analysis_NA$analyte)

sums <- analysis_NA %>%
  group_by(Sample_Date, analyte) %>%
  summarize(total_na = n())


analysis_NA %>% writexl::write_xlsx("data/analysis_NAs.xlsx")
