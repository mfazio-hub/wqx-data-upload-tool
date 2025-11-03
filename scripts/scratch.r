analysis_NA <- final_table %>%
  filter(
    is.na(`date run`)
  ) %>%
  select(
    ID,
    Layer.x,
    Date,
    analyte
  )

unique(analysis_NA$ID)
unique(analysis_NA$Date)
unique(analysis_NA$analyte)

sums <- analysis_NA %>% group_by(ID, Date) %>% summarize(total_na = n())
