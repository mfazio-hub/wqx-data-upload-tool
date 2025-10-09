# Use this to check what data is already in WQX
df <- dataRetrieval::readWQPdata(
  project = "SRC-SRSWQ",
  service = "ResultWQX3",
  dataProfile = "basicPhysChem"
)

# View unique dates
unique(df$Activity_StartDate)


sel <- df %>% select(c(Result_Characteristic, Result_MeasureUnit))

uniques <- unique(df[, c("Result_Characteristic", "Result_MeasureUnit")])
