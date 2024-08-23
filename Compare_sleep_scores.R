

# Function to create continuous time series
create_continuous_time_series <- function(data) {
  data$Time <- as.POSIXct(data$Time, format="%H:%M:%S")
  
  continuous_time_series <- data.frame(Datetime = as.POSIXct(character()), Sleep_Stage = character(), stringsAsFactors = FALSE)
  
  # Loop through each row of the original data
  for (i in 1:nrow(data)) {
    start_time <- data$Time[i]
    duration <- data$Duration[i]
    sleep_stage <- gsub("Sleep stage ", "", data$Annotation[i])
    
    # Skip if duration is less than 30 seconds (which would result in 0 intervals)
    if (duration < 30) next
    
    # Generate 30-second intervals
    num_intervals <- duration / 30
    intervals <- seq(from = start_time, by = "30 sec", length.out = num_intervals)
    
    # Append to the continuous time series
    continuous_time_series <- rbind(
      continuous_time_series,
      data.frame(Datetime = intervals, Sleep_Stage = sleep_stage, stringsAsFactors = FALSE)
    )
  }
  
  return(continuous_time_series)
}


data <- cathy_scores
cathy <- create_continuous_time_series(data)
cathy <- cathy[c(531:773),]

sleepscorer <- create_continuous_time_series(sleepscorer)
sleepscorer <- sleepscorer[c(532:773),]
  
#get new datasets and trim them
len_cathy <- nrow(cathy)
len_sleepscorer <- nrow(sleepscorer)
min_len <- min(len_cathy, len_sleepscorer)
cathy <- cathy[1:min_len, ]
sleepscorer <- sleepscorer[1:min_len, ]


x<-cbind(cathy[c(1:242),],sleepscorer)


merged_data <- merge(cathy, sleepscorer, by = "Datetime", suffixes = c("_1", "_2"))

# Compare the Sleep_Stage columns
merged_data$Agreement <- merged_data$Sleep_Stage_1 == merged_data$Sleep_Stage_2

# Calculate the percentage agreement
percentage_agreement <- mean(merged_data$Agreement) * 100


percentage_agreement

x$Agreement <- x$Sleep_Stage_1 == x$Sleep_Stage_2

write.csv(x, file="comparisonCW.csv")
