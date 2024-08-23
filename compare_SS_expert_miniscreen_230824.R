library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(zoo)
library(psych)


expert_scores <-"C:/Users/Admin/University of Edinburgh/Ambient-BD - Documents/Workstream 1_Assessing data collection methods/PSG Datasets/haaglanden-medisch-centrum-sleep-staging-database-1.1/recordings/SN001_sleepscoring.txt"

researcher_scores <- "C:/Users/Admin/Downloads/cathy_SN001_scores"

#######################################################
#
# get the expert scores in the right format
#
########################################################


scores <- read.csv(expert_scores)

#remove lights on and off row
scores <- scores[is.na(scores$Linked.channel) | scores$Linked.channel == " ", ]
scores$Time <- strptime(scores$Time, format = " %H.%M.%S")

# get the sleep stage data
set_value <- function(x) {
  if (grepl("N2", x)) {
    return("N2")
  } else if (grepl("N1", x)) {
    return("N1")
  } else if (grepl("N3", x)) {
    return("N3")
  } else if (grepl("R", x)) {
    return("REM")
  } else if (grepl("W", x)) {
    return("W")
  } else {
    return(NA)
  }
}

# Loop through each row and apply the function
for (i in 1:nrow(scores)) {
  scores$sleep[i] <- set_value(scores[i,5])
}

expert_data <- scores

#######################################################
#
# get the miniscreen scores in the right format
#
########################################################

# Read the data from the text file
lines <- readLines(researcher_scores)
 
#get the date
base_date <- lines[grep("Rec. date:", lines) + 1]  

# extract only the hypogram data lines
start_index <- grep("Events Channel_\\d+ Hypnogr.", lines) + 2  #need to make the hypogr channel a wildcard as it varies

# find finish index sometimes indicated by "Channel_" otherwise use the end of file
channel <- grep("Channel_", lines) # this gets all "Channel_"
finish_index <- channel[channel > start_index] #this gets the next channel after the start of hyponogram

# Determine finish_index allowing for there being no more channels
finish_index <- if (length(finish_index) > 0) {
  min(channel)  # Use the first occurrence after start_index
} else {
  length(lines)  # Use the last line if no subsequent "Channel_" is found
}

# Extract hypnogram data lines
hypno_lines <- lines[start_index:(finish_index - 1)]

# Extract the time part from each line
times <- sapply(hypno_lines, function(line) {
  sub("Start: ([0-9]{2}:[0-9]{2}:[0-9]{2});.*", "\\1", line)
})

# Extract codes based on the presence of specific keywords
sleep_scores <- sapply(hypno_lines, function(line) {
  if (grepl("Awake", line)) {
    "W"
  } else if (grepl("N2", line)) {
    "N2"
  } else if (grepl("N1", line)) {
    "N1"
  } else if (grepl("N3", line)) {
    "N3"
  } else if (grepl("R", line)) {
    "REM"
  } else {
    NA
  }
})

# Create a data frame
sleepdata <- data.frame(Time = times, SleepScore = sleep_scores, stringsAsFactors = FALSE, row.names = NULL)


# Convert Time to POSIXct with a default date
sleepdata$Time <- as.POSIXct(sleepdata$Time, format = "%H:%M:%S", tz = "UTC")

# make new dataframe continuous time series
start_time <- sleepdata$Time[1]
end_time <- sleepdata$Time[nrow(sleepdata)]+86400
complete_time_sequence <- seq(from = start_time, to = end_time, by = "30 secs")
complete_time_df <- data.frame(Time = complete_time_sequence)

# get the PSG data into this frame
researcher_data <- merge(complete_time_df, sleepdata, by = "Time", all.x = TRUE)

# Fill missing SleepScore values with the last observation carried forward
researcher_data$SleepScore <- na.locf(researcher_data$SleepScore)


#########################################################################
#
# comparing miniview and expert scores
#
#########################################################################

# get the times consistent
expert_data$Time <- as.POSIXct(expert_data$Time, format = "%H:%M:%S", tz = "UTC")

# Extract the time portion only from DateTime
researcher_data$time_only <- format(researcher_data$Time, format = "%H:%M:%S")
expert_data$time_only <- format(expert_data$Time, format = "%H:%M:%S")
 
# make sure they are the same
researcher_data$time_only == expert_data$time_only

# make a dataframe with all scores
merged_data <- merge(researcher_data, expert_data, by = "time_only")

# Compare the Sleep_Stage columns
merged_data$Agreement <- merged_data$SleepScore == merged_data$sleep

# Calculate the percentage agreement
mean(merged_data$Agreement) * 100

# cohens kappa
cohen.kappa(cbind(merged_data$SleepScore, merged_data$sleep))
