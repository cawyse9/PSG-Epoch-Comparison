install.packages("edfReader")
library(edfReader)

setwd("C:/Users/Admin/University of Edinburgh/Ambient-BD - Documents/Workstream 1_Assessing data collection methods/PSG Datasets/haaglanden-medisch-centrum-sleep-staging-database-1.1/recordings")

# Load the EDF file
edf_file <- readEdfHeader("SN001.edf")

# List of signals in the EDF file
signals <- readEdfSignals(edf_file)

# Example: subset the first signal
first_signal <- signals[[1]]

# Specify the subset range
start_sample <- 1
end_sample <- 1000  # e.g., first 1000 samples
subset_signal <- first_signal$signal[start_sample:end_sample]

# Do something with the subset_signal
print(subset_signal)