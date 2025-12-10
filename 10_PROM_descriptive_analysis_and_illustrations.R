#THIS SCRIPT IS FOR DESCRIPTIVE ANALYSIS OF DATA FROM THE EQ-5D-5L AND WOUND-QOL QUESTIONNAIRES

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata2.csv')
apple_data <- read.csv ('Path_to_the_folder_in_the_working_directory/apple_data.csv')
orange_data <- read.csv ('Path_to_the_folder_in_the_working_directory/orange_data.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:
library(dplyr)
library(ggplot2) 
library(tidyr)
library(xlsx)

#____________________________________________________________________________________________________

# We create a tibble of all the continuous PROM data variables that we want to perform descriptive statistics on:

cont_PROM_variables <- RCTdata3 %>% 
  select(health_score_baseline, #baseline VAS score from EQ-5D-5L
         health_score_end, #end of follow-up VAS score from EQ-5D-5L
         change_VAS_score,#all variables beginning with change represent change in respective scores from baseline to end of follow-up
         baseline_woundglscore,
         end_woundglscore,
         change_woundglscore,
         eqindex_base, #baseline EQ-5D-5L index score
         eqindex_end,#baseline EQ-5D-5L index score
         change_eqindex  )

#____________________________________________________________________________________
# Define a function for summarizing the continuous variables from the tibble cont_PROM_variables
sumStat <- function(x){
  ntotal=length(x)   
  nmiss=ntotal-table(is.na(x))[1] # antal manglende vaerdier
  n=ntotal-nmiss                  # antal med observerede vaerdier
  mean=mean(x,na.rm=T)
  median=median(x,na.rm=T)
  Q1=quantile(x, probs=.25,na.rm=T)
  Q3=quantile(x, probs=.75,na.rm=T)
  SD=sd(x,na.rm=T)
  min=min(x,na.rm=T)
  max=max(x,na.rm=T)
  samlet <- data.frame(n,nmiss,mean,median,SD, Q1,Q3,min,max)
  row.names(samlet) <- NULL
  return( samlet ) 
}

sumsta_PROM <- cont_PROM_variables %>%
  group_by(RCTdata3$masked_group) %>%
  summarise_all(sumStat)

# Transpose the summary table so that randomization groups are columns and variables are rows.
PROMresult_transposed <- t(sumsta_PROM)

# Assign the first row as column names
colnames(PROMresult_transposed) <- PROMresult_transposed[1, ]

# Remove the first row
PROMresult_transposed <- PROMresult_transposed[-1, ]

#Export the summarized PROM results to an Excel file.
write.xlsx(PROMresult_transposed, file='Path_to_the_folder_in_the_working_directory/table_PROM_results.xlsx', sheetName = "PROMresult_transposed", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#___________________________________________________________________________________________________________________

# Investigate missing data for the 5 dimensions in the descriptive system of the EQ-5D-5L.

#Missing data from the baseline descriptive system:
variables_of_interest_base <- c(
  "eq5d_mb_5l_den_dan",
  "eq5d_sc_5l_den_dan",
  "eq5d_ua_5l_den_dan",
  "eq5d_pd_5l_den_dan",
  "eq5d_ad_5l_den_dan"
)

# Create a logical vector indicating rows with any missing data in the specified variables that variables_of_interest_base contains.
missing_data_base <- apply(RCTdata3[variables_of_interest_base], 1, function(row) any(is.na(row)))

# Subset the data to include only rows with missing data
missing_data_rows_base <- RCTdata3[missing_data_base, ]

# Count the number of rows with missing data for each randomization group
missing_data_count_by_group_base <- table(missing_data_rows_base$masked_group)

# Print the count - counting how many patients had missing data for at least one of the dimensions:
print(missing_data_count_by_group_base)

#Missing data from the end of follow-up descriptive system:
variables_of_interest <- c(
  "eq5d_mb_5l_den_dan_slut",
  "eq5d_sc_5l_den_dan_slut",
  "eq5d_ua_5l_den_dan_slut",
  "eq5d_pd_5l_den_dan_slut",
  "eq5d_ad_5l_den_dan_slut"
)

# Create a logical vector indicating rows with any missing data in the specified variables listed in variables_of_interest
missing_data <- apply(RCTdata3[variables_of_interest], 1, function(row) any(is.na(row)))

# Subset the data to include only rows with missing data
missing_data_rows <- RCTdata3[missing_data, ]

# Count the number of rows with missing data for each randomization group
missing_data_count_by_group <- table(missing_data_rows$masked_group)

# Print the count - counting how many patients had missing data for at least one of the dimensions:
print(missing_data_count_by_group)

#____________________________________________________________________
#Create histograms for each group illustrating EQ-5D-5L VAS results at baseline and end of follow-up.

#1)Baseline EQ-5D-5L VAS histogram for the Apple group.
baseEQVAS_histogram_apple <- ggplot(apple_data, aes(x = health_score_baseline)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Baseline EQ-5D-5L VAS frequency distribution for the apple group", x = "EQ-5D-5L VAS score", y = "Frequency")

baseEQVAS_histogram_apple

#Save the baseline EQ-5D-5L VAS histogram for the Apple group
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_EQVAS_applegroup_histogram.pdf", baseEQVAS_histogram_apple)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_EQVAS_applegroup_histogram.jpeg", baseEQVAS_histogram_apple)

#_____________
#2)Baseline EQ-5D-5L VAS histogram for the Orange group.
baseEQVAS_histogram_orange <- ggplot(orange_data, aes(x = health_score_baseline)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "Baseline EQ-5D-5L VAS frequency distribution for the Orange group", x = "EQ-5D-5L VAS score", y = "Frequency")

#Save the baseline EQ-5D-5L VAS histogram for the Orange group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_EQVAS_orangegroup_histogram.pdf", baseEQVAS_histogram_orange)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_EQVAS_orangegroup_histogram.jpeg", baseEQVAS_histogram_orange)

#________________

#3)End-of-follow-up EQ-5D-5L VAS histogram for the Apple group.
endEQVAS_histogram_apple <- ggplot(apple_data, aes(x = health_score_end)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "End EQ-5D-5L VAS frequency distribution for the apple group", x = "EQ-5D-5L VAS score", y = "Frequency")

endEQVAS_histogram_apple

#Save the end-of-follow-up EQ-5D-5L VAS histogram for the Apple group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_EQVAS_applegroup_histogram.pdf", endEQVAS_histogram_apple)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_EQVAS_applegroup_histogram.jpeg", endEQVAS_histogram_apple)

#_____________
#4)End-of-follow-up EQ-5D-5L VAS histogram for the Orange group.
endEQVAS_histogram_orange <- ggplot(orange_data, aes(x = health_score_end)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  labs(title = "End EQ-5D-5L VAS frequency distribution for the Orange group", x = "EQ-5D-5L VAS score", y = "Frequency")

endEQVAS_histogram_orange

#Save the end-of-follow-up EQ-5D-5L VAS histogram for the Orange group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_EQVAS_orangegroup_histogram.pdf", endEQVAS_histogram_orange)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_EQVAS_orangegroup_histogram.jpeg", endEQVAS_histogram_orange)

#_________________________________________________________________________________________________________________________
#HISTOGRAM FOR WOUND-QOL GLOBAL SCORE at baseline and end of follow-up:
#1) Baseline Wound-QoL global score histogram for the Apple group.
base_w_global_score_histogram_apple <- ggplot(apple_data, aes(x = baseline_woundglscore)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Baseline Wound-QoL global score distribution for the apple group", x = "Wound_QoL global score", y = "Frequency")
base_w_global_score_histogram_apple

#Save the baseline Wound-QoL histogram for the Apple group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_woundglobal_applegroup_histogram.pdf", base_w_global_score_histogram_apple)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_woundglobal_applegroup_histogram.jpeg", base_w_global_score_histogram_apple)

#_____________
#2)Baseline Wound-QoL global score histogram for the Orange group.
base_w_global_score_histogram_orange <- ggplot(orange_data, aes(x = baseline_woundglscore)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  labs(title = "Baseline Wound-QoL global score distribution for the Orange group", x = "Wound_QoL global score", y = "Frequency")
base_w_global_score_histogram_orange

#Save the baseline Wound-QoL histogram for the Orange group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_woundglobal_orangegroup_histogram.pdf", base_w_global_score_histogram_orange)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/baseline_woundglobal_orangegroup_histogram.jpeg", base_w_global_score_histogram_orange)

#________________

#3)End-of-follow-up Wound-QoL global score histogram for the Apple group.

end_w_global_score_histogram_apple <- ggplot(apple_data, aes(x = end_woundglscore)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "End Wound-QoL global score distribution for the apple group", x = "Wound_QoL global score", y = "Frequency")
end_w_global_score_histogram_apple

#Save the end-of-follow-up Wound-QoL histogram for the Apple group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_woundglobalscore_applegroup_histogram.pdf", end_w_global_score_histogram_apple)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_woundglobalscore_applegroup_histogram.jpeg", end_w_global_score_histogram_apple)

#_____________
#4)End-of-follow-up Wound-QoL global score histogram for the Orange group.

  end_w_global_score_histogram_orange <- ggplot(orange_data, aes(x = end_woundglscore)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  labs(title = "End Wound-QoL global score distribution for the Orange group", x = "Wound_QoL global score", y = "Frequency")
end_w_global_score_histogram_orange

#Save the end-of-follow-up Wound-QoL histogram for the Orange group.
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_woundglobalscore_orangegroup_histogram.pdf", end_w_global_score_histogram_orange)
ggsave("Path_to_the_folder_in_the_working_directory/PROM_diagrams/END_woundglobalscore_orangegroup_histogram.jpeg", end_w_global_score_histogram_orange)
