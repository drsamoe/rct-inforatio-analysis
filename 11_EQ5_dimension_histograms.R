#THIS SCRIPT IS FOR CREATING HISTOGRAMS OF DIMENSION SCORES FROM THE EQ-5D-5L DESCRIPTIVE SYSTEM

#     1) upload the blinded datasets for the apple_group and orange_group, respectively:
apple_data <- read.csv ('Path_to_the_folder_in_the_working_directory/apple_data.csv')
orange_data <- read.csv ('Path_to_the_folder_in_the_working_directory/orange_data.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:

install.packages('stringr')
library(stringr)

install.packages('gridExtra')
library(gridExtra)

library(dplyr)

library(ggplot2) 

library(tidyr)

#________________________________________________________

#Summary of the variable names for the five dimensions in the descriptive system of EQ-5D-5L
# baseline variables:
# mobility: eq5d_mb_5l_den_dan
# selfcare: eq5d_sc_5l_den_dan
# usual activity: eq5d_ua_5l_den_dan
# pain: eq5d_pd_5l_den_dan
# anxiety depression: eq5d_ad_5l_den_dan

#EQ 5 end of follow-up variables:
# mobility: eq5d_mb_5l_den_dan_slut
# selfcare: eq5d_sc_5l_den_dan_slut
# usual activity: eq5d_ua_5l_den_dan_slut
# pain: eq5d_pd_5l_den_dan_slut
# anxiety depression: eq5d_ad_5l_den_dan_slut

#______________________________________________________________________________________________
# CREATING HISTOGRAMS FOR EACH OF THE 5 DIMENSIONS, SEPARATED BY RANDOMIZATION GROUP

#MOBILITY FOR APPLE_GROUP
#Make a dataframe only including mobility data for baseline and end of follow-up:
apple_mobility_data <- data.frame(
  eq5d_mb_5l_den_dan = apple_data$eq5d_mb_5l_den_dan,
  eq5d_mb_5l_den_dan_slut = apple_data$eq5d_mb_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score from the dataframe apple_mobility_data
# Note: this means that if patients miss either the baseline or end of follow-up score, patients are excluded from the histogram of this specific dimension (mobility)
apple_mobility_data <- na.omit(apple_mobility_data)

# Retrieve tables of the variables as a descriptive analysis:
table(apple_mobility_data$eq5d_mb_5l_den_dan)
table(apple_mobility_data$eq5d_mb_5l_den_dan_slut)

# Create data frames that calculate frequencies of scores for baseline and end of follow-up scores: 
# Calculate frequencies for baseline mobility score - eq5d_mb_5l_den_dan
freq_eq5d_mb_5l_den_dan <- apple_mobility_data %>%
  group_by(eq5d_mb_5l_den_dan) %>%
  summarize(freq_eq5d_mb_5l_den_dan = n())

# Calculate frequencies for end of follow-up mobility score - eq5d_mb_5l_den_dan_slut
freq_eq5d_mb_5l_den_dan_slut <- apple_mobility_data %>%
  group_by(eq5d_mb_5l_den_dan_slut) %>%
  summarize(freq_eq5d_mb_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_mb_5l_den_dan <- categories %>%
  left_join(freq_eq5d_mb_5l_den_dan, by = c('Category' = 'eq5d_mb_5l_den_dan')) %>%
  mutate(freq_eq5d_mb_5l_den_dan = ifelse(is.na(freq_eq5d_mb_5l_den_dan), 0, freq_eq5d_mb_5l_den_dan))

freq_eq5d_mb_5l_den_dan_slut_apple <- categories %>%
  left_join(freq_eq5d_mb_5l_den_dan_slut, by = c('Category' = 'eq5d_mb_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_mb_5l_den_dan_slut = ifelse(is.na(freq_eq5d_mb_5l_den_dan_slut), 0, freq_eq5d_mb_5l_den_dan_slut))

# Create a dataframe with category names, freq_eq5d_mb_5l_den_dan, and freq_eq5d_mb_5l_den_dan_slut
apple_mobility_data2 <- data.frame(
  Category = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'),
  freq_eq5d_mb_5l_den_dan = freq_eq5d_mb_5l_den_dan$freq_eq5d_mb_5l_den_dan,
  freq_eq5d_mb_5l_den_dan = freq_eq5d_mb_5l_den_dan_slut$freq_eq5d_mb_5l_den_dan_slut
)

# Print the new dataframe
print(apple_mobility_data2)

# Reshape the frequency data into a longer format
apple_mobility_data3 <- apple_mobility_data2 %>%
  pivot_longer(cols = c(freq_eq5d_mb_5l_den_dan, freq_eq5d_mb_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
apple_mobility_data3$Category <- factor(apple_mobility_data3$Category,
                                        levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'),
                                        labels = str_wrap(c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'), width = 10))

#Now the histogram can be created
apple_mobility_plot <- ggplot(apple_mobility_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_mb_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Mobility in the apple_group",
       x = "",
       y = "Number of respondents") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the x-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 14
    legend.text = element_text(size = 18) # Increase the size of the legend text to 14
  ) +
  theme(legend.position = "none") + # Remove the legend
  ylim(0, 15) # Set y-axis range from 0 to 15 # Remove the legend

print(apple_mobility_plot)

#save the histogram
ggsave("Path_to_the_folder_in_the_working_directory/mobility_change_apple_group.pdf", apple_mobility_plot)
ggsave("Path_to_the_folder_in_the_working_directory/mobility_change_apple_group.jpeg", apple_mobility_plot)


# MOBILITY FOR ORANGE_GROUP
#Make a dataframe only including mobility data for baseline and end of follow-up:
orange_mobility_data <- data.frame(
  eq5d_mb_5l_den_dan = orange_data$eq5d_mb_5l_den_dan,
  eq5d_mb_5l_den_dan_slut = orange_data$eq5d_mb_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score from the dataframe 
orange_mobility_data <- na.omit(orange_mobility_data)

# Retrieve tables of the variables as a descriptive analysis:
table(orange_mobility_data$eq5d_mb_5l_den_dan)
table(orange_mobility_data$eq5d_mb_5l_den_dan_slut)

# Create data frames that calculate frequencies of scores for baseline and end of follow-up scores: 
# Calculate frequencies for baseline mobility score - eq5d_mb_5l_den_dan
freq_eq5d_mb_5l_den_dan <- orange_mobility_data %>%
  group_by(eq5d_mb_5l_den_dan) %>%
  summarize(freq_eq5d_mb_5l_den_dan = n())

# Calculate frequencies for end of follow-up mobility score - eq5d_mb_5l_den_dan_slut
freq_eq5d_mb_5l_den_dan_slut <- orange_mobility_data %>%
  group_by(eq5d_mb_5l_den_dan_slut) %>%
  summarize(freq_eq5d_mb_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_mb_5l_den_dan <- categories %>%
  left_join(freq_eq5d_mb_5l_den_dan, by = c('Category' = 'eq5d_mb_5l_den_dan')) %>%
  mutate(freq_eq5d_mb_5l_den_dan = ifelse(is.na(freq_eq5d_mb_5l_den_dan), 0, freq_eq5d_mb_5l_den_dan))

freq_eq5d_mb_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_mb_5l_den_dan_slut, by = c('Category' = 'eq5d_mb_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_mb_5l_den_dan_slut = ifelse(is.na(freq_eq5d_mb_5l_den_dan_slut), 0, freq_eq5d_mb_5l_den_dan_slut))

# Create a dataframe with category names, freq_eq5d_mb_5l_den_dan, and freq_eq5d_mb_5l_den_dan_slut
orange_mobility_data2 <- data.frame(
  Category = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'),
  freq_eq5d_mb_5l_den_dan = freq_eq5d_mb_5l_den_dan$freq_eq5d_mb_5l_den_dan,
  freq_eq5d_mb_5l_den_dan_slut = freq_eq5d_mb_5l_den_dan_slut$freq_eq5d_mb_5l_den_dan_slut
)

# Print the new dataframe
print(orange_mobility_data2)

# Reshape the frequency data into a longer format
orange_mobility_data3 <- orange_mobility_data2 %>%
  pivot_longer(cols = c(freq_eq5d_mb_5l_den_dan, freq_eq5d_mb_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
orange_mobility_data3$Category <- factor(orange_mobility_data3$Category,
                                         levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'),
                                         labels = str_wrap(c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'), width = 10))

#Now the histogram can be created
orange_mobility_plot <- ggplot(orange_mobility_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_mb_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Mobility in the orange_group",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the y-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 15
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.0, "cm")# Increase the size of the legend text to 14
  ) +
  guides(fill = guide_legend(title = "")
  ) +
  ylim(0, 15) # Set y-axis range from 0 to 15

print(orange_mobility_plot)

#save the histogram
ggsave("Path_to_the_folder_in_the_working_directory/mobility_change_orange_group.pdf", orange_mobility_plot)
ggsave("Path_to_the_folder_in_the_working_directory/mobility_change_orange_group.jpeg", orange_mobility_plot)

#_____
#Combine the mobility plots:
# Combine the two plots side by side
combined_plot_mobility <- grid.arrange(apple_mobility_plot, orange_mobility_plot, ncol = 2)
# Save the combined plot
ggsave("Path_to_the_folder_in_the_working_directory/mobility_change_combined.pdf", combined_plot_mobility, width = 16, height = 6)

ggsave("Path_to_the_folder_in_the_working_directory/mobility_change_combined.jpeg", combined_plot_mobility, width = 16, height = 6)


#____________________________________________________

#SELFCARE FOR APPLE_GROUP
# Create the initial data frame
apple_selfcare_data <- data.frame(
  eq5d_sc_5l_den_dan = apple_data$eq5d_sc_5l_den_dan,
  eq5d_sc_5l_den_dan_slut = apple_data$eq5d_sc_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score
apple_selfcare_data <- na.omit(apple_selfcare_data)

# Retrieve tables of the variables as a descriptive analysis:
table(apple_selfcare_data$eq5d_sc_5l_den_dan)
table(apple_selfcare_data$eq5d_sc_5l_den_dan_slut)

# Calculate frequencies baseline score
freq_eq5d_sc_5l_den_dan <- apple_selfcare_data %>%
  group_by(eq5d_sc_5l_den_dan) %>%
  summarize(freq_eq5d_sc_5l_den_dan = n())

# Calculate frequencies for end of follow-up score
freq_eq5d_sc_5l_den_dan_slut <- apple_selfcare_data %>%
  group_by(eq5d_sc_5l_den_dan_slut) %>%
  summarize(freq_eq5d_sc_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_sc_5l_den_dan <- categories %>%
  left_join(freq_eq5d_sc_5l_den_dan, by = c('Category' = 'eq5d_sc_5l_den_dan')) %>%
  mutate(freq_eq5d_sc_5l_den_dan = ifelse(is.na(freq_eq5d_sc_5l_den_dan), 0, freq_eq5d_sc_5l_den_dan))

freq_eq5d_sc_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_sc_5l_den_dan_slut, by = c('Category' = 'eq5d_sc_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_sc_5l_den_dan_slut = ifelse(is.na(freq_eq5d_sc_5l_den_dan_slut), 0, freq_eq5d_sc_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_sc_5l_den_dan, and freq_eq5d_sc_5l_den_dan_slut
apple_selfcare_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_sc_5l_den_dan = freq_eq5d_sc_5l_den_dan$freq_eq5d_sc_5l_den_dan,
  freq_eq5d_sc_5l_den_dan_slut = freq_eq5d_sc_5l_den_dan_slut$freq_eq5d_sc_5l_den_dan_slut
)

# Print the new dataframe
print(apple_selfcare_data2)

# Reshape the frequency data into a longer format
apple_selfcare_data3 <- apple_selfcare_data2 %>%
  pivot_longer(cols = c(freq_eq5d_sc_5l_den_dan, freq_eq5d_sc_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
apple_selfcare_data3$Category <- factor(apple_selfcare_data3$Category,
                                        levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress'),
                                        labels = str_wrap(c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress'), width = 10))

# Create the plot
apple_selfcare_plot <- ggplot(apple_selfcare_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_sc_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Self-care in the apple_group",
       x = "",
       y = "Number of respondents") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the x-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 14
    legend.text = element_text(size = 18) # Increase the size of the legend text to 14
  ) +
  theme(legend.position = "none") +
  ylim(0, 15) # Set y-axis range from 0 to 15 # Remove the legend

print(apple_selfcare_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/selfcare_change_apple_group.pdf", apple_selfcare_plot)
ggsave("Path_to_the_folder_in_the_working_directory/selfcare_change_apple_group.jpeg", apple_selfcare_plot)


# SELFCARE FOR ORANGE_GROUP
# Create the initial data frame
orange_selfcare_data <- data.frame(
  eq5d_sc_5l_den_dan = orange_data$eq5d_sc_5l_den_dan,
  eq5d_sc_5l_den_dan_slut = orange_data$eq5d_sc_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
orange_selfcare_data <- na.omit(orange_selfcare_data)

# Retrieve tables of the variables as a descriptive analysis:
table(orange_selfcare_data$eq5d_sc_5l_den_dan)
table(orange_selfcare_data$eq5d_sc_5l_den_dan_slut)

# Calculate frequencies for eq5d_sc_5l_den_dan
freq_eq5d_sc_5l_den_dan <- orange_selfcare_data %>%
  group_by(eq5d_sc_5l_den_dan) %>%
  summarize(freq_eq5d_sc_5l_den_dan = n())

# Calculate frequencies for eq5d_sc_5l_den_dan_slut
freq_eq5d_sc_5l_den_dan_slut <- orange_selfcare_data %>%
  group_by(eq5d_sc_5l_den_dan_slut) %>%
  summarize(freq_eq5d_sc_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_sc_5l_den_dan <- categories %>%
  left_join(freq_eq5d_sc_5l_den_dan, by = c('Category' = 'eq5d_sc_5l_den_dan')) %>%
  mutate(freq_eq5d_sc_5l_den_dan = ifelse(is.na(freq_eq5d_sc_5l_den_dan), 0, freq_eq5d_sc_5l_den_dan))

freq_eq5d_sc_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_sc_5l_den_dan_slut, by = c('Category' = 'eq5d_sc_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_sc_5l_den_dan_slut = ifelse(is.na(freq_eq5d_sc_5l_den_dan_slut), 0, freq_eq5d_sc_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_sc_5l_den_dan, and freq_eq5d_sc_5l_den_dan_slut
orange_selfcare_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_sc_5l_den_dan = freq_eq5d_sc_5l_den_dan$freq_eq5d_sc_5l_den_dan,
  freq_eq5d_sc_5l_den_dan_slut = freq_eq5d_sc_5l_den_dan_slut$freq_eq5d_sc_5l_den_dan_slut
)

# Print the new dataframe
print(orange_selfcare_data2)

# Reshape the frequency data into a longer format
orange_selfcare_data3 <- orange_selfcare_data2 %>%
  pivot_longer(cols = c(freq_eq5d_sc_5l_den_dan, freq_eq5d_sc_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
orange_selfcare_data3$Category <- factor(orange_selfcare_data3$Category,
                                         levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress'),
                                         labels = str_wrap(c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress'), width = 10))

# Create the plot
orange_selfcare_plot <- ggplot(orange_selfcare_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_sc_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Self-care in the orange_group",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the y-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 15
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.0, "cm") # Increase the size of the legend text to 14
  ) +
  guides(fill = guide_legend(title = "")) +
  ylim(0, 15) # Set y-axis range from 0 to 15

print(orange_selfcare_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/selfcare_change_orange_group.pdf", orange_selfcare_plot)
ggsave("Path_to_the_folder_in_the_working_directory/selfcare_change_orange_group.jpeg", orange_selfcare_plot)

#_____
# Combine the selfcare plots:
# Combine the two plots side by side
combined_plot_selfcare <- grid.arrange(apple_selfcare_plot, orange_selfcare_plot, ncol = 2)
# Save the combined plot
ggsave("Path_to_the_folder_in_the_working_directory/selfcare_change_combined.pdf", combined_plot_selfcare, width = 16, height = 6)

ggsave("Path_to_the_folder_in_the_working_directory/selfcare_change_combined.jpeg", combined_plot_selfcare, width = 16, height = 6)


#________________________________________
# USUAL ACTIVITIES FOR APPLE_GROUP
# Create the initial data frame
apple_usual_activities_data <- data.frame(
  eq5d_ua_5l_den_dan = apple_data$eq5d_ua_5l_den_dan,
  eq5d_ua_5l_den_dan_slut = apple_data$eq5d_ua_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
apple_usual_activities_data <- na.omit(apple_usual_activities_data)

# Retrieve tables of the variables as a descriptive analysis:
table(apple_usual_activities_data$eq5d_ua_5l_den_dan)
table(apple_usual_activities_data$eq5d_ua_5l_den_dan_slut)

# Calculate frequencies for eq5d_ua_5l_den_dan
freq_eq5d_ua_5l_den_dan <- apple_usual_activities_data %>%
  group_by(eq5d_ua_5l_den_dan) %>%
  summarize(freq_eq5d_ua_5l_den_dan = n())

# Calculate frequencies for eq5d_ua_5l_den_dan_slut
freq_eq5d_ua_5l_den_dan_slut <- apple_usual_activities_data %>%
  group_by(eq5d_ua_5l_den_dan_slut) %>%
  summarize(freq_eq5d_ua_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_ua_5l_den_dan <- categories %>%
  left_join(freq_eq5d_ua_5l_den_dan, by = c('Category' = 'eq5d_ua_5l_den_dan')) %>%
  mutate(freq_eq5d_ua_5l_den_dan = ifelse(is.na(freq_eq5d_ua_5l_den_dan), 0, freq_eq5d_ua_5l_den_dan))

freq_eq5d_ua_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_ua_5l_den_dan_slut, by = c('Category' = 'eq5d_ua_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_ua_5l_den_dan_slut = ifelse(is.na(freq_eq5d_ua_5l_den_dan_slut), 0, freq_eq5d_ua_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_ua_5l_den_dan, and freq_eq5d_ua_5l_den_dan_slut
apple_usual_activities_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_ua_5l_den_dan = freq_eq5d_ua_5l_den_dan$freq_eq5d_ua_5l_den_dan,
  freq_eq5d_ua_5l_den_dan_slut = freq_eq5d_ua_5l_den_dan_slut$freq_eq5d_ua_5l_den_dan_slut
)

# Print the new dataframe
print(apple_usual_activities_data2)

# Reshape the frequency data into a longer format
apple_usual_activities_data3 <- apple_usual_activities_data2 %>%
  pivot_longer(cols = c(freq_eq5d_ua_5l_den_dan, freq_eq5d_ua_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
apple_usual_activities_data3$Category <- factor(apple_usual_activities_data3$Category,
                                                levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities'),
                                                labels = str_wrap(c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities'), width = 10))

# Create the plot
apple_usual_activities_plot <- ggplot(apple_usual_activities_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_ua_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Usual activities in the apple_group",
       x = "",
       y = "Number of respondents") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the x-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 14
    legend.text = element_text(size = 18) # Increase the size of the legend text to 14
  ) +
  theme(legend.position = "none") +
  ylim(0, 15) # Set y-axis range from 0 to 15 # Remove the legend

print(apple_usual_activities_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/usual_activities_change_apple_group.pdf", apple_usual_activities_plot)
ggsave("Path_to_the_folder_in_the_working_directory/usual_activities_change_apple_group.jpeg", apple_usual_activities_plot)


# USUAL ACTIVITIES FOR ORANGE_GROUP
# Create the initial data frame
orange_usual_activities_data <- data.frame(
  eq5d_ua_5l_den_dan = orange_data$eq5d_ua_5l_den_dan,
  eq5d_ua_5l_den_dan_slut = orange_data$eq5d_ua_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
orange_usual_activities_data <- na.omit(orange_usual_activities_data)

# Retrieve tables of the variables as a descriptive analysis:
table(orange_usual_activities_data$eq5d_ua_5l_den_dan)
table(orange_usual_activities_data$eq5d_ua_5l_den_dan_slut)

# Calculate frequencies for eq5d_ua_5l_den_dan
freq_eq5d_ua_5l_den_dan <- orange_usual_activities_data %>%
  group_by(eq5d_ua_5l_den_dan) %>%
  summarize(freq_eq5d_ua_5l_den_dan = n())

# Calculate frequencies for eq5d_ua_5l_den_dan_slut
freq_eq5d_ua_5l_den_dan_slut <- orange_usual_activities_data %>%
  group_by(eq5d_ua_5l_den_dan_slut) %>%
  summarize(freq_eq5d_ua_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_ua_5l_den_dan <- categories %>%
  left_join(freq_eq5d_ua_5l_den_dan, by = c('Category' = 'eq5d_ua_5l_den_dan')) %>%
  mutate(freq_eq5d_ua_5l_den_dan = ifelse(is.na(freq_eq5d_ua_5l_den_dan), 0, freq_eq5d_ua_5l_den_dan))

freq_eq5d_ua_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_ua_5l_den_dan_slut, by = c('Category' = 'eq5d_ua_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_ua_5l_den_dan_slut = ifelse(is.na(freq_eq5d_ua_5l_den_dan_slut), 0, freq_eq5d_ua_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_ua_5l_den_dan, and freq_eq5d_ua_5l_den_dan_slut
orange_usual_activities_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_ua_5l_den_dan = freq_eq5d_ua_5l_den_dan$freq_eq5d_ua_5l_den_dan,
  freq_eq5d_ua_5l_den_dan_slut = freq_eq5d_ua_5l_den_dan_slut$freq_eq5d_ua_5l_den_dan_slut
)

# Print the new dataframe
print(orange_usual_activities_data2)

# Reshape the frequency data into a longer format
orange_usual_activities_data3 <- orange_usual_activities_data2 %>%
  pivot_longer(cols = c(freq_eq5d_ua_5l_den_dan, freq_eq5d_ua_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
orange_usual_activities_data3$Category <- factor(orange_usual_activities_data3$Category,
                                                 levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities'),
                                                 labels = str_wrap(c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities'), width = 10))

# Create the plot
orange_usual_activities_plot <- ggplot(orange_usual_activities_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_ua_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Usual activities in the orange_group",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the y-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 15
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.0, "cm") # Increase the size of the legend text to 14
  ) +
  guides(fill = guide_legend(title = "")) +
  ylim(0, 15) # Set y-axis range from 0 to 15

print(orange_usual_activities_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/usual_activities_change_orange_group.pdf", orange_usual_activities_plot)
ggsave("Path_to_the_folder_in_the_working_directory/usual_activities_change_orange_group.jpeg", orange_usual_activities_plot)

#_____
# Combine the usual activities plots:
# Combine the two plots side by side
combined_plot_usual_activities <- grid.arrange(apple_usual_activities_plot, orange_usual_activities_plot, ncol = 2)
# Save the combined plot
ggsave("Path_to_the_folder_in_the_working_directory/usual_activities_change_combined.pdf", combined_plot_usual_activities, width = 16, height = 6)

ggsave("Path_to_the_folder_in_the_working_directory/usual_activities_change_combined.jpeg", combined_plot_usual_activities, width = 16, height = 6)



#____________________________
# PAIN_DISCOMFORT FOR APPLE_GROUP
# Create the initial data frame
apple_pain_discomfort_data <- data.frame(
  eq5d_pd_5l_den_dan = apple_data$eq5d_pd_5l_den_dan,
  eq5d_pd_5l_den_dan_slut = apple_data$eq5d_pd_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
apple_pain_discomfort_data <- na.omit(apple_pain_discomfort_data)

# Retrieve tables of the variables as a descriptive analysis:
table(apple_pain_discomfort_data$eq5d_pd_5l_den_dan)
table(apple_pain_discomfort_data$eq5d_pd_5l_den_dan_slut)

# Calculate frequencies for eq5d_pd_5l_den_dan
freq_eq5d_pd_5l_den_dan <- apple_pain_discomfort_data %>%
  group_by(eq5d_pd_5l_den_dan) %>%
  summarize(freq_eq5d_pd_5l_den_dan = n())

# Calculate frequencies for eq5d_pd_5l_den_dan_slut
freq_eq5d_pd_5l_den_dan_slut <- apple_pain_discomfort_data %>%
  group_by(eq5d_pd_5l_den_dan_slut) %>%
  summarize(freq_eq5d_pd_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_pd_5l_den_dan <- categories %>%
  left_join(freq_eq5d_pd_5l_den_dan, by = c('Category' = 'eq5d_pd_5l_den_dan')) %>%
  mutate(freq_eq5d_pd_5l_den_dan = ifelse(is.na(freq_eq5d_pd_5l_den_dan), 0, freq_eq5d_pd_5l_den_dan))

freq_eq5d_pd_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_pd_5l_den_dan_slut, by = c('Category' = 'eq5d_pd_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_pd_5l_den_dan_slut = ifelse(is.na(freq_eq5d_pd_5l_den_dan_slut), 0, freq_eq5d_pd_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_pd_5l_den_dan, and freq_eq5d_pd_5l_den_dan_slut
apple_pain_discomfort_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_pd_5l_den_dan = freq_eq5d_pd_5l_den_dan$freq_eq5d_pd_5l_den_dan,
  freq_eq5d_pd_5l_den_dan_slut = freq_eq5d_pd_5l_den_dan_slut$freq_eq5d_pd_5l_den_dan_slut
)

# Print the new dataframe
print(apple_pain_discomfort_data2)

# Reshape the frequency data into a longer format
apple_pain_discomfort_data3 <- apple_pain_discomfort_data2 %>%
  pivot_longer(cols = c(freq_eq5d_pd_5l_den_dan, freq_eq5d_pd_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
apple_pain_discomfort_data3$Category <- factor(apple_pain_discomfort_data3$Category,
                                               levels = c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort'),
                                               labels = str_wrap(c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort'), width = 10))

# Create the plot
apple_pain_discomfort_plot <- ggplot(apple_pain_discomfort_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_pd_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Pain/Discomfort in the apple_group",
       x = "",
       y = "Number of respondents") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the x-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 14
    legend.text = element_text(size = 18) # Increase the size of the legend text to 14
  ) +
  theme(legend.position = "none") +
  ylim(0, 15) # Set y-axis range from 0 to 15 # Remove the legend

print(apple_pain_discomfort_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/pain_discomfort_change_apple_group.pdf", apple_pain_discomfort_plot)
ggsave("Path_to_the_folder_in_the_working_directory/pain_discomfort_change_apple_group.jpeg", apple_pain_discomfort_plot)


# PAIN_DISCOMFORT FOR ORANGE_GROUP
# Create the initial data frame
orange_pain_discomfort_data <- data.frame(
  eq5d_pd_5l_den_dan = orange_data$eq5d_pd_5l_den_dan,
  eq5d_pd_5l_den_dan_slut = orange_data$eq5d_pd_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
orange_pain_discomfort_data <- na.omit(orange_pain_discomfort_data)

# Retrieve tables of the variables as a descriptive analysis:
table(orange_pain_discomfort_data$eq5d_pd_5l_den_dan)
table(orange_pain_discomfort_data$eq5d_pd_5l_den_dan_slut)

# Calculate frequencies for eq5d_pd_5l_den_dan
freq_eq5d_pd_5l_den_dan <- orange_pain_discomfort_data %>%
  group_by(eq5d_pd_5l_den_dan) %>%
  summarize(freq_eq5d_pd_5l_den_dan = n())

# Calculate frequencies for eq5d_pd_5l_den_dan_slut
freq_eq5d_pd_5l_den_dan_slut <- orange_pain_discomfort_data %>%
  group_by(eq5d_pd_5l_den_dan_slut) %>%
  summarize(freq_eq5d_pd_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_pd_5l_den_dan <- categories %>%
  left_join(freq_eq5d_pd_5l_den_dan, by = c('Category' = 'eq5d_pd_5l_den_dan')) %>%
  mutate(freq_eq5d_pd_5l_den_dan = ifelse(is.na(freq_eq5d_pd_5l_den_dan), 0, freq_eq5d_pd_5l_den_dan))

freq_eq5d_pd_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_pd_5l_den_dan_slut, by = c('Category' = 'eq5d_pd_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_pd_5l_den_dan_slut = ifelse(is.na(freq_eq5d_pd_5l_den_dan_slut), 0, freq_eq5d_pd_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_pd_5l_den_dan, and freq_eq5d_pd_5l_den_dan_slut
orange_pain_discomfort_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_pd_5l_den_dan = freq_eq5d_pd_5l_den_dan$freq_eq5d_pd_5l_den_dan,
  freq_eq5d_pd_5l_den_dan_slut = freq_eq5d_pd_5l_den_dan_slut$freq_eq5d_pd_5l_den_dan_slut
)

# Print the new dataframe
print(orange_pain_discomfort_data2)

# Reshape the frequency data into a longer format
orange_pain_discomfort_data3 <- orange_pain_discomfort_data2 %>%
  pivot_longer(cols = c(freq_eq5d_pd_5l_den_dan, freq_eq5d_pd_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
orange_pain_discomfort_data3$Category <- factor(orange_pain_discomfort_data3$Category,
                                                levels = c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort'),
                                                labels = str_wrap(c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort'), width = 10))

# Create the plot
orange_pain_discomfort_plot <- ggplot(orange_pain_discomfort_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_pd_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Pain/Discomfort in the orange_group",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the y-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 15
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.0, "cm") # Increase the size of the legend text to 14
  ) +
  guides(fill = guide_legend(title = "")) +
  ylim(0, 15) # Set y-axis range from 0 to 15

print(orange_pain_discomfort_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/pain_discomfort_change_orange_group.pdf", orange_pain_discomfort_plot)
ggsave("Path_to_the_folder_in_the_working_directory/pain_discomfort_change_orange_group.jpeg", orange_pain_discomfort_plot)

#_____
# Combine the pain/discomfort plots:
# Combine the two plots side by side
combined_plot_pain_discomfort <- grid.arrange(apple_pain_discomfort_plot, orange_pain_discomfort_plot, ncol = 2)
# Save the combined plot
ggsave("Path_to_the_folder_in_the_working_directory/pain_discomfort_change_combined.pdf", combined_plot_pain_discomfort, width = 16, height = 6)

ggsave("Path_to_the_folder_in_the_working_directory/pain_discomfort_change_combined.jpeg", combined_plot_pain_discomfort, width = 16, height = 6)


#_____________________________________
# ANXIETY_DEPRESSION FOR APPLE_GROUP
# Create the initial data frame
apple_anxiety_depression_data <- data.frame(
  eq5d_ad_5l_den_dan = apple_data$eq5d_ad_5l_den_dan,
  eq5d_ad_5l_den_dan_slut = apple_data$eq5d_ad_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
apple_anxiety_depression_data <- na.omit(apple_anxiety_depression_data)

# Retrieve tables of the variables as a descriptive analysis:
table(apple_anxiety_depression_data$eq5d_ad_5l_den_dan)
table(apple_anxiety_depression_data$eq5d_ad_5l_den_dan_slut)

# Calculate frequencies for eq5d_ad_5l_den_dan
freq_eq5d_ad_5l_den_dan <- apple_anxiety_depression_data %>%
  group_by(eq5d_ad_5l_den_dan) %>%
  summarize(freq_eq5d_ad_5l_den_dan = n())

# Calculate frequencies for eq5d_ad_5l_den_dan_slut
freq_eq5d_ad_5l_den_dan_slut <- apple_anxiety_depression_data %>%
  group_by(eq5d_ad_5l_den_dan_slut) %>%
  summarize(freq_eq5d_ad_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_ad_5l_den_dan <- categories %>%
  left_join(freq_eq5d_ad_5l_den_dan, by = c('Category' = 'eq5d_ad_5l_den_dan')) %>%
  mutate(freq_eq5d_ad_5l_den_dan = ifelse(is.na(freq_eq5d_ad_5l_den_dan), 0, freq_eq5d_ad_5l_den_dan))

freq_eq5d_ad_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_ad_5l_den_dan_slut, by = c('Category' = 'eq5d_ad_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_ad_5l_den_dan_slut = ifelse(is.na(freq_eq5d_ad_5l_den_dan_slut), 0, freq_eq5d_ad_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_ad_5l_den_dan, and freq_eq5d_ad_5l_den_dan_slut
apple_anxiety_depression_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_ad_5l_den_dan = freq_eq5d_ad_5l_den_dan$freq_eq5d_ad_5l_den_dan,
  freq_eq5d_ad_5l_den_dan_slut = freq_eq5d_ad_5l_den_dan_slut$freq_eq5d_ad_5l_den_dan_slut
)

# Print the new dataframe
print(apple_anxiety_depression_data2)

# Reshape the frequency data into a longer format
apple_anxiety_depression_data3 <- apple_anxiety_depression_data2 %>%
  pivot_longer(cols = c(freq_eq5d_ad_5l_den_dan, freq_eq5d_ad_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
apple_anxiety_depression_data3$Category <- factor(apple_anxiety_depression_data3$Category,
                                                  levels = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed'),
                                                  labels = str_wrap(c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed'), width = 10))

# Create the plot
apple_anxiety_depression_plot <- ggplot(apple_anxiety_depression_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_ad_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Anxiety/Depression in the apple_group",
       x = "",
       y = "Number of respondents") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the x-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 14
    legend.text = element_text(size = 18) # Increase the size of the legend text to 14
  ) +
  theme(legend.position = "none") +
  ylim(0, 15) # Set y-axis range from 0 to 15 # Remove the legend

print(apple_anxiety_depression_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/anxiety_depression_change_apple_group.pdf", apple_anxiety_depression_plot)
ggsave("Path_to_the_folder_in_the_working_directory/anxiety_depression_change_apple_group.jpeg", apple_anxiety_depression_plot)


# ANXIETY_DEPRESSION FOR ORANGE_GROUP
# Create the initial data frame
orange_anxiety_depression_data <- data.frame(
  eq5d_ad_5l_den_dan = orange_data$eq5d_ad_5l_den_dan,
  eq5d_ad_5l_den_dan_slut = orange_data$eq5d_ad_5l_den_dan_slut
)

# Remove patients with missing data for baseline or end of follow-up score 
orange_anxiety_depression_data <- na.omit(orange_anxiety_depression_data)

# Retrieve tables of the variables as a descriptive analysis:
table(orange_anxiety_depression_data$eq5d_ad_5l_den_dan)
table(orange_anxiety_depression_data$eq5d_ad_5l_den_dan_slut)

# Calculate frequencies for eq5d_ad_5l_den_dan
freq_eq5d_ad_5l_den_dan <- orange_anxiety_depression_data %>%
  group_by(eq5d_ad_5l_den_dan) %>%
  summarize(freq_eq5d_ad_5l_den_dan = n())

# Calculate frequencies for eq5d_ad_5l_den_dan_slut
freq_eq5d_ad_5l_den_dan_slut <- orange_anxiety_depression_data %>%
  group_by(eq5d_ad_5l_den_dan_slut) %>%
  summarize(freq_eq5d_ad_5l_den_dan_slut = n())

# Complete list of categories
categories <- data.frame(
  Category = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed')
)

# Merge with calculated frequencies, filling missing values with zero
# this is to ensure that categories with no observations are still 
# included on the x-axis in the plot that we create
freq_eq5d_ad_5l_den_dan <- categories %>%
  left_join(freq_eq5d_ad_5l_den_dan, by = c('Category' = 'eq5d_ad_5l_den_dan')) %>%
  mutate(freq_eq5d_ad_5l_den_dan = ifelse(is.na(freq_eq5d_ad_5l_den_dan), 0, freq_eq5d_ad_5l_den_dan))

freq_eq5d_ad_5l_den_dan_slut <- categories %>%
  left_join(freq_eq5d_ad_5l_den_dan_slut, by = c('Category' = 'eq5d_ad_5l_den_dan_slut')) %>%
  mutate(freq_eq5d_ad_5l_den_dan_slut = ifelse(is.na(freq_eq5d_ad_5l_den_dan_slut), 0, freq_eq5d_ad_5l_den_dan_slut))

# Create a dataframe with Category, freq_eq5d_ad_5l_den_dan, and freq_eq5d_ad_5l_den_dan_slut
orange_anxiety_depression_data2 <- data.frame(
  Category = categories$Category,
  freq_eq5d_ad_5l_den_dan = freq_eq5d_ad_5l_den_dan$freq_eq5d_ad_5l_den_dan,
  freq_eq5d_ad_5l_den_dan_slut = freq_eq5d_ad_5l_den_dan_slut$freq_eq5d_ad_5l_den_dan_slut
)

# Print the new dataframe
print(orange_anxiety_depression_data2)

# Reshape the frequency data into a longer format
orange_anxiety_depression_data3 <- orange_anxiety_depression_data2 %>%
  pivot_longer(cols = c(freq_eq5d_ad_5l_den_dan, freq_eq5d_ad_5l_den_dan_slut), names_to = "Variable", values_to = "Frequency")

# Apply str_wrap to the levels of the Category factor
orange_anxiety_depression_data3$Category <- factor(orange_anxiety_depression_data3$Category,
                                                   levels = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed'),
                                                   labels = str_wrap(c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed'), width = 10))

# Create the plot
orange_anxiety_depression_plot <- ggplot(orange_anxiety_depression_data3, aes(x = Category, y = Frequency)) +
  geom_bar(aes(fill = ifelse(Variable == "freq_eq5d_ad_5l_den_dan", "Baseline", "End of follow-up")),
           stat = "identity", position = position_dodge2(width = 1.4, preserve = "total", padding = 0.02)) +
  labs(title = "Anxiety/Depression in the orange_group",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Baseline" = "steelblue4", "End of follow-up" = "steelblue1")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = 'bold'), # Center the title and make it bold
    legend.position = c(0.95, 0.95), # Position the legend in the upper right corner of the plot area
    legend.justification = c(1, 1), # Adjust the justification so the legend box is within the plot area
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18), # Increase the size of the y-axis labels to 14
    axis.title.y = element_text(size = 20), # Increase the size of the y-axis label to 15
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.0, "cm") # Increase the size of the legend text to 14
  ) +
  guides(fill = guide_legend(title = "")) +
  ylim(0, 15) # Set y-axis range from 0 to 15

print(orange_anxiety_depression_plot)

# Save the plot
ggsave("Path_to_the_folder_in_the_working_directory/anxiety_depression_change_orange_group.pdf", orange_anxiety_depression_plot)
ggsave("Path_to_the_folder_in_the_working_directory/anxiety_depression_change_orange_group.jpeg", orange_anxiety_depression_plot)

#_____
# Combine the anxiety/depression plots:
# Combine the two plots side by side
combined_plot_anxiety_depression <- grid.arrange(apple_anxiety_depression_plot, orange_anxiety_depression_plot, ncol = 2)
# Save the combined plot
ggsave("Path_to_the_folder_in_the_working_directory/anxiety_depression_change_combined.pdf", combined_plot_anxiety_depression, width = 16, height = 6)

ggsave("Path_to_the_folder_in_the_working_directory/anxiety_depression_change_combined.jpeg", combined_plot_anxiety_depression, width = 16, height = 6)
