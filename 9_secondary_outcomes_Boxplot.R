#THIS SCRIPT IS FOR CREATING A BOX PLOT OF THE SECONDARY TRIAL OUTCOMES:
#     - CHANGE IN EQ-5D-5L VAS SCORE AND WOUND-QOL GLOBAL SCORE FROM BASELINE TO END OF FOLLOW-UP

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata2.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:

library(ggplot2) 

#_____________________________________________________________________________________________________

# CREATING A BOX BLOT FOR CHANGE IN EQ-5D-5L VAS SCORE (change_vas_score):

boxplot_EQVAS <- ggplot(RCTdata3, aes(x = masked_group, y = change_VAS_score, fill = masked_group)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "",
    y = "Change in VAS Score",
    fill = "Allocation group"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("apple_group" = "steelblue3", "orange_group" = "thistle"),
    labels = c("apple_group" = "Randomization group apple", "orange_group" = "Randomization group orange")
  ) +
  scale_x_discrete(
    labels = c("apple_group" = "Randomization group apple", "orange_group" = "Randomization group orange")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0,  hjust = 0.5, size = 16, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16
                                , face = "bold", margin = margin(r = 10)),# Adjust y-axis text as well for consistency
    legend.position = "none" # This removes the legend
  )

print(boxplot_EQVAS)

#Saving the box plot: 
ggsave(filename = "Path_to_the_folder_in_the_working_directory/boxplot_EQVAS.png", plot = boxplot_EQVAS, width = 8, height = 6, units = "in")

#____________________________________________________________________________________________________________________

# CREATING A BOX BLOT FOR CHANGE IN WOUND-QOL GLOBAL SCORE (change_woundglscore):

boxplot_WoundQoL <- ggplot(RCTdata3, aes(x = masked_group, y = change_woundglscore, fill = masked_group)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "",
    y = "Change in Wound-QoL global score",
    fill = "Allocation group") +
  theme_minimal() +
  scale_fill_manual(values = c("apple_group" = "steelblue3", "orange_group" = "thistle"),
                    labels = c("apple_group" = "Randomization group apple", "orange_group" = "Randomization group orange")) +
  scale_x_discrete(labels = c("apple_group" = "Randomization group apple", "orange_group" = "Randomization group orange")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0,  hjust = 0.5, size = 16, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16
                                , face = "bold", margin = margin(r = 10)),# Adjust y-axis text as well for consistency
    legend.position = "none" # This removes the legend
  )

print(boxplot_WoundQoL)

#save: 
ggsave(filename = "Path_to_the_folder_in_the_working_directory/boxplot_WoundQoL.png", plot = boxplot_WoundQoL, width = 8, height = 6, units = "in")

