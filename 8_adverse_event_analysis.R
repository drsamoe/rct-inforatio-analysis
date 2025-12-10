#THIS SCRIPT IS FOR ANALYSIS OF ADVERSE EVENTS

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata2.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:
install.packages("epitools")
library(epitools)

library(dplyr)
library(tidyr)

#_____________________________________________________________________________________________________
# Adverse event analyses are performed according to the 'as treated' principle, 
# as specified in the predefined statistical analysis plan.

# In REDCap, the variable 'allocatedtreat' was used to indicate whether a patient 
# actually received the allocated treatment. 
# This variable appeared only for patients who did NOT meet the per-protocol criteria 
# (due to branching logic in REDCap).

# Coding of 'allocatedtreat':
#   1 = the patient did NOT receive the allocated treatment (meaning patients from the control group who received the inforatio technique by mistake
#       or patients from the intervention group not receiving the inforatio technique at all)
#   0 = the patient DID receive the allocated treatment

# For any patient where allocatedtreat == 1, the value of 'masked_group' must be 
# revised so that the patient is reassigned to the opposite treatment group when we analyse
# adverse events on the as treated basis in this script.

#First, we investigate whether there were any patients who did not receive the allocated treatment
tapply(RCTdata3$allocatedtreat, RCTdata3$masked_group, table)

#If any have the value 1 for allocatedtreat they must be reassigned to the opposite masked_group
# Create a new variable 'as_treated' based on masked_group
RCTdata3$as_treated <- RCTdata3$masked_group
# If any have the value 1 for allocatedtreat masked_group is reassigned to the opposite group
# for patients who did NOT receive allocated treatment, by using the following codes:
#     RCTdata3$as_treated[RCTdata3$allocatedtreat == 1 & RCTdata3$masked_group == "apple_group"] <- "orange_group"
#     RCTdata3$as_treated[RCTdata3$allocatedtreat == 1 & RCTdata3$masked_group == "orange_group"] <- "apple_group"

#____________________________________________________________________________________________________
#Days from baseline to adverse events are calculated in the script for analyzing cont_variables (4_descriptive_analysis_continuous_variables)
#____________________________________________________________________________________________________

# Calculate, by as_treated group, the mean and range for ulcers with positive values of diff_area (cm2) and percentage increase 
# (positive values means that the ulcers became larger during follow-up) 
results_for_ulcer_area_increases <- RCTdata3 %>%
  filter(diff_area > 0) %>% # Filter positive diff_area values 
  group_by(as_treated) %>% # Group by allocation group
  summarize(
    mean_diff_area = mean(diff_area, na.rm = TRUE), # Mean of diff_area
    range_diff_area = paste0(min(diff_area, na.rm = TRUE), " - ", max(diff_area, na.rm = TRUE)), # Range of diff_area
    mean_pct_increase = mean(pct_increase_area, na.rm = TRUE), # Mean of percentage increase
    range_pct_increase = paste0(min(pct_increase_area, na.rm = TRUE), " - ", max(pct_increase_area, na.rm = TRUE)) # Range of percentage increase
  )

print(results_for_ulcer_area_increases)

#________________________________________________________________________________________________________________________
# we perform frequency tables for the following adverse event outcomes by as treated group (as_treated)
#         amputation
tapply(RCTdata3$amputation, RCTdata3$as_treated, table)
#         amput_indexreason, #variable for amputations recorded as related to the index ulcer
tapply(RCTdata3$amput_indexreason, RCTdata3$as_treated, table)
#         amputation_indexulcer, #variable informing if amputations involved the index ulcer location or the amputation level was distal to the index ulcer location
tapply(RCTdata3$amputation_indexulcer, RCTdata3$as_treated, table)
#         amputlevel, #Informs on which level patients were amputated. 
tapply(RCTdata3$amputlevel, RCTdata3$as_treated, table) 
#         amput_major_minor, #informs whether amputations where above or below ankle level
tapply(RCTdata3$amput_major_minor, RCTdata3$as_treated, table)
#         death,
tapply(RCTdata3$death, RCTdata3$as_treated, table)
#         death_ulcerrelated,#death recorded as related to the index ulcer
tapply(RCTdata3$death_ulcerrelated, RCTdata3$as_treated, table)
#         hospitalization,
tapply(RCTdata3$hospitalization, RCTdata3$as_treated, table)
#         hospitulcerrelated, # informs whether hospitalization was recorded as index ulcer related, based on the following categories "definetely_related","probably_related","probably_not_related",'definitely_not_related'
tapply(RCTdata3$hospitulcerrelated, RCTdata3$as_treated, table)
#         binary_hospitulcerrelated,# informs whether hospitalization was "definetely_related"/"probably_related" (value 1) or "probably_not_related"/'definitely_not_related'(value 0)
tapply(RCTdata3$binary_hospitulcerrelated, RCTdata3$as_treated, table)
#         other_serious_adv,
tapply(RCTdata3$other_serious_adv, RCTdata3$as_treated, table)
#         no_serious_adv,#representing patients who did not experience any of the serious adverse events during follow-up
tapply(RCTdata3$no_serious_adv, RCTdata3$as_treated, table)
#         ulcer_infection, #infection of the index ulcer
tapply(RCTdata3$ulcer_infection, RCTdata3$as_treated, table)
#         exposed_structures, #exposure of bone, tendon and/or joint in index ulcer
tapply(RCTdata3$exposed_structures, RCTdata3$as_treated, table)
#         osteomyelitis,#ostemyelitis of underlying bone was diagnosed
tapply(RCTdata3$osteomyelitis, RCTdata3$as_treated, table)
#         OR_revision, #surgical wound intervention of the index ulcer in an operating theater
tapply(RCTdata3$OR_revision, RCTdata3$as_treated, table)
#         other_nonserious_adv,
tapply(RCTdata3$other_nonserious_adv, RCTdata3$as_treated, table)
#         change_area,#reports whether ulcers became smaller, larger or unchanged in area from baseline to end of follow-up
tapply(RCTdata3$change_area, RCTdata3$as_treated, table)
#         larger_area,#reports whether ulcers became larger from baseline to end of follow-up (larger==1, not larger==0)
tapply(RCTdata3$larger_area, RCTdata3$as_treated, table)
#         oneyear_amput, #informs whether patients had an amputation at any level of their index limb within one year from baseline
tapply(RCTdata3$oneyear_amput, RCTdata3$as_treated, table)
#         oneyearamput_majmin, #informs whether amputations where above or below ankle level
tapply(RCTdata3$oneyearamput_majmin, RCTdata3$as_treated, table)
#         oneyear_death, #death within one year from follow-up
tapply(RCTdata3$oneyear_death, RCTdata3$as_treated, table)
#         oneyeardeath_ulcerrelated,# death recorded as related to the index ulcer in patient medical journals
tapply(RCTdata3$oneyeardeath_ulcerrelated, RCTdata3$as_treated, table)

# Counting patients in each group who did not have any of the predefined non-serious adverse events (related to the index ulcer) 
# recorded during follow-up - these adverse events were predefined in the Table 2 of the protocol article (see READ.ME file for link to the article)
RCTdata3$zero_nonserAEs <- rowSums(RCTdata3[, c('larger_area', 'ulcer_infection', 'exposed_structures', 'osteomyelitis', 'OR_revision', 'other_nonserious_adv')], na.rm = TRUE) == 0
tapply(RCTdata3$zero_nonserAEs, RCTdata3$as_treated, sum)

#____________________
#ANALYSIS of the predefined SAFETY OUTCOMES - see Statistical analysis plan section in the protocol article

#1) Safety outcome: number of patients who experienced index ulcer-related adverse events during follow-up:
# a) calculate how many patients had no index ulcer-related adverse events in each group 
# - these are listed in the firste column 'Adverse events related to the index ulcer' in Table 2 of the protocol article :
RCTdata3$zero_ulcerrelated_AE <- rowSums(RCTdata3[, c('larger_area', 'ulcer_infection', 'exposed_structures', 'osteomyelitis', 
                                                                 'OR_revision', 'amputation',
                                                                 'death_ulcerrelated','binary_hospitulcerrelated')], na.rm = TRUE) == 0
tapply(RCTdata3$zero_ulcerrelated_AE, RCTdata3$as_treated, sum)

#b) Creating a binary variable where incidence of one or more ulcer-related AEs==1 
#   and no ulcer-related AEs==0
RCTdata3$ulcer_related_AEs_binary <- ifelse(RCTdata3$zero_ulcerrelated_AE, 0, 1)
tapply(RCTdata3$ulcer_related_AEs_binary, RCTdata3$as_treated, table)

#c) Chi square test of ulcer_related_AEs_binary: 
# Create the 2x2 contingency table
ulcerrelated_binary_AEs_table <- table(RCTdata3$as_treated, RCTdata3$ulcer_related_AEs_binary)

# Print the contingency table
print(ulcerrelated_binary_AEs_table)

#test expected number in chi square table:
chisq.test(ulcerrelated_binary_AEs_table)$expected
#If any count in the table are below five in the expected chi square table, the Fisher's Exact test must be conducted
#Otherwise Chi square test will be conducted by default:
chisq.test(ulcerrelated_binary_AEs_table)

#d) Relative risks with 95% CI:
# Calculate relative risks and 95% confidence intervals
RR_ulcerrelated_binary_AEs <- riskratio(ulcerrelated_binary_AEs_table)

# Print the result
print(RR_ulcerrelated_binary_AEs)

#--------------
# 2) Safety outcome: number of patients who experienced at least one of the following during follow-up:
#    index limb amputation, all-cause mortality, ulcer-related hospitalization 
#       - also predefined in the Statistical analysis plan section of the protocol article

# a) calculate how many patients had no index limb amputation, all-cause mortality and/or  ulcer-related hospitalization in each group:
RCTdata3$zero_custom_serious_AE <- rowSums(RCTdata3[, c('binary_hospitulcerrelated', 'amputation', 'death')], na.rm = TRUE) == 0
tapply(RCTdata3$zero_custom_serious_AE, RCTdata3$as_treated, sum)

#b) Creating a binary variable where incidence of one or more of the above serious adverse events==1 
#   and none==0
RCTdata3$custom_serious_AE_binary <- ifelse(RCTdata3$custom_serious_AE, 0, 1)
tapply(RCTdata3$custom_serious_AE_binary, RCTdata3$as_treated, table)

#c) Chi square test of custom_serious_AE_binary: 
# Create the 2x2 contingency table
custom_serious_AE_binary_table <- table(RCTdata3$as_treated, RCTdata3$custom_serious_AE_binary)

# Print the contingency table
print(custom_serious_AE_binary_table)

#test expected number in chi square table:
chisq.test(custom_serious_AE_binary_table)$expected
#If any count in the table are below five in the expected chi square table, the Fisher's Exact test must be conducted
#Otherwise Chi square test will be conducted by default:
chisq.test(custom_serious_AE_binary_table)

#d) Relative risks with 95% CI:
# Calculate relative risks and 95% confidence intervals
RR_custom_serious_AE <- riskratio(custom_serious_AE_binary_table)

# Print the result
print(RR_custom_serious_AE )