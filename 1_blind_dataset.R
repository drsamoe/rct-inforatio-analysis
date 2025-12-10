# Begin with:
#     1) Creating a new project
#     2) Ensure that the working directory is set to the folder in which the new project is

#______________________________________________________________________________________________

#     3) Upload the datasets imported from Redcap 
#          (two different datasets are imported from RedCap because a new recruiting center
#           was included after trial commencement. This required the creation of a separate
#            RedCap Project to enable stratification of randomization according to which 
#             hospital patients were recruited from recruiting center)

#     3a) Upload the dataset for Zealand University Hospital Koege (value=1), Zealand University Hospital Nykoebing(value=3) and Herlev Hospital (value=2). 
#         In the parantheses after the read.csv code: a) write the path of the folder within the working directory, followed by b) /, followed by c) the name of the dataset file
KOE_NYK_HER_hospital_data <- read.csv('Path_to_the_folder/name_of_the_file.csv')

#     3b) Upload the dataset for Slagelse Hospital (value=1).
#         As described in 3a): In the parantheses after the read.csv code: a) write the path of the folder within the working directory, followed by b) /, followed by c) the name of the dataset file
SLA_hospital_data <- read.csv('Path_to_the_folder/name_of_the_file.csv')

#     3c) Change the value for Slagelse hospital (the name of the variable is 'hospital') from 1 to 4 to avoid that it has the same code as Zealand University Hospital Koege when the datasets are merged: 
SLA_hospital_data$hospital[SLA_hospital_data$hospital == 1] <- 4
# Now, check that the hospital has the correct value, which should now be 4
SLA_hospital_data$hospital

#______________________________________________________________________________________________

#     4) Merge the abovementioned two datasets:
all_hospitals_data <- rbind(KOE_NYK_HER_hospital_data, SLA_hospital_data)

#_____________________________________________________________________________________________________________

#     5) Ensure that each patient have one row in the dataset
#       Explanation: Each patient ID has several rows in the dataset because, in the datasets imported from RedCap there is a row for every event (trial visit)
#       The following coding is used to merge the events into one row per patient:
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)

combined_data <- all_hospitals_data %>%
  pivot_wider(
    id_cols = study_id,
    names_from = redcap_event_name,
    values_from = -c(study_id, redcap_event_name),  # Excludes specific columns - the columns for the patients' study id and the event names
    values_fn = first
  )

#     Delete the columns that only contain missing values (here we call the new dataset filtered_data)
filtered_data <- combined_data %>%
  select(-where(~all(is.na(.))))

#     Now check if there is the correct number of rows, which should be 100 (the number of patients recruited in the RCT)
#      We do this by counting any variable in the dataset (in the following code, we chose the variable called 'deform___1_baseline_arm_1' for foot deformities registered at the baseline visit):
length(filtered_data$deform___1_baseline_arm_1)

#______________________________________________________________________________________________

#     6) After the data has been corrected to only have one row per patient, all variables have a suffix/end part that describe the event (trial visit) at which the variables were registered
#          e.g. the variable for age, registered at baseline, is called age_baseline_arm_1
#           to improve comprehension, we will delete all the suffixes, which describe the event at which the variables were collected:

#The suffix _baseline_arm_1 is deleted. This was the suffix for data recorded at the baseline trial visit
# Getting the current variable names:
current_names <- names(filtered_data)

# Use sub() to remove "_baseline_arm_1" from variable names
new_names <- sub("_baseline_arm_1$", "", current_names)

# Rename the variables in your dataset
names(filtered_data) <- new_names

#-----------

# The suffix _1_rs_follow_up_arm_1 is deleted. This was the suffix for data recorded at the one-year follow-up
current_names2 <- names(filtered_data)
new_names2 <- sub("_1_rs_follow_up_arm_1$", "", current_names2)
names(filtered_data) <- new_names2

#-----------

# The suffix _efterregistrering_arm_1 is deleted. This was the suffix for the baseline data recorded after the baseline visit ended. These data were blood sample results, and registration of the questionnaires that the patients were asked to complete on paper after the baseline visit ended.
current_names3 <- names(filtered_data)
new_names3 <- sub("_efterregistrering_arm_1$", "", current_names3)
names(filtered_data) <- new_names3

#-----------

# The suffix _registreringer_und_arm_1 is deleted. This was the suffix for data recorded at the follow-up visits
current_names4 <- names(filtered_data)
new_names4 <- sub("_registreringer_und_arm_1$", "", current_names4)
names(filtered_data) <- new_names4

# #-----------

# The suffix _registrering_efter_arm_1 is deleted. This was the suffix for data recorded just after clinical follow-up ended: summary of the outcome measures healing and adverse events; patient questionnaires completed at end of clinical follow-up; and summary of protocol adherence.
current_names5 <- names(filtered_data)
new_names5 <- sub("_registrering_efter_arm_1$", "", current_names5)
names(filtered_data) <- new_names5

#________________________________________________________

#      7) The one patient missing data regarding randomization group is defined to be in the intervention group (value=2 for the randomization group variable)
#          Explanation: this patient is missing randomization group data in the dataset because the randomization was conducted by tossing a coin rather than randomizing in RedCap.
#           The reason for this was that RedCap was temporarily acute shutdown (without any preceding notice) when the patient came for their baseline visit
#           The coin toss resulted in heads, which prior to the toss was decided to define the intervention group. Therefore this patient was categorized to be in the intervention group. 

filtered_data$randomization_group[is.na(filtered_data$randomization_group)] <- 2

#           We can now check if the above code successfully categorized the patient into a randomization group. 
#           This is done by ensuring that none of the patients in the dataset have missing data for the randomization group variable (the output of the below code should be zero) : 
sum(is.na(filtered_data$randomization_group))
#           We can also check how many patients are in each randomization group now:
table(filtered_data$randomization_group)

#____________________________________________________________________________________________________________
  
#   8) BLINDING: The dataset is now ready for blinding of the randomization group variable.
#       To achieve this, a new variable named "masked_group" is created, containing masked values that replace the original randomization group variable (randomization_group, with values 1 for the control group and 2 for the intervention group).
#       The person responsible for blinding will substitute the values 1 and 2 with the labels "orange_group" and "apple_group", respectively, in a new dataset called full_data—ensuring that the analyst remains unaware of which label corresponds to which original group.
fulldata <- filtered_data %>% mutate(masked_group = ifelse(randomization_group == 1, "new_name_for_the_control_group_inserted_here", "new_name_for_the_intervention_group_inserted_here"))

# We can check whether the creation of masked_group was successful:
fulldata$masked_group
fulldata$randomization_group

# The next step is to randomize the order of rows in the dataframe using the sample() function in R. We now call the dataset RCTfulldata.
RCTfulldata <- fulldata[sample(nrow(fulldata)), ]

# Reset row names, so that the random order we just made in the above code for rows will not be visible (by deleting the original row numbers)
rownames(RCTfulldata) <- NULL

#It is important that the person blinding the dataset, makes a notes for themselves about which label corresponds to which original group
# The original randomization_group variable is deleted to ensure blinding of the dataset
RCTfulldata_2 <- RCTfulldata %>% select(-randomization_group)

#Now, we can check whether the masked_group variable is still a part of the dataset:
RCTfulldata_2$masked_group
# And whether randomization_group is actually deleted from the dataset:
RCTfulldata_2$randomization_group

#_________________________________________________________________________________________________________________________________________

#     9) Save the final blinded dataset 'RCTfulldata_2' as a csv file in the project working directory,
write.csv(RCTfulldata_2, file = "path_to_the_folder_in_the_working_directory/RCTdata.csv", row.names = FALSE)
# RCTdata is the name of the new csv file, which is blinded and ready for analysis by the blinded analyst.
#row.names=FALSE means that the column counting the rows is not included in the exported csv file.




