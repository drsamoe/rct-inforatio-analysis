#THIS SCRIPT IS FOR ANALYSIS OF THE CATEGORICAL VARIABLES RECORDED AT BASELINE

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata2.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve the packages needed from the library:
library(dplyr)

install.packages("tidyr")
library(tidyr)

#____________________________________________________________________________________________________

#     3) Now, we select all the categorical variables that were recorded at baseline to create a tibble/dataframe (called cat_variables) 
#         solely consisting of the categorical variables selected in the following code:
cat_variables <- RCTdata3 %>% 
  select(female, #whether the patient was female
         diab_type,#diabetes type
         
         hospital, #the hospital patients were recruited from

         palpable,#presence of palpable pedal pulses
         firstever,#whether the index ulcer was the first chronic foot ulcer that the patient had ever experienced
         other_ulcers, #variable for whether patients had other foot ulcers than the index ulcer at baseline
         plantar, #whether the ulcer was located on the plantar aspect of the foot
         localisation, #index ulcer located on forefoot, midfoot or heel
         neuropathy,
         offloading,#the type of offloading the patient used at baseline
         
         mobility, #is the patient able to walk (with/without aid)? if not, is the patient able to stand?
         
         egfr_over90, #if patients had an eGFR above 90 the result was presented as >90 without further specifications. This is a binary variable informing whether eGFR was >90.
         dialysis,#whether the patient is receiving dialysis treatment
         
         first_toe_amput,#first toe amputated on index foot
         secondtofifth_amput, #one or more of the 2nd to 5th toe was amputated on index foot
         metatars_amput,# amputation at transmetatarsal level on index foot
         forefoot_amput,  # the entire forefoot was amputated on the index foot
         charcot_deform, #deformities due to Charchot arthropathy on the index foot
         no_deform, #none of the five abovementioned deformities present on the index foot at baseline
         
         amput_otherex,#any amputations of the non-index lower extremity
         
         smoking,
         alcohol_more_than_recommended, #according to Danish Health Authorities at the time of trial commencement. see script regarding preparation of data for further elaboration (2_prepare_dataset_for_analysis).
         alcohol_abuse_total,#whether patients were diagnosed with alcohol abuse
         
         
         hypertension,
         heart_fail,# heart failure
         AMI,#history of acute myocardial infarction
         apoplexia,#history of stroke
         KOL, #diagnosed with COPD (chronic obstructive pulmonary disease)
         retinopati,#retinopathy
         cirrosis,#cirrhosis
         egfr_binary, #value=1 if patients had eGFR<60 - defined as kidney insufficiency in the trial protocol
         no_comorbid, #whether the patient had none of the eight abovementioned comorbidities
         number_comorbidities,#number of the eight abovementioned recorded
         four_or_more_comorb,# whether patients had four or more of the abovementioned eight comorbidities

         masked_group# the blinded variable for randomization group
         
  )

#________________________________________________________________________________________________________

#       4) We generate patient counts for each categorical variable in the tibble defined above as cat_variables. 
#           The patient counts are reported for each randomization group.

#       4a) First, create a list to store frequency tables. For each categorical variable from the cat_variables, the code generates a table of patient counts by randomization group and saves it in the list frequency_tables
frequency_tables <- list()
for (var_name in names(cat_variables)) {
  freq_table <- cat_variables %>%
    group_by(masked_group, !!sym(var_name)) %>%
    summarise(Frequency = n()) %>%
    pivot_wider(names_from = !!sym(var_name), values_from = Frequency, values_fill = 0)
    #Save the resulting frequency table (freq_table) in the list, using the categorical variable name as the list entry name.
  frequency_tables[[var_name]] <- freq_table
}

# Now frequency_tables is a list containing frequency tables for each variable

#       4b) The frequency tables are accessed for each of the categorical variable:
frequency_tables$female
frequency_tables$diab_type

frequency_tables$hospital

frequency_tables$palpable
frequency_tables$firstever
frequency_tables$other_ulcers
frequency_tables$plantar
frequency_tables$localisation
frequency_tables$neuropathy
frequency_tables$offloading

frequency_tables$mobility

frequency_tables$egfr_over90
frequency_tables$dialysis #due to branching logic in RedCap, dialysis was only recorded for patients who were diagnosed with kidney insufficiency. Therefore, for patients who did not have the kidney insufficiency diagnosis the dialysis variable will appear as missing (NA)

frequency_tables$first_toe_amput
frequency_tables$secondtofifth_amput
frequency_tables$metatars_amput
frequency_tables$forefoot_amput
frequency_tables$charcot_deform
frequency_tables$no_deform

frequency_tables$amput_otherex

frequency_tables$smoking
frequency_tables$alcohol_more_than_recommended
frequency_tables$alcohol_abuse_total

frequency_tables$hypertension
frequency_tables$heart_fail
frequency_tables$AMI
frequency_tables$apoplexia
frequency_tables$KOL
frequency_tables$retinopati
frequency_tables$cirrosis
frequency_tables$egfr_binary
frequency_tables$no_comorbid
frequency_tables$number_comorbidities
frequency_tables$four_or_more_comorb

#_______________________________________________________________________________________________________

#       5) Compare the categorical variables between randomization group by using chi square test (with Yate's continuity correction) or Fisher's exact test

#       5a) Similar to the above section 4a), we create a list, now to store contingency tables for all the variables selected in cat_variables divided by randomization group: 
cat_var_contingencytables <- list()
for (var_name in names(cat_variables)) {
  contingency_table <- table(cat_variables$masked_group, cat_variables[[var_name]]) # Creating contingency tables for the categorical variables divided by masked_group
  cat_var_contingencytables[[var_name]] <- contingency_table
}

#       5b) After generating the contingency tables, we perform two steps for each categorical variable:
#                 * Step 1: Assess the expected cell counts of the contingency table (obtained via chisq.test(...)$expected) to determine whether the assumptions of the Chi-square test are satisfied.
#                 * Step 2: Select the appropriate test based on these expected counts:
#                           - If all expected cell counts are ≥ 5, a Chi-square test with Yates’ continuity correction is applied (chisq.test).
#                           - If any expected cell count is < 5, Fisher’s exact test is used instead (fisher.test).
#                           Note: the output of chisq.test (and occasionally fisher.test) will be NA if a variable has no variation 
#                           that is, if all patients in both groups have the same response and the contingency table contains only one category.

#       The two steps are conducted below for each variable (notice the meaning of the variables are explained above, in the section that generate the tibble cat_variables):
#Sex:
cat_var_contingencytables$female # this code returns the contingency table for the respective variable, but is not necessary for conducting the chi square or fisher's test.
chisq.test(cat_var_contingencytables$female)$expected # assessing expected values
#INSERT the proper test: chisq.test(cat_var_contingencytables$female) or fisher.test(cat_var_contingencytables$female)

#Diabetes type:
chisq.test(cat_var_contingencytables$diab_type)$expected
#INSERT proper.test(cat_var_contingencytables$diab_type)

#Hospital patients were recruited from:
chisq.test(cat_var_contingencytables$hospital)$expected
#INSERT proper.test(cat_var_contingencytables$hospital)

#Presence of palpable pedal pulses
chisq.test(cat_var_contingencytables$palpable)$expected
#INSERT proper.test(cat_var_contingencytables$palpable)

#First ever ulcer (whether the index ulcer was the first chronic foot ulcer that the patient had ever experienced):
chisq.test(cat_var_contingencytables$firstever)$expected
#INSERT proper.test(cat_var_contingencytables$firstever)

#Other ulcers (whether patients had other foot ulcers than the index ulcer at baseline):
chisq.test(cat_var_contingencytables$other_ulcers)$expected
#INSERT proper.test(cat_var_contingencytables$other_ulcers)

#Plantar location (whether the ulcer was located on the plantar aspect of the foot):
chisq.test(cat_var_contingencytables$plantar)$expected
#INSERT proper.test(cat_var_contingencytables$plantar)

# Ulcer location (index ulcer located on forefoot, midfoot or heel):
chisq.test(cat_var_contingencytables$localisation)$expected
#INSERT proper.test(cat_var_contingencytables$localisation)

#Neuropathy:
chisq.test(cat_var_contingencytables$neuropathy)$expected
#INSERT proper.test(cat_var_contingencytables$neuropathy)

#Offloading (the type of offloading the patient used at baseline):
chisq.test(cat_var_contingencytables$offloading)$expected
#INSERT proper.test(cat_var_contingencytables$offloading)

#Mobility (patient able to walk (with/without aid)? if not, is the patient able to stand?):
chisq.test(cat_var_contingencytables$mobility)$expected
#INSERT proper.test(cat_var_contingencytables$mobility)

#egfr_over90: regarding this variable, we’re not interested in a chi square test only a descriptive analysis in case we want to look into how many patients had an eGFR value reported as >90.

#Dialysis (whether the patients received dialysis treatment):
chisq.test(cat_var_contingencytables$dialysis)$expected
#INSERT proper.test(cat_var_contingencytables$dialysis)

#First toe amputated on index foot:
chisq.test(cat_var_contingencytables$first_toe_amput)$expected
#INSERT proper.test(cat_var_contingencytables$first_toe_amput)

#One or more of the 2nd to 5th toe was amputated on index foot:
chisq.test(cat_var_contingencytables$secondtofifth_amput)$expected
#INSERT proper.test(cat_var_contingencytables$secondtofifth_amput)

#Amputation at transmetatarsal level on index foot:
chisq.test(cat_var_contingencytables$metatars_amput)$expected
#INSERT proper.test(cat_var_contingencytables$metatars_amput)

#The entire forefoot amputated on the index foot:
chisq.test(cat_var_contingencytables$forefoot_amput)$expected
#INSERT proper.test(cat_var_contingencytables$forefoot_amput)

#Deformities due to Charchot arthropathy on the index foot:
chisq.test(cat_var_contingencytables$charcot_deform)$expected
#INSERT proper.test(cat_var_contingencytables$charcot_deform)

#None of the five abovementioned deformities present on the index foot at baseline:
chisq.test(cat_var_contingencytables$no_deform)$expected
#INSERT proper.test(cat_var_contingencytables$no_deform)

#Any amputations of the non-index lower extremity:
chisq.test(cat_var_contingencytables$amput_otherex)$expected
#INSERT proper.test(cat_var_contingencytables$amput_otherex)

#Smoking:
chisq.test(cat_var_contingencytables$smoking)$expected
#INSERT proper.test(cat_var_contingencytables$smoking)

#Alcohol intake more than recommended by the Danish Health Authorities at the time of trial commencement:
chisq.test(cat_var_contingencytables$alcohol_more_than_recommended)$expect
#INSERT proper.test(cat_var_contingencytables$alcohol_more_than_recommended)

#Diagnosed with alcohol abuse:
chisq.test(cat_var_contingencytables$alcohol_abuse_total)$expected
#INSERT proper.test(cat_var_contingencytables$alcohol_abuse_total)

#Hypertension:
chisq.test(cat_var_contingencytables$hypertension)$expected
#INSERT proper.test(cat_var_contingencytables$hypertension)

#Heart failure:
chisq.test(cat_var_contingencytables$heart_fail)$expected
#INSERT proper.test(cat_var_contingencytables$heart_fail)

#History of AMI:
chisq.test(cat_var_contingencytables$AMI)$expected
#INSERT proper.test(cat_var_contingencytables$AMI)

#History of stroke:
chisq.test(cat_var_contingencytables$apoplexia)$expected
#INSERT proper.test(cat_var_contingencytables$apoplexia)

#COPD:
chisq.test(cat_var_contingencytables$KOL)$expected
#INSERT proper.test(cat_var_contingencytables$KOL)

#Retinopathy:
chisq.test(cat_var_contingencytables$retinopati)$expected
#INSERT proper.test(cat_var_contingencytables$retinopati)

#Cirrhosis:
chisq.test(cat_var_contingencytables$cirrosis)$expected
#INSERT proper.test(cat_var_contingencytables$cirrosis)

#Baseline eGFR<60 – defined in the protocol as kidney insufficiency:
chisq.test(cat_var_contingencytables$egfr_binary)$expected
#INSERT proper.test(cat_var_contingencytables$egfr_binary)

#None of the abovementioned eight comorbidities present at baseline:
chisq.test(cat_var_contingencytables$no_comorbid)$expected
#INSERT proper.test(cat_var_contingencytables$no_comorbid)

# Whether patients had four or more of the abovementioned eight comorbidities:
chisq.test(cat_var_contingencytables$four_or_more_comorb)$expected
#INSERT proper.test(cat_var_contingencytables$four_or_more_comorb)
