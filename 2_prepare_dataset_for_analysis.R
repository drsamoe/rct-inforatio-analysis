#PREPARATION OF THE DATASET AFTER BLINDING, e.g. changing variable names to improve comprehension of outputs

# Begin with:
#     1) Creating a new project
#     2) Ensure that the working directory is set to the folder in which the new project is

#     3) upload the blinded dataset - notice that we called the blinded dataset 'RCTdata'.
RCTdata2 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata.csv')

#_____________________________________________________________________________________________________

#     4) Install packages needed for this R script and retrieve them from the library:

install.packages("dplyr")
library(dplyr)

#_____________________________________________________________________________________________________

#      5) A variable is created for baseline area (the unit is cm2)
#       notice, according to the published protocol, baseline ulcer area was the average value of three out of five ulcer area measurements performed by a nurse blinded to randomization group.
#       the minimum and maximum measurements were excluded from the calculation of the average.
RCTdata2$base_ulcer_area <- (RCTdata2$baselinearea_1 + RCTdata2$baselinearea_2 +RCTdata2$baselinearea_3)/3

#_____________________________________________________________________________________________________

#      6) similar to 5), a variable is created for the ulcer area (cm2) at the patients' end of follow-up:
RCTdata2$end_ulcer_area <- (RCTdata2$endarea_1 + RCTdata2$endarea_2 +RCTdata2$endarea_3)/3

#_____________________________________________________________________________________________________

#      7) A variable is created for the absolute change in ulcer area (cm2) from baseline to end of follow-up:
#         area reduction will be expressed as negative values and enlargements as positive values
RCTdata2$diff_area <- RCTdata2$end_ulcer_area - RCTdata2$base_ulcer_area

#_____________________________________________________________________________________________________

#      8) A variable is created for the percentage change in ulcer area from baseline to end of follow-up:
          # as for the variable diff_area defined above, area reduction will be expressed as negative values and enlargements as positive values
RCTdata2$pct_increase_area <- (RCTdata2$diff_area/RCTdata2$base_ulcer_area)*100

#_____________________________________________________________________________________________________

#     9) We rename the values of a variable (change_area) that reports whether ulcer became smaller, larger or unchanged in area from baseline to end of follow-up
RCTdata2 <- RCTdata2 %>% 
  mutate(change_area = case_when(
    change_area == 1 ~ "smaller",
    change_area == 2 ~ "larger",
    change_area == 3 ~ "unchanged",
    TRUE        ~ as.character(change_area)
  ))

#Also, create a variable for wound area larger yes/no:
RCTdata2$larger_area <- ifelse(RCTdata2$change_area == 'larger', 1, 0)

#_____________________________________________________________________________________________________

#     10) Add variables for baseline ulcer duration expressed in weeks and in years. In RedCap, ulcer duration was recorded in weeks.

# baseline ulcer duration in months:
RCTdata2$ulcer_duration_MONTHS <- RCTdata2$ulcer_duration/52*12

#baseline ulcer duration in years:
RCTdata2$ulcer_duration_YEARS <- RCTdata2$ulcer_duration/52

#_____________________________________________________________________________________________________

#     11) time treated at specialized unit is this variable: beginfollow_to_base - But it is recorded in days in RedCap. 
#         We want to report it in weeks and in months:
RCTdata2$weeks_beginfollow_to_base <- RCTdata2$beginfollow_to_base/7
RCTdata2$beginfollow_to_base_MONTHS <- RCTdata2$beginfollow_to_base/365*12

#_____________________________________________________________________________________________________

#      12) A variable, pct_missed_visits, is defined for percentage of missed trial visits 
#         Explanation of variable names in the below code: 
#           - no_visits=number of visits the patient attended (this number depends on when the patients ended their follow-up and whether they had any trial visits that were missed during their follow-up. 
#           - no_missedvisits=number of visits that the patients missed.
RCTdata2$pct_missed_visits <- RCTdata2$no_missedvisits/(RCTdata2$no_visits + RCTdata2$no_missedvisits)*100

#_____________________________________________________________________________________________________

#     13) In the logistic regression analysis of healing, one of the predefined covariates was whether four or more of the following predefined comorbidities were present at baseline: 
#           Hypertension, previous myocardial infarction, previous stroke, congestive heart failure, chronic obstructive pulmonary disease,liver cirrhosis, kidney insufficiency defined as eGFR (estimated Glomerular Filtration Rate)<60 mL/min 1.73 m2 and diabetic retinopathy

#     13a) Seven of the eight comorbidities (all but kidney insufficiency) were renamed in the code below (kidney insufficiency is addressed later below under 13 b):
RCTdata2 <- RCTdata2 %>% rename(hypertension = comorbidities___1)

RCTdata2 <- RCTdata2 %>% rename(heart_fail = comorbidities___2)

RCTdata2 <- RCTdata2 %>% rename(AMI = comorbidities___4)

RCTdata2 <- RCTdata2 %>% rename(apoplexia = comorbidities___5)

RCTdata2 <- RCTdata2 %>% rename(KOL = comorbidities___6)

RCTdata2 <- RCTdata2 %>% rename(retinopati = comorbidities___7)

RCTdata2 <- RCTdata2 %>% rename(cirrosis = comorbidities___8)


# The variable comorbidities___9 is for patients who had none of the listed eight comorbidities, and therefore it is renamed: 'no_comorbid'
RCTdata2 <- RCTdata2 %>% rename(no_comorbid = comorbidities___9)

# Notice, in the above codes, that comorbidities___3 is not renamed. This variable (comorbidities___3) was for whether patients were diagnosed with kidney insufficiency when they entered the trial.
# However, instead of using the variable comorbidities___3 we will use a binary variable reporting whether baseline eGFR was <60, because an eGFR<60 at baseline was defined in the protocol as kidney insufficiency. 

#     13b) this is how we defined a new binary variable: eGFR <60 or eGFR ≥60 based on baseline blood sample results:
RCTdata2$egfr_binary <- ifelse(RCTdata2$base_egfr < 60, 1, 0)

#   For curiosty, it is possible to look into how many had eGFR<60 compared to how many were diagnosed with kidney insufficiency when they entered the trial:
tapply(RCTdata2$egfr_binary, RCTdata2$masked_group, table)

tapply(RCTdata2$comorbidities___3, RCTdata2$masked_group, table)

#     13c) Next step is to define a variable that reports how many of the eight predefined comorbidities were present at baseline
RCTdata2$number_comorbidities <- rowSums(RCTdata2[, c("hypertension", "heart_fail", "egfr_binary", "AMI", "apoplexia", "KOL", "retinopati", "cirrosis")])

#     13d) Finally, we can create a binary variable reporting the presence at baseline of >= 4 of the predefined comorbidities:
RCTdata2$four_or_more_comorb <- ifelse(RCTdata2$number_comorbidities >= 4, 1, 0)
#explanation: the value 1 for the new variable four_or_more_comorb means that the patient had four or more of the abovementioned comorbidities.

#_________________________________________________________________________________________________________________________

#     14) Categorical variables for various foot deformities in the index foot, present at baseline, are renamed for improved comprehension:

RCTdata2 <- RCTdata2 %>% rename(first_toe_amput = deform___1) #first toe amputated

RCTdata2 <- RCTdata2 %>% rename(secondtofifth_amput = deform___2) # one or more of the 2nd to fifth toe was amputated

RCTdata2 <- RCTdata2 %>% rename(metatars_amput = deform___3) # amputation at transmetatarsal level

RCTdata2 <- RCTdata2 %>% rename(charcot_deform = deform___4) #deformities due to Charchot arthropathy

RCTdata2 <- RCTdata2 %>% rename(forefoot_amput = deform___6) # the entire forefoot was amputated

RCTdata2 <- RCTdata2 %>% rename(no_deform = deform___5) #none of the above listed deformities were present at baseline

#_________________________________________________________________________________________________________________________

#     15) Rename the values for the binary variable called female, which informs sex
RCTdata2 <- RCTdata2 %>% 
  mutate(female = case_when(
    female == 1 ~ "female", 
    female == 0 ~ "male",
    TRUE        ~ as.character(female)
  ))

#_________________________________________________________________________________________________________________________

#     16) Rename the values for the diabetes type variable
RCTdata2 <- RCTdata2 %>% 
  mutate(diabetes = case_when(
    diabetes == 1 ~ "type1",
    diabetes == 2 ~ "type2",
    diabetes == 3 ~ "other_type",
    diabetes == 4 ~ "unknown_type",
    TRUE        ~ as.character(diabetes)
  ))

#_________________________________________________________________________________________________________________________

#     17) Rename values for the variable informing whether patients had other ulcers than the index ulcer at baseline
RCTdata2 <- RCTdata2 %>% 
  mutate(other_ulcers = case_when(
    other_ulcers == 1 ~ "none",
    other_ulcers == 2 ~ "on_index_foot",
    other_ulcers == 3 ~ "on_contralat_foot",
    other_ulcers == 4 ~ "on_both_feet",
    TRUE        ~ as.character(other_ulcers)
  ))

#_________________________________________________________________________________________________________________________

#     18) Rename values for the variable informing index foot off-loading device at baseline:
RCTdata2 <- RCTdata2 %>% 
  mutate(offloading = case_when(
    offloading == 1 ~ "no_support", #complete off-loading of the index foot, meaning no support on that foot - e.g. if patients use wheel chairs and never use their index foot for standing.
    offloading == 2 ~ "normal_footwear", #standard retail footwear
    offloading == 3 ~ "normal_footwear_with_special_insoles", #standard retail footwear with specialized insoles
    offloading == 4 ~ "therapeutic_sandal", #therapeutic sandals (comes with specialized insoles)
    offloading == 5 ~'semiorthopedic_footwear', #semi-orthopaedic footwear defined as retail or prefabricated shoes with structural modifications (e.g., extra depth, extra width, rocker sole) but not fully custom-made.
    offloading == 6 ~'costum_made_footwear', # hand-made fully custom-made footwear
    offloading == 7 ~'removable_boot', #removable knee-high off-loading (rarely mid-calf)
    offloading == 8 ~'TCC',#total contact cast (non-removable knee-high off-loading)
    TRUE        ~ as.character(offloading)
  ))

#_________________________________________________________________________________________________________________________

#     19) Rename values for the variable informing whether the index ulcer was located on the plantar aspect of the foot:
RCTdata2 <- RCTdata2 %>% 
  mutate(plantar = case_when(
    plantar == 1 ~ "plantar",
    plantar == 2 ~ "not_plantar",
    TRUE        ~ as.character(plantar)
  ))

#_________________________________________________________________________________________________________________________

#     20) Rename values for the variable informing index ulcer location
RCTdata2 <- RCTdata2 %>% 
  mutate(localisation = case_when(
    localisation == 1 ~ "forefoot",
    localisation == 2 ~ "midfoot",
    localisation == 3 ~ "heel",
    TRUE        ~ as.character(localisation)
  ))

#_________________________________________________________________________________________________________________________

#     21) Rename values for the variable informing patient mobility at baseline
RCTdata2 <- RCTdata2 %>% 
  mutate(mobility = case_when(
    mobility == 1 ~ "walk_without_aid",
    mobility == 2 ~ "walk_with_aid",
    mobility == 3 ~ "can_only_stand",
    mobility == 4 ~ 'cannot_stand_or_walk',
    TRUE        ~ as.character(mobility)
  ))

#_________________________________________________________________________________________________________________________

#     22) Rename values for the variable informing smoking
RCTdata2 <- RCTdata2 %>% 
  mutate(smoking = case_when(
    smoking == 1 ~ "smoker",
    smoking == 2 ~ "previous_smoker",
    smoking == 3 ~ "never_smoked",
    TRUE        ~ as.character(smoking)
  ))

#_________________________________________________________________________________________________________________________

#     23) Rename values for the variable informing whether the non-index lower extremity had been amputated at any level at the time of baseline: 
RCTdata2 <- RCTdata2 %>% 
  mutate(amput_otherex = case_when(
    amput_otherex == 1 ~ "no_contralat_amput", #If they had not been amputated on the non-index index extremity
    amput_otherex == 2 ~ "contralat_distal_amput",#amputation distal to ankle level
    amput_otherex == 3 ~ "contralat_proximal_amput",#amputation proximal to ankle level
    TRUE        ~ as.character(amput_otherex)
  ))

#_________________________________________________________________________________________________-

#     24) Defining variables regarding alcohol intake
# At the time of trial commencement the Danish Health Authorities recommended a maximum alcohol intake of seven units for female and 14 units for male. 
# Alcohol intake was recorded at baseline in a categorical manner: whether patients had no intake, a weekly intake that followed recommendations by health authorities, or a weekly intake that exceeded the recommendations.
# As the recommendations, at the time of trial commencement was different for males and females, the categorical variables for alcohol intake were also different based on sex.

# To improve comprehension of the variables for alcohol intake, the values are renamed.

# Renaming the values for female weekly alcohol intake: 
RCTdata2 <- RCTdata2 %>% 
  mutate(femalealcohol = case_when(
    femalealcohol == 1 ~ "more_than_seven",
    femalealcohol == 2 ~ "less_than_seven",
    femalealcohol == 3 ~ "no_consumption",
    TRUE        ~ as.character(femalealcohol)
  ))

table(RCTdata2$femalealcohol)

# Renaming the values for male weekly alcohol intake: 
RCTdata2 <- RCTdata2 %>% 
  mutate(malealcohol = case_when(
    malealcohol == 1 ~ "more_than_fourteen",
    malealcohol == 2 ~ "less_than_fourteen",
    malealcohol == 3 ~ "no_consumption",
    TRUE        ~ as.character(malealcohol)
  ))

table(RCTdata2$malealcohol)

# Now, we need to create a binary variable that informs whether patients exceeded recommendations for weekly alcohol intake.
# First, we need to create a variable
RCTdata2$alcohol_more_than_recommended <- ifelse(
  (RCTdata2$femalealcohol == "more_than_seven"  & !is.na(RCTdata2$femalealcohol)) |
    (RCTdata2$malealcohol  == "more_than_fourteen" & !is.na(RCTdata2$malealcohol)),
  1, 0
)

# In REDCap, the field indicating whether patients were diagnosed with alcohol abuse was displayed only if their reported weekly alcohol intake exceeded the health authority recommendations, due to the branching logic applied in the system.
# Therefore, also the variable informing alcohol abuse is different for males and females. We will now merged the alcohol abuse data for females and males into one binary alcohol abuse variable (the value 1 = patient diagnosed with alcohol abuse).
RCTdata2$alcohol_abuse_total <- ifelse(
  RCTdata2$alcoholabuse_female %in% c(1) |
    RCTdata2$alcoholabuse_male %in% c(1), #the value 1 indicates that the patient is diagnosed with alcohol abuse
  1,
  0
)

#_________________________________________________________________________________________________________________________
#       25) Different categorical variables for adverse events needs to be renamed for improved comprehension

#       25a) Renaming categories for serious adverse events (serious_adv___1 og 2 are amputation and death, for which there are already categorical variables called death and amputation)
#            (the serious adverse event category is predefined in the protocol)
RCTdata2 <- RCTdata2 %>% rename(hospitalization = serious_adv___3)

RCTdata2 <- RCTdata2 %>% rename(other_serious_adv = serious_adv___4)

RCTdata2 <- RCTdata2 %>% rename(no_serious_adv = serious_adv___5) #representing patients who did not experience any of the serious adverse events during follow-up

#       25b) Renaming the values for hospitulcerrelated - a variable informing whether hospitalizations were related to the index ulcer
RCTdata2 <- RCTdata2 %>% 
  mutate(hospitulcerrelated = case_when(
    hospitulcerrelated == 1 ~ "definitely_related",
    hospitulcerrelated== 2 ~ "probably_related",
    hospitulcerrelated == 3 ~ "probably_not_related",
    hospitulcerrelated == 4 ~ 'definitely_not_related',
    TRUE        ~ as.character(hospitulcerrelated)
  ))

#       25c) Create a binary variable for whether hospitalizations were ulcer-related
#            this binary variable informs whether hospitalization was "definetely_related"/"probably_related" (value 1) or "probably_not_related"/'definitely_not_related'(value 0)
RCTdata2 <- RCTdata2 %>% 
  mutate(binary_hospitulcerrelated = case_when(
    hospitulcerrelated %in% c("definitely_related", "probably_related") ~ 1,
    hospitulcerrelated %in% c("probably_not_related", "definitely_not_related") ~ 0,
    TRUE ~ NA_real_ 
  ))

#       25d) Renaming the categories for non serious adverse events (listed in table 2 in the protocol article). All of the non-serious adverse events are index ulcer related.

RCTdata2 <- RCTdata2 %>% rename(ulcer_infection = nonseriousadverse___1) #infection of the index ulcer

RCTdata2 <- RCTdata2 %>% rename(exposed_structures = nonseriousadverse___2) #exposure of bone, tendon and/or joint in index ulcer

RCTdata2 <- RCTdata2 %>% rename(osteomyelitis = nonseriousadverse___3) #ostemyelitis of underlying bone was diagnosed

RCTdata2 <- RCTdata2 %>% rename(OR_revision = nonseriousadverse___4) #surgical wound intervention of the index ulcer in an operating theater

RCTdata2 <- RCTdata2 %>% rename(other_nonserious_adv = nonseriousadverse___5) #other adverse events

#       25e) Rename values for amputlevel, informing at which level patients had amputations during follow-up
RCTdata2 <- RCTdata2 %>% 
  mutate(amputlevel = case_when(
    amputlevel == 1 ~ "toe", 
    amputlevel == 2 ~ "transmetatarsal", 
    amputlevel == 3 ~ "forefoot_amputation", 
    amputlevel == 4 ~ "crus", 
    amputlevel == 5 ~'femur', 
    TRUE        ~ as.character(amputlevel)
  ))

#       25f) Defining a variable informing whether amputations where above or below ankle level
RCTdata2 <- RCTdata2 %>% 
  mutate(amput_major_minor = case_when(
    amputlevel %in% c("femur", "crus") ~ "above_ankle",
    amputlevel %in% c("toe", "transmetatarsal", "forefoot_amputation") ~ "below_ankle",
    TRUE ~ NA_character_
  ))

#       25g) Rename values for variable informing whether amputations performed within one year after baseline were above or below ankle
RCTdata2 <- RCTdata2 %>% 
  mutate(oneyearamput_majmin = case_when(
    oneyearamput_majmin == 1 ~ "below_ankle",
    oneyearamput_majmin == 2 ~ "above_ankle",
    TRUE        ~ as.character(oneyearamput_majmin)
  ))

#_________________________________________________________________________________________________________________

#     26) Now, we define variables for change in PROM outcomes from baseline to end of follow-up.
#         Background information: The RCT includes PROM (patient-reported outcome) dara from the EQ-5D-5L and Wound-QoL questionnaires, which were filled out by patients at baseline and at end of follow-up.
#                                 The predefined secondary outcomes of the RCT are change in EQ-5D-5L VAS score and Wound-QoL global score from baseline to end of follow-up.

#     26a) Define a variable for the secondary outcome, change in EQ-5D-5L VAS score, from baseline to end of follow-up
#           explanation of the variable names used:
                #baseline VAS score from the EQ-5D-5L: health_score_baseline
                #end of follow-up VAS score from the EQ-5D-5L: health_score_end
RCTdata2$change_VAS_score <- (RCTdata2$health_score_end-
                                   RCTdata2$health_score_baseline)

#     26b) Define a variable for the secondary outcome, change in Wound-QoL global score, from baseline to end of follow-up
#           explanation of the variable names used:
                #baseline Wound-QoL global score from the Wound-QoL questionnaire: baseline_woundglscore
                #end of follow-up Wound-QoL global score from the Wound-QoL questionnaire: end_woundglscore
RCTdata2$change_woundglscore <- (RCTdata2$end_woundglscore - RCTdata2$baseline_woundglscore)

#     26c) Besides the secondary outcomes, we like to define a variable for change in EQ-5D-5L index score from baseline to end of follow-up
#           explanation of the variable names used:
                #baseline EQ-5D-5L index score: eqindex_base
                #end of follow-up Q-5D-5L index score: eqindex_end
RCTdata2$change_eqindex <- (RCTdata2$eqindex_end - RCTdata2$eqindex_base)

#________________________________________________________________________
#     27) Next step is renaming the values for the five dimensions of the EQ-5D-5L descriptive system to improve comprehension.
#         elaboration: the EQ-5D-5L consists of two parts. One is the descriptive system and the other is a VAS scale. change in VAS score is one of the secondary outcomes of the RCT, and is adressed above.
#                       The descriptive system is not one of the trial outcomes but is calculated merely to describe the patient responses to this part of the EQ-5D-5L. 
#                       The descriptive system contains five questions also called dimensions - the EQ index score mentioned above is calculated from the scores of these five dimensions.

# Before renaming the values of the dimensions: here's a summary of the variable names for the different dimension responses at baseline and end of follow-up. 
# Variables for EQ-5D-5L dimension responses at baseline:
# 1) mobility at baseline: eq5d_mb_5l_den_dan
# 2) self care at baseline: eq5d_sc_5l_den_dan
# 3) usual activity at baseline: eq5d_ua_5l_den_dan
# 4) pain at baseline: eq5d_pd_5l_den_dan
# 5) anxiety/depression at baseline: eq5d_ad_5l_den_dan

# Variables for EQ-5D-5L dimension responses at end of follow-up:
# 1) mobility at end of follow-up: eq5d_mb_5l_den_dan_slut
# 2) self care at end of follow-up: eq5d_sc_5l_den_dan_slut
# 3) usual activity at end of follow-up: eq5d_ua_5l_den_dan_slut
# 4) pain at end of follow-up: eq5d_pd_5l_den_dan_slut
# 5) anxiety/depression at end of follow-up: eq5d_ad_5l_den_dan_slut

# Each dimension has five levels, which are represented by the values 1 to 5. The meaning of the values are the following:
#           1 = 'no problems'
#           2 ='slight problems'
#           3 = 'moderate problems'
#           4 = 'severe problems'
#           5 = 'extreme problems'

#------------------
#       27a) Naming values for baseline EQ-5D-5L Mobility (eq5d_mb_5l_den_dan)
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_mb_5l_den_dan = case_when(
    eq5d_mb_5l_den_dan == 1 ~ 'No problems',
    eq5d_mb_5l_den_dan == 2 ~ 'Slight problems',
    eq5d_mb_5l_den_dan == 3 ~ 'Moderate problems',
    eq5d_mb_5l_den_dan == 4 ~ 'Severe problems',
    eq5d_mb_5l_den_dan == 5 ~ 'Unable to walk about',
    TRUE        ~ as.character(eq5d_mb_5l_den_dan)
  ))

#Keep the original order: 
RCTdata2$eq5d_mb_5l_den_dan <- factor(RCTdata2$eq5d_mb_5l_den_dan, levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'))

#------------------
#       27b)Naming values for end of follow-up EQ-5D-5L mobility score (eq5d_mb_5l_den_dan_slut)
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_mb_5l_den_dan_slut = case_when(
    eq5d_mb_5l_den_dan_slut == 1 ~ 'No problems',
    eq5d_mb_5l_den_dan_slut == 2 ~ 'Slight problems',
    eq5d_mb_5l_den_dan_slut == 3 ~ 'Moderate problems',
    eq5d_mb_5l_den_dan_slut == 4 ~ 'Severe problems',
    eq5d_mb_5l_den_dan_slut == 5 ~ 'Unable to walk about',
    TRUE        ~ as.character(eq5d_mb_5l_den_dan_slut)
  ))

#Keep the original order: 
RCTdata2$eq5d_mb_5l_den_dan_slut <- factor(RCTdata2$eq5d_mb_5l_den_dan_slut,
                                           levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to walk about'))
#------------------
#       27c) Naming values for baseline EQ-5D-5L Self care (eq5d_sc_5l_den_dan)
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_sc_5l_den_dan = case_when(
    eq5d_sc_5l_den_dan == 1 ~ 'No problems',
    eq5d_sc_5l_den_dan == 2 ~ 'Slight problems',
    eq5d_sc_5l_den_dan == 3 ~ 'Moderate problems',
    eq5d_sc_5l_den_dan == 4 ~ 'Severe problems',
    eq5d_sc_5l_den_dan == 5 ~ 'Unable to wash or dress',
    TRUE        ~ as.character(eq5d_sc_5l_den_dan)
  ))

#Keep the original order: 
RCTdata2$eq5d_sc_5l_den_dan <- factor(RCTdata2$eq5d_sc_5l_den_dan, levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress'))

#------------------
#       27d) Naming values for end of follow-up EQ-5D-5L self care score (eq5d_sc_5l_den_dan_slut):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_sc_5l_den_dan_slut = case_when(
    eq5d_sc_5l_den_dan_slut == 1 ~ 'No problems',
    eq5d_sc_5l_den_dan_slut == 2 ~ 'Slight problems',
    eq5d_sc_5l_den_dan_slut == 3 ~ 'Moderate problems',
    eq5d_sc_5l_den_dan_slut == 4 ~ 'Severe problems',
    eq5d_sc_5l_den_dan_slut == 5 ~ 'Unable to wash or dress',
    TRUE        ~ as.character(eq5d_sc_5l_den_dan_slut)
  ))

#Keep the original order: 
RCTdata2$eq5d_sc_5l_den_dan_slut <- factor(RCTdata2$eq5d_sc_5l_den_dan_slut, levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to wash or dress'))

#------------------
#       27e) Naming values for baseline EQ-5D-5L Usual activities (eq5d_ua_5l_den_dan):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_ua_5l_den_dan = case_when(
    eq5d_ua_5l_den_dan == 1 ~ 'No problems',
    eq5d_ua_5l_den_dan == 2 ~ 'Slight problems',
    eq5d_ua_5l_den_dan == 3 ~ 'Moderate problems',
    eq5d_ua_5l_den_dan == 4 ~ 'Severe problems',
    eq5d_ua_5l_den_dan == 5 ~ 'Unable to do usual activities',
    TRUE        ~ as.character(eq5d_ua_5l_den_dan)
  ))

#Keep the original order: 
RCTdata2$eq5d_ua_5l_den_dan <- factor(RCTdata2$eq5d_ua_5l_den_dan, levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities'))

#------------------
#       27f) Naming values for end of follow-up EQ-5D-5L usual activity score (eq5d_ua_5l_den_dan_slut):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_ua_5l_den_dan_slut = case_when(
    eq5d_ua_5l_den_dan_slut == 1 ~ 'No problems',
    eq5d_ua_5l_den_dan_slut == 2 ~ 'Slight problems',
    eq5d_ua_5l_den_dan_slut == 3 ~ 'Moderate problems',
    eq5d_ua_5l_den_dan_slut == 4 ~ 'Severe problems',
    eq5d_ua_5l_den_dan_slut == 5 ~ 'Unable to do usual activities',
    TRUE        ~ as.character(eq5d_ua_5l_den_dan_slut)
  ))

#Keep the original order: 
RCTdata2$eq5d_ua_5l_den_dan_slut <- factor(RCTdata2$eq5d_ua_5l_den_dan_slut, levels = c('No problems', 'Slight problems', 'Moderate problems', 'Severe problems', 'Unable to do usual activities'))

#------------------
#       27g) Naming values for baseline EQ-5D-5L pain (eq5d_pd_5l_den_dan):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_pd_5l_den_dan = case_when(
    eq5d_pd_5l_den_dan == 1 ~ 'No pain or discomfort',
    eq5d_pd_5l_den_dan == 2 ~ 'Slight pain or discomfort',
    eq5d_pd_5l_den_dan == 3 ~ 'Moderate pain or discomfort',
    eq5d_pd_5l_den_dan == 4 ~ 'Severe pain or discomfort',
    eq5d_pd_5l_den_dan == 5 ~ 'Extreme pain or discomfort',
    TRUE        ~ as.character(eq5d_pd_5l_den_dan)
  ))

#Keep the original order: 
RCTdata2$eq5d_pd_5l_den_dan <- factor(RCTdata2$eq5d_pd_5l_den_dan, 
                                      levels = c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort'))

#------------------
#       27h) Naming values for end of follow-up EQ-5D-5L pain (eq5d_pd_5l_den_dan_slut):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_pd_5l_den_dan_slut = case_when(
    eq5d_pd_5l_den_dan_slut == 1 ~ 'No pain or discomfort',
    eq5d_pd_5l_den_dan_slut == 2 ~ 'Slight pain or discomfort',
    eq5d_pd_5l_den_dan_slut == 3 ~ 'Moderate pain or discomfort',
    eq5d_pd_5l_den_dan_slut == 4 ~ 'Severe pain or discomfort',
    eq5d_pd_5l_den_dan_slut == 5 ~ 'Extreme pain or discomfort',
    TRUE        ~ as.character(eq5d_pd_5l_den_dan_slut)
  ))

#Keep the original order: 
RCTdata2$eq5d_pd_5l_den_dan_slut <- factor(RCTdata2$eq5d_pd_5l_den_dan_slut, 
                                          levels = c('No pain or discomfort', 'Slight pain or discomfort', 'Moderate pain or discomfort', 'Severe pain or discomfort', 'Extreme pain or discomfort'))

#------------------
#       27i) Naming values for BASELINE EQ-5D-5L anxiety/depression (eq5d_ad_5l_den_dan):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_ad_5l_den_dan = case_when(
    eq5d_ad_5l_den_dan == 1 ~ 'Not anxious or depressed',
    eq5d_ad_5l_den_dan == 2 ~ 'Slightly anxious or depressed',
    eq5d_ad_5l_den_dan == 3 ~ 'Moderately anxious or depressed',
    eq5d_ad_5l_den_dan == 4 ~ 'Severely anxious or depressed',
    eq5d_ad_5l_den_dan == 5 ~ 'Extremely anxious or depressed',
    TRUE        ~ as.character(eq5d_ad_5l_den_dan)
  ))

#Keep the original order: 
RCTdata2$eq5d_ad_5l_den_dan <- factor(RCTdata2$eq5d_ad_5l_den_dan, 
                                      levels = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed'))

#------------------
#       27j) Naming values for end of follow-up EQ-5D-5L anxiety/depression (eq5d_ad_5l_den_dan_slut):
RCTdata2 <- RCTdata2 %>% 
  mutate( eq5d_ad_5l_den_dan_slut = case_when(
    eq5d_ad_5l_den_dan_slut == 1 ~ 'Not anxious or depressed',
    eq5d_ad_5l_den_dan_slut == 2 ~ 'Slightly anxious or depressed',
    eq5d_ad_5l_den_dan_slut == 3 ~ 'Moderately anxious or depressed',
    eq5d_ad_5l_den_dan_slut == 4 ~ 'Severely anxious or depressed',
    eq5d_ad_5l_den_dan_slut == 5 ~ 'Extremely anxious or depressed',
    TRUE        ~ as.character(eq5d_ad_5l_den_dan_slut)
  ))

#Keep the original order: 
RCTdata2$eq5d_ad_5l_den_dan_slut <- factor(RCTdata2$eq5d_ad_5l_den_dan_slut, 
                                           levels = c('Not anxious or depressed', 'Slightly anxious or depressed', 'Moderately anxious or depressed', 'Severely anxious or depressed', 'Extremely anxious or depressed'))

#_________________________________________________________________________________________________________________________

#     28) The masked_group variable must be a factor variable in the analyses that will be performed.
#         We ensure that masked_group is a factor variable in the dataset:
RCTdata2$masked_group <- as.factor(RCTdata2$masked_group)

#_____________________________________________________________________________________
#     29) We save the dataset RCTdata2, which is the dataset prepared for analysis
write.csv(RCTdata2, file = "path_to_the_folder_in_the_working_directory/RCTdata2.csv")

#_____________________________________________________________________________________

#     30) We define a separate dataset that only includes the per-protocol (PP) population 
# (explanation: the value 0 for the 'perprotocol' variable is for patients that violated the protocol and therefore did not belong to the PP population)
#Define a dataset that only contain perprotocol patients:
PP_population <- RCTdata2[RCTdata2$perprotocol == 1, ]
write.csv(PP_population, file = "path_to_the_folder_in_the_working_directory/PP_population.csv")

#_____________________________________________________________________________________

#     31) We also define two new data sets, one for each randomization group:
orange_data <- RCTdata2 %>%
  filter(masked_group == "orange_group")
write.csv(orange_data, file = "path_to_the_folder_in_the_working_directory/orange_data.csv")

apple_data <- RCTdata2 %>%
  filter(masked_group == "apple_group")
write.csv(apple_data, file = "path_to_the_folder_in_the_working_directory/apple_data.csv")
#___________________________________________________________________________

