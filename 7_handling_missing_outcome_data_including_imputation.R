# THIS SCRIPT ASSESSES THE PROPORTION OF MISSING DATA FOR PRIMARY AND SECONDARY OUTCOMES
# AND DECIDES WHETHER TO USE COMPLETE-CASE ANALYSIS OR MULTIPLE IMPUTATION AS MAIN ANALYSIS OF THE RESPECTIVE OUTCOMES.

# For the primary outcome (healing_blinded), this script also describes worst-best and best-worst case sensitivity analyses at the end of the script.

# Protocol definition:
# - If more than 5% of data is missing for an outcome, multiple imputation will be conducted.
#   Complete-case analysis will then be performed as a sensitivity analysis.
# - If 5% or less of data is missing for an outcome, the main analysis will be complete-case analysis,
#   and multiple imputation will not be performed.

# Additional considerations:
# - For secondary outcomes, missing data due to death will NOT be counted when evaluating the 5% threshold (as predefined in the statistical analysis plan).
# - For the healing outcome, this is not relevant, as non-healing was predefined for patients who died during follow-up.

# Elaboration on best-worst/worst-best sensitivity analysis for healing:
# - If any patient has missing data for healing, a worst-best and best-worst case analysis will be conducted as an additional sensitivity analysis.

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('move_to_final_analysis_file/scripts_for_TEST/TEST_RCTdata2.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:

install.packages("mice")
library(mice)

library(dplyr)
library(lme4)

#_____________________________________________________________________________________________________
# First, convert 'healing_blinded' values where 2, representing non-healing, is converted to 0 (healing is already defined as value=1 in the dataset exported from RedCap)
RCTdata3$healing_blinded[RCTdata3$healing_blinded == 2] <- 0
#make sure that the healing outcome is a factor variable
RCTdata3$healing_blinded <- factor(RCTdata3$healing_blinded)
#_____________________________________________________________________________________________________
# Before deciding if one or more of the outcomes must be imputed, we need to assess the proportion of missing data for each variable
# the below code returns number and percentage of missing data for each of the variables that are defined as primary and secondary outcomes:
sapply(RCTdata3[, c("healing_blinded", "change_VAS_score", "change_woundglscore")], 
       function(x) c(CountMissing = sum(is.na(x)), PercentMissing = mean(is.na(x))*100))

# Remember, for secondary outcomes, missing data due to death will not be counted when evaluating the 5% threshold.
# Therefore, we must assess the number and percentage of missing data for the secondary outcomes exclusively in the population
# that did not experience death during follow-up
# Exclude dead patients
RCTdata_alive <- RCTdata3[RCTdata3$death != 1, ]
# Run missing data summary only on alive patients
sapply(RCTdata_alive[, c("change_VAS_score", "change_woundglscore")], 
       function(x) c(CountMissing = sum(is.na(x)), PercentMissing = mean(is.na(x)) * 100))

# Variables with more than 5% missing data will be analyzed using multiple imputation (MI).
# Analysis using MI is presented below for each of the variables.

#____________________________________________________________________________________________________
# MULTIPLE IMPUTATION
# Create initial MI setup without imputing anything yet

init <- mice(RCTdata3, maxit = 0)  # maxit = 0 → zero iterations; just generates default methods and predictor structure

# Extract predictor matrix (meaning which variables are used to impute others)
pred <- init$predictorMatrix      

# Variables to impute
vars_to_impute <- c("healing_blinded", "change_VAS_score", "change_woundglscore")

# To begin with all variables in the dataset are defined as predictors by default by mice
# Now, we exclude variables that we assessed did not make sense to use as predictors for the three outcomes we want to impute

pred[, c("study_id",   #study_id are anonymized individual names composed by several letters and numbers that by no means can reveal randomization group.
         "X",          # X is row numbers
         "death",
         'oneyear_death',  #all one-year outcome cannot be predictors of healing and change in PROM scores within 20weeks
         "datediff_blood", #informing how many days before or after the baseline visit blood samples were actually taken (a window of +/- 28 days was allowed)
         "losttofollow", #whether patients were lost to follow-up
         "amputation_indexulcer", #whether amputations during follow-up were index ulcer related
         "daystoamput",
         "amputlevel", 
         "inforatiotoamput","amputation_nonindex","amputation_nonindex_level",
         "daysbeforedeath","inforatiotodeath","death_ulcerrelated",
         "days_amputation_to_eq5", #for amputated patients: how many days went from amputation to EQ-5D-5L response        
         #all variables related to end PROM scores will be excluded: 
         "eq5d_mb_5l_den_dan_slut","eq5d_sc_5l_den_dan_slut","eq5d_ua_5l_den_dan_slut","eq5d_pd_5l_den_dan_slut","eq5d_ad_5l_den_dan_slut",
         "health_score_end","eqprofile_end","eqindex_end", "change_eqindex",
         "endwound_item1","endwound_item2","endwound_item3","endwound_item4","endwound_item5","endwound_item6","endwound_item7","endwound_item8","endwound_item9","endwound_item10","endwound_item11","endwound_item12","endwound_item13","endwound_item14","endwound_item15","endwound_item16","endwound_item17","end_woundglscore",
         "perprotocol","no_visits","no_missedvisits", "pct_missed_visits", "allocatedtreat","missedfollow","pp_noneligible", #different variables describing whether patients were per protocol and why
         "clinicheal","daystoclinheal","healing_blindedobserver_1","healing_blindedobserver_2", "daystoblindheal",
         "serious_adv___1","serious_adv___2", #variable for death and amputation (already available as the variables called 'death' and 'amputation' but merely recorded twice in RedCap, "serious_adv___1","serious_adv___2" is from a multiple choice RedCap item regarding serious adverse events)
         "timeshospit","daystohospit","inforatiotohospit","dayshospit", "binary_hospitulcerrelated",
         #days to nonserious adverse events are not included as predictors: 
         "daystoinfection","daystonecrosis","daystoexpose","daystoostit","daystooperation","inforatiotoinfection","inforatiotonecrosis","inforatiotoexpose","inforatiotoostit","inforatiotooperation","finishinfection","daystonoinfection",
         #end of follow-up ulcer area is not included as predictor, including change in area:
         "endarea_1","endarea_2","endarea_3","endarea_4","endarea_5","change_area", "end_ulcer_area","diff_area","pct_increase_area","larger_area",
         #All variables for one-year outcome are excluded as predictors: 
         "oneyeardaystodeath","oneyeardeath_ulcerrelated","oneyear_amput","amput_beforeendfollow","oneyeardaystoamput","oneyearamput_majmin",
         #Information about whether PROMs were filled out/responded to by the patients and whether  responses were complete:
         "base_eq_filled","complete_basewoundqol","base_eq_filled_slut","complete_endwoundqol","correct_endwoundqol", "correct_basewoundqol"
         )] <- 0 

#masked_group is removed as a predictor for imputation
pred["masked_group", ] <- 0  # treatment arm stays as randomized

# Amputation is removed as a predictor of healing as non-healing is defined when patients were amputated during follow-up
#but maintained as a predictor when we impute changes in PROM scores
pred["healing_blinded", "amputation"] <- 0  
pred["healing_blinded", "amput_major_minor"] <- 0  #"amput_major_minor" informs whether amputations were above or below ankle level

# prevent self-prediction of the variables we want to impute
pred[cbind(vars_to_impute, vars_to_impute)] <- 0  

#Some variables were expressed in several ways
#e.g. height and weight was reported as well as BMI
#- in these cases just one of the variables were picked
#The variables that we chose to exclude, because the same subject was expressed in several variables, were the following:

pred[, c ("diab_type", #one variable called diabetes also reports diabetes type
"height","weight", #bmi is chosen as the predictor variable
"comorbidities___3", #the category for patients being diagnosed with kidney insufficiency, but instead we use a variable informing baseline eGFR <60 as predefined in the protocol
"femalealcohol","alcoholabuse_female","malealcohol","alcoholabuse_male", #instead the common variables for both sexes were included as a predictors: "alcohol_more_than_recommended" and "alcohol_abuse_total"
"av_glucose", #average plasma glucose levels calculated based on the HbA1c blood test
"base_egfr", "base_krea","egfr_over90", #baseline eGFR <60 was chosen as the predictor variable, as predefined in the protocol
"eq5d_mb_5l_den_dan","eq5d_sc_5l_den_dan","eq5d_ua_5l_den_dan","eq5d_pd_5l_den_dan","eq5d_ad_5l_den_dan", #the baseline dimensions from the EQ-5D-5L are not included as predictors, instead the baseline VAS score is included as a predictor
"eqprofile_base", #neither is baseline health profile included, however baseline EQ index scores are included as predictors ("eqindex_base")
#the specific item responses from baseline Wound-Qol are not included (baseline global score is):
"baselinewound_item1","baselinewound_item2","baselinewound_item3","baselinewound_item4","baselinewound_item5","baselinewound_item6","baselinewound_item7","baselinewound_item8","baselinewound_item9","baselinewound_item10","baselinewound_item11","baselinewound_item12","baselinewound_item13","baselinewound_item14","baselinewound_item15","baselinewound_item16","baselinewound_item17",
"baselinearea_1","baselinearea_2","baselinearea_3","baselinearea_4","baselinearea_5", #The individual five ulcer area measures are not included, just the average of the three middle values expressed by the variable: base_ulcer_area
"number_comorbidities",#number of the eight predefined comorbidities- is already expressed by the binary variable number of comorbidities =>4 or <4 ("four_or_more_comorb")
"ulcer_duration_MONTHS","ulcer_duration_YEARS","weeks_beginfollow_to_base","beginfollow_to_base_MONTHS" #ulcer duration and time treated at a specialized wound care unit is expressed in different variable as days, weeks and months - we keep just the variables expressed by days as predictors
)] <- 0 

# Note, the dataset which hold the predictor variables for imputation include the predefined auxiliary variables from the protocol
# these variables are:
#     "hospital"
#     "base_ulcer_area"
#     "ulcer_duration"
#     "plantar"
#     "age"
#     "bmi"
#     "smoking"
#     "alcohol_more_than_recommended" (according to Danish Health Authorities at the time of trial commencement - see script regarding preparation of data (2_prepare_dataset_for_analysis) for further elaboration on this variable alcohol_more_than_recommended),
#     "health_score_baseline" (health_score_baseline is the baseline EQ VAS score)
#     "baseline_woundglscore"
#     "base_hba1c"
#     "egfr_binary"
#     "amputation" (amputation is the variable for amputation of the index limb at any level during follow-up)

#Define methods for each outcome we are going to impute
method <- init$method
method[] <- "" #to make sure only the three variables healing_blinded, change_VAS_score and change_woundglscore are  imputed, we first define that none of the variables from pred must be imputed and afterwards define that we want to impute the three variables by defining a method for each of them.
method["healing_blinded"] <- "logreg"  # logistic regression imputation for binary outcome
method["change_VAS_score"] <- 'pmm'    # predictive mean matching for continuous outcomes
method["change_woundglscore"] <- 'pmm' 

#Run multiple imputation:
imp <- mice(RCTdata3,
            m = 50,                # number of imputations generated is 50 as described in the protocol
            method = method,       # method defined above for each of the outcomes
            predictorMatrix = pred,
            maxit = 20,            # number of times the imputation process is repeated to reach a stable final result (convergence)
            seed = 12345) # ensures reproducibility: R uses the same random starting point, so the imputations and pooled results will be identical if the code is rerun

#Next step is diagnostics to check imputations
# a) Trace plots: check convergence of the imputation algorithm
plot(imp)  
# Each line shows how the imputed values change over iterations.
# Ideally, we want lines to stabilize (not jump up/down), indicating the algorithm has converged.

# b) Density plot: compare distribution of imputed vs observed values
densityplot(imp, ~ healing_blinded)  
# Shows the distribution of the imputed values for "healing_blinded" across all datasets.
# Ideally, we want the imputed values to overlap reasonably with the observed values.

# c) Strip plot: visualize individual imputed values
stripplot(imp, healing_blinded ~ .imp, pch = 20)  
# Each point represents an imputed value in each dataset.
# Helps spot unusual/impossible values (e.g., 2 or -1 for a binary outcome, which shouldn't occur).

# What we use the results from a)+b)+c) for:
# - Ensure the MI algorithm converged properly (trace plots)
# - Check plausibility of imputed values (density + strip plots)
# - Identify any obvious errors before running analysis


#-------------------
# The main analysis for the primary outcome 'healing_blinded' is conducted using MI

# Run the mixed-effects logistic regression on each imputed dataset
# imp_fit_healing will store the results for all imputed datasets
imp_fit_healing <- with(imp, glmer(
  healing_blinded ~ masked_group + ulcer_duration_MONTHS + base_ulcer_area +
    plantar + base_hba1c + four_or_more_comorb +
    (1 | hospital),
  family = binomial
))

# Extract coefficients and variances from each imputed model
betas <- lapply(imp_fit_healing$analyses, fixef)
vars  <- lapply(imp_fit_healing$analyses, function(m) vcov(m))

# Convert to matrices
betas_mat <- do.call(rbind, betas)

# Rubin’s rules pooling
m <- length(betas)                           # number of imputations
qbar <- colMeans(betas_mat)                  # mean across imputations
ubar <- Reduce("+", vars) / m                # average within-imputation variance
b <- cov(betas_mat)                          # between-imputation variance
tvar <- ubar + (1 + 1/m) * b                 # total variance

# Standard errors
se <- sqrt(diag(tvar))

# Combine into a summary table
pooled_results_imp <- data.frame(
  Estimate = qbar,
  StdError = se,
  Lower95  = qbar - 1.96 * se,
  Upper95  = qbar + 1.96 * se
)

print(pooled_results_imp)

#Elaborations about the Rubin's rules pooling approach: 
# The estimate, that we name qbar in the last code block,
# is simply the average of the regression coefficients
# obtained across all imputed datasets. Each imputation
# gives slightly different estimates (q1, q2, ..., qm),
# and qbar is their mean. This pooled estimate serves as
# the central value in Rubin's rules, around which the
# total variance is calculated.

# The estimate, that we name ubar, is the average of the
# within-imputation variances. Each imputed dataset gives
# its own variance estimate for the coefficients, and ubar
# is the mean of those. It reflects the uncertainty you
# would have if you only analyzed one imputed dataset.

# The estimate, that we name b, is the between-imputation
# variance. It measures how much the regression coefficients
# differ across imputations. In other words, b captures the
# extra uncertainty introduced by the fact that missing data
# were imputed in multiple different ways.

# The estimate, that we name tvar, is the total variance.
# It combines ubar (within-imputation variance) and b
# (between-imputation variance), adjusted for the number
# of imputations. tvar therefore represents the overall
# uncertainty of the pooled estimates, accounting for both
# the variability inside each imputation and the variability
# across imputations.

#--------------------
# Linear regression model for change in EQVAS score using MI

# Fit the model on each imputed dataset
imp_fit_EQVAS <- with(imp, lm(change_VAS_score ~ masked_group + health_score_baseline))

# Pool results across all imputed datasets
pooled_EQVAS <- pool(imp_fit_EQVAS)

# Retrieve estimated coefficients with p-values and 95%CI
summary(pooled_EQVAS, conf.int=TRUE)

# ───────────────────────────────────────────────
# Linear regression model for change in Wound-QoL global score using MI

# Fit the model on each imputed dataset
imp_fit_WoundQoL <- with(imp, lm(change_woundglscore ~ masked_group + baseline_woundglscore))

# Pool results across all imputed datasets
pooled_WoundQoL <- pool(imp_fit_WoundQoL)

# Retrieve estimated coefficients with p-values and 95%CI
summary(pooled_WoundQoL, conf.int=TRUE)

#____________________________________________________________________________________________________
# CONDUCTING WORST-BEST AND BEST-WORST SENSITIVITY ANALYSIS FOR HEALING ASSESSED BLINDLY

# 1) Make a copy of the dataset
sensitivity_data <- RCTdata3

# 2) Identify missing values for healing
missing_idx <- which(is.na(sensitivity_data$healing_blinded))

# ------------------------------------------------
# Best-worst case (apple_group assumed to have best outcome)
# - Missing values for apple_group are imputed as healed (1)
# - Missing values for orange_group are imputed as not healed (0)
sensitivity_data_best_worst <- sensitivity_data

sensitivity_data_best_worst$healing_blinded[missing_idx] <- ifelse(
  sensitivity_data_best_worst$masked_group[missing_idx] == "apple_group", 1, 0
)

# Run logistic regression
best_worst_model <- glmer(
  healing_blinded ~ masked_group + ulcer_duration_MONTHS + base_ulcer_area +
    plantar + base_hba1c + four_or_more_comorb +
    (1 | hospital),
  data = sensitivity_data_best_worst,
  family = binomial
)

# Output results
summary(best_worst_model)
confint(best_worst_model)

# ------------------------------------------------
# Worst-best case (apple_group assumed to have worst outcome)
# - Missing values for apple_group are imputed as not healed (0)
# - Missing values for orange_group are imputed as healed (1)
sensitivity_data_worst_best <- sensitivity_data

sensitivity_data_worst_best$healing_blinded[missing_idx] <- ifelse(
  sensitivity_data_worst_best$masked_group[missing_idx] == "apple_group", 0, 1
)

# Run logistic regression
worst_best_model <- glmer(
  healing_blinded ~ masked_group + ulcer_duration_MONTHS + base_ulcer_area +
    plantar + base_hba1c + four_or_more_comorb +
    (1 | hospital),
  data = sensitivity_data_worst_best,
  family = binomial
)

# Output results
summary(worst_best_model)
confint(worst_best_model)

# Note: After unblinding, if apple_group represents the intervention group,
# the current naming of best-worst and worst-best case analyses will be maintained.
# If, after unblinding, apple_group represents the control group, the names should be switched
# so that the first model is called worst-best and the second model best-worst,
# to ensure the naming reflects which group is assumed to have the best outcome.


