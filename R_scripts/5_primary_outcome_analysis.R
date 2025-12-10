#THIS SCRIPT IS FOR ANALYSIS OF COMPLETE HEALING WITHIN TRIAL FOLLOW-UP. THE PRIMARY OUTCOME OF THE RCT IS HEALING ASSESSED BY BLINDED OBSERVERS ON IMAGES.
# Note, the analyses in this script are complete-case analyses
# Another script (7_handling_missing_outcome_data_including_imputation) assess proportion of missing data for healing 
# to 1) determine whether multiple imputation is needed for the main analysis of healing
# and 2) whether worst-best and best-worst sensitivity analyses must be conducted for healing.
# the 7_handling_missing_outcome_data_including_imputation script also include codes for multiple imputation and worst-best/best-worst analysis.
# the READ.ME file refers to a protocol article from BMJ Open -here the predefined statistical analysis is outlined

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata2.csv')
PP_population <- read.csv ('Path_to_the_folder_in_the_working_directory/PP_population.csv')
#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:

library(dplyr)
library(ggplot2)  
library(tidyr)

install.packages("car")
library(car)

install.packages("lme4")
library(lme4)

install.packages("ResourceSelection")
library(ResourceSelection)

#_________________________________________________________________________________
# First, the values of the variable for healing assessed on images by blinded observers (healing_blinded) must be defined so that healing==1 and non-healing==0.
# This is to have it as a binary variable, which can be analyzed in a logistic regression analysis.

# Convert 'healing_blinded' values where 2, representing non-healing, is converted to 0 (healing is already defined as value=1 in the dataset exported from RedCap)
#both for the RCTdata3 and PP_population dataset:
RCTdata3$healing_blinded[RCTdata3$healing_blinded == 2] <- 0
PP_population$healing_blinded[PP_population$healing_blinded == 2] <- 0

# the values for the other three healing outcomes (healing_blindedobserver_1,healing_blindedobserver_2, healing_blinded) are already 
# defined with value==1 defining healing and value==0 defining non-healing, in the dataset exported from RedCap

#__________________________________________________________________________________
#     4) Now, we select the different variables for healing to create a tibble/dataframe (called healing_variables) 
#         solely consisting of the healing variables selected in the following code:
healing_variables <- RCTdata3 %>% 
  select(clinicheal, #healing observed at the last trial visit by a non-blinded health care professional at the wound care outpatient clinic of the recruiting hospital
         healing_blindedobserver_1,#healing assessment by blinded observer 1 on images (there were two blinded observers in total)
         healing_blindedobserver_2,#healing assessment by blinded observer 2 
         healing_blinded,#healing assessment reported by the blinded observers after disagreements had been discussed and a final agreement was achieved
         masked_group
  )

#________________________________________________________________________________
#     5) We generate patient counts for each of the healing variables in the tibble defined above as healing_variables. 
#           The patient counts are reported for each randomization group.

#     5a) First, create a list to store frequency tables. 
healing_frequency_tables <- list()
for (var_name in names(healing_variables)) {
  healing_freq_table <- healing_variables %>%
    group_by(masked_group, !!sym(var_name)) %>%
    summarise(Frequency = n()) %>%
    pivot_wider(names_from = !!sym(var_name), values_from = Frequency, values_fill = 0)
  #Save the resulting frequency table (healing_freq_table) in the list, using the healing variable name as the list entry name.
  healing_frequency_tables[[var_name]] <- healing_freq_table
}

# Now healing_frequency_tables is a list containing frequency tables for each variable

#       5b) The frequency tables are accessed for each of the healing variable:

healing_frequency_tables$clinicheal
healing_frequency_tables$healing_blindedobserver_1
healing_frequency_tables$healing_blindedobserver_2
healing_frequency_tables$healing_blinded

#       5c) The code above in 5b) reports frequency tables of healing for the ITT population - now we will look at healing in the PP population: 
tapply(PP_population$healing_blinded, PP_population$masked_group, table)
tapply(PP_population$clinicheal, PP_population$masked_group, table)

#___________________________________________________________________________________

#The predefined analysis of healing was a mixed effects logistic regression with:
#       - hospital as a random effect variable
#       - the following fixed effect variables: 
#                   - ulcer location (plantar/not plantar): plantar (value for plantar location = 1 & for not plantar location = 2)
#                   - ulcer duration at baseline (months): ulcer_duration_MONTHS 
#                   - baseline ulcer area (cm2): base_ulcer_area
#                   - baseline HbA1c (Hemoglobin A1c):base_hba1c
#                   - presence of four or more of the following diagnoses (variable name: four_or_more_comorb); hypertension, history of acute myocardial infarction, history of stroke, congestive heart failure, chronic obstructive pulmonary disease, liver cirrhosis, eGFR (estimated Glomerular Filtration Rate)<60 mL/min 1.73 m2 and diabetic retinopathy

# IMPORTANT NOTE ABOUT REFERENCE GROUP FOR MASKED_GROUP IN THE REGRESSION MODELS:
# After unblinding, the control group will be set as reference for the variable masked_group.

# By default, R orders factor levels alphabetically, meaning that apple_group
# will automatically become the reference category in the logistic regression models
# unless the reference is manually specified.
# If the apple_group is revealed as the control group no changes will be made.
# But if the orange_group turns out to represent the control group, the orange_group 
# will be defined as the reference group to run the regression analyses again.
# This is done by the following code:
#   RCTdata3$masked_group <- relevel(RCTdata3$masked_group, ref = "orange_group")
#
# The current reference level can be verified using:
#   levels(RCTdata3$masked_group)

#_________________________________________________________________________________________

#THE PREDEFINED PRIMARY ANALYSIS OF HEALING was the above described mixed effects logistic regression conducted on the ITT population and with healing assessed blindly (healing_blinded) as the dependent variable:
# Note, if missing data for healing is more than 5%, the below, which is a complete case analysis, will merely be a sensitivity analysis according to the protocol predefined statistical analysis plan
# and the primary analysis will be an analysis using multiple imputation (the approach is described and coded in another script called 7_handling_missing_outcome_data_including_imputation)

# Fit the mixed-effects logistic regression model
primary_model <- glmer(healing_blinded ~ masked_group + ulcer_duration_MONTHS + base_ulcer_area+
                         plantar + base_hba1c + four_or_more_comorb +
                         (1 | hospital),
                       data = RCTdata3, family = binomial)

# Get the summary of the model
primary_model_summary <- summary(primary_model)

# Extract coefficients for fixed effects only
primary_modelfixed_effects <- fixef(primary_model)

# Convert coefficients to odds ratios
primary_modelodds_ratios <- exp(primary_modelfixed_effects)

# Get confidence intervals for fixed effects and convert them to CIs for the odds ratio:
primary_modelconf_int <- confint(primary_model, parm = "beta_", method = "Wald")
primary_modelconf_int_exp <- exp(primary_modelconf_int)

# Combine results into a data frame
primary_modelresults <- data.frame(
  Term = names(primary_modelfixed_effects),
  OddsRatio = primary_modelodds_ratios,
  CI_Lower = primary_modelconf_int_exp[, 1],
  CI_Upper = primary_modelconf_int_exp[, 2],
  p_value = primary_model_summary$coefficients[, "Pr(>|z|)"]
)

# Print the results
print(primary_modelresults)

#________________________________________________________________________________________

#SENSITIVITY ANALYSES FOR THE MIXED EFFECT LOGISTIC REGRESSION
# the following sensitivity analyses were predefined in the protocol:
#             - analysis where the dependent variable is the non-blinded assessment of healing (clinicheal) at the last trial visit performed by health care professionals at the wound care unit
#             - analysis of blindly assessed healing on images in the per protocol population (PP_population)

#---------------
# THE SENSITIVITY ANALYSIS WHERE NON-BLINDED CLINICALLY ASSESSED HEALING IS THE DEPENDENT VARIABLE INSTEAD OF THE BLINDLY ASSESSED HEALING ON IMAGES:
sensitivity_model_clinicheal <- glmer(clinicheal ~ masked_group + ulcer_duration_MONTHS + base_ulcer_area+
                                        plantar + base_hba1c + four_or_more_comorb +
                                        (1 | hospital),
                                      data = RCTdata3, family = binomial)


# Get the summary of the model
sensitivity_model_clinichealsummary <- summary(sensitivity_model_clinicheal)

# Extract coefficients for fixed effects only
sensitivity_model_clinichealfixed_effects <- fixef(sensitivity_model_clinicheal)

# Convert coefficients to odds ratios
sensitivity_model_clinichealodds_ratios <- exp(sensitivity_model_clinichealfixed_effects)

# Get confidence intervals for fixed effects and convert them to CIs for the odds ratio:
sensitivity_model_clinichealconf_int <- confint(sensitivity_model_clinicheal, parm = "beta_", method = "Wald")
sensitivity_model_clinichealconf_int_exp <- exp(sensitivity_model_clinichealconf_int)

# Combine results into a data frame
sensitivity_model_clinichealresults <- data.frame(
  OddsRatio = sensitivity_model_clinichealodds_ratios,
  CI_Lower = sensitivity_model_clinichealconf_int_exp[, 1],
  CI_Upper = sensitivity_model_clinichealconf_int_exp[, 2],
  p_value = sensitivity_model_clinichealsummary$coefficients[, "Pr(>|z|)"]
)

# Print the results
print(sensitivity_model_clinichealresults)

#_________________________________________________________________________________
#THE SENSITIVITY ANALYSIS FOR THE PERPROTOCOL POPULATION WITH BLINDLY ASSESSED HEALING AS THE DEPENDENT VARIABLE

PP_blindhealing_model <- glmer(healing_blinded ~ masked_group + ulcer_duration_MONTHS + base_ulcer_area+
                                 plantar + base_hba1c + four_or_more_comorb +
                                 (1 | hospital),
                               data = PP_population, family = binomial)

# Get the summary of the model
PP_blindhealing_model_summary <- summary(PP_blindhealing_model)

# Extract coefficients for fixed effects only
PP_blindhealing_modelfixed_effects <- fixef(PP_blindhealing_model)

# Convert coefficients to odds ratios
PP_blindhealing_modelodds_ratios <- exp(PP_blindhealing_modelfixed_effects)

# Get confidence intervals for fixed effects and convert them to CIs for the odds ratio:
PP_blindhealing_modelconf_int <- confint(PP_blindhealing_model, parm = "beta_", method = "Wald")
PP_blindhealing_modelconf_int_exp <- exp(PP_blindhealing_modelconf_int)

# Combine results into a data frame
PP_blindhealing_modelresults <- data.frame(
  OddsRatio = PP_blindhealing_modelodds_ratios,
  CI_Lower = PP_blindhealing_modelconf_int_exp[, 1],
  CI_Upper = PP_blindhealing_modelconf_int_exp[, 2],
  p_value = PP_blindhealing_model_summary$coefficients[, "Pr(>|z|)"]
)

# Print the results
print(PP_blindhealing_modelresults)

#_______________________________________________________________________________
#TEST ASSUMPTIONS for primary_model logistic regression (the primary analysis that was performed on the ITT population and where blindly assessed healing was the dependent variable)

#       a) HOSMER-LEMESHOW - testing Goodness of fit

# Perform the Hosmer-Lemeshow test with predicted probabilities, using 10 groups
hosleme_test_primary_model <- hoslem.test(RCTdata3$healing_blinded, 
                                          predict(primary_model, type = "response"),
                                          g = 10)

print(hosleme_test_primary_model)


#How to interpret the results from this test:
# High p-value (> 0.05) → No evidence of poor fit (model fits the data adequately)
# Low p-value (< 0.05) → Evidence of lack of fit (the model may not fit the data well)
# If the p-value is < 0.05, the test provides evidence of poor model fit, indicating that the logistic regression model may not adequately describe the data.

#--------------------------------

#       b) LINEARITY of the logit
# Only relevant for the continuous variables in the model – not the categorical.
# The logistic regression model assumes that the relationship between each continuous predictor 
# and the log-odds of the outcome is linear.
# We test this by creating Pearson-residual plots with a LOESS smoother for each continuous variable.

# The following predictor variables will be assessed for linearity of the logit:
# - ulcer duration
# - ulcer area
# - hba1c

# If the scatter plot shows clear violations of the linearity assumption, it indicates that
# the OR estimates for that specific variable are not valid, as the violation suggests that 
# the estimated effect (OR) may be biased or misleading.

# Note: Any violations of the linearity assumption among continuous predictors do not affect 
# the validity of the OR for the randomization group (masked_group), which is a binary variable 
# and the primary focus of the logistic regression model.

# Get the Pearson residuals
primary_model_pearson_residuals <- resid(primary_model, type = "pearson")

# Create scatterplots of Pearson residuals against continuous predictor variables
continuous_predictors <- c("ulcer_duration", "base_ulcer_area", "base_hba1c")


# Loop through each continuous predictor and save the corresponding plot
for (predictor in continuous_predictors) {
  plot_data <- data.frame(Pearson_Residuals = primary_model_pearson_residuals, Predictor = RCTdata3[[predictor]])
  
  primary_model_residplot <- ggplot(plot_data, aes(x = Predictor, y = Pearson_Residuals)) +
    geom_point() +
    geom_smooth(method = "loess") +
    xlab(predictor) +
    ylab("Pearson Residuals") +
    ggtitle(paste("Scatterplot of Pearson Residuals for model without logged variables vs. ", predictor))
  
  # Define the filename for the saved plot based on the predictor variable
  filename <- paste("path_to_folder_in_working_directory/", predictor, "_residplot.png", sep = "")
  
  # Save the plot
  ggsave(filename, plot = primary_model_residplot, device = "png", width = 10, height = 6, units = "in")
}

#--------------------------------
#       c) COOK'S DISTANCE
# Cook's distance is used to identify influential observations in the logistic regression model.
# Influential observations are data points that, if removed, would substantially change 
# the estimated regression coefficients (including the odds ratios).
#
# Cook's distance measures the influence of each entire observation based on:
# 1) Leverage – how unusual the combination of predictor values is (across ALL predictors).
#    Continuous predictors often contribute more because they vary widely, 
#    but categorical predictors can also create influential points 
#    (e.g., rare categories or unusual category combinations).
# 2) Residual size – how poorly the model predicts that observation.
#
# It identifies observations (patients) whose overall pattern has a large impact on the model.
#
# A commonly used rule of thumb is that observations with Cook's distance > 4/n 
# (where n is the sample size) may be considered influential and should be inspected further.
# Influential points do not necessarily indicate an error but may warrant closer investigation 
# for potential data issues or clinically unusual characteristics.
#
# Compute Cook's distance
cd <- cooks.distance(primary_model)

cook_data <- data.frame(Observation = 1:length(cd), CooksD = cd)

ggplot(cook_data, aes(x = Observation, y = CooksD)) +
  geom_point() +
  geom_hline(yintercept = 4/length(cd), color = "red") + #above the red line are the observations with Cook's distance > 4/n
  labs(title = "Cook's Distance Plot", x = "Observation", y = "Cook's Distance")

# If the Cook's distance plot or cutoff suggests influential observations,
# we can inspect the exact Cook's distance values for each participant with this code:
round(cooks.distance(primary_model), 2)
# This helps identify which specific observations have unusually high influence.

#--------------------------------
#       d) MULTICOLLINARITY
# Checks for high correlation between predictor variables.
# High multicollinearity inflates standard errors and makes coefficients unstable,
# meaning that the estimated effect of a predictor can change drastically with small changes
# in the data, and confidence intervals become wider, reducing interpretability.
# VIF (Variance Inflation Factor) > 5-10 is considered problematic.

# Elaborations on VIF interpretation:
# - VIF ≤ 5: low to moderate multicollinearity
# - 5 < VIF ≤ 10: moderate to high multicollinearity
# - VIF > 10: serious multicollinearity, coefficients may be highly unreliable

VIF <- vif(primary_model)
VIF
