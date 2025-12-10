#THIS SCRIPT IS FOR THE PREDEFINED LINEAR REGRESSION ANALYSIS OF THE SECONDARY TRIAL OUTCOMES:
#CHANGE IN THE EQ-5D-5L VAS SCORE AND WOUND-QOL GLOBAL SCORE FROM BASELINE TO END OF FOLLOW-UP

# Note, the analyses in this script are complete-case analyses
# Another script (7_handling_missing_outcome_data_including_imputation) assess proportion of missing data for the secondary outcomes
# to determine whether multiple imputation is needed for the main analysis of the secondary outcomes
# the 7_handling_missing_outcome_data_including_imputation script also include codes for multiple imputation of the secondary outcomes
# the READ.ME file refers to a protocol article from BMJ Open -here the predefined statistical analysis is outlined

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('Path_to_the_folder_in_the_working_directory/RCTdata2.csv')
PP_population <- read.csv ('Path_to_the_folder_in_the_working_directory/PP_population.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:
library(dplyr)
library(ggplot2) 
library(tidyr)
library(car)

#______________________________________________________________________________________________________
#The predefined analyses of the secondary outcomes was linear regression models that included baseline scores
#of the respective outcome (EQ VAS or Wound-QoL global score) as a covariate. The main analysis was on the ITT population
#and the predefined sensitivity analysis was on the per protocol population.

# IMPORTANT NOTE ABOUT REFERENCE GROUP FOR MASKED_GROUP IN THE REGRESSION MODELS:
# After unblinding, the control group will be set as reference for the variable masked_group.

# By default, R orders factor levels alphabetically, meaning that apple_group
# will automatically become the reference category in linear regression models
# unless the reference is manually specified.
# If the apple_group is revealed as the control group, no changes will be made.
# But if the orange_group turns out to represent the control group, the orange_group 
# will be defined as the reference group to run the regression analyses again.
#
# This is done with the following code:
#   RCTdata3$masked_group <- relevel(RCTdata3$masked_group, ref = "orange_group")
#
# The current reference level can be verified using:
#   levels(RCTdata3$masked_group)

#______________________________________________________________________________________________________
#THE MAIN ANALYSES FOR THE SECONDARY OUTCOMES:
# (However note, if missing data for the outcomes are more than 5%, the below, which are a complete case analyses, will merely be a sensitivity analyses according to the protocol
# and the main analyses will be an analysis using multiple imputation (the approach is described and coded in another script called 7_handling_missing_outcome_data_including_imputation))

#Linear regression model for change in EQVAS score in the ITT population
#First step is to create a dataset where patients that miss data on change in VAS score are excluded
no_missing_RCTdata3 <- subset(RCTdata3, !is.na(change_VAS_score))

linreg_model_EQVAS <- lm(change_VAS_score ~ masked_group + health_score_baseline, data=no_missing_RCTdata3)

summary(linreg_model_EQVAS)         # retrieving the estimated coefficients with p-values
confint(linreg_model_EQVAS)         # retrieving confidence intervals of the coefficients

#_____________________________________
#Linear regression model for change in Wound-QoL global score in the ITT population
#First step is to create a dataset where patients that miss data on change in global score are excluded
no_missing_woundglscore_RCTdata3 <- subset(RCTdata3, !is.na(change_woundglscore))

linreg_model_woundglscore <- lm(change_woundglscore ~ masked_group + baseline_woundglscore, data=no_missing_woundglscore_RCTdata3)

summary(linreg_model_woundglscore)         # retrieving the estimated coefficients with p-values
confint(linreg_model_woundglscore)         # retrieving confidence intervals of the coefficients

#_________________________________________________________________________________
#NOW, THE SENSITIVITY ANALYSES ON THE PERPROTOCOL POPULATION

#Linear regression model for change in EQVAS score in the PP population
#First step is to create a dataset where patients that miss data on change in VAS score are excluded
no_missing_PP_data <- subset(PP_population, !is.na(change_VAS_score))

PP_linreg_model_EQVAS <- lm(change_VAS_score ~ masked_group + health_score_baseline, data=no_missing_PP_data)

summary(PP_linreg_model_EQVAS)         # retrieving the estimated coefficients with p-values
confint(PP_linreg_model_EQVAS)         # retrieving confidence intervals of the coefficients

#_______________________
#Linear regression model for change in Wound-QoL global score in the PP population
#First step is to create a dataset where patients that miss data on change in global score are excluded
no_missing_woundglscore_PP_data <- subset(PP_population, !is.na(change_woundglscore))

PP_linreg_model_woundglscore <- lm(change_woundglscore ~ masked_group + baseline_woundglscore, data=no_missing_woundglscore_PP_data)

summary(PP_linreg_model_woundglscore)         # retrieving the estimated coefficients with p-values
confint(PP_linreg_model_woundglscore)         # retrieving confidence intervals of the coefficients

#__________________________________________________________
#TEST ASSUMPTIONS FOR THE REGRESSION MODELS PERFORMED ON THE ITT POPULATION

#The ITT POPULATION model for EQVAS - linreg_model_EQVAS
# 1) Normal distribution of residuals using a QQ-plot
plot( linreg_model_EQVAS, which=2)

# 2) Homoscedasticity – residuals plotted against fitted (= predicted) values
plot( linreg_model_EQVAS, which=1)

# 3) Linearity must be assessed for quantitative explanatory variables in the model
#Thus, linearity is assessed for baseline EQVAS (health_score_baseline):
plot( no_missing_RCTdata3$change_VAS_score ~ no_missing_RCTdata3$health_score_baseline, col = "blue")
abline(linreg_model_EQVAS, col = "red", lwd = 2)
# Explanation: This plots the observed change in health score (change_VAS_score)
# against the predictor variable health_score_baseline, and overlays the regression line
# from the model linreg_model_EQVAS.
# Interpretation: By visually inspecting this plot one can assess whether the relationship
# between health_score_baseline and change_VAS_score appears linear. If the data points 
# follow the line reasonably well, the linearity assumption is likely satisfied.

# 4) Check for collinearity
# In a multiple regression model there is a risk that some covariates may be collinear.
# To investigate collinearity we use Variance Inflation Factor (VIF).
VIF_ITT_EQVAS <- vif( linreg_model_EQVAS)
VIF_ITT_EQVAS
#values above 5–10 are problematic
# Explanation: VIF indicates how much the variance of each coefficient is inflated due to collinearity.

#5) DIAGNOSTICS: check for outliers
# A commonly used cutoff for large Cook’s distance is 4/n (n being the sample size)
plot(linreg_model_EQVAS, which=4) 

#add the 4/n line with below code - insert sample size to replace insert_sample_size_here. 
#Note: the sample size will depend on how many patients had missing data on change in EQ VAS score
abline( 4/insert_sample_size_here, 0, col = "red", lty = 2)

# If we want the numerical Cook’s distance values for each individual:
round( cooks.distance( linreg_model_EQVAS ), 2)

#__________________________________________________________
#Similar steps are used to test assumption for:
  #The ITT POPULATION model for Wound_QoL global score - - linreg_model_woundglscore

# 1) Normal distribution of residuals using a QQ-plot
plot( linreg_model_woundglscore, which=2)

# 2) Homoscedasticity – residuals plotted against fitted (= predicted) values
plot( linreg_model_woundglscore, which=1)    

# 3) Linearity is assessed for baseline Wound-QoL global score:
plot( no_missing_woundglscore_RCTdata3$change_woundglscore ~ no_missing_woundglscore_RCTdata3$baseline_woundglscore, col = "blue")
abline(linreg_model_woundglscore, col = "red", lwd = 2)

# 4) Check for collinearity
VIF_ITT_woundglscore <- vif( linreg_model_woundglscore)
VIF_ITT_woundglscore

#5) DIAGNOSTICS: check for outliers
plot(linreg_model_woundglscore, which=4) 
#insert sample size to replace insert_sample_size_here:
abline( 4/insert_sample_size_here, 0, col = "red", lty = 2)
# If we want the numerical Cook’s distance values for each individual:
round( cooks.distance( linreg_model_woundglscore ), 2)



