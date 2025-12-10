#THIS SCRIPT IS FOR ANALYSIS OF THE CONTINUOUS VARIABLES RECORDED BOTH AT BASELINE AND AFTER END OF FOLLOW-UP

#     1) upload the blinded dataset - notice that we called the dataset 'RCTdata2' in the preceding script where the data was prepared for analysis (2_prepare_dataset_for_analysis).
RCTdata3 <- read.csv ('path_to_folder_in_working_directory/RCTdata2.csv')

#_____________________________________________________________________________________________________

#     2) Install necessary packages not already installed in the preceding scripts and retrieve all packages needed from the library:
library(dplyr)

install.packages("ggplot2")
library(ggplot2)  

install.packages("coin")
library(coin)

install.packages("xlsx")
library(xlsx)

#____________________________________________________________________________________________________
#     3) Now, we select the continuous variables that were recorded at baseline and after end of follow-up to create a tibble/dataframe (called cont_variables) 
#         solely consisting of the continuous variables selected in the following code:
cont_variables <- RCTdata3 %>% 
  select(
#baseline variables:
         age, 
         diab_duration, 
         ulcer_duration,
         toe_pressure,#toe pressure in index foot, taken only for patients without palpable pedal pulses
         weeks_beginfollow_to_base,#the time span in weeks that patients have been receiving treatment of their index ulcer at a specialized wound care unit before baseline
         bmi,#BMI
         datediff_blood,#for practical reasons blood samples taken in the interval of ±28. The variable 'datediff_blood' indicates how many days the sample was taken relative to baseline: negative values reflect samples taken before baseline, and positive values reflect samples taken after.
         base_hba1c,
         base_krea,#baseline creatinine
         base_egfr, #the maximum reported eGFR was >90 - this variable therefore has a ceiling effect meaning that a mean does not make sense to report for this variable - we can beforehand decide that this variable should be treated as a non-parametric variable
         number_comorbidities, #number of the eight predefined variables explained in the script regarding analysis of baseline categorical variables (3_descriptive_analysis_baseline_categorical_variables)
         base_ulcer_area, #baseline ulcer area
         
#variables recorded after end of follow-up: 
      #time to healing variables:
         daystoclinheal, #time from baseline to healing observed at a trial visit by non-blinded health care staff
         daystoblindheal, #Time from baseline to healing, as determined from photographs by blinded assessors, for ulcers where both assessors agreed that healing was evident in the end-of-follow-up image.
 
      #ulcer area at end of follow-up
         end_ulcer_area,#ulcer area at end of follow-up
         diff_area, # difference in ulcer area: end of follow-up ulcer area minus baseline ulcer area
         pct_increase_area, #percentage of ulcer area increase(positive value) or decrease (negative value) from baseline to end of follow-up
 
      #time from baseline to adverse events:
         daystoamput,#amputation at any level of the index extremity
         daysbeforedeath,
         daystohospit,#first hospitalization during follow-up, if any
         daystoinfection,#index ulcer infection
         daystonecrosis, #necrosis appearing in the index ulcer
         daystoexpose, #exposed bone, joint or tendon in the index ulcer
         daystoostit, #the timepoint at which osteomyelitis underlying the index ulcer was diagnosed
         daystooperation, #surgical revision of the index ulcer at an operating theater
         oneyeardaystodeath, #days to death event within the one-year follow-up
         oneyeardaystoamput,#days to amputation at any level of the index extremity within the one-year follow-up
 
      #For the intervention group, the below variables beginning with 'inforatioto' measure the time from the most recent Inforatio technique treatment session to the occurrence of an adverse event.
      #For the control group, they measure the time from their most recent scheduled trial visit that corresponds to an Inforatio treatment visit in the intervention arm (i.e., baseline or the 3-, 6-, 9-, or 12-week visits).
      #This approach was used primarily to preserve blinding; otherwise, the variables below would reveal allocation because the control group would consistently have NA values.
         inforatiotoamput,
         inforatiotohospit,         
         inforatiotodeath,
         inforatiotoinfection,
         inforatiotonecrosis,
         inforatiotoexpose,
         inforatiotoostit,
         inforatiotooperation,
 
       #other variables recorded after end of follow-up:
         days_amputation_to_eq5, #for patients going through amputatations at any level of their index extremity: how many days that went from the day of amputation to responding to the EQ-5D-5L questionnaire
         no_missedvisits,#number of missed visits during follow-up
         no_infora_visits, #number of missed visits where the inforatio technique would, according to the protocol, be performed on patients from the intervention group (these are the 3-,6-,9-, and 12-week visits) - it is recorded for both groups whether these 3-,6-,9-, and 12-week visits were missed
         pct_missed_visits, #percentage of missed trial visits within the follow-up period of each patient
         timeshospit#how many times patients were hospitalized during follow-up
  )

#___________________________________________________________________________________________________
# The function `sumStat1` is defined to generate descriptive statistics for the continuous variables listed in the tibble `cont_variables` above.
# Although the function computes several descriptive measures, only a selected subset is included in the final output. This output is then saved as an Excel table.
# If additional measures from the function are needed, they can be included by adding them to the selection in the line creating `samlet`, for example:
# samlet <- data.frame( ... select the desired measures here ... )
# Later in the script, we also evaluate the distribution of both the baseline variables and the blindly assessed time-to-healing variable. 
# For variables that are non-parametric, a separate function (`sumStat_nonparametric`) is defined and used to report the median and quartiles.
sumStat1 <- function(x){
  ntotal <- length(x)   
  nmiss  <- sum(is.na(x))
  n      <- ntotal - nmiss
  mean_x   <- mean(x, na.rm = TRUE)
  median_x <- median(x, na.rm = TRUE)
  Q1       <- quantile(x, probs = 0.25, na.rm = TRUE)
  Q3       <- quantile(x, probs = 0.75, na.rm = TRUE)
  SD_x     <- sd(x, na.rm = TRUE)
  SEM_x    <- SD_x / sqrt(n)
  min_x    <- min(x, na.rm = TRUE)
  max_x    <- max(x, na.rm = TRUE)
samlet <- data.frame(
    n     = n,
    mean  = mean_x,
    SD    = SD_x,
    min   = min_x,
    max   = max_x,
    nmiss = nmiss
  )
  row.names(samlet) <- NULL
  return(samlet)
}

sumsta1_result <- cont_variables %>%
  group_by(RCTdata3$masked_group) %>%
  summarise_all(sumStat1)

# Transpose the data - to have randomization group in the columns and the descriptive statistics results in rows for each variable:
result_transposed1 <- t(sumsta1_result)

# Assign the first row as column names
colnames(result_transposed1) <- result_transposed1[1, ]

# Remove the first row
result_transposed1 <- result_transposed1[-1, ]

#Create an Excel file for the table:
write.xlsx(result_transposed1, file='path_to_folder_in_working_directory/table_continuous_data.xlsx', sheetName = "result_transposed1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#_____________________________________________________________________________________________________

#Box plots displaying each baseline variable and time to healing:

#Age:
ggplot(RCTdata3, aes(x = masked_group, y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot of age at baseline, presented by treatment group")

#Diabetes duration: 
ggplot(RCTdata3, aes(x = masked_group, y = diab_duration)) +
  geom_boxplot() +
  labs(title = "Boxplot of diabetes duration at baseline, presented by treatment group")

#Ulcer_duration
ggplot(RCTdata3, aes(x = masked_group, y = ulcer_duration)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline ulcer duration by treatment group")

#Toe_pressure (for patients without palpable pedal pulses)
ggplot(RCTdata3, aes(x = masked_group, y = toe_pressure)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline toe pressure by treatment group")

#Weeks_beginfollow_to_base
ggplot(RCTdata3, aes(x = masked_group, y = weeks_beginfollow_to_base)) +
  geom_boxplot() +
  labs(title = "Boxplot of number of weeks index ulcers where treated at a specialized wound care unit up untill baseline, presented by treatment group")

#BMI:
ggplot(RCTdata3, aes(x = masked_group, y = bmi)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline BMI by treatment group") 

#Datediff_blood
ggplot(RCTdata3, aes(x = masked_group, y = datediff_blood)) +
  geom_boxplot()

#base_hba1c
ggplot(RCTdata3, aes(x = masked_group, y = base_hba1c)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline HbA1c by treatment group")

#base_krea
ggplot(RCTdata3, aes(x = masked_group, y = base_krea)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline plasma creatinine by treatment group")

#base_egfr 
ggplot(RCTdata3, aes(x = masked_group, y = base_egfr)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline eGFR by treatment group")

#number_comorbidities
ggplot(RCTdata3, aes(x = masked_group, y = number_comorbidities)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline number of predefined comorbidities recorded, presented by treatment group")

#Baseline ulcer area:
ggplot(RCTdata3, aes(x = masked_group, y = base_ulcer_area)) +
  geom_boxplot() +
  labs(title = "Boxplot of baseline ulcer area by treatment group")

#Time to healing from baseline to the timepoint where images where taken - specifically images for which ulcer healing was reported by blinded observers:
ggplot(RCTdata3, aes(x = masked_group, y = daystoblindheal)) +
  geom_boxplot() +
  labs(title = "Boxplot of time to healing, presented by treatment group")

#________________________________________________________________________________________
# Next step: Assessing the distributions of baseline characteristics continuous variables and the time to healing in each randomization group, using histograms and the Shapiro–Wilk test.
# The distributions are evaluated to determine whether the data are approximately normally distributed (parametric) or non-normally distributed (non-parametric), in order to guide:
#           - The selection of descriptive statistics: mean and standard deviation for variables with approximately normal distributions, and median and interquartile range for variables with non-normal distributions.
#           - The selection of appropriate statistical tests for comparing randomization groups: a t-test for variables with approximately normal distributions, and a Mann–Whitney U test for variables with non-normal distributions.

#Interpretation of the Shapiro-Wilks test: if the p-value >0.05 for both randomization groups, it suggests that the variable follows a roughly normal distribution.

# For variables showing near-normal distributions, the selection of a t-test also requires that the assumption of homogeneity of variances between groups is assessed. 
# Homogeneity of variances is evaluated using an F-test. If the F-test indicates equal variances, a Student’s t-test is applied; if the variances are unequal, a Welch t-test is used instead.

# Log transformation of variables:
# If histograms show that data distribution is heavily skewed, the variable in question will be assessed for normality after log transformation (both using histograms and Shapiro-Wilks test on the log-transformed variable).
# If the log-transformed variable looks roughly normal, a t-test will be conducted on the log-transformed variable. If not, a Mann-Whitney U test will be performed on the original variable without transformation.
# When reporting results, we show means (or medians for non-parametric variables) on the original scale for easier interpretation.
# Thus, for log-transformed variables, geometric means (exp(mean(log(name_of_variable)))) are reported to reflect the original scale.
# The p-value is taken directly from the t-test on the transformed data (no adjustment needed for the original scale).

#___________________________________________________________________________________________________________________

# HISTOGRAMS (with density curves of the data and theoretical normal curve) and Shapiro Wilks tests to assess normality:

#ASSESSING NATURE OF DATA FOR AGE
#a) Histograms
# Create histogram for apple_group
apple_age_hist <- ggplot(apple_data, aes(x = age)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$age), sd = sd(apple_data$age)),
                color = 'red', size = 1.5) +
  labs(x = 'Age', y = 'Density', title = 'Age for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_age_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_age_hist.png", plot = apple_age_hist, width = 8, height = 6)

#Create histogram for orange_group
orange_age_hist <- ggplot(orange_data, aes(x = age)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$age), sd = sd(orange_data$age)),
                color = 'red', size = 1.5) +
  labs(x = 'Age', y = 'Density', title = 'Age for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_age_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_age_hist.png", plot = orange_age_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$age)
shapiro.test(orange_data$age)

#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA FOR DIABETES DURATION
#a) Histograms
# Create histogram for apple_group
apple_DMduration_hist <- ggplot(apple_data, aes(x = diab_duration )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$diab_duration), sd = sd(apple_data$diab_duration)),
                color = 'red', size = 1.5) +
  labs(x = 'Diabetes duration (years)', y = 'Density', title = 'Diabetes duration for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_DMduration_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_DMduration_hist.png", plot = apple_DMduration_hist, width = 8, height = 6)

# Create histogram for orange_group
orange_DMduration_hist <- ggplot(orange_data, aes(x = diab_duration)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$diab_duration), sd = sd(orange_data$diab_duration)),
                color = 'red', size = 1.5) +
  labs(x = 'Diabetes duration (years)', y = 'Density', title = 'Diabetes duration for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_DMduration_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_DMduration_hist.png", plot = orange_DMduration_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$diab_duration)
shapiro.test(orange_data$diab_duration)

#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA FOR ULCER DURATION
#a) Histograms
# Create histogram for apple_group
apple_ulcerduration_hist <- ggplot(apple_data, aes(x = ulcer_duration )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$ulcer_duration), sd = sd(apple_data$ulcer_duration)),
                color = 'red', size = 1.5) +
  labs(x = 'Ulcer duration (weeks)', y = 'Density', title = 'Ulcer duration for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_ulcerduration_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_ulcerduration_hist.png", plot = apple_ulcerduration_hist, width = 8, height = 6)

# Create histogram for orange_group
orange_ulcerduration_hist <- ggplot(orange_data, aes(x = ulcer_duration)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$ulcer_duration), sd = sd(orange_data$ulcer_duration)),
                color = 'red', size = 1.5) +
  labs(x = 'Ulcer duration (weeks)', y = 'Density', title = 'Ulcer duration for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_ulcerduration_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_ulcerduration_hist.png", plot = orange_ulcerduration_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$ulcer_duration)
shapiro.test(orange_data$ulcer_duration)

#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA FOR TOE PRESSURE
#a) Histograms
# Create histogram for apple_group toe pressure
apple_toe_pressure_hist <- ggplot(apple_data, aes(x = toe_pressure )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$toe_pressure), sd = sd(apple_data$toe_pressure)),
                color = 'red', size = 1.5) +
  labs(x = 'Toe pressure (mmHg)', y = 'Density', title = 'Toe pressure for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_toe_pressure_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_toe_pressure_hist.png", plot = apple_toe_pressure_hist, width = 8, height = 6)

# Create histogram for orange_group toe pressure
orange_toe_pressure_hist <- ggplot(orange_data, aes(x = toe_pressure)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$toe_pressure), sd = sd(orange_data$toe_pressure)),
                color = 'red', size = 1.5) +
  labs(x = 'Toe pressure (mmHg)', y = 'Density', title = 'Toe pressure for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_toe_pressure_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_toe_pressure_hist.png", plot = orange_toe_pressure_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$toe_pressure)
shapiro.test(orange_data$toe_pressure)

#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA For weeks_beginfollow_to_base
#a) Histograms
# Create histogram for apple_group weeks_beginfollow_to_base
apple_followduration_hist <- ggplot(apple_data, aes(x = weeks_beginfollow_to_base )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$weeks_beginfollow_to_base), sd = sd(apple_data$weeks_beginfollow_to_base)),
                color = 'red', size = 1.5) +
  labs(x = 'Days', y = 'Density', title = 'Duration of treatment at a wound care outpatient clinic before baseline for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_followduration_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_followduration_hist.png", plot = apple_followduration_hist, width = 8, height = 6)

#Create histogram for orange_group weeks_beginfollow_to_base
orange_followduration_hist <- ggplot(orange_data, aes(x = weeks_beginfollow_to_base)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$weeks_beginfollow_to_base), sd = sd(orange_data$weeks_beginfollow_to_base)),
                color = 'red', size = 1.5) +
  labs(x = 'Days', y = 'Density', title = 'Duration of treatment at a wound care outpatient clinic before baseline for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_followduration_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_followduration_hist.png", plot = orange_followduration_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$weeks_beginfollow_to_base )
shapiro.test(orange_data$weeks_beginfollow_to_base )

#_____________________________________________________________________________________________________
#ASSESSING NATURE OF DATA For BMI
#a) Histograms
# Create histogram for apple_group BMI
apple_bmi_hist <- ggplot(apple_data, aes(x = bmi)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth= 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$bmi), sd = sd(apple_data$bmi)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'BMI (kg/m²)', y = 'Density', title = 'BMI (Body Mass Index) for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_bmi_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_bmi_hist.png", plot = apple_bmi_hist, width = 8, height = 6)

#Create histogram for orange_group BMI
orange_bmi_hist <- ggplot(orange_data, aes(x = bmi)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$bmi), sd = sd(orange_data$bmi)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'BMI (kg/m²)', y = 'Density', title = 'BMI (Body Mass Index) for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_bmi_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_bmi_hist.png", plot = orange_bmi_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$bmi)
shapiro.test(orange_data$bmi )

#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA For base_hba1c
#a) Histograms
# Create histogram for apple_group base_hba1c
apple_hba_hist <- ggplot(apple_data, aes(x = base_hba1c)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth= 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$base_hba1c), sd = sd(apple_data$base_hba1c)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'HbA1c (mmol/mol)', y = 'Density', title = 'HbA1c (glycated hemoglobin) for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_hba_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_hba_hist.png", plot = apple_hba_hist, width = 8, height = 6)

#Create histogram for orange_group base_hba1c
orange_hba_hist <- ggplot(orange_data, aes(x = base_hba1c)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$base_hba1c), sd = sd(orange_data$base_hba1c)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'HbA1c (mmol/mol)', y = 'Density', title = 'HbA1c (glycated hemoglobin) for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_hba_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_hba_hist.png", plot = orange_hba_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$base_hba1c)
shapiro.test(orange_data$base_hba1c )

#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA For base_krea
#a) Histograms
# Create histograms for apple_group base_krea
apple_crea_hist <- ggplot(apple_data, aes(x = base_krea)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth= 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$base_krea), sd = sd(apple_data$base_krea)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'µmol/L', y = 'Density', title = 'Plasma-creatinine for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_crea_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_crea_hist.png", plot = apple_crea_hist, width = 8, height = 6)

# Create histograms for orange_group base_krea
orange_crea_hist <- ggplot(orange_data, aes(x = base_krea)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$base_krea), sd = sd(orange_data$base_krea)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'µmol/L', y = 'Density', title = 'Plasma-creatinine for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_crea_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_crea_hist.png", plot = orange_crea_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$base_krea)
shapiro.test(orange_data$base_krea)
#_________________________________________________________________________________________________________
#ASSESSING NATURE OF DATA For number_comorbidities 
#a) Histograms
# Create histogram for apple_group number_comorbidities 
apple_nr_comorb_hist <- ggplot(apple_data, aes(x = number_comorbidities )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth= 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$number_comorbidities ), sd = sd(apple_data$number_comorbidities )),
                color = 'red', linewidth = 1.5) +
  labs(x = 'Number of comorbidities', y = 'Density', title = 'Number of comorbidities recorded for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_nr_comorb_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_nr_comorb_hist.png", plot = apple_nr_comorb_hist, width = 8, height = 6)

#Create histogram for orange_group number_comorbidities 
orange_nr_comorb_hist <- ggplot(orange_data, aes(x = number_comorbidities )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$number_comorbidities ), sd = sd(orange_data$number_comorbidities )),
                color = 'red', linewidth = 1.5) +
  labs(x = 'Number of comorbidities', y = 'Density', title = 'Number of comorbidities recorded for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_nr_comorb_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_nr_comorb_hist.png", plot = orange_nr_comorb_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$number_comorbidities )
shapiro.test(orange_data$number_comorbidities )

#____________________________________________________________________________
#NATURE OF DATA For BASELINE ULCER AREA
#a) Histograms
# Create histogram for apple_group base_ulcer_area
apple_base_area_hist <- ggplot(apple_data, aes(x = base_ulcer_area)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth= 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$base_ulcer_area), sd = sd(apple_data$base_ulcer_area)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'Ulcer area (cm2)', y = 'Density', title = 'Baseline ulcer area for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_base_area_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_base_area_hist.png", plot = apple_base_area_hist, width = 8, height = 6)

#Create histogram for orange_group base_ulcer_area
orange_base_area_hist <- ggplot(orange_data, aes(x = base_ulcer_area)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$base_ulcer_area), sd = sd(orange_data$base_ulcer_area)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'Ulcer area (cm2)', y = 'Density', title = 'Baseline ulcer area for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_base_area_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_base_area_hist.png", plot = orange_base_area_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$base_ulcer_area)
shapiro.test(orange_data$base_ulcer_area)

#__________________________________________________________________________________
#ASSESSÍNG NATURE OF DATA For BLINDED AGREED Time to healing
#a) Histograms
# Create histogram for apple_group daystoblindheal
apple_blindheal_hist <- ggplot(apple_data, aes(x = daystoblindheal)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$daystoblindheal), sd = sd(apple_data$daystoblindheal)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'Days to observed healing', y = 'Density', title = 'Time to blindly assessed healing for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_blindheal_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_blindheal_hist.png", plot = apple_blindheal_hist, width = 8, height = 6)

# Create histogram for orange_group daystoblindheal
orange_blindheal_hist <- ggplot(orange_data, aes(x = daystoblindheal)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$daystoblindheal), sd = sd(orange_data$daystoblindheal)),
                color = 'red', linewidth = 1.5) +
  labs(x = 'Days to observed healing', y = 'Density', title = 'Time to blindly assessed healing for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_blindheal_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_blindheal_hist.png", plot = orange_blindheal_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$daystoblindheal)
shapiro.test(orange_data$daystoblindheal)

#--------------------
#CODE FOR ASSESSING NORMALITY OF LOG TRANSFORMED VARIABLES 
# - log transformation is relevant for variables with heavily skewed distribution
#Notice that the codes for histograms and Shapiro-Wilks test below are templates - to perform a code 'insert_variable_here' has to be replaced by the name of the variable in question

#a) First, log transformed variables must be added to both the dataset for apple_group and orange_group
apple_data$log_insert_variable_here <- log(apple_data$insert_variable_here)
orange_data$log_insert_variable_here <- log(orange_data$insert_variable_here)

#a) Histograms of log transformed variables
# Create histogram for apple_group
apple_log_insert_variable_here_hist <- ggplot(apple_data, aes(x = log_insert_variable_here )) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(apple_data$log_insert_variable_here), sd = sd(apple_data$log_insert_variable_here)),
                color = 'red', size = 1.5) +
  labs(x = 'log transformed insert unit measure of the variable here', y = 'Density', title = 'Log transformed insert_variable_here for the apple group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(apple_log_insert_variable_here_hist)

ggsave(filename = "path_to_folder_in_working_directory/apple_log_insert_variable_here_hist.png", plot = apple_log_insert_variable_here_hist, width = 8, height = 6)

# Create histogram for orange_group
orange_log_insert_variable_here_hist <- ggplot(orange_data, aes(x = log_insert_variable_here)) +
  geom_histogram(fill = 'steelblue1', color = 'black', bins = 30, aes(y = ..density..)) +
  geom_density(color = 'blue', size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(orange_data$log_insert_variable_here), sd = sd(orange_data$log_insert_variable_here)),
                color = 'red', size = 1.5) +
  labs(x = 'Log transformed insert unit measure of the variable here', y = 'Density', title = 'Log transformed insert_variable_here for the orange group') +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = 'topright') +
  guides(fill = guide_legend(title = 'Distribution'))

print(orange_log_insert_variable_here_hist)

ggsave(filename = "path_to_folder_in_working_directory/orange_log_insert_variable_here_hist.png", plot = orange_log_insert_variable_here_hist, width = 8, height = 6)

#b) Perform Shapiro-Wilk test for each group
shapiro.test(apple_data$log_insert_variable_here )
shapiro.test(orange_data$log_insert_variable_here )

#________________________________________________________________
# Now, we have assessed normality. Therefore we now know which variables are non-parametric.
# Descriptive statistics of non-parametric variables will now be performed

# First, select the non-parametric variables to create a new tibble only containing the non-parametric variables:
non_parametric_variables <- RCTdata3 %>% 
  select(insert_all_non_parametric_variables_here,
         base_egfr #we beforehand assessed eGFR to be non-parametric - see above in the script were we create tibble cont_variables
         )

#create a function (sumStat_nonparametric) that retrieves median, quartiles, minimum and maximum:
sumStat_nonparametric <- function(x){
  ntotal=length(x)   
  nmiss=ntotal-table(is.na(x))[1] # antal manglende vaerdier
  n=ntotal-nmiss                  # antal med observerede vaerdier
  mean=mean(x,na.rm=T)
  median=median(x,na.rm=T)
  Q1=quantile(x, probs=.25,na.rm=T)
  Q3=quantile(x, probs=.75,na.rm=T)
  SD=sd(x,na.rm=T)
  SEM <- SD / sqrt(n)
  min=min(x,na.rm=T)
  max=max(x,na.rm=T)
  samlet <- data.frame(n,median,Q1,Q3,min,max)
  row.names(samlet) <- NULL
  return( samlet ) 
}

sumStat_nonparametric_result <- non_parametric_variables %>%
  group_by(RCTdata3$masked_group) %>%
  summarise_all(sumStat_nonparametric)

# Transpose the data - to have randomization group in the columns and the descriptive statistics results in rows for each variable:
result_nonparametric <- t(sumStat_nonparametric_result)

# Assign the first row as column names
colnames(result_nonparametric) <- result_nonparametric[1, ]

# Remove the first row
result_nonparametric <- result_nonparametric[-1, ]

#Create an Excel file for the table:
write.xlsx(result_nonparametric, file='path_to_folder_in_working_directory/table_nonparametric_data.xlsx', sheetName = "result_nonparametric", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#____________________________________________________________________________________________________________
#For the variables with roughly normal distribution, the groups are compared with t-test in the following order:
#(Notice that the codes are templates, and to perform a code 'insert_variable_here' has to be replaced by the name of the variable in question)

#First, an F-test is conducted. This tests whether the variances of the variable in question are equal between the two groups.
      #A p-value >0.05 → variances are not significantly different (assumption of equal variances is reasonable meaning the Student's t-test is ok).
      #A p-value <0.05 → variances differ → use Welch’s t-test.
var.test(insert_variable_here~masked_group, data = RCTdata3)

#Student's t-test is conducted when p>0.05 for the F-test
t.test(insert_variable_here~masked_group, data = RCTdata3, var.equal=TRUE)

#Welch test is conducted when p<0.05 for the F-test
t.test(insert_variable_here~masked_group, data = RCTdata3)

#Notice, in case of a log transformed variable, the values for confidence intervals and for mean must be converted to the original scale before reported:
tt_insert_variable_here <- t.test(insert_variable_here~masked_group, data = RCTdata3)
      #original scale mean
original_scale_mean_insert_variable_here <- exp(tt_insert_variable_here$estimate)
original_scale_mean_insert_variable_here

      #original scale CI
orig_CI_insert_variable_here <- exp(tt_insert_variable_here$conf.int)
orig_CI_insert_variable_here 

#____________  
#For the non-parametric variables, the groups are compared with a Mann-Withney U test (also called the Wilcoxon rank-sum test)
wilcox.test(RCTdata3$insert_variable_here ~ RCTdata3$masked_group)
