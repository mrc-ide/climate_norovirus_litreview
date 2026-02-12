# this script is for meta-analysis and sensitivity analysis within the context of the systematic review

library(dplyr)
library(ggplot2)
library(openxlsx)
library(plan)
library(tidyverse)
library(ggh4x)
library(cowplot)
library(patchwork)

#read data
df <- readxl::read_xlsx("Supplementary Materials 3.xlsx", sheet = "Epi_community_pri_sec_care")

# Summary figure for studies of only good quality and "significant" or "not tested" significance levels for a selected environmental variable
# Environmental variables (for ecological and epidemiological) can be "pH", "temperature", "radiance", "UV wavelength", "UV dose", "precipitation", "absolute humidity", "wind speed (and offshore versus onshore)", "atmospheric pressure", "soil moisture", "daytime hours", "relative humidity"
# If adapting to laboratory studies, the environmental variables can additionally be "normal fluorescent light" and "dessication"

epidemiological <- filter(df, type_of_study_cleaned == "epidemiological") %>%
                   select(first_author, year_of_article, environmental_variables, `CASP TOTAL`, statistical_significance, coefficient_type, coefficient, coefficient_lower_CI, coefficient_higher_CI, environmental_variables_cleaned, continuous_or_dichotomised)

epidemiological_for_pairwise <- unique(epidemiological) %>% filter(!is.na(coefficient) & coefficient_type == "pairwise correlation")
epidemiological <- unique(epidemiological) %>% filter(!is.na(coefficient) & !(is.na(coefficient_lower_CI)))

epidemiological <- mutate(epidemiological, coefficient = as.numeric(coefficient))
epidemiological_for_pairwise <- mutate(epidemiological_for_pairwise, coefficient = as.numeric(coefficient))
epidemiological_for_pairwise <- mutate(epidemiological_for_pairwise, coefficient_lower_CI = as.numeric(coefficient_lower_CI))
epidemiological_for_pairwise <- mutate(epidemiological_for_pairwise, coefficient_higher_CI = as.numeric(coefficient_higher_CI))
epidemiological <- mutate(epidemiological, coefficient_lower_CI = as.numeric(coefficient_lower_CI))
epidemiological <- mutate(epidemiological, coefficient_higher_CI = as.numeric(coefficient_higher_CI))

epidemiological <- mutate(epidemiological, variance = (coefficient_higher_CI - coefficient_lower_CI)^2 / (1.96*2)^2)


#  processing the meta-analysis for the studies presented in the main text by weighting with the inverse of the invariance 
summary_presented <- epidemiological %>% filter(`CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% summarise(number_presented = n()) %>% ungroup()
pairwise_presented <- epidemiological_for_pairwise %>% filter(`CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% summarise(number_presented = n()) %>% ungroup()

summary_presented <- left_join(epidemiological, summary_presented, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))
pairwise_presented <- left_join(epidemiological_for_pairwise, pairwise_presented, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))

summary_presented <- summary_presented %>% filter(`CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(mean_presented = case_when(number_presented == 1 ~ coefficient,
                                                                                                                                                                                                                                                          number_presented > 1 ~ sum(coefficient/variance)/sum(1/variance)))

pairwise_presented <- pairwise_presented %>% filter(`CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(mean_presented = mean(coefficient))

summary_presented <- summary_presented %>% filter(`CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(min_95_presented = case_when(number_presented == 1 ~ coefficient_lower_CI,
                                                                                                                                                                                                                                                            number_presented > 1 ~ sum(coefficient/variance)/sum(1/variance) - 1.96 * sqrt(1/sum(1/variance))))

summary_presented <- summary_presented %>% filter(`CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(max_95_presented = case_when(number_presented == 1 ~ coefficient_higher_CI,
                                                                                                                                                                                                                                                            number_presented > 1 ~ sum(coefficient/variance)/sum(1/variance) + 1.96 * sqrt(1/sum(1/variance))))
summary_presented <- rbind(summary_presented, pairwise_presented)
summary_presented <- filter(summary_presented, !is.na(mean_presented))


#  processing the meta-analysis for the studies included in the appendix by weighting with the inverse of the invariance 
summary_presented_with_poor <- epidemiological %>% filter(statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% summarise(number_presented_with_poor = n()) %>% ungroup()
pairwise_presented_with_poor <- epidemiological_for_pairwise %>% filter(statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% summarise(number_presented_with_poor = n()) %>% ungroup()

summary_presented_with_poor <- left_join(epidemiological, summary_presented_with_poor, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))
pairwise_presented_with_poor <- left_join(epidemiological_for_pairwise, pairwise_presented_with_poor, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))

summary_presented_with_poor <- summary_presented_with_poor %>% filter(statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(mean_presented_with_poor = case_when(number_presented_with_poor == 1 ~ coefficient,
                                                                                                                                                                                                                                                     number_presented_with_poor > 1 ~ sum(coefficient/variance)/sum(1/variance)))

pairwise_presented_with_poor <- pairwise_presented_with_poor %>% filter(statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(mean_presented_with_poor = mean(coefficient))

summary_presented_with_poor <- summary_presented_with_poor %>% filter(statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(min_presented_with_poor = case_when(number_presented_with_poor == 1 ~ coefficient_lower_CI,
                                                                                                                                                                                                                                                       number_presented_with_poor > 1 ~ sum(coefficient/variance)/sum(1/variance) - 1.96 * sqrt(1/sum(1/variance))))

summary_presented_with_poor <- summary_presented_with_poor %>% filter(statistical_significance != "insignificant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(max_presented_with_poor = case_when(number_presented_with_poor == 1 ~ coefficient_higher_CI,
                                                                                                                                                                                                                                                       number_presented_with_poor > 1 ~ sum(coefficient/variance)/sum(1/variance) + 1.96 * sqrt(1/sum(1/variance))))
summary_presented_with_poor <- rbind(summary_presented_with_poor, pairwise_presented_with_poor)
summary_presented_with_poor <- filter(summary_presented_with_poor, !is.na(mean_presented_with_poor))


#  processing the meta-analysis for the studies included in the appendix by weighting with the inverse of the invariance 
summary_best <- epidemiological %>% filter(`CASP TOTAL` != "POOR" & statistical_significance == "significant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% summarise(number_best = n()) %>% ungroup()
pairwise_best <- epidemiological_for_pairwise %>% filter(`CASP TOTAL` != "POOR" & statistical_significance == "significant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% summarise(number_best = n()) %>% ungroup()

summary_best <- left_join(epidemiological, summary_best, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))
pairwise_best <- left_join(epidemiological_for_pairwise, pairwise_best, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))

summary_best <- summary_best %>% filter(`CASP TOTAL` != "POOR" & statistical_significance == "significant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(mean_best = case_when(number_best == 1 ~ coefficient,
                                                                                                                                                                                                                                                number_best > 1 ~ sum(coefficient/variance)/sum(1/variance)))

pairwise_best <- pairwise_best %>% filter(`CASP TOTAL` != "POOR" & statistical_significance == "significant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(mean_best = mean(coefficient))

summary_best <- summary_best %>% filter(`CASP TOTAL` != "POOR" & statistical_significance == "significant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(min_best = case_when(number_best == 1 ~ coefficient_lower_CI,
                                                                                                                                                                                                                                                  number_best > 1 ~ sum(coefficient/variance)/sum(1/variance) - 1.96 * sqrt(1/sum(1/variance))))

summary_best <- summary_best %>% filter(`CASP TOTAL` != "POOR" & statistical_significance == "significant") %>% group_by(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised) %>% mutate(max_best = case_when(number_best == 1 ~ coefficient_higher_CI,
                                                                                                                                                                                                                                                  number_best > 1 ~ sum(coefficient/variance)/sum(1/variance) + 1.96 * sqrt(1/sum(1/variance))))
summary_best <- rbind(summary_best, pairwise_best)
summary_best <- filter(summary_best, !is.na(mean_best))


# keep relevant columns
summary_presented <- summary_presented[, !(names(summary_presented) %in% c("first_author", "year_of_article", "environmental_variables", "CASP TOTAL", "statistical_significance", "coefficient", "coefficient_lower_CI", "coefficient_higher_CI", "variance"))]
summary_presented_with_poor <- summary_presented_with_poor[, !(names(summary_presented_with_poor) %in% c("first_author", "year_of_article", "environmental_variables", "CASP TOTAL", "statistical_significance", "coefficient", "coefficient_lower_CI", "coefficient_higher_CI", "variance"))]
summary_best <- summary_best[, !(names(summary_best) %in% c("first_author", "year_of_article", "environmental_variables", "CASP TOTAL", "statistical_significance", "coefficient", "coefficient_lower_CI", "coefficient_higher_CI", "variance"))]


summary_presented <- summary_presented[!duplicated(summary_presented), ]
summary_presented_with_poor <- summary_presented_with_poor[!duplicated(summary_presented_with_poor), ]
summary_best <- summary_best[!duplicated(summary_best), ]


# join all summaries together
summary <- full_join(summary_presented, summary_presented_with_poor, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))
summary <- full_join(summary, summary_best, by = c("environmental_variables_cleaned", "coefficient_type", "continuous_or_dichotomised"))
remove(summary_presented, summary_presented_with_poor, summary_best)


summary <- filter(summary, !is.na(coefficient_type)) %>% group_by(environmental_variables_cleaned) %>% arrange(environmental_variables_cleaned, coefficient_type, continuous_or_dichotomised)

# round to the appropriate number of digits / note: regression slope coefficient is small hence 4 digits.
new <- summary %>% filter(coefficient_type == "regression slope") %>% mutate(across(is.numeric, round, digits = 4))
summary <- summary %>% filter(coefficient_type != "regression slope") %>% mutate(across(is.numeric, round, digits = 2))
summary <- rbind(summary, new)
remove(new)


# create summary statistics workbook
wb <- createWorkbook()
addWorksheet(wb, "sensitivity_analysis")

writeData(wb, "sensitivity_analysis", summary, startRow = 1, startCol = 1)

saveWorkbook(wb, file = "figures_data/summary_and_sensitivity_analysis.xlsx", overwrite = TRUE)

# Important note: the separation by type of meta-analysis (presented in the main text, and the included sensitivity analysis for poorer and better inclusion criteria for the meta-analysis) was done manually in excel
# The changing from long to wide format as shown in the meta-analysis table in the main-text and supplementary materials was also done manually using pivot table and manual sorting




