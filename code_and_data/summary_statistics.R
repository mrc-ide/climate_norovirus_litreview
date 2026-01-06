# this script is for producing brief summary statistics as outlined below

library(dplyr)
library(ggplot2)
library(openxlsx)

#read data
df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Data Detailed")

df <- filter(df, environmental_variables != "pH")

# create summary statistics workbook
wb <- createWorkbook()

##################################################################################
# create a new sheet for by study, by environmental variable and by norovirus type
addWorksheet(wb, "study_envrnm_nov_type")

summary <- df %>% select(covidence_id, type_of_study_cleaned, environmental_variables_cleaned, norovirus_types_cleaned_3) %>% 
                  unique() %>% 
                  group_by(type_of_study_cleaned, environmental_variables_cleaned, norovirus_types_cleaned_3) %>%
                  summarise(count = n(), .groups = "drop") 

writeData(wb, "study_envrnm_nov_type", summary, startRow = 1, startCol = 1)

#########################################################################
# create a new sheet for by study, by environmental variable, direction of relationship, quality, significance and region
addWorksheet(wb, "study_envrnm_direction_type")

summary <- df %>% select(covidence_id, type_of_study_cleaned, environmental_variables_cleaned, type_of_relationship_2, direction_of_relationship, region_type_cleaned_2, `CASP TOTAL`, statistical_significance) %>%
                  unique() %>%
                  group_by(type_of_study_cleaned, region_type_cleaned_2, environmental_variables_cleaned, direction_of_relationship, `CASP TOTAL`, statistical_significance) %>%
                  summarise(count = n(), .groups = "keep")

writeData(wb, "study_envrnm_direction_type", summary, startRow = 1, startCol = 1)

#########################################################################
# create a new sheet for by study, by environmental variable, direction of relationship for high quality and significant relationships
addWorksheet(wb, "study_envrnm_direction_select")

summary <- df %>% filter((`CASP TOTAL` == "GOOD" | `CASP TOTAL` == "ADEQUATE") & statistical_significance != "insignificant") %>%
                  select(covidence_id, type_of_study_cleaned, environmental_variables_cleaned, type_of_relationship_2, direction_of_relationship) %>%
                  unique() %>%
                  group_by(type_of_study_cleaned, environmental_variables_cleaned, direction_of_relationship) %>%
                  summarise(count = n(), .groups = "keep") 

summary_2 <- summary %>% group_by(type_of_study_cleaned, environmental_variables_cleaned) %>%
                         summarise(count_all = sum(count), .groups = "keep") 
  
summary <- left_join(summary, summary_2, by = c("type_of_study_cleaned", "environmental_variables_cleaned"))
remove(summary_2)
summary$proportion <- round(summary$count/summary$count_all*100, 2)

writeData(wb, "study_envrnm_direction_select", summary, startRow = 1, startCol = 1)

################################################################
# sheet by study type and tested mechanisms of influence
addWorksheet(wb, "study_mechanism_tested")

summary <- df %>% select(covidence_id, type_of_study_cleaned, mechanism_of_influence_cleaned, mechanism_of_influence_tested_or_assumed, environmental_variables_cleaned) %>%
                  filter(mechanism_of_influence_tested_or_assumed == "tested") %>%
                  unique() %>%
                  group_by(type_of_study_cleaned, mechanism_of_influence_cleaned) %>%
                  summarise(count = n(), variables = list(unique(environmental_variables_cleaned)), .groups = "keep")

writeData(wb, "study_mechanism_tested", summary, startRow = 1, startCol = 1)

################################################################
# sheet by study type and assumed mechanisms of influence
addWorksheet(wb, "study_mechanism_assumed")

summary <- df %>% select(covidence_id, type_of_study_cleaned, mechanism_of_influence_cleaned, mechanism_of_influence_tested_or_assumed, environmental_variables_cleaned) %>%
  filter(mechanism_of_influence_tested_or_assumed == "assumed") %>%
  unique() %>%
  group_by(type_of_study_cleaned, mechanism_of_influence_cleaned) %>%
  summarise(count = n(), variables = list(unique(environmental_variables_cleaned)), .groups = "keep")

writeData(wb, "study_mechanism_assumed", summary, startRow = 1, startCol = 1)

saveWorkbook(wb, file = "figures_data/summary_statistics.xlsx", overwrite = TRUE)

################################################################
# sheet by study type and all mechanisms of influence
addWorksheet(wb, "study_mechanism_all")

summary <- df %>% select(covidence_id, type_of_study_cleaned, mechanism_of_influence_cleaned, environmental_variables_cleaned) %>%
  unique() %>%
  group_by(type_of_study_cleaned, mechanism_of_influence_cleaned) %>%
  summarise(count = n(), variables = list(unique(environmental_variables_cleaned)), .groups = "keep",)

writeData(wb, "study_mechanism_all", summary, startRow = 1, startCol = 1)

saveWorkbook(wb, file = "figures_data/summary_statistics.xlsx", overwrite = TRUE)


