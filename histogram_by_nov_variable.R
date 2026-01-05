install.packages("plan")

library(dplyr)
library(ggplot2)
library(openxlsx)
library(plan)
library(tidyverse)

#read data
df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Data Detailed")

##################################################################################
count <- select(df, first_author, type_of_study_cleaned, norovirus_types)

count <- unique(count[, c('first_author', 'type_of_study_cleaned', 'norovirus_types')])

count <- group_by(count, type_of_study_cleaned, norovirus_types) %>% summarise(count = n(), .groups = 'drop')

plot <- ggplot(count, aes(x=norovirus_types, y=count)) + 
        geom_bar(stat = "identity") + 
        facet_wrap(~type_of_study_cleaned, scales = "free_x") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot

ggsave("figures_data/studies_by_norovirus_types.png", height = 10, width = 12)

##################################################################################
count <- select(df, first_author, type_of_study_cleaned, norovirus_types_cleaned_3)

count <- unique(count[, c('first_author', 'type_of_study_cleaned', 'norovirus_types_cleaned_3')])

count <- group_by(count, type_of_study_cleaned, norovirus_types_cleaned_3) %>% summarise(count = n(), .groups = 'drop')

plot <- ggplot(count, aes(x=norovirus_types_cleaned_3, y=count)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~type_of_study_cleaned, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot

ggsave("figures_data/studies_by_norovirus_types_cleaned_3.png", height = 10, width = 12)



