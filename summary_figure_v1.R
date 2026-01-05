install.packages("plan")
install.packages("ggh4x")

library(dplyr)
library(ggplot2)
library(openxlsx)
library(plan)
library(tidyverse)
library(ggh4x)

#read data
df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Data Detailed")

##################
# Function Section

# Summary figure for studies of all qualities and significance levels for a selected environmental variable
# Environmental variables (for ecological and epidemiological) can be "pH", "temperature", "radiance", "UV wavelength", "UV dose", "precipitation", "absolute humidity", "wind speed (and offshore versus onshore)", "atmospheric pressure", "soil moisture", "daytime hours", "relative humidity"
# If adapting to laboratory studies, the environmental variables can additionally be "normal fluorescent light" and "dessication"

all_studies <- function(env_var){
  
  # Studies of all qualities and significance levels
  epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological" | type_of_study_cleaned == "ecological") & environmental_variables_cleaned == env_var) %>%
                   select(first_author, year_of_article, region_type_cleaned_2, norovirus_types, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting)
  
  epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -region_type_cleaned_2, -norovirus_types, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting)
  
  epidemiological <- unique(epidemiological)
  
  # remove categorical data
  epidemiological <- filter(epidemiological, boundary != "na" & boundary != "warm" & boundary != "cold" & direction_of_relationship != "na")
  
  epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary)) 
  
  # specify what variables and groups the y-axis values will be composed of
  # the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, such as direction_of_relationship
  epidemiological <- mutate(epidemiological, plot_value = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, direction_of_relationship, norovirus_variables_cleaned, setting, sep = ", "))
  epidemiological <- mutate(epidemiological, plot_value_display = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, sep = ", "))
  
  # specify order
  # epidemiological <- epidemiological %>% group_by(direction_of_relationship, region_type_cleaned_2, first_author, year_of_article, `CASP TOTAL`) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary, `CASP TOTAL`) %>% ungroup()
  epidemiological <- epidemiological %>% arrange(boundary) 
  
  order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n()) 
  epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
  remove(order)
  
  epidemiological <- mutate(epidemiological, `CASP TOTAL` = case_when(`CASP TOTAL` == "GOOD" ~ "Good and Adequate", `CASP TOTAL` == "ADEQUATE" ~ "Good and Adequate", `CASP TOTAL` == "POOR" ~ "Poor"))
  
  plot <- ggplot(data = epidemiological, aes(reorder(plot_value, -order), boundary, colour=direction_of_relationship, alpha = `CASP TOTAL`)) + geom_line(linewidth=4) + 
        geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, col="black")  +
        geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 3, col="black")  +
        # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
        # facet_wrap(~direction_of_relationship, scales = "free_y", strip.position = "left") + 
        # facet_wrap(~norovirus_types, scales = "free_y", strip.position = "left") + 
        coord_flip() +
        scale_alpha_discrete(range = c(1, 0.5)) +
        labs(x = "Group", y = "Variable Boundaries", colour = "directionality", alpha = "CASP") +
        scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
        ggtitle(env_var) + 
        guides(alpha = guide_legend(override.aes = aes(label = ""))) # this line removes the "a" from the legend for the transparency
  
  plot
  
  return(plot)
  }

# Summary figure for studies of only good quality and "significant" or "not tested" significance levels for a selected environmental variable
# Environmental variables (for ecological and epidemiological) can be "pH", "temperature", "radiance", "UV wavelength", "UV dose", "precipitation", "absolute humidity", "wind speed (and offshore versus onshore)", "atmospheric pressure", "soil moisture", "daytime hours", "relative humidity"
# If adapting to laboratory studies, the environmental variables can additionally be "normal fluorescent light" and "dessication"
good_studies <- function(env_var){
  
  epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological" | type_of_study_cleaned == "ecological") & environmental_variables_cleaned == env_var & `CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>%
                     select(first_author, year_of_article, region_type_cleaned_2, norovirus_types, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting)
  
  epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -region_type_cleaned_2, -norovirus_types, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting)
  
  epidemiological <- unique(epidemiological)
  
  # remove categorical data
  epidemiological <- filter(epidemiological, boundary != "na" & boundary != "warm" & boundary != "cold" & direction_of_relationship != "na")
  
  epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary)) 
  
  # specify what variables and groups the y-axis values will be composed of
  # the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, specifically direction_of_relationship
  epidemiological <- mutate(epidemiological, plot_value = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, direction_of_relationship, norovirus_variables_cleaned, setting, sep = ", "))
  epidemiological <- mutate(epidemiological, plot_value_display = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, sep = ", "))
  
  # specify order
  # epidemiological <- epidemiological %>% group_by(direction_of_relationship, region_type_cleaned_2, first_author, year_of_article) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary) %>% ungroup()
  epidemiological <- epidemiological %>% arrange(boundary) 
  
  order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n()) 
  epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
  remove(order)
  
  plot <- ggplot(epidemiological, aes(reorder(plot_value, -order), boundary, colour = direction_of_relationship)) + geom_line(linewidth=4) +
          geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, col="black")  +
          geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 3, col="black")  +
          # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
          coord_flip() +
          labs(x = "Group", y = "Variable Boundaries", colour = "directionality") +
          scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
          ggtitle(env_var)
  
  plot
}

########################################################
# Create figures for environmental variables of interest
# Environmental variables (for ecological and epidemiological) can be "pH", "temperature", "radiance", "UV wavelength", "UV dose", "precipitation", "absolute humidity", "wind speed (and offshore versus onshore)", "atmospheric pressure", "soil moisture", "daytime hours", "relative humidity"
# If adapting to laboratory studies, the environmental variables can additionally be "normal fluorescent light" and "dessication"

# Temperature
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "temperature", ".png"), plot = all_studies("temperature"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_good_studies_", "temperature", ".png"), plot = good_studies("temperature"), height = 10, width = 20)

ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "pH", ".png"), plot = all_studies("pH"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "radiance", ".png"), plot = all_studies("radiance"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "UV wavelength", ".png"), plot = all_studies("UV wavelength"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "UV dose", ".png"), plot = all_studies("UV dose"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "precipitation", ".png"), plot = all_studies("precipitation"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "absolute humidity", ".png"), plot = all_studies("absolute humidity"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "wind speed (and offshore versus onshore)", ".png"), plot = all_studies("wind speed (and offshore versus onshore)"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "atmospheric pressure", ".png"), plot = all_studies("atmospheric pressure"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "soil moisture", ".png"), plot = all_studies("soil moisture"), height = 10, width = 20)
ggsave(paste0("figures_data/summary_figures/summary_figure_all_studies_", "daytime hours", ".png"), plot = all_studies("daytime hours"), height = 10, width = 20)



#####################################
# Temperature new: instead of removing categorical variables, it is replacing them

epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological" | type_of_study_cleaned == "ecological") & environmental_variables_cleaned == "temperature") %>%
  select(first_author, year_of_article, region_type_cleaned_2, norovirus_types, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, environmental_unit_type, data_analysis_methods)

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -region_type_cleaned_2, -norovirus_types, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting, -environmental_unit_type, -data_analysis_methods)

epidemiological <- unique(epidemiological)

# assign grouping based on whether the environmental variable boundaries are either categorical or quantitative
epidemiological <- mutate(epidemiological, environmental_unit_type = case_when(boundary == "na" | environmental_unit_type == "na" ~ "categorical",
                                                                               environmental_unit_type != "categorical" ~ "quantitative",
                                                                               TRUE ~ environmental_unit_type))

# replace categorical data with minimum and maximum
min <- as.character(min(as.numeric(epidemiological$boundary), na.rm=TRUE))
max <- as.character(max(as.numeric(epidemiological$boundary), na.rm=TRUE))
  
epidemiological <- mutate(epidemiological, boundary = case_when((boundary_type == "environmental_boundary_lower" & boundary == "na") |  boundary == "cold"  ~ min, 
                                                                (boundary_type == "environmental_boundary_higher" & boundary == "na") |  boundary == "warm" ~ max, 
                                                                TRUE ~ boundary))

remove(min, max)

epidemiological <- filter(epidemiological, direction_of_relationship != "na")

epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary)) 

# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, specifically direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, direction_of_relationship, norovirus_variables_cleaned, setting, data_analysis_methods, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, sep = ", "))

# specify order
epidemiological <- epidemiological %>% group_by(environmental_unit_type, direction_of_relationship, region_type_cleaned_2, first_author, year_of_article) %>% arrange(desc(environmental_unit_type), direction_of_relationship, region_type_cleaned_2, boundary, `CASP TOTAL`) %>% ungroup()

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n()) 
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, `CASP TOTAL` = case_when(`CASP TOTAL` == "GOOD" ~ "Good and Adequate", `CASP TOTAL` == "ADEQUATE" ~ "Good and Adequate", `CASP TOTAL` == "POOR" ~ "Poor"))

plot <- ggplot(epidemiological, aes(reorder(plot_value, -order), boundary, colour = direction_of_relationship, linetype = environmental_unit_type, alpha = `CASP TOTAL`)) + geom_line(linewidth = 2) +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, col="black", size=2)  +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 1, col="black", size=2)  +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black", size=2)  +
  coord_flip() +
  scale_linetype_manual(values=c("dotted", "solid"))+
  labs(x = "Group", y = "Variable Boundaries", colour = "Directionality", alpha = "CASP", linetype = "Environmental variable type") +
  scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  ggtitle("Temperature") +
  facet_wrap(~factor(environmental_unit_type, c("quantitative", "categorical")), scales = "free_y", strip.position = "left", ncol = 1) + 
  theme(axis.text.y=element_text(size=6), legend.key.size = unit(1, 'cm')) +
  scale_alpha_discrete(range = c(1, 0.3)) +
  guides(alpha = guide_legend(override.aes = aes(label = ""))) # this line removes the "a" from the legend for the transparency

plot

ggsave(paste0("figures_data/summary_figures/summary_figure_categorical_test_", "temperature", ".png"), height = 10, width = 20)




##############################################
# Testing figure for environmental variables other than temperature, converting everything to categorical

epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological" | type_of_study_cleaned == "ecological") & environmental_variables_cleaned == "precipitation") %>%
  select(first_author, year_of_article, region_type_cleaned_2, norovirus_types, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, environmental_unit_type, data_analysis_methods)

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -region_type_cleaned_2, -norovirus_types, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting, -environmental_unit_type, -data_analysis_methods)

epidemiological <- unique(epidemiological)

# assign grouping based on whether the environmental variable boundaries are either categorical or quantitative
# This bit is not really necessary for other than temperature, but leaving it in in case can put everything in a function
epidemiological <- mutate(epidemiological, environmental_unit_type = case_when(boundary == "na" | environmental_unit_type == "na" ~ "categorical",
                                                                               environmental_unit_type != "categorical" ~ "quantitative",
                                                                               TRUE ~ environmental_unit_type))

epidemiological <- mutate(epidemiological, boundary = case_when((boundary_type == "environmental_boundary_lower" & boundary != "dry")  ~ "dry", 
                                                                (boundary_type == "environmental_boundary_higher" & boundary != "wet") ~ "wet", 
                                                                TRUE ~ boundary))

epidemiological <- mutate(epidemiological, boundary_2 = case_when((boundary == "dry")  ~ 0, 
                                                                  (boundary == "wet") ~ 10))

epidemiological <- filter(epidemiological, direction_of_relationship != "na")

# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, specifically direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, direction_of_relationship, norovirus_variables_cleaned, setting, data_analysis_methods, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, setting, sep = ", "))

# specify order
epidemiological <- epidemiological %>% group_by(direction_of_relationship, region_type_cleaned_2, first_author, year_of_article) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary, `CASP TOTAL`) %>% ungroup()

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n()) 
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, `CASP TOTAL` = case_when(`CASP TOTAL` == "GOOD" ~ "Good and Adequate", `CASP TOTAL` == "ADEQUATE" ~ "Good and Adequate", `CASP TOTAL` == "POOR" ~ "Poor"))

options(repr.plot.width = 3, repr.plot.height =2)

plot <- ggplot(epidemiological, aes(reorder(plot_value, -order), boundary_2, colour = direction_of_relationship, alpha = `CASP TOTAL`)) + geom_line(linewidth = 1) + 
  geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 0.2, col="black", size=2)  +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 0.2, col="black", size=2)  +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 0.2, col="black", size=2)  +
  coord_flip() +
  # scale_linetype_manual(values=c("dotted", "solid")) +
  labs(x = "Group", y = "Variable Boundaries", colour = "Directionality", alpha = "CASP", linetype = "Environmental variable type") +
  scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  scale_y_continuous(breaks = epidemiological$boundary_2, labels = epidemiological$boundary) +
  ggtitle("Precipitation") +
  # facet_wrap(~factor(environmental_unit_type, c("quantitative", "categorical")), scales = "free_y", strip.position = "left", ncol = 1) + 
  theme(axis.text.y=element_text(size=6), legend.key.size = unit(1, 'cm')) +
  scale_alpha_discrete(range = c(1, 0.3)) +
  guides(alpha = guide_legend(override.aes = aes(label = ""))) # this line removes the "a" from the legend for the transparency

plot

ggsave(paste0("figures_data/summary_figures/summary_figure_categorical_test_", "precipitation", ".png"), height = 10, width = 20)



###################
# Testing precipitation standardisation

# Studies of all qualities and significance levels
epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological" | type_of_study_cleaned == "ecological") & environmental_variables_cleaned == "precipitation" & `CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>%
  select(first_author, year_of_article, region_type_cleaned_2, norovirus_types, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, precipitation_standardisation_denominator)

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -region_type_cleaned_2, -norovirus_types, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting, -precipitation_standardisation_denominator)

epidemiological <- unique(epidemiological)

# remove categorical data
epidemiological <- filter(epidemiological, precipitation_standardisation_denominator != "na" & first_author != "Zhaoqi Wang" & boundary != "na" & boundary != "dry" & boundary != "rainy" & boundary != "rainy (CSO)" & boundary != "rainy (2-36 mm)" & boundary != "flood" & direction_of_relationship != "na")

epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary)) 


epidemiological <- mutate(epidemiological, precipitation_standardisation_denominator = as.numeric(precipitation_standardisation_denominator))
# epidemiological <- mutate(epidemiological, boundary = boundary/precipitation_standardisation_denominator)


# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, such as direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, direction_of_relationship, norovirus_variables_cleaned, setting, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, sep = ", "))

# specify order
# epidemiological <- epidemiological %>% group_by(direction_of_relationship, region_type_cleaned_2, first_author, year_of_article, `CASP TOTAL`) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary, `CASP TOTAL`) %>% ungroup()
# epidemiological <- epidemiological %>% arrange(boundary) 
epidemiological <- epidemiological %>% arrange(boundary, region_type_cleaned_2)

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n()) 
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, `CASP TOTAL` = case_when(`CASP TOTAL` == "GOOD" ~ "Good and Adequate", `CASP TOTAL` == "ADEQUATE" ~ "Good and Adequate", `CASP TOTAL` == "POOR" ~ "Poor"))

plot <- ggplot(data = epidemiological, aes(reorder(plot_value, -order), boundary, colour=direction_of_relationship, alpha = `CASP TOTAL`)) + geom_line(linewidth=4) + 
  geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 15, col="black")  +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 21, col="black")  +
  # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
  # facet_wrap(~direction_of_relationship, scales = "free_y", strip.position = "left") + 
  # facet_wrap(~norovirus_types, scales = "free_y", strip.position = "left") + 
  coord_flip() +
  scale_alpha_discrete(range = c(1, 0.3)) +
  labs(x = "Group", y = "Variable Boundaries (mm)", colour = "directionality", alpha = "CASP") +
  scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  ggtitle("Non-Standardised Precipitation") + 
  guides(alpha = guide_legend(override.aes = aes(label = ""))) # this line removes the "a" from the legend for the transparency

plot

ggsave(paste0("figures_data/summary_figures/summary_figure_non-standardised_", "precipitation", ".png"), height = 10, width = 20)







########################################
# Testing for absolute and relative humidity and air pressure

# Studies of all qualities and significance levels
epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological" | type_of_study_cleaned == "ecological") & (environmental_variables_cleaned == "relative humidity" | environmental_variables_cleaned == "absolute humidity" | environmental_variables_cleaned == "atmospheric pressure") & `CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>%
  select(first_author, year_of_article, region_type_cleaned_2, norovirus_types, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, environmental_variables_cleaned, data_analysis_methods)

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -region_type_cleaned_2, -norovirus_types, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting, -environmental_variables_cleaned, -data_analysis_methods)

epidemiological <- unique(epidemiological)

# standardising atmospheric pressure for the second article
epidemiological <- mutate(epidemiological, boundary = case_when(boundary == "972" ~ "-12.5", 
                                                                boundary == "997" ~ "12.5", 
                                                                TRUE ~ boundary))

# remove categorical data
epidemiological <- filter(epidemiological, boundary != "na" & boundary != "warm" & boundary != "cold" & direction_of_relationship != "na")

epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary)) 


# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, such as direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, direction_of_relationship, norovirus_variables_cleaned, setting, data_analysis_methods, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(region_type_cleaned_2, first_author, year_of_article, norovirus_types, environmental_variables, sep = ", "))

# specify order
# epidemiological <- epidemiological %>% group_by(direction_of_relationship, region_type_cleaned_2, first_author, year_of_article, `CASP TOTAL`) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary, `CASP TOTAL`) %>% ungroup()
epidemiological <- epidemiological %>% arrange(boundary) 

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n()) 
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, `CASP TOTAL` = case_when(`CASP TOTAL` == "GOOD" ~ "Good and Adequate", `CASP TOTAL` == "ADEQUATE" ~ "Good and Adequate", `CASP TOTAL` == "POOR" ~ "Poor"))



plot <- ggplot(data = epidemiological, aes(reorder(plot_value, -order), boundary, colour=direction_of_relationship, alpha = `CASP TOTAL`)) + geom_line(linewidth=4) + 
  geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, col="black")  +
  geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 3, col="black")  +
  # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
  # facet_wrap(~direction_of_relationship, scales = "free_y", strip.position = "left") + 
  # facet_wrap(~environmental_variables_cleaned, scales = "free", strip.position = "left", ncol = 1) + 
  coord_flip() +
  scale_alpha_discrete(range = c(1, 0.5)) +
  labs(x = "Group", y = "Variable Boundaries", colour = "directionality", alpha = "CASP") +
  scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  ggtitle("Standardised Atmospheric Pressure, Absolute Humidity and Relative Humidity") + 
  force_panelsizes(rows = c(0.3, 0.3, 1)) +
  guides(alpha = guide_legend(override.aes = aes(label = ""))) # this line removes the "a" from the legend for the transparency

plot

ggsave(paste0("figures_data/summary_figures/summary_figure_", "humidity_and_pressure", ".png"), height = 10, width = 20)






