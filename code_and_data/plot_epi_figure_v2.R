# the purpose of the script is to plot (1) the study characteristics, (2) numerical lower and upper boundaries of the exposure climate variable when available, (3) the size of the effect of the association
# the script was built for epidemiological studies and not for the otuside-host ecological and laboratory associations described in the review

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

##########################
# Plotting for temperature

epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological") & ((environmental_boundary_lower != "na" & environmental_boundary_lower != "cold") | !is.na(standardised_relative_percent_change_of_odds_per_unit)) & (environmental_variables_cleaned == "ambient temperature" | environmental_variables_cleaned == "ambient temperature anomaly" | environmental_variables_cleaned == "water temperature") & `CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>%
                   select(first_author, year_of_article, country_and_region, norovirus_types_display, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, age_groups, standardised_relative_percent_change_of_odds_per_unit, standardised_relative_percent_change_per_unit_lower, standardised_relative_percent_change_per_unit_higher, standardised_linear_or_binary, coefficient_type, data_analysis_methods, coefficient)

epidemiological <- filter(epidemiological, first_author != "Nicolas Yin")

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -country_and_region, -norovirus_types_display, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -setting, -age_groups, -standardised_relative_percent_change_of_odds_per_unit, -standardised_relative_percent_change_per_unit_lower, -standardised_relative_percent_change_per_unit_higher, -standardised_linear_or_binary, -coefficient_type, -data_analysis_methods, -coefficient)

epidemiological <- unique(epidemiological)

# remove categorical data
# epidemiological <- filter(epidemiological, !is.na(standardised_relative_percent_change_of_odds_per_unit))
# epidemiological <- filter(epidemiological, boundary != "na" & boundary != "warm" & boundary != "cold" & direction_of_relationship != "na")

epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary))
epidemiological <- mutate(epidemiological, coefficient = as.numeric(coefficient))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_of_odds_per_unit = as.numeric(standardised_relative_percent_change_of_odds_per_unit))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_per_unit_lower = as.numeric(standardised_relative_percent_change_per_unit_lower))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_per_unit_higher = as.numeric(standardised_relative_percent_change_per_unit_higher))

# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, specifically direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(country_and_region, first_author, year_of_article, norovirus_types_display, environmental_variables, norovirus_variables_cleaned, setting, age_groups, data_analysis_methods, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(country_and_region, year_of_article, norovirus_types_display, environmental_variables, setting, age_groups, sep = ", "))

# specify order
# epidemiological <- epidemiological %>% group_by(direction_of_relationship, country_and_region, first_author, year_of_article) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary) %>% ungroup()
epidemiological <- epidemiological %>% arrange(boundary, country_and_region, norovirus_types_display, environmental_variables)

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n())
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, reference = paste0(word(first_author , -1  , -1), " et al. ", year_of_article))

plot1 <- ggplot(epidemiological, aes(reorder(plot_value, -order), boundary, colour = direction_of_relationship)) + geom_line(linewidth=4) +
        theme_minimal() +
        # geom_text(data = epidemiological[epidemiological$coefficient_type == "pairwise correlation" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = round(coefficient, digits = 2)), nudge_y = 5, nudge_x = 0.15, col="black")  +
        # geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, nudge_x = 0.15, col="black")  +
        # geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 3, col="black")  +
        # # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
        coord_flip() +
        labs(x = "", y = expression(paste("Variable boundaries in "^"o","C"), sep = ""), colour = "Directionality") +
        scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
        # ggtitle("Temperature") +
        # annotate("text", x = 29, y = -25, label = "Temperature") +
        scale_color_discrete(breaks=c("positive", "negative")) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        theme(legend.position = c(0.75, 0.1)) +
        theme(text = element_text(size = 14))

plot1

plot2 <- ggplot(data=epidemiological, aes(reorder(plot_value, -order), y = standardised_relative_percent_change_of_odds_per_unit, ymin = standardised_relative_percent_change_per_unit_lower, ymax = standardised_relative_percent_change_per_unit_higher, shape = coefficient_type)) +
        theme_minimal() +
        geom_pointrange() +
        # geom_text(data = epidemiological[epidemiological$coefficient_type == "pairwise correlation" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = ""), nudge_y = 1, nudge_x = 0.15, col="black")  +
        geom_text(aes(label = paste0(standardised_linear_or_binary)), nudge_y = 0.2, nudge_x =  0.6, col="black", size = 5.5)  +
        geom_hline(yintercept = 0, lty = 2) +  # add a dotted line at x=1 after flip
        coord_flip() +  # flip coordinates (puts labels on y axis)
        labs(shape = "Type of Measure") +
        xlab("") + ylab(expression(paste("Metric change (%) per an increase in 1 "^"o","C", sep = ""))) +
        # scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
        # ggtitle("Relative Change of Norovirus Burden") +
        # annotate("text", x = 29, y = -15, label = "Relative Change of Norovirus Burden") +
        scale_shape_discrete(breaks=c("odds ratio", "rate ratio", "regression slope", "relative risk")) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        theme(legend.position = c(0.80, 0.90)) +
        theme(text = element_text(size = 14))

plot2

epidemiological <- bind_rows(epidemiological, data.frame(reference = "Reference",
                             country_and_region = "Country", age_groups = "Age Group", norovirus_types_display = "Norovirus Type", environmental_variables = "Variable")) %>% 
                   mutate(model = fct_rev(fct_relevel(reference, "Reference")))

epidemiological$country_and_region[epidemiological$country_and_region == "19 Low- and Middle-Income Countries (Global South)"] <- "19 LMIC"
epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "NoV-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"
epidemiological$environmental_variables[epidemiological$environmental_variables == "7-day (3 to 9 days prior sampling) average average daily temperatures"] <- "average weekly temperature"

# https://www.khstats.com/blog/forest-plots/
plot0 <- ggplot(epidemiological, aes(y = reorder(plot_value, -order))) +
  geom_text(aes(x = -0.2, label = reference), hjust = 0, fontface = ifelse(epidemiological$reference == "Reference", "bold", "plain")) +
  geom_text(aes(x = 0.7, label = country_and_region), hjust = 0, fontface = ifelse(epidemiological$country_and_region == "Country", "bold", "plain")) +
  geom_text(aes(x = 1.3, label = age_groups), hjust = 0, fontface = ifelse(epidemiological$age_groups == "Age Group", "bold", "plain")) +
  geom_text(aes(x = 2.0, label = norovirus_types_display), hjust = 0, fontface = ifelse(epidemiological$norovirus_types_display == "Norovirus Type", "bold", "plain")) +
  geom_text(aes(x = 4.5, label = environmental_variables), hjust = 0, fontface = ifelse(epidemiological$environmental_variables == "Variable", "bold", "plain")) +
  # coord_flip() +
  theme_void() +
  coord_cartesian(xlim = c(0, 6)) +
  # scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  ggtitle(" ") +
# scale_color_discrete(breaks=c("positive", "negative")) +
# theme(legend.position = c(0.75, 0.1)) +
  update_geom_defaults("text", list(size = 4))

plot0

# if misaligned vertically, attempt to modify the t argument to 0, 1, 2, or 3 except for the first plot
layout <- c(
  area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 2, l = 8, b = 30, r = 9), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 2, l = 10, b = 30, r = 11) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

# final plot arrangement
plot <- plot0 + plot1 + plot2 + plot_layout(design = layout) # + plot_annotation(title = "Figure 1. Variability of Norovirus Burden Relative to Ambient Temperature") & theme(plot.title = element_text(size = 25))

plot

# plot <- plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 2, nrow = 1, rel_widths=c(3,1))
# 
# plot

ggsave(paste0("figures_data/summary_figures/summary_figure_good_studies_RR_", "temperature", ".png"), plot = plot, height = 10, width = 20)

############################
# Plotting for precipitation 

# Studies of all qualities and significance levels
epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological") & (environmental_boundary_lower != "na" | !is.na(standardised_relative_percent_change_of_odds_per_unit)) & (environmental_variables_cleaned == "precipitation" | environmental_variables_cleaned == "precipitation anomaly") & `CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>%
  select(first_author, year_of_article, country_and_region, norovirus_types_display, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, precipitation_standardisation_denominator, age_groups, standardised_relative_percent_change_of_odds_per_unit, standardised_relative_percent_change_per_unit_lower, standardised_relative_percent_change_per_unit_higher, standardised_linear_or_binary, concomitant_conditions, coefficient_type, data_analysis_methods, coefficient, environmental_variables_cleaned)

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -country_and_region, -norovirus_types_display, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -precipitation_standardisation_denominator, -setting, -age_groups, -standardised_relative_percent_change_of_odds_per_unit, -standardised_relative_percent_change_per_unit_lower, -standardised_relative_percent_change_per_unit_higher, -standardised_linear_or_binary, -concomitant_conditions, -coefficient_type, -data_analysis_methods, -coefficient, -environmental_variables_cleaned)

epidemiological <- unique(epidemiological)

# remove categorical data
# epidemiological <- filter(epidemiological, !is.na(standardised_relative_percent_change_of_odds_per_unit))
# epidemiological <- filter(epidemiological, precipitation_standardisation_denominator != "na" & first_author != "Zhaoqi Wang" & boundary != "na" & boundary != "dry" & boundary != "rainy" & boundary != "rainy (CSO)" & boundary != "rainy (2-36 mm)" & boundary != "flood" & direction_of_relationship != "na")

epidemiological <- filter(epidemiological, first_author != "Nicolas Yin")

epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary))
epidemiological <- mutate(epidemiological, coefficient = as.numeric(coefficient))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_of_odds_per_unit = as.numeric(standardised_relative_percent_change_of_odds_per_unit))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_per_unit_lower = as.numeric(standardised_relative_percent_change_per_unit_lower))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_per_unit_higher = as.numeric(standardised_relative_percent_change_per_unit_higher))

epidemiological <- mutate(epidemiological, precipitation_standardisation_denominator = as.numeric(precipitation_standardisation_denominator))

# If necessary to standardise the precipitation values by a time denominator to "amount of precipitation per day", uncomment below
# epidemiological <- mutate(epidemiological, boundary = boundary/precipitation_standardisation_denominator)

# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, specifically direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(country_and_region, first_author, year_of_article, norovirus_types_display, environmental_variables, norovirus_variables_cleaned, setting, age_groups, data_analysis_methods, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(country_and_region, year_of_article, norovirus_types_display, environmental_variables, setting, age_groups, sep = ", "))

# specify order
# epidemiological <- epidemiological %>% group_by(direction_of_relationship, country_and_region, first_author, year_of_article) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary) %>% ungroup()
epidemiological <- epidemiological %>% arrange(boundary, country_and_region)

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n())
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, reference = paste0(word(first_author , -1  , -1), " et al. ", year_of_article))
plot1 <- ggplot(epidemiological, aes(reorder(plot_value, -order), boundary, colour = direction_of_relationship)) + geom_line(linewidth=4) +
  theme_minimal() +
  # geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, nudge_x = 0.15, col="black")  +
  # geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 3, col="black")  +
  # # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
  coord_flip() +
  labs(x = "", y = "Precipitation boundaries in mm", colour = "Directionality") +
  scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  # ggtitle("Non-standardised Precipitation") +
  scale_color_discrete(breaks=c("positive", "negative")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.position = c(0.75, 0.1)) +
  theme(text = element_text(size = 13))

plot1

plot2 <- ggplot(data=epidemiological, aes(reorder(plot_value, -order), y=standardised_relative_percent_change_of_odds_per_unit, ymin=standardised_relative_percent_change_per_unit_lower, ymax=standardised_relative_percent_change_per_unit_higher, shape = coefficient_type)) +
  theme_minimal() +
  geom_pointrange() +
  # geom_text(aes(label = standardised_linear_or_binary), nudge_y = 0.5, nudge_x =  0.3, col="black")  +
  geom_text(aes(label = paste0(standardised_linear_or_binary)), nudge_y = 0.05, nudge_x =  0.3, col="black")  +
  geom_hline(yintercept = 0, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(shape = "Type of Measure") +
  xlab("") + ylab("Metric change (%) per an increase in 1 unit of precipitation") +
  # scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  # ggtitle("Relative Change of Norovirus Burden") +
  scale_shape_discrete(breaks=c("odds ratio", "rate ratio", "regression slope", "relative risk", "prevalence ratio")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.position = c(0.20, 0.15)) +
  theme(text = element_text(size = 13))

plot2

epidemiological <- bind_rows(epidemiological, data.frame(reference = "Reference",
                                                         country_and_region = "Country", age_groups = "Age Group", norovirus_types_display = "Norovirus Type", environmental_variables = "Variable")) %>% 
  mutate(model = fct_rev(fct_relevel(reference, "Reference")))

epidemiological$country_and_region[epidemiological$country_and_region == "19 Low- and Middle-Income Countries (Global South)"] <- "19 LMIC"
# epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "NoV-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"
epidemiological$environmental_variables[epidemiological$environmental_variables == "pressure deviation: 7-day (3 to 9 days prior sampling) average surface pressure"] <- "average weekly pressure deviation"
epidemiological$environmental_variables[epidemiological$environmental_variables == "7-day (3 to 9 days prior sampling) average for relative humidity"] <- "average weekly relative humidity"
epidemiological$environmental_variables[epidemiological$environmental_variables == "precipitation deviation: 7-day (3 to 9 days prior sampling) sum for daily total precipitation"] <- "total weekly precipitation deviation"
epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "Norovirus-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"
epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "NoV-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"


# https://www.khstats.com/blog/forest-plots/
plot0 <- ggplot(epidemiological, aes(y = reorder(plot_value, -order))) +
  geom_text(aes(x = -0.2, label = reference), hjust = 0, fontface = ifelse(epidemiological$reference == "Reference", "bold", "plain")) +
  geom_text(aes(x = 0.7, label = country_and_region), hjust = 0, fontface = ifelse(epidemiological$country_and_region == "Country", "bold", "plain")) +
  geom_text(aes(x = 1.3, label = age_groups), hjust = 0, fontface = ifelse(epidemiological$age_groups == "Age Group", "bold", "plain")) +
  geom_text(aes(x = 2.1, label = norovirus_types_display), hjust = 0, fontface = ifelse(epidemiological$norovirus_types_display == "Norovirus Type", "bold", "plain")) +
  geom_text(aes(x = 4.8, label = environmental_variables), hjust = 0, fontface = ifelse(epidemiological$environmental_variables == "Variable", "bold", "plain")) +
  # coord_flip() +
  theme_void() +
  coord_cartesian(xlim = c(0, 6)) +
  # scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  ggtitle(" ")
# scale_color_discrete(breaks=c("positive", "negative")) +
# theme(legend.position = c(0.75, 0.1)) +
# theme(text = element_text(size = 13))

plot0

# if misaligned vertically, attempt to modify the t argument to 0, 1, 2, or 3 except for the first plot
layout <- c(
  area(t = -1, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 3, l = 8, b = 30, r = 9), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 3, l = 10, b = 30, r = 12) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

# final plot arrangement
plot <- plot0 + plot1 + plot2 + plot_layout(design = layout) # + plot_annotation(title = "Figure 3. Variability of Norovirus Burden Relative to Precipitation") & theme(plot.title = element_text(size = 18))

plot

# plot <- plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 2, nrow = 1, rel_widths=c(3,1))
# 
# plot

ggsave(paste0("figures_data/summary_figures/summary_figure_good_studies_RR_", "precipitation", ".png"), plot = plot, height = 10, width = 20)

#############################################################
# Plotting for absolute and relative humidity and air pressure

# Studies of all qualities and significance levels
epidemiological <- filter(df, (type_of_study_cleaned == "epidemiological") & (environmental_boundary_lower != "na" | !is.na(standardised_relative_percent_change_of_odds_per_unit)) & (environmental_variables_cleaned == "relative humidity" | environmental_variables_cleaned == "absolute humidity" | environmental_variables_cleaned == "atmospheric pressure" | environmental_variables_cleaned == "vapor pressure" | environmental_variables_cleaned == "atmospheric pressure deviation") & `CASP TOTAL` != "POOR" & statistical_significance != "insignificant") %>%
                   select(first_author, year_of_article, country_and_region, norovirus_types_display, environmental_variables, environmental_boundary_lower, environmental_boundary_higher, direction_of_relationship, `CASP TOTAL`, statistical_significance, norovirus_variables_cleaned, setting, precipitation_standardisation_denominator, age_groups, standardised_relative_percent_change_of_odds_per_unit, standardised_relative_percent_change_per_unit_lower, standardised_relative_percent_change_per_unit_higher, standardised_linear_or_binary, concomitant_conditions, coefficient_type, data_analysis_methods, coefficient, environmental_variables_cleaned)

epidemiological <- epidemiological %>% gather(key = boundary_type, value = boundary, -first_author, -country_and_region, -norovirus_types_display, -direction_of_relationship, -`CASP TOTAL`, -statistical_significance, -environmental_variables, -year_of_article, -norovirus_variables_cleaned, -precipitation_standardisation_denominator, -setting, -age_groups, -standardised_relative_percent_change_of_odds_per_unit, -standardised_relative_percent_change_per_unit_lower, -standardised_relative_percent_change_per_unit_higher, -standardised_linear_or_binary, -concomitant_conditions, -coefficient_type, -data_analysis_methods, -coefficient, -environmental_variables_cleaned)

epidemiological <- unique(epidemiological)

# standardising atmospheric pressure for the second article
epidemiological <- mutate(epidemiological, boundary = case_when(boundary == "972" ~ "-12.5",
                                                                boundary == "997" ~ "12.5",
                                                                TRUE ~ boundary))
# remove categorical data
# epidemiological <- filter(epidemiological, !is.na(standardised_relative_percent_change_of_odds_per_unit))
# epidemiological <- filter(epidemiological, precipitation_standardisation_denominator != "na" & first_author != "Zhaoqi Wang" & boundary != "na" & boundary != "dry" & boundary != "rainy" & boundary != "rainy (CSO)" & boundary != "rainy (2-36 mm)" & boundary != "flood" & direction_of_relationship != "na")
# epidemiological <- filter(epidemiological, boundary != "na" & boundary != "warm" & boundary != "cold" & direction_of_relationship != "na")

epidemiological <- filter(epidemiological, first_author != "Nicolas Yin")

epidemiological <- mutate(epidemiological, boundary = as.numeric(boundary))
epidemiological <- mutate(epidemiological, coefficient = as.numeric(coefficient))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_of_odds_per_unit = as.numeric(standardised_relative_percent_change_of_odds_per_unit))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_per_unit_lower = as.numeric(standardised_relative_percent_change_per_unit_lower))
epidemiological <- mutate(epidemiological, standardised_relative_percent_change_per_unit_higher = as.numeric(standardised_relative_percent_change_per_unit_higher))

epidemiological <- mutate(epidemiological, precipitation_standardisation_denominator = as.numeric(precipitation_standardisation_denominator))
# epidemiological <- mutate(epidemiological, boundary = boundary/precipitation_standardisation_denominator)


# specify what variables and groups the y-axis values will be composed of
# the second plot_value_display is necessary in order to remove parts of the name that are not to be displayed, specifically direction_of_relationship
epidemiological <- mutate(epidemiological, plot_value = paste(country_and_region, first_author, year_of_article, norovirus_types_display, environmental_variables, norovirus_variables_cleaned, setting, age_groups, concomitant_conditions, data_analysis_methods, sep = ", "))
epidemiological <- mutate(epidemiological, plot_value_display = paste(country_and_region, year_of_article, norovirus_types_display, environmental_variables, setting, age_groups, sep = ", "))

# specify order
# epidemiological <- epidemiological %>% group_by(direction_of_relationship, country_and_region, first_author, year_of_article) %>% arrange(direction_of_relationship, region_type_cleaned_2, boundary) %>% ungroup()
epidemiological <- epidemiological %>% arrange(boundary, environmental_variables_cleaned, country_and_region)

order <- as.tibble(unique(epidemiological$plot_value)) %>% rename(plot_value = value) %>% mutate(order = 1:n())
epidemiological <- left_join(epidemiological, order, by = "plot_value") # %>% mutate(order = factor(order, levels = unique(order)))
remove(order)

epidemiological <- mutate(epidemiological, reference = paste0(word(first_author , -1  , -1), " et al. ", year_of_article))

plot1 <- ggplot(epidemiological, aes(reorder(plot_value, -order), boundary, colour = direction_of_relationship)) + geom_line(linewidth=4) +
  theme_minimal() +
  # geom_text(data = epidemiological[epidemiological$statistical_significance=="significant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "*"), nudge_y = 1, nudge_x = 0.15, col="black")  +
  # geom_text(data = epidemiological[epidemiological$statistical_significance=="insignificant" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "ns"), nudge_y = 3, col="black")  +
  # # geom_text(data = epidemiological[epidemiological$statistical_significance=="not tested" & epidemiological$boundary_type=="environmental_boundary_higher",], aes(label = "na"), nudge_y = 1, col="black")  +
  coord_flip() +
  labs(x = "", y = "Variable boundaries", colour = "Directionality") +
  scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  # ggtitle("Standardised Atmospheric Pressure, \nAbsolute Humidity and Relative Humidity") +
  scale_color_discrete(breaks=c("positive", "negative")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.position = c(0.75, 0.1)) +
  theme(text = element_text(size = 13))

plot1

plot2 <- ggplot(data=epidemiological, aes(reorder(plot_value, -order), y=standardised_relative_percent_change_of_odds_per_unit, ymin=standardised_relative_percent_change_per_unit_lower, ymax=standardised_relative_percent_change_per_unit_higher, shape = coefficient_type)) +
  theme_minimal() +
  geom_pointrange() +
  # geom_text(aes(label = standardised_linear_or_binary), nudge_y = 0.5, nudge_x =  0.3, col="black")  +
  geom_text(aes(label = paste0(standardised_linear_or_binary)), nudge_y = 0.05, nudge_x =  0.45, col="black")  +
  geom_hline(yintercept = 0, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(shape = "Type of Measure") +
  xlab("") + ylab("Metric change (%) per an increase in 1 unit") +
  # scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  # ggtitle("Relative Change of Norovirus Burden\n") +
  scale_shape_discrete(breaks=c("odds ratio", "rate ratio", "regression slope", "relative risk")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.position = c(0.20, 0.15)) +
  theme(text = element_text(size = 13))

plot2

epidemiological <- bind_rows(epidemiological, data.frame(reference = "Reference",
                                                         country_and_region = "Country", age_groups = "Age Group", norovirus_types_display = "Norovirus Type", environmental_variables = "Variable")) %>% 
  mutate(model = fct_rev(fct_relevel(reference, "Reference")))

epidemiological$country_and_region[epidemiological$country_and_region == "19 Low- and Middle-Income Countries (Global South)"] <- "19 LMIC"
# epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "NoV-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"
epidemiological$environmental_variables[epidemiological$environmental_variables == "pressure deviation: 7-day (3 to 9 days prior sampling) average surface pressure"] <- "average weekly pressure deviation"
epidemiological$environmental_variables[epidemiological$environmental_variables == "7-day (3 to 9 days prior sampling) average for relative humidity"] <- "average weekly relative humidity"
epidemiological$environmental_variables[epidemiological$environmental_variables == "precipitation deviation: 7-day (3 to 9 days prior sampling) sum for daily total precipitation"] <- "total weekly precipitation deviation"
epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "Norovirus-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"
epidemiological$norovirus_types_display[epidemiological$norovirus_types_display == "NoV-associated gastroenteritis outbreaks"] <- "NoV-associated outbreaks"

# https://www.khstats.com/blog/forest-plots/
plot0 <- ggplot(epidemiological, aes(y = reorder(plot_value, -order))) +
  geom_text(aes(x = -0.3, label = reference), hjust = 0, fontface = ifelse(epidemiological$reference == "Reference", "bold", "plain")) +
  geom_text(aes(x = 0.7, label = country_and_region), hjust = 0, fontface = ifelse(epidemiological$country_and_region == "Country", "bold", "plain")) +
  geom_text(aes(x = 1.3, label = age_groups), hjust = 0, fontface = ifelse(epidemiological$age_groups == "Age Group", "bold", "plain")) +
  geom_text(aes(x = 2.0, label = norovirus_types_display), hjust = 0, fontface = ifelse(epidemiological$norovirus_types_display == "Norovirus Type", "bold", "plain")) +
  geom_text(aes(x = 4.8, label = environmental_variables), hjust = 0, fontface = ifelse(epidemiological$environmental_variables == "Variable", "bold", "plain")) +
  # coord_flip() +
  theme_void() +
  coord_cartesian(xlim = c(0, 6)) +
  # scale_x_discrete(labels = lapply(with(distinct(epidemiological, plot_value, .keep_all = TRUE), reorder(plot_value_display, -order)), as.character) %>% rev()) + # this line replaces the axis values with the desired ones (specifically, a shorter form). DOES NOT WORK WHEN FACETS ARE PRESENT
  ggtitle(" ") +
# scale_color_discrete(breaks=c("positive", "negative")) +
# theme(legend.position = c(0.75, 0.1)) +
# theme(text = element_text(size = 13))
  update_geom_defaults("text", list(size = 4))

plot0

# if misaligned vertically, attempt to modify the t argument to 0, 1, 2, or 3 except for the first plot
layout <- c(
  area(t = 0, l = 0, b = 30, r = 6), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
  area(t = 1, l = 7, b = 30, r = 8), # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  area(t = 1, l = 9, b = 30, r = 10) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
)

# final plot arrangement
plot <- plot0 + plot1 + plot2 + plot_layout(design = layout)  # + plot_annotation(title = "Figure 2. Variability of Norovirus Burden Relative to Precipitation, Standardised Atmospheric Pressure, Absolute Humidity and Relative Humidity") & theme(plot.title = element_text(size = 18))

plot

# plot <- plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 2, nrow = 1, rel_widths=c(3,1))
# 
# plot

ggsave(paste0("figures_data/summary_figures/summary_figure_good_studies_RR_", "humidity_and_pressure", ".png"), plot = plot, height = 10, width = 20)




