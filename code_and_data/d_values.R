library(dplyr)
library(ggplot2)
library(openxlsx)
library(plan)
library(tidyverse)
library(ggh4x)
library(cowplot)
library(stringr)
library(scales) 
library(mltools)
library(data.table)
library(mgcv)
library(gratia)
library(effects)
library(ggeffects)
library(patchwork)

# read data and select one relevant sheet by uncommeting the command line if plotting for mediums that are (1) fomites, (2) liquid, (3) food
# df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Lab_Fomites")
# df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Lab_Liquid")
# df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Lab_Food_Chain")

# Plotting and processing is for each climate variable separately

#############
#############
# Temperature
# this subsection runs for fomites, liquid and food mediums. At the end of the subsection, plot only with the commands applicable to the selected medium

data <- filter(df, environmental_variables_cleaned == "temperature" & !is.na(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`)) %>% select(covidence_id, setting, laboratory_methods, `data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, norovirus_types_cleaned_3)

data <- mutate(data, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)` = strsplit(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, ","))

data_new <- data.frame()

for (i in 1:nrow(data)){
    s <- 0
    for (j in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
      if (str_detect(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][j], regex("\\(")) == TRUE){
        # number of datapoints
        s <- s + 1
      }
    }

    # number of elements per datapoint. Should be either two or four
    s2 <- length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])/s

    for (m in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
      counter <- 0
      while (counter < s){
        counter_2 <- 1
        position <- counter * s2 + 1
        while (counter_2 <= s2){
          new_row <- data[i,]
          if (s2 == 2){
            new_row$temp <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
            new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
            new_row$lower_D <- NA
            new_row$higher_D <- NA
          }
          if (s2 == 4){
            new_row$temp <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
            new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
            new_row$lower_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 2]
            new_row$higher_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 3]
          }
          counter_2 <- counter_2 + 1
        }
        
        if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("week")) == TRUE){
          new_row$time_units <- "weeks"}
        if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("day")) == TRUE){
          new_row$time_units <- "days"}
        if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("hour")) == TRUE){
          new_row$time_units <- "hours"}
        if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("min")) == TRUE){
          new_row$time_units <- "minutes"}
        if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("sec")) == TRUE){
          new_row$time_units <- "seconds"}

        data_new <- base::rbind(data_new, new_row)
        counter <- counter + 1
      }
    }
  }

data <- data_new
remove(data_new, new_row, counter, counter_2, i, j, m, position, s, s2)

data <- data[!duplicated(data), ]

data <- mutate(data, temp = as.numeric(gsub('\\(', '', temp)))
data <- mutate(data, D_value = as.numeric(gsub('\\)', '', D_value)))
data <- mutate(data, lower_D = as.numeric(gsub('\\)', '', lower_D)))
data <- mutate(data, higher_D = as.numeric(gsub('\\)', '', higher_D)))

# data <- filter(data, laboratory_methods == "plaque assay" | laboratory_methods == "TCID50")
# data <- filter(data, laboratory_methods == "RT-qPCR" | laboratory_methods == "molecular beacon NASBA technique with enzymatic treatment")

data <- mutate(data, assay_type = case_when(
  laboratory_methods == "plaque assay" | laboratory_methods == "TCID50" | laboratory_methods == "human intestinal enteroids assay" | laboratory_methods == "human intestinal enteroid assay" | laboratory_methods == "human intestinal enteroid assay coupled to RT-qPCR" ~ "Infectivity Assay",
  laboratory_methods == "RT-qPCR" | laboratory_methods == "molecular beacon NASBA technique with enzymatic treatment" | laboratory_methods == "long-template RT-PCR" | laboratory_methods == "short genome RT-qPCR" | laboratory_methods == "ddRT-PCR" | laboratory_methods == "RT-ddPCR" | laboratory_methods == "long-genome RT-qPCR" | laboratory_methods == "enzyme treatment RT-qPCR" | laboratory_methods == "intact capsid (enzymatic pretreatment) RT-qPCR" | laboratory_methods == "enzymatic treatment RT-qPCR" ~ "Molecular Assay"))

# If necessary, select alternative groupings from below while commenting out the grouping from above

# data <- mutate(data, new_assay_type = case_when(
#   laboratory_methods == "plaque assay" | laboratory_methods == "TCID50"  ~ "Infectivity Assay",
#   laboratory_methods == "human intestinal enteroids assay" | laboratory_methods == "human intestinal enteroid assay" | laboratory_methods == "human intestinal enteroid assay coupled to RT-qPCR" ~ "Enteroid Assay",
#   laboratory_methods == "RT-qPCR" | laboratory_methods == "molecular beacon NASBA technique with enzymatic treatment" | laboratory_methods == "long-template RT-PCR" | laboratory_methods == "short genome RT-qPCR" | laboratory_methods == "ddRT-PCR" | laboratory_methods == "RT-ddPCR" | laboratory_methods == "long-genome RT-qPCR" | laboratory_methods == "enzyme treatment RT-qPCR" | laboratory_methods == "intact capsid (enzymatic pretreatment) RT-qPCR" | laboratory_methods == "enzymatic treatment RT-qPCR" ~ "Molecular Assay"))

# data <- mutate(data, assay_type = case_when(
#   laboratory_methods == "plaque assay" | laboratory_methods == "TCID50" | laboratory_methods == "human intestinal enteroids assay" | laboratory_methods == "human intestinal enteroid assay" | laboratory_methods == "human intestinal enteroid assay coupled to RT-qPCR" ~ "Infectivity Assay",
#   laboratory_methods == "RT-qPCR" | laboratory_methods == "long-template RT-PCR" | laboratory_methods == "short genome RT-qPCR" | laboratory_methods == "ddRT-PCR" | laboratory_methods == "RT-ddPCR" | laboratory_methods == "long-genome RT-qPCR" ~ "Molecular Assay",
#   laboratory_methods == "molecular beacon NASBA technique with enzymatic treatment" | laboratory_methods == "enzyme treatment RT-qPCR" | laboratory_methods == "intact capsid (enzymatic pretreatment) RT-qPCR" | laboratory_methods == "enzymatic treatment RT-qPCR" ~ "Enzymatic Treatment Molecular Assay"))


data_new <- mutate(data, D_value = case_when(
      time_units == "weeks" ~ D_value * 7,
      time_units == "days" ~ D_value,
      time_units == "hours" ~ D_value/24,
      time_units == "minutes" ~ D_value/(24*60),
      time_units == "seconds" ~ D_value/(24*60*60)))

data_new <- filter(data_new, D_value > 0)

# Box plot testing

data_new <- mutate(data_new, temp_box = case_when(
            temp < 0.5 ~ "<= 0",
            temp >=  0.5 & temp < 10.5 ~ "1 - 10",
            temp >=  10.5 & temp < 20.5 ~ "11 - 20",
            temp >=  20.5 & temp < 30.5 ~ "21 - 30",
            temp >=  30.5 & temp < 40.5 ~ "31 - 40",
            temp >=  40.5 & temp < 50.5 ~ "41 - 50",
            temp >=  50.5 & temp < 60.5 ~ "51 - 60",
            temp >=  60.5 & temp < 70.5 ~ "61 - 70",
            temp >=  70.5 & temp < 80.5 ~ "71 - 80",
            temp >=  80.5 & temp < 90.5 ~ "81 - 90",
            temp >=  90.5 & temp < 100.5 ~ "91 - 100",
            temp >= 100.5 ~ "> 101"))

data_new$temp_box <- factor(data_new$temp_box , levels=c("<= 0", "1 - 10", "11 - 20", "21 - 30", "31 - 40", "41 - 50", "51 - 60", "61 - 70", "71 - 80", "81 - 90", "91 - 100", "> 101"))

#####################################
# Run this when fomites were selected 

data_new <- mutate(data_new, setting = case_when(
              setting == "stainless steel surface" ~ "stainless steel",
              TRUE ~ setting))

summary <- data_new %>% group_by(assay_type, temp_box) %>% summarize(mean = mean(D_value))

summary <- data_new %>% group_by(assay_type, temp_box) %>% summarize(mean_good = mean(D_value[norovirus_types_cleaned_3 == "NoVGI" | norovirus_types_cleaned_3 == "NoVGII"]), 
                                                                                      mean_other = mean(D_value[norovirus_types_cleaned_3 != "NoVGI" & norovirus_types_cleaned_3 != "NoVGII"]))

plot <- ggplot(data_new, aes(x=as.factor(temp_box), y = D_value, fill = assay_type)) + 
        theme_minimal() +
        geom_boxplot() +
        geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3, data_new$setting)), size = 3) +
        labs(x = "Temperature (°C)", y = "D-value (days)", colour = "Type of Fomite", fill = "Type of Assay") +
        scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 

plot

ggsave(paste0("figures_data/summary_figures/temperature_fomites_predicted", ".png"), plot = plot, height = 7, width = 14)

############################################                                                   
# Run this when liquid mediums were selected 
                                                    
data_new <- mutate(data_new, setting_new_type = case_when(
  setting == "processed tap water" ~ "laboratory solutions and tap water",
  setting == "culture medium" ~ "laboratory solutions and tap water",
  setting == "surface water" ~ "surface and ground waters",
  setting == "ground water" ~ "surface and ground waters",
  setting == "groundwater" ~ "surface and ground waters",
  setting == "buffer solution" ~ "laboratory solutions and tap water",
  setting == "wastewater" ~ "wastewater",
  setting == "water" ~ "laboratory solutions and tap water",
  setting == "drinking water" ~ "laboratory solutions and tap water",
  setting == "river water" ~ "surface and ground waters",
  setting == "raw surface water" ~ "surface and ground waters",
  setting == "filtererd surface water" ~ "surface and ground waters",
  setting == "estuarine water" ~ "surface and ground waters",
  setting == "molecular water" ~ "laboratory solutions and tap water",
  setting == "river water mesocosm" ~ "surface and ground waters",
  setting == "deionised water" ~ "laboratory solutions and tap water",
  setting == "biofilm (wastewater reactor)" ~ "wastewater"))

summary <- data_new %>% group_by(setting_new_type, assay_type, temp_box) %>% summarize(mean = mean(D_value))

plot <- ggplot(data_new, aes(x=as.factor(temp_box), y = D_value, fill = assay_type)) + 
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width = 0.75), aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3), fill = assay_type), size = 3) +
  labs(x = "Temperature (°C)", y = "D-value (days)", colour = "Type of Fomite", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~setting_new_type, strip.position = "left", ncol = 1, labeller = label_wrap_gen(width=25)) +
  theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 
  
plot

ggsave(paste0("figures_data/summary_figures/temperature_liquid_predicted", ".png"), plot = plot, height = 7, width = 14)

##########################################                                              
# Run this when food mediums were selected 
                                              
data_new <- mutate(data_new, setting_new_type = case_when(
  setting == "turkey deli meat" ~ "oysters and turkey meat",
  setting == "strawberry surface" ~ "vegetables and fruits",
  setting == "lettuce surface" ~ "vegetables and fruits",
  setting == "oysters" ~ "oysters and turkey meat",
  setting == "hydroponic nutrient solution" ~ "hydroponic nutrient solution",
  setting == "cow milk" ~ "milk",
  setting == "milk" ~ "milk",
  setting == "oysters midgut gland" ~ "oysters and turkey meat",
  setting == "peppers surface" ~ "vegetables and fruits",
  setting == "snail meat" ~ "oysters and turkey meat",
  setting == "snail viscera" ~ "oysters and turkey meat",
  setting == "raspberry" ~ "vegetables and fruits",
  setting == "strawberry" ~ "vegetables and fruits"))

summary <- data_new %>% group_by(setting_new_type, assay_type, temp_box) %>% summarize(mean = mean(D_value))

data_new <- filter(data_new, setting_new_type != "milk" & setting_new_type != "hydroponic nutrient solution") 

plot <- ggplot(data_new, aes(x=as.factor(temp_box), y = D_value, fill = assay_type)) + 
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3)), size = 3) +
  labs(x = "Temperature (°C)", y = "D-value (days)", colour = "Type of Fomite", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~setting_new_type, strip.position = "left", ncol = 1, labeller = label_wrap_gen(width=25)) +
  theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 

plot

ggsave(paste0("figures_data/summary_figures/temperature_food_predicted", ".png"), plot = plot, height = 7, width = 14)

###################
###################
# Relative Humidity
# this climate variable is applicable only for fomites. No sufficient data was available for plotting for liquid and food mediums
                                              
data <- filter(df, environmental_variables_cleaned == "relative humidity" & !is.na(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`)) %>% select(covidence_id, setting, laboratory_methods, `data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, norovirus_types_cleaned_3, temperature_concomitant)

data <- mutate(data, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)` = strsplit(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, ","))

data_new <- data.frame()

for (i in 1:nrow(data)){
  s <- 0
  for (j in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    if (str_detect(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][j], regex("\\(")) == TRUE){
      # number of datapoints
      s <- s + 1
    }
  }

  # number of elements per datapoint. Should be either two or four
  s2 <- length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])/s

  for (m in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    counter <- 0
    while (counter < s){
      counter_2 <- 1
      position <- counter * s2 + 1
      while (counter_2 <= s2){
        new_row <- data[i,]
        if (s2 == 2){
          new_row$relative_humidity <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- NA
          new_row$higher_D <- NA
        }
        if (s2 == 4){
          new_row$relative_humidity <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 2]
          new_row$higher_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 3]
        }
        counter_2 <- counter_2 + 1
      }
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("week")) == TRUE){
        new_row$time_units <- "weeks"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("day")) == TRUE){
        new_row$time_units <- "days"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("hour")) == TRUE){
        new_row$time_units <- "hours"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("min")) == TRUE){
        new_row$time_units <- "minutes"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("sec")) == TRUE){
        new_row$time_units <- "seconds"}

      data_new <- rbind(data_new, new_row)
      counter <- counter + 1
    }
  }
}

data <- data_new
remove(data_new, new_row, counter, counter_2, i, j, m, position, s, s2)

data <- data[!duplicated(data), ]

data <- mutate(data, relative_humidity = as.numeric(gsub('\\(', '', relative_humidity)))
data <- mutate(data, D_value = as.numeric(gsub('\\)', '', D_value)))
data <- mutate(data, lower_D = as.numeric(gsub('\\)', '', lower_D)))
data <- mutate(data, higher_D = as.numeric(gsub('\\)', '', higher_D)))
data <- mutate(data, temperature_concomitant = as.numeric(temperature_concomitant))

data <- mutate(data, assay_type = case_when(
  laboratory_methods == "plaque assay" | laboratory_methods == "binding assay" ~ "Infectivity Assay",
  laboratory_methods == "RT-qPCR" | laboratory_methods == "molecular beacon NASBA technique with enzymatic treatment" ~ "Molecular Assay"))

data <- mutate(data, setting = case_when(
  setting == "stainless steel surface" ~ "stainless steel",
  TRUE ~ setting))

data_new <- mutate(data, D_value = case_when(
  time_units == "weeks" ~ D_value * 7,
  time_units == "days" ~ D_value,
  time_units == "hours" ~ D_value/24,
  time_units == "minutes" ~ D_value/(60*24),
  time_units == "seconds" ~ D_value/(60*60*24)))

data_new <- filter(data_new, D_value > 0)

# Box plot testing

data_new <- mutate(data_new, relative_humidity_box = case_when(
  relative_humidity < 20.5 ~ "0 - 20",
  relative_humidity >=  20.5 & relative_humidity < 40.5 ~ "21 - 40",
  relative_humidity >=  40.5 & relative_humidity < 60.5 ~ "41 - 60",
  relative_humidity >=  60.5 & relative_humidity < 80.5 ~ "61 - 80",
  relative_humidity >=  80.5 & relative_humidity < 100.5 ~ "81 - 100"))

data_new$relative_humidity_box <- factor(data_new$relative_humidity_box , levels=c("0 - 20", "21 - 40", "41 - 60", "61 - 80", "81 - 100"))

summary <- data_new %>% group_by(assay_type, relative_humidity_box, norovirus_types_cleaned_3) %>% summarize(mean = mean(D_value))

plot <- ggplot(data_new, aes(x=as.factor(relative_humidity_box), y = D_value, fill = assay_type)) + 
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3, data_new$setting)), size = 3) +
  labs(x = "Relative Humidity (%)", y = "D-value (days)", colour = "Type of Fomite", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 

plot

ggsave(paste0("figures_data/summary_figures/relative_humidity_fomites_predicted", ".png"), plot = plot, height = 7, width = 14)

########################
########################
# UV wavelength and dose
# applicable to fomite and liquid mediums
                                              
data <- filter(df, environmental_variables_cleaned == "UV dose" & !is.na(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`)) %>% select(covidence_id, setting, laboratory_methods, `data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, norovirus_types_cleaned_3)

data <- mutate(data, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)` = strsplit(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, ","))

data_new <- data.frame()

for (i in 1:nrow(data)){
  s <- 0
  for (j in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    if (str_detect(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][j], regex("\\(")) == TRUE){
      # number of datapoints
      s <- s + 1
    }
  }
  
  # number of elements per datapoint. Should be either two or four
  s2 <- length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])/s
  
  for (m in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    counter <- 0
    while (counter < s){
      counter_2 <- 1
      position <- counter * s2 + 1
      while (counter_2 <= s2){
        new_row <- data[i,]
        if (s2 == 2){
          new_row$UV_wavelength <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- NA
          new_row$higher_D <- NA
        }
        if (s2 == 4){
          new_row$UV_wavelength <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 2]
          new_row$higher_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 3]
        }
        counter_2 <- counter_2 + 1
      }
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("week")) == TRUE){
        new_row$time_units <- "weeks"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("day")) == TRUE){
        new_row$time_units <- "days"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("hour")) == TRUE){
        new_row$time_units <- "hours"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("min")) == TRUE){
        new_row$time_units <- "minutes"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("sec")) == TRUE){
        new_row$time_units <- "seconds"}
      
      data_new <- rbind(data_new, new_row)
      counter <- counter + 1
    }
  }
}

data <- data_new
remove(data_new, new_row, counter, counter_2, i, j, m, position, s, s2)

data <- data[!duplicated(data), ]

data <- mutate(data, UV_wavelength = as.numeric(gsub('\\(', '', UV_wavelength)))
data <- mutate(data, D_value = as.numeric(gsub('\\)', '', D_value)))
data <- mutate(data, lower_D = as.numeric(gsub('\\)', '', lower_D)))
data <- mutate(data, higher_D = as.numeric(gsub('\\)', '', higher_D)))

data <- mutate(data, assay_type = case_when(
  laboratory_methods == "plaque assay" | laboratory_methods == "TCID50" | laboratory_methods == "Zebrafish assay and RT-qPCR" | laboratory_methods == "integrated cell culture RT-qPCR" ~ "Infectivity Assay",
  laboratory_methods == "RT-qPCR" | laboratory_methods == "enzymatic treatment RT-qPCR" | laboratory_methods == "pre-treatment" ~ "Molecular Assay"))

data <- mutate(data, setting = case_when(
  setting == "stainless steel surface" ~ "stainless steel",
  TRUE ~ setting))

data_new <- filter(data, D_value > 0)

# Box plot testing

data_new <- mutate(data_new, UV_wavelength_box = case_when(
  UV_wavelength ==  220 ~ "220",
  UV_wavelength ==  253.7 | UV_wavelength == 254 ~ "254",
  UV_wavelength ==  260 ~ "260",
  UV_wavelength ==  265 ~ "265",
  UV_wavelength ==  279 ~ "279",
  UV_wavelength ==  280 & !(covidence_id == "1274" | covidence_id == 1274) ~ "280",
  UV_wavelength ==  280 & (covidence_id == "1274" | covidence_id == 1274) ~ "280-315",
  UV_wavelength ==  315 & (covidence_id == "1274" | covidence_id == 1274) ~ "315-400"))

data_new$UV_wavelength_box <- factor(data_new$UV_wavelength_box , levels=c("220", "254", "260", "265", "279", "280", "280-315", "315-400"))

##############################################
# run this subsection for plotting for fomites
                                              
plot <- ggplot(data_new, aes(x=as.factor(UV_wavelength_box), y = D_value, fill = assay_type)) + 
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3, data_new$setting)), size = 3) +
  labs(x = "Wavelength (nm)", y = "D10-value (mJ/cm2)", colour = "Type of Fomite", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 

plot

ggsave(paste0("figures_data/summary_figures/UV_wavelength_fomites_predicted", ".png"), plot = plot, height = 7, width = 14)

#####################################################
# run this subsection for plotting for liquid mediums
                                              
data_new <- mutate(data_new, setting_new_type = case_when(
  setting == "processed tap water" ~ "laboratory solutions and tap water",
  setting == "culture medium" ~ "laboratory solutions and tap water",
  setting == "surface water" ~ "surface and ground waters",
  setting == "ground water" ~ "surface and ground waters",
  setting == "buffer solution" ~ "laboratory solutions and tap water",
  setting == "wastewater" ~ "wastewater",
  setting == "water" ~ "laboratory solutions and tap water",
  setting == "drinking water" ~ "laboratory solutions and tap water",
  setting == "river water" ~ "surface and ground waters",
  setting == "raw surface water" ~ "surface and ground waters",
  setting == "filtererd surface water" ~ "surface and ground waters",
  setting == "estuarine water" ~ "surface and ground waters",
  setting == "molecular water" ~ "laboratory solutions and tap water",
  setting == "river water mesocosm" ~ "surface and ground waters",
  setting == "deionised water" ~ "laboratory solutions and tap water",
  setting == "biofilm (wastewater reactor)" ~ "wastewater",
  setting == "partially treated wastewater" ~ "wastewater",
  setting == "filtered groundwater" ~ "surface and ground waters",
  setting == "filtered culture medium" ~ "laboratory solutions and tap water",
  setting == "wastewater effluent" ~ "wastewater"))

plot <- ggplot(data_new, aes(x=as.factor(UV_wavelength_box), y = D_value, fill = assay_type)) +
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3)), size = 3) +
  labs(x = "Wavelength (nm)", y = "D10-value (mJ/cm2)", colour = "Type of Fomite", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  # facet_wrap(~setting_new_type, strip.position = "left", ncol = 1, labeller = label_wrap_gen(width=25)) +
  theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 

plot

summary <- data_new %>% group_by(assay_type, UV_wavelength_box, norovirus_types_cleaned_3) %>% summarize(mean = mean(D_value))

ggsave(paste0("figures_data/summary_figures/UV_wavelength_liquid_predicted", ".png"), plot = plot, height = 7, width = 14)


###################
# Processing for pH
# applicable to liquid mediums
# Although pH was removed from the inclusion in the systematic review, a plotting subsection for it was built if useful
                                              
data <- filter(df, environmental_variables_cleaned == "pH" & !is.na(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`)) %>% select(covidence_id, setting, laboratory_methods, `data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, norovirus_types_cleaned_3)

data <- mutate(data, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)` = strsplit(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, ","))

data_new <- data.frame()

for (i in 1:nrow(data)){
  s <- 0
  for (j in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    if (str_detect(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][j], regex("\\(")) == TRUE){
      # number of datapoints
      s <- s + 1
    }
  }
  
  # number of elements per datapoint. Should be either two or four
  s2 <- length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])/s
  
  for (m in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    counter <- 0
    while (counter < s){
      counter_2 <- 1
      position <- counter * s2 + 1
      while (counter_2 <= s2){
        new_row <- data[i,]
        if (s2 == 2){
          new_row$pH <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- NA
          new_row$higher_D <- NA
        }
        if (s2 == 4){
          new_row$pH <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 2]
          new_row$higher_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 3]
        }
        counter_2 <- counter_2 + 1
      }
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("week")) == TRUE){
        new_row$time_units <- "weeks"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("day")) == TRUE){
        new_row$time_units <- "days"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("hour")) == TRUE){
        new_row$time_units <- "hours"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("min")) == TRUE){
        new_row$time_units <- "minutes"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("sec")) == TRUE){
        new_row$time_units <- "seconds"}
      
      data_new <- rbind(data_new, new_row)
      counter <- counter + 1
    }
  }
}

data <- data_new
remove(data_new, new_row, counter, counter_2, i, j, m, position, s, s2)

data <- data[!duplicated(data), ]

data <- mutate(data, pH = as.numeric(gsub('\\(', '', pH)))
data <- mutate(data, D_value = as.numeric(gsub('\\)', '', D_value)))
data <- mutate(data, lower_D = as.numeric(gsub('\\)', '', lower_D)))
data <- mutate(data, higher_D = as.numeric(gsub('\\)', '', higher_D)))

data <- mutate(data, assay_type = case_when(
  laboratory_methods == "plaque assay" | laboratory_methods == "TCID50" | laboratory_methods == "binding assay" ~ "Infectivity Assay",
  laboratory_methods == "RT-qPCR" ~ "Molecular Assay"))

data_new <- mutate(data, D_value = case_when(
  time_units == "weeks" ~ D_value * 7,
  time_units == "days" ~ D_value,
  time_units == "hours" ~ D_value/24,
  time_units == "minutes" ~ D_value/(24*60),
  time_units == "seconds" ~ D_value/(24*60*60)))

data_new <- filter(data_new, D_value > 0)

# Box plot testing

data_new <- mutate(data_new, pH_box = case_when(
  pH >= 0 & pH < 2.5 ~ "0 - 2",
  pH >= 2.5 & pH < 5.5 ~ "3 - 5",
  pH >= 5.5 & pH < 8.5 ~ "6 - 8",
  pH >= 8.5 & pH < 11.5 ~ "9 - 11",
  pH >= 11.5 & pH < 14.5 ~ "12 - 14"))

data_new$pH_box <- factor(data_new$pH_box , levels=c("0 - 2", "3 - 5", "6 - 8", "9 - 11", "12 - 14"))

plot <- ggplot(data_new, aes(x=as.factor(pH_box), y = D_value, fill = assay_type)) + 
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3, data_new$setting)), size = 3) +
  labs(x = "pH", y = "D-value (days)", colour = "Type of Fomite", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 17)) + scale_fill_grey(start = 0.5, end = 1) 

plot

summary <- data_new %>% group_by(assay_type, pH_box) %>% summarize(mean = mean(D_value))

ggsave(paste0("figures_data/summary_figures/pH_liquid_predicted", ".png"), plot = plot, height = 7, width = 14)


#########################
# Processing for Radiance
# applicable to liquid mediums. No sufficient data was available for plotting for fomite mediums
                                              
data <- filter(df, environmental_variables_cleaned == "radiance" & norovirus_variables_cleaned != "S90 (MJ m−2)" & !is.na(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`)) %>% select(covidence_id, setting, laboratory_methods, `data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, norovirus_types_cleaned_3)

data <- mutate(data, `D-value (temp/humidity/others, D-value, lower D-value, higher D-value)` = strsplit(`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`, ","))

data_new <- data.frame()

for (i in 1:nrow(data)){
  s <- 0
  for (j in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    if (str_detect(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][j], regex("\\(")) == TRUE){
      # number of datapoints
      s <- s + 1
    }
  }
  
  # number of elements per datapoint. Should be either two or four
  s2 <- length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])/s
  
  for (m in 1:length(data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]])){
    counter <- 0
    while (counter < s){
      counter_2 <- 1
      position <- counter * s2 + 1
      while (counter_2 <= s2){
        new_row <- data[i,]
        if (s2 == 2){
          new_row$radiance <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- NA
          new_row$higher_D <- NA
        }
        if (s2 == 4){
          new_row$radiance <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position]
          new_row$D_value <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 1]
          new_row$lower_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 2]
          new_row$higher_D <- data$`D-value (temp/humidity/others, D-value, lower D-value, higher D-value)`[[i]][position + 3]
        }
        counter_2 <- counter_2 + 1
      }
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("week")) == TRUE){
        new_row$time_units <- "weeks"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("day")) == TRUE){
        new_row$time_units <- "days"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("hour")) == TRUE){
        new_row$time_units <- "hours"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("min")) == TRUE){
        new_row$time_units <- "minutes"}
      if (str_detect(data$`data (temp/humidity/others)(times)(time_unit)(measurement_log_reduction)`[[i]], regex("sec")) == TRUE){
        new_row$time_units <- "seconds"}
      
      data_new <- rbind(data_new, new_row)
      counter <- counter + 1
    }
  }
}

data <- data_new
remove(data_new, new_row, counter, counter_2, i, j, m, position, s, s2)

data <- data[!duplicated(data), ]

data <- mutate(data, radiance = gsub('\\(', '', radiance))
data <- mutate(data, radiance = gsub(' ', '', radiance))
data <- mutate(data, D_value = as.numeric(gsub('\\)', '', D_value)))
data <- mutate(data, lower_D = as.numeric(gsub('\\)', '', lower_D)))
data <- mutate(data, higher_D = as.numeric(gsub('\\)', '', higher_D)))

data <- mutate(data, assay_type = case_when(
  laboratory_methods == "plaque assay" ~ "Infectivity Assay",
  laboratory_methods == "RT-qPCR" | laboratory_methods == "extrapolated RT-qPCR" | laboratory_methods == "enzymatic treatment RT-qPCR" ~ "Molecular Assay"))

data_new <- mutate(data, D_value = case_when(
  time_units == "weeks" ~ D_value * 7,
  time_units == "days" ~ D_value,
  time_units == "hours" ~ D_value/24,
  time_units == "minutes" ~ D_value/(24*60),
  time_units == "seconds" ~ D_value/(24*60*60)))

data_new <- filter(data_new, D_value > 0)

# Box plot testing

data_new$radiance <- factor(data_new$radiance , levels=c("dark", "sunlight"))

plot <- ggplot(data_new, aes(x=as.factor(radiance), y = D_value, fill = assay_type)) + 
  theme_minimal() +
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75),aes(group = assay_type, colour = paste(sep = " - ", data_new$norovirus_types_cleaned_3, data_new$setting)), size = 3) +
  labs(x = "Radiance", y = "D-value (days)", colour = "Type of Medium", fill = "Type of Assay") + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(text = element_text(size = 15)) + scale_fill_grey(start = 0.5, end = 1) + theme(legend.position = "bottom", legend.direction = "vertical", legend.box = "horizontal") + guides(color = guide_legend(ncol = 2))

plot

summary <- data_new %>% group_by(assay_type, radiance, norovirus_types_cleaned_3) %>% summarize(mean = mean(D_value))

ggsave(paste0("figures_data/summary_figures/radiance_liquid_predicted", ".png"), plot = plot, height = 9, width = 9)


                                              
