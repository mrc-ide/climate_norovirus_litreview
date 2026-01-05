install.packages('CoordinateCleaner')
install.packages('countrycode')
install.packages('ggrepel')
install.packages('MetBrewer')

library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(countrycode)
library(ggrepel)
library(MetBrewer)
library(scales)

#read data
df <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Data General")

df <- filter(df, environmental_variables != "pH")

#read data for multi-country studies
df_multi <- readxl::read_xlsx("Data extraction norovirus.xlsx", sheet = "Breakdown of Multicountry")

#================================================================================================================
#quick plot of studies by year

df %>%
  group_by(year_of_article) %>%
  summarise(count = n()) %>%
  ggplot()+
  aes(x = year_of_article, y=count) +
  geom_col(fill = viridis::viridis(9)[5])+
  theme_minimal()+
  labs(x = "Year of article",
       y =  "Study count")+
  # scale_fill_viridis_c(option = "magma", end = 0.8)+
  theme(legend.position = "none", text = element_text(size = 18)) + 
  scale_y_continuous(breaks = pretty_breaks())

ggsave("figures_data/studies_by_year.png", height = 10, width = 12)

#================================================================================================================
# quick plot of studies by country

#get world map
world_map <- map_data("world")

#plot background
map_bg <- ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group),
               fill="grey") +
  coord_quickmap(ylim = c(-51, 70)) +
  theme_void()

# #get midpoints of countries in case it is handy later
# countries <- CoordinateCleaner::countryref
#
# countries <- countries %>% select(name, iso3, centroid.lon, centroid.lat) %>%
#   group_by( iso3) %>%
#   summarise(long = centroid.lon[[1]], lat = centroid.lat[[1]], name = name[[1]])
#
# countries <-  countries %>% mutate(name = toupper(name))

# match case
world_map <- world_map %>% mutate(region = toupper(region))
df <- df %>% mutate(country_and_region = toupper(country_and_region))
df_multi <- df_multi %>% mutate(country_and_region = toupper(country_and_region))

# # optional: check which country values do or do not join appropriately
# joined_countries <- filter(df[, c("covidence_id", "country_and_region")], country_and_region != "NA") %>% left_join(world_map[!duplicated(world_map[, "region"]), c("long", "lat", "region")], by = c("country_and_region" = "region"))
# joined_countries <- filter(df_multi[, c("covidence_id", "country_and_region")], country_and_region != "NA") %>% left_join(world_map[!duplicated(world_map[, "region"]), c("long", "lat", "region")], by = c("country_and_region" = "region"))

# join 'em up
world_map <- world_map %>% left_join(filter(rbind(df[, c("covidence_id", "country_and_region")], df_multi[, c("covidence_id", "country_and_region")]), country_and_region != "NA"), by = c("region" = "country_and_region"))

# add the continents
world_map <- world_map %>% mutate(continent = countrycode::countrycode(sourcevar = region,
                                                                       origin = "country.name",
                                                                       destination = "continent"))

#add study count
world_map <- world_map %>%
  group_by(region) %>%
  mutate(study_count = length(na.omit(unique(covidence_id)))) %>%
  ungroup() %>%
  group_by(continent) %>%
  mutate(continent_study_count = length(na.omit(unique(covidence_id)))) %>%
  mutate(iso3 = countrycode(sourcevar = region, origin = "country.name", destination = "iso3c"))

g_world <- map_bg + geom_polygon(data = world_map %>%
                 filter(!is.na(region)),
                 aes(x=long, y = lat, group = group, fill = (study_count))) +
                 labs( fill = "Study count") +
                 scale_fill_viridis_c(option = "viridis", na.value = "azure2", limits = c(1, max(world_map$study_count, na.rm = TRUE)), end = 0.8, breaks = breaks_pretty())+
                 #scale_fill_manual(values = c("azure2",viridis::magma(16)))+
                 theme(legend.position = "bottom", text = element_text(size = 18)) +
                 borders(database = "world", regions = ".", colour = "grey50", size = 0.1)

g_world

ggsave("figures_data/studies_by_country.png", height = 10, width = 12)

world_map %>% ungroup() %>% select(region, study_count) %>% unique() %>% write.csv("figures_data/by_country_count.csv", row.names = FALSE)
