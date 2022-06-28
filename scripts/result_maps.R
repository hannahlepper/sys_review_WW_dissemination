#main libraries
library(plyr)
library(tidyverse)

#Get a map of the world
#https://geojson-maps.ash.ms/
library(geojsonio)
maps_data <- geojson_read("data/custom.geo.json", what = "sp")

library(broom)
maps_data_fortified <- tidy(maps_data, region = "adm0_a3")

ggplot(maps_data_fortified, aes(x = long, y = lat, group = group)) +
    geom_polygon() 

#Match names in googlesheet to ISO 3/world bank codes
#https://datahub.io/core/country-codes#resource-country-codes
country_codes <- read_csv("data/country-codes.csv") %>% 
    select(., `ISO3166-1-Alpha-3`, official_name_en) %>%
    setNames(., c("adm_03", "country"))

#get countries and studies

studied_countries <- lapply(1:nrow(all_data), function(i) {
    countries <- unlist(str_split(all_data$country[i], ", |; |,|;")) %>%
        unlist %>% trimws
    data.frame(select(all_data[i,], study_id, finding, hospital_based), country = countries)
}) %>% 
    bind_rows() %>%
    unique 

studied_countries2 <- left_join(studied_countries, country_codes, by = "country") %>%
    mutate(., adm_03 = ifelse(country == "Taiwan", "TWN", adm_03))

studied_countries_hosp_summary <- studied_countries2 %>%
    subset(., hospital_based == "Yes") %>%
    ddply(., .(adm_03), 
    summarise, n_studies = length(country),
               prop_positive = sum(str_detect(replace_na(finding, ""), "ositive"))/length(country))

map_data_core_studies <- left_join(maps_data_fortified, studied_countries_core_summary, 
        by = c("id" = "adm_03"))

library(viridis)

#positive/negative findings
png("plots/positive_study_proportion.png", width = 35, height = 18, units = "cm", res = 600)
ggplot(map_data_core_studies, aes(x = long, y = lat, group = group, fill = prop_positive)) +
    geom_polygon(color = "black") +
    scale_fill_viridis(na.value = "white") +
    theme_bw() +
    labs(x = "", y = "", fill = "Proportion of\nstudies with a\npositive finding") +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.1, 0.5))
dev.off()

#number of studies included in any way
partial_studied_countries <- lapply(1:nrow(partial_data), function(i) {
    countries <- unlist(str_split(partial_data$Country[i], ", |; |,|;")) %>%
        unlist %>% trimws
    data.frame(select(partial_data[i,], study_id), country = countries)
}) %>% 
    bind_rows() %>%
    unique 

partial_studied_countries2 <- left_join(partial_studied_countries, country_codes, by = "country") %>%
    mutate(., adm_03 = ifelse(country == "Taiwan", "TWN", adm_03))

all_studies_summary <- bind_rows(partial_studied_countries2, select(studied_countries2, -finding, -hospital_based)) %>%
    unique() %>%
    ddply(., .(adm_03), summarise, n_studies = length(country), country = country[1])

all_studies_map <- left_join(maps_data_fortified, all_studies_summary, by = c("id"="adm_03"))

png("plots/num_all_studies.png", width = 35, height = 18, units = "cm", res = 600)
ggplot(all_studies_map, aes(x = long, y = lat, group = group, fill = n_studies)) +
    geom_polygon(color = "black") +
    scale_fill_viridis(na.value = "white") +
    theme_bw() +
    labs(x = "", y = "", fill = "Number of\nstudies") +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.1, 0.5))
dev.off()


#number of hospital-based studies
studied_countries_hosp_based <- subset(studied_countries2, hospital_based == "Yes") %>%
    ddply(., .(adm_03), summarise, n_studies = length(country))
map_data_hosp_studies <- left_join(maps_data_fortified, studied_countries_hosp_based, 
        by = c("id" = "adm_03"))

png("plots/num_hosp_studies.png", width = 35, height = 18, units = "cm", res = 600)
ggplot(map_data_hosp_studies, aes(x = long, y = lat, group = group, fill = n_studies)) +
    geom_polygon(color = "black") +
    scale_fill_viridis(breaks = c(1,3,5,7,9,11,13,15), na.value = "white") +
    theme_bw() +
    labs(x = "", y = "", fill = "Number of\nstudies") +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.1, 0.5))
dev.off()
