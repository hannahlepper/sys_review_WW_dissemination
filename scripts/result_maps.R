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
    setNames(., c("adm_03", "country")) %>%
    mutate(., country = gsub(",", "",country))

#get countries and studies
country_df <- select(all_data, study_id, finding, Core_partial, country = country)

studied_countries <- lapply(1:nrow(country_df), function(n) {
        countries = str_split(country_df$country[n], ",|;") %>%
            unlist() %>%
            trimws() 
        data.frame(country_df[n, -4], country = countries)
    }) %>%
    bind_rows()


studied_countries2 <- left_join(studied_countries, country_codes, by = "country") %>%
    mutate(., adm_03 = ifelse(country == "Taiwan", "TWN", adm_03))

studied_countries_summary <- studied_countries2 %>%
    ddply(., .(adm_03), summarise, 
               n_studies = length(country),
               prop_positive = sum(str_detect(replace_na(finding, ""), "ositive"))/length(country))

map_data_studies <- left_join(maps_data_fortified, studied_countries_summary, 
        by = c("id" = "adm_03"))

library(viridis)

#positive/negative findings
png("plots/positive_study_proportion.png", width = 35, height = 18, units = "cm", res = 600)
ggplot(map_data_studies, aes(x = long, y = lat, group = group, fill = prop_positive)) +
    geom_polygon(color = "black") +
    scale_fill_viridis(na.value = "white") +
    theme_bw() +
    labs(x = "", y = "", fill = "Proportion of\nstudies with a\npositive finding") +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.1, 0.5))
dev.off()

#number of studies included in any way
png("plots/num_all_studies.png", width = 35, height = 18, units = "cm", res = 600)
ggplot(map_data_studies, aes(x = long, y = lat, group = group, fill = n_studies)) +
    geom_polygon(color = "black") +
    scale_fill_viridis(na.value = "white") +
    theme_bw() +
    labs(x = "", y = "", fill = "Number of\nstudies") +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.position = c(0.1, 0.5))
dev.off()
