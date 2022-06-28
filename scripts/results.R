
#Describe findings and directions
findings <- subset(all_data, str_detect(finding, "ositive|egative")) %>%
    select(., Screening_version, direction, finding, study_id, hospital_based)
findings_only <- lapply(1:nrow(findings), function(i) {
    ds <- str_trim(tolower(unlist(str_split(findings$direction[i], ";"))))
    data.frame(finding = tolower(findings$finding[i]), direction = ds, screen_version = findings$Screening_version[i], study_id = findings$study_id[i])
}) %>% bind_rows()

table(findings_only$direction, tolower(findings_only$finding), findings_only$screen_version)

table(partial_data$Route, partial_data$Conclusion)

#Clinical or not clinical source
findings$clin <- sapply(findings$direction, function(i) as.numeric(str_detect(i, "hww|HWW|atient|nursing|ospital")))
findings_clin <- subset(findings, str_detect(direction, "hww|HWW|atient|nursing|ospital") & 
    !(str_detect(direction, "Community|to environment")))
table(findings_clin$clin, tolower(findings_clin$finding), findings_clin$Screening_version)

table(findings_clin$direction, tolower(findings_clin$finding), findings_clin$Screening_version)

#Describe year and country of studies
#1. year of study - those looking at sources of resistance with colour for hospital based vs. not hospital based studies
findings <- subset(all_data, str_detect(finding, "ositive|egative")) %>%
    select(., study_id, Screening_version, direction, finding, year, hospital_based)

year_of_study <- lapply(1:length(unique(findings$study_id)), function(n) {
    si <- unique(findings$study_id)[n]
    n_rows_study <- sum(findings$study_id == si)
    data.frame(study_id = si, 
               direction = ifelse(n_rows_study == 0, 
                                            findings$direction[findings$study_id == si], 
                                            paste(findings$direction[findings$study_id == si], collapse = "; ")),
                year = unique(findings$year[n]),
                hospital_based = findings$hospital_based[n])
}) %>% bind_rows() %>% 
    subset(., !(str_detect(direction, "to_environment"))) 

year_of_study_s <- ddply(year_of_study, .(year, hospital_based), summarise, n_studies = length(year))

png("plots/num_studies_sources.png", width = 15, height = 6, units = "cm", res = 300)
ggplot(year_of_study_s, aes(year,n_studies, fill = hospital_based)) + 
    geom_col(position = "stack") +
    theme_bw() +
    labs(x = "Publication year", y = "Number of studies", fill = "Hospital\nbased?") +
    theme(legend.position = c(0.1, 0.7))
dev.off()

png("plots/num_studies_diss.png", width = 15, height = 6, units = "cm", res = 300)
ggplot(partial_data, aes(Year)) + geom_bar(stat = "count") +
    labs(x = "Publication year", y = "Number of studies") + 
    theme_bw()
dev.off()

#AB by study type
#1. by direction
abs_direction_only <- subset(abs_direction, str_detect(finding, "ositive|egative"))

abs_study_only <- unique(select(abs_direction_only, -dir, -finding))

abs_direction2 <- left_join(findings_only, abs_study_only, by = "study_id")

abs_g <- gather(abs_direction2, key = "AB", value = "yn", -study_id, -direction) %>%
    subset(., yn == 1) %>%
    mutate(., AB = as.factor(AB))

colours <- c("#F0A3FF","#0075DC","#993F00","#4C005C","#2BCE48", "#FFCC99","#808080","#94FFB5","#8F7C00","#9DCC00",
             "#C20088","#003380","#FFA405","#FFA8BB","#426600","#FF0010","#5EF1F2","#00998F","#740AFF","#990000", "#FFFF00");
colours1 <- c("#353D3F","#795548","#E81F63","#F94D2C","#71A344",
              "#CFBC4D","#FF9800","#4CAF50","#EED829","#8BC34A",
              "#6360C7","#673AB7","#3F51B5","#607D8B","#9C28B0",
              "#54BADB","#009688","#0CA9E9","#A1A1A1","#1E3A68",
              "#FFFF00");
colours2 <- c(colours, colours1)

ab_colours <- colours2[unique(abs_g$AB)]
names(ab_colours) <- unique(abs_g$AB)

png("plots/ABs_by_direction.png", width = 15, height = 10, units = "cm", res = 600)
ggplot(abs_g, aes(direction, fill = AB)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = ab_colours) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(y = "Proportion of studies", x = "")
dev.off()

abs_finding <- select(abs_direction, -dir) %>%
    mutate(., finding2 = str_match(tolower(finding), "positive|negative")) %>%
    mutate(., finding2 = ifelse(is.na(finding2), "none", finding2))

abs_finding_g <- gather(abs_finding, key = "AB", value = "yn", -study_id, -finding, -finding2) %>%
    subset(., yn == 1) %>%
    mutate(., AB = as.factor(AB))

png("plots/ABs_by_finding.png", width = 7, height = 10, units = "cm", res = 600)
ggplot(abs_finding_g, aes(finding2, fill = AB)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = ab_colours) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(y = "Proportion of studies", x = "")
dev.off()

png("plots/ab_legend.png", width = 30, height = 10, units = "cm", res = 600)
ggplot(abs_finding_g, aes(finding2, fill = AB)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = ab_colours) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "", x = "", fill = "Antibiotic resistance phenotype studied") +
    guides(fill = guide_legend(title.position = "top"))
dev.off()

abs_only <- unique(select(abs_direction, -dir, -finding)) %>%
    select(., -study_id) %>%
    as.matrix %>%
    apply(., 2, as.numeric) %>%
    colSums
sort(abs_only)

studies_only <- unique(select(abs_direction, -dir, -finding)) %>%
    select(., -study_id) %>%
    as.matrix %>%
    apply(., 2, as.numeric) %>%
    colSums

#Bacteria studied
#Most commonly studied bacteria
org_studied <- org_study_id %>%
    select(., -study_id) %>%
    as.matrix %>%
    apply(., 2, as.numeric) %>%
    colSums

#Plot by direction
orgs_direction <- left_join(findings_only, org_study_id, by = "study_id")
orgs_dir_g <- gather(orgs_direction, key = "bacterial_group", value = "yn", -study_id, -direction, -finding) %>%
    subset(., yn == 1)

bac_colours <- colours2[1:length(unique(orgs_dir_g$bacterial_group))]
names(bac_colours) <- unique(orgs_dir_g$bacterial_group)

png("plots/org_by_dir.png", width = 15, height = 10, units = "cm", res = 600)
ggplot(orgs_dir_g, aes(direction, fill = bacterial_group)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = bac_colours) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Proportion of studies", x = "")
dev.off()

#Plot by finding
png("plots/org_by_finding.png", width = 7, height = 10, units = "cm", res = 600)
ggplot(orgs_dir_g, aes(finding, fill = bacterial_group)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = bac_colours) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Proportion of studies", x = "")
dev.off()

#Legend
png("plots/bac_legend.png", width = 30, height = 10, units = "cm", res = 600)
ggplot(orgs_dir_g, aes(finding, fill = bacterial_group)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = bac_colours) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(y = "", x = "", fill = "Bacterial group studied") +
    guides(fill = guide_legend(title.position = "top"))
dev.off()

#Sample types
sample_types <- select(all_data, study_id, `Sampled hospital patients?`, 
                        `Sampled hospital environment?`, `Sampled hospital sewage?`, 
                        `Sampled HCW?`) %>%
                .[-which(findings$hospital_based == "Yes"),] %>%
                gather(., key = "st", value = "yn", -study_id)
table(sample_types$st, sample_types$yn)

#Statistics
all_study_find <- select(all_data, Core_partial, study_id, direction, finding, Screening_version)
all_study_find2 <- lapply(1:nrow(all_study_find), function(i) {
    if(all_study_find$Core_partial[i] == "partial") {
        data.frame(finding = "none", direction = "None", 
               screen_version = all_study_find$Screening_version[i], 
               study_id = all_study_find$study_id[i])
    } else {
       ds <- str_trim(tolower(unlist(str_split(all_study_find$direction[i], ";"))))
        data.frame(finding = tolower(all_study_find$finding[i]), direction = ds, 
               screen_version = all_study_find$Screening_version[i], 
               study_id = all_study_find$study_id[i]) 
    }
    
}) %>% bind_rows()

stat_use <- select(all_data, study_id, statistics, stat_type) %>% unique

stat_use2 <- left_join(all_study_find2, stat_use, by = "study_id") 

table(stat_use2$statistics, stat_use2$finding)

all_stat_types <- lapply(1:nrow(stat_use2), function(i) {
    sts <- str_trim(tolower(unlist(str_split(stat_use2$stat_type[i], ";|,"))))
    data.frame(stat_type = sts, study_id = stat_use$study_id[i], 
               finding = stat_use2$finding[i], direction = stat_use2$direction[i])
}) %>% bind_rows()
all_stat_types2 <- subset(all_stat_types, !(direction == "None"))

st_type_colours <- colours[1:length(unique(all_stat_types2$stat_type))]
names(st_type_colours) <- unique(all_stat_types2$stat_type)

png("plots/stat_type_by_direction.png", width = 15, height = 10, units = "cm", res = 600)
ggplot(all_stat_types2, aes(direction, fill = stat_type)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = st_type_colours) +
    theme_bw() +
    theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Proportion of studies")
dev.off()

png("plots/stat_type_by_finding.png", width = 7, height = 10, units = "cm", res = 600)
ggplot(all_stat_types, aes(finding, fill = stat_type)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = st_type_colours) +
    theme_bw() +
    theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Proportion of studies", fill = "Type of statistics") 
dev.off()

png("plots/stat_type_legend.png", width = 30, height = 10, units = "cm", res = 300)
ggplot(all_stat_types, aes(finding, fill = stat_type)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = st_type_colours) +
    theme_bw() +
    theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "", fill = "Type of statistics") +
    guides(fill = guide_legend(title.position = "top"))
dev.off()

#Type of laboratory methods
typing <- select(all_data, study_id, highest_res_typing_group, highest_org_typing) %>% 
    unique
table(typing$highest_res_typing_group)
table(typing$highest_org_typing)

#Antimicrobial quantification
antimicrobial_quant <- select(all_data, study_id, antimicrobialquant) %>% unique
table(antimicrobial_quant$antimicrobialquant)

#No duplicates
studies_unique <- select(all_data, finding, study_id, hospital_based, Core_partial, Screening_version, hospital_based) %>%
    unique

length(unique(c(partial_study_id, study_id)))
