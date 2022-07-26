
#Describe findings and directions
findings <- subset(all_data, str_detect(finding, "ositive|egative")) %>%
    select(., Screening_version, direction, finding, study_id, hospital_based)
findings_only <- lapply(1:nrow(findings), function(i) {
    ds <- str_trim(tolower(unlist(str_split(findings$direction[i], ";"))))
    data.frame(finding = tolower(findings$finding[i]), direction = ds, screen_version = findings$Screening_version[i], study_id = findings$study_id[i])
}) %>% bind_rows()

table(findings_only$direction, tolower(findings_only$finding))

#Describe year and country of studies

#1. year of study - those looking at sources of resistance with colour for hospital based vs. not hospital based studies
year_df <- select(all_data, Core_partial, study_id, year)
min(year_df$year)
summary(year_df$year)

year_df$Core_partial <- case_when(year_df$Core_partial == "full" ~ "Yes", year_df$Core_partial == "partial" ~ "No", TRUE ~ "")

png("plots/all_studies_timeline.png", width = 15, height = 6, units = "cm", res = 300)
ggplot(year_df, aes(year, fill = Core_partial)) + 
    geom_bar(stat = "count") +
    theme_bw() +
    labs(x = "Publication year", y = "Number of studies", fill = "Evidence\nstatement?") +
    theme(legend.position = c(0.1, 0.7))
dev.off()


#AB by study type
#1. by direction
directions_ids_only <- lapply(1:nrow(abs_direction), function(r) {
        dirs <- unlist(str_split(abs_direction$dir[r], ";|,")) %>%
            trimws
        dirs <- ifelse(is.na(dirs)|all(dirs == "-"), "Inconclusive", dirs)
        data.frame(study_id = abs_direction$study_id[r], dir = dirs)
    }) %>% bind_rows()
num_studies <- ddply(directions_ids_only, .(dir), summarise, n_studies = length(dir))
directions_ids_only <- left_join(directions_ids_only, num_studies, by = "dir")

abs_dir_plotdf <- directions_ids_only  %>%
    left_join(., select(abs_direction, -dir), by = "study_id") %>%
    mutate(., dir = factor(dir, levels = c("Inconclusive", "Patients to HWW", 
        "Patients to MWW", "HWW to MWW", "Hospital to MWW (indirect)"), ordered = TRUE)) 
abs_g <- gather(abs_dir_plotdf, key = "AB", value = "yn", -study_id, -dir, -n_studies) %>%
    subset(., yn == 1) %>%
    mutate(., AB = as.factor(AB))

abs_studies <- ddply(abs_g, .(AB), summarise, 
    n_studies = length(study_id))

abs_g <- left_join(select(abs_g, -n_studies), abs_studies, by = "AB")

abs_g$AB <- factor(abs_g$AB, 
    levels = abs_studies$AB[order(abs_studies$n_studies, decreasing = TRUE)], ordered = TRUE)

colours <- c("#F0A3FF","#0075DC","#993F00","#4C005C","#2BCE48", "#FFCC99","#808080","#94FFB5","#8F7C00","#9DCC00",
             "#C20088","#003380","#FFA405","#FFA8BB","#426600","#FF0010","#5EF1F2","#00998F","#740AFF","#990000", "#FFFF00");
colours1 <- c("#353D3F","#795548","#E81F63","#F94D2C","#71A344",  "#CFBC4D","#FF9800","#4CAF50","#EED829","#8BC34A",
              "#6360C7","#673AB7","#3F51B5","#607D8B","#9C28B0","#54BADB","#009688","#0CA9E9","#A1A1A1","#1E3A68", "#FFFF00");
colours2 <- c(colours, colours1)

dir_colours <- scales::viridis_pal()(length(unique(abs_g$dir)))
names(dir_colours) <- unique(abs_g$dir)

png("plots/ABs_by_direction.png", width = 30, height = 18, units = "cm", res = 600)
ggplot(subset(abs_g, n_studies > 5), aes(AB, fill = dir)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_colour_manual(values = dir_colours) +
    labs(y = "Proportion of studies", x = "", fill = "Direction of study conclusion") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom", plot.margin = margin(l = 50)) +
    guides(fill = guide_legend(title.position = "top")) +
    geom_text(mapping = aes(AB, y = -0.05, label = n_studies)) 
dev.off()


#number of studies per antibiotic
abs_only <- unique(select(abs_direction, -dir, -finding)) %>%
    select(., -study_id) %>%
    as.matrix %>%
    apply(., 2, as.numeric) %>%
    colSums
sort(abs_only)

#number of ATC groups per study
studies_only <- select(abs_direction, -dir, -finding, -(`No ATC group found`), -(`All (WGS, MG, large PCR panel)`)) %>%
    unique() %>%
    select(., -study_id) %>%
    as.matrix %>%
    apply(., 2, as.numeric) %>%
    rowSums 
sort(studies_only)
mean(studies_only)
sd(studies_only)

#Check if we can see if conclusive studies were helped by different kinds of antibiotics tested
studies_phen_comp <- data.frame(select(abs_direction, study_id, dir, finding), n_abs = studies_only) %>%
        mutate(n_abs_cut = case_when(
                n_abs == 0 ~ "0",
                n_abs < 5 ~ "1-5",
                n_abs < 10 ~ "6 - 10",
                n_abs < 15 ~ "11 - 15",
                n_abs < 21 ~ "16 - 21"
        )) %>%
        mutate(n_abs_cut = ifelse(abs_direction[,"All (WGS, MG, large PCR panel)"] ==1, "all", n_abs_cut)) %>%
        subset(., !(n_abs_cut == "0"))

studies_phen_comp2 <- ddply(studies_phen_comp, .(n_abs_cut), summarise, 
    n_studies_in_cat = length(n_abs_cut),
    prop_conclusive = sum(kutils::isNA(finding, na.strings = "<NA>"))/length(n_abs))

ggplot(studies_phen_comp2, aes(n_abs_cut, prop_conclusive)) + 
    geom_point() 

#Bacteria studied
#Most commonly studied bacteria
org_studied <- org_study_id %>%
    select(., -study_id) %>%
    as.matrix %>%
    apply(., 2, as.numeric) %>%
    colSums
sort(org_studied)

#Plot by direction
orgs_direction <- left_join(directions_ids_only, org_study_id, by = "study_id")
orgs_dir_g <- gather(orgs_direction, key = "bacterial_group", value = "yn", -study_id, -dir, -n_studies) %>%
    subset(., yn == 1)

bac_colours <- colours2[1:length(unique(orgs_dir_g$bacterial_group))]
names(bac_colours) <- unique(orgs_dir_g$bacterial_group)

n_bac <- ddply(orgs_dir_g, .(bacterial_group), summarise, n_studies = length(bacterial_group))
orgs_dir_g <- left_join(select(orgs_dir_g, -n_studies), n_bac, by = "bacterial_group") %>%
    mutate(., bacterial_group = factor(bacterial_group, 
                        levels = n_bac$bacterial_group[order(n_bac$n_studies, decreasing = TRUE)], 
                        ordered = TRUE),
              dir = factor(dir, levels = c("Inconclusive", "Patients to HWW", 
        "Patients to MWW", "HWW to MWW", "Hospital to MWW (indirect)"), ordered = TRUE))

png("plots/org_by_dir.png", width = 25, height = 15, units = "cm", res = 600)
ggplot(orgs_dir_g, aes(bacterial_group, fill = dir)) + 
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = c("#3B528BFF", "#21908CFF","#5DC863FF","#FDE725FF" )) +
    labs(y = "Proportion of studies", x = "", fill = "Direction of study conclusion") +
    geom_text(mapping = aes(bacterial_group, y = -0.05, label = n_studies)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom", plot.margin = margin(l = 5)) +
    guides(fill = guide_legend(title.position = "top")) 
dev.off()


#Sample types
sample_types <- select(all_data, study_id, `Sampled hospital patients?`, 
                        `Sampled hospital environment?`, `Sampled hospital sewage?`, 
                        `Sampled HCW?`) %>%
                .[-which(findings$hospital_based == "Yes"),] %>%
                gather(., key = "st", value = "yn", -study_id)
table(sample_types$st, sample_types$yn)

sample_combinations <- select(all_data, study_id, `Sampled hospital patients?`, 
                        `Sampled hospital environment?`, `Sampled hospital sewage?`, 
                        `Sampled HCW?`, direction) %>%
                        setNames(., .("study_id", "patients", "environment", "sewage", "hcw", "dir"))

table(sample_combinations$patients, sample_combinations$sewage)

#Statistics
table(all_data$statistics, tolower(all_data$finding),   useNA = "ifany")

all_study_find <- select(all_data, Core_partial, study_id, direction, finding, Screening_version)
all_study_find2 <- lapply(1:nrow(all_study_find), function(i) {
    if(all_study_find$Core_partial[i] == "partial") {
        data.frame(finding = "none", direction = "None", 
               screen_version = all_study_find$Screening_version[i], 
               study_id = all_study_find$study_id[i])
    } else {
       ds <- str_trim(unlist(str_split(all_study_find$direction[i], ";")))
        data.frame(finding = tolower(all_study_find$finding[i]), direction = ds, 
               screen_version = all_study_find$Screening_version[i], 
               study_id = all_study_find$study_id[i]) 
    }
    
}) %>% bind_rows()

stat_use <- select(all_data, study_id, statistics, stat_type, finding) %>% unique %>%
    mutate(., finding = ifelse(is.na(finding), "Inconclusive", tolower(finding)))

n_studies <- ddply(stat_use, .(finding), summarise, n_studies = length(finding))
all_stat_types <- lapply(1:nrow(stat_use), function(i) {
    sts <- str_trim(tolower(unlist(str_split(stat_use$stat_type[i], ";|,"))))
    data.frame(stat_type = sts, study_id = stat_use$study_id[i], finding = stat_use$finding[i])
}) %>% bind_rows() %>%
    left_join(.,n_studies, by = "finding")

stat_use$stat_type <- tolower(stat_use$stat_type)
sum(as.numeric(str_detect(stat_use$stat_type, "frequentist")))
sum(as.numeric(str_detect(stat_use$stat_type, "cluster")))
sum(as.numeric(str_detect(stat_use$stat_type, "phylogenetics")))
sum(as.numeric(str_detect(stat_use$stat_type, "permutation")))
sum(as.numeric(str_detect(stat_use$stat_type, "machine learning")))
sum(as.numeric(str_detect(stat_use$stat_type, "bayesian")))
sum(as.numeric(str_detect(stat_use$stat_type, "network analysis")))
sum(as.numeric(str_detect(stat_use$stat_type, "unclear")))

n_stats <- ddply(all_stat_types, .(stat_type), summarise, n_studies = length(stat_type))
all_stat_types <- left_join(select(all_stat_types, -n_studies), n_stats, by = "stat_type")

all_stat_types$stat_type <- factor(all_stat_types$stat_type, 
    levels = n_stats$stat_type[order(n_stats$n_studies, decreasing = T)],
    ordered = T)

png("plots/stat_type_by_finding.png", width = 15, height = 10, units = "cm", res = 600)
ggplot(all_stat_types, aes(stat_type, fill = finding)) +
    geom_bar(stat = "count", position = "fill") +
    viridis::scale_fill_viridis(discrete = TRUE) +
    labs(x = "", y = "Proportion of studies", fill = "Study conclusion") +
    geom_text(mapping = aes(stat_type, y = -0.05, label = n_studies)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom") +
    guides(fill = guide_legend(title.position = "top"))     
dev.off()

#Type of laboratory methods
typing <- select(all_data, study_id, highest_res_typing_group, highest_org_typing, direction) %>% 
    unique
table(typing$highest_res_typing_group)
table(typing$highest_org_typing)

study_typing <- left_join(all_study_find2, select(typing, -direction), by = "study_id") %>%
    subset(., !(direction == "None")) %>%
    mutate(., highest_org_typing = ifelse(highest_org_typing == "-", "None", highest_org_typing),
              highest_res_typing_group = ifelse(highest_res_typing_group == "Gene sequencing", "Gene sequence", highest_res_typing_group),
              direction = factor(direction, levels = c("Patients to HWW", 
        "Patients to MWW", "HWW to MWW", "Hospital to MWW (indirect)"), ordered = TRUE))

study_type_org <- select(study_typing, direction, highest_org_typing) %>%
    mutate(., typing_group = "Bacterial typing method",
              typing_method = ifelse(highest_org_typing == "Phenotypic", "Phenotypic (bacterial)", highest_org_typing)) %>%
              select(., -highest_org_typing)
study_type_res <- select(study_typing, direction, highest_res_typing_group) %>%
    mutate(., typing_group = "Resistance typing method", typing_method = highest_res_typing_group) %>%
    select(., -highest_res_typing_group)

study_type_g <- bind_rows(study_type_org, study_type_res)

n_studies_type <- ddply(study_type_g, .(typing_method), summarise, 
    n_studies = length(typing_method))

study_type_g <- left_join(study_type_g, n_studies_type, by = "typing_method") %>%
    mutate(., typing_method = factor(typing_method, 
                    levels = n_studies_type$typing_method[order(n_studies_type$n_studies, decreasing = TRUE)],
                    ordered = TRUE))

png("plots/stat_type_direction.png", width = 15, height = 20, units = "cm", res = 600)
ggplot(study_type_g, aes(typing_method, fill = direction)) +
    geom_bar(stat = "count", position = "fill") +
    scale_fill_manual(values = c("#3B528BFF", "#21908CFF","#5DC863FF","#FDE725FF" )) +
    labs(x = "", y = "Proportion of studies", fill = "Direction of study conclusion") +
    geom_text(mapping = aes(typing_method, y = -0.05, label = n_studies)) +
    facet_wrap(~typing_group, ncol = 1, scales = "free") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom", plot.margin = margin(r = 17)) +
    guides(fill = guide_legend(title.position = "top"))
dev.off()

#Antimicrobial quantification
antimicrobial_quant <- select(all_data, study_id, antimicrobialquant) %>% unique
table(antimicrobial_quant$antimicrobialquant)

#No duplicates
studies_unique <- select(all_data, finding, study_id, hospital_based, Core_partial, Screening_version, hospital_based) %>%
    unique

length(unique(c(partial_study_id, study_id)))
