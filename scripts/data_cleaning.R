#get data in from Sheets
library(googlesheets4)
library(plyr)
library(tidyverse)

all_data <- read_sheet("https://docs.google.com/spreadsheets/d/1RO2Zkydw2MyuFRNoLIWFVXD3iQH5U4lLP99v4I_ouXk/edit#gid=0")
ab_groups <- read_csv("data/ATC_tables.csv") %>%
    subset(., !(Level_5 == "Antibiotics")) #website scraped in a previous project

all_data <- subset(all_data, !(is.na(title)))

#study id
study_id <- paste0(word(all_data$author, 1), "_", 
                   word(all_data$title, 1), "_", 
                   word(all_data$title, -1), "_", 
                   all_data$year) %>%
    gsub(",|-| |[.]|?", "", .)
all_data$study_id <- study_id

all_data <- subset(all_data, !(is.na(study_id)))

#antibiotics simplified to ATC level 4
#Some antibiotics that were often used but need a special case

split_abs <- function(ab_string) {
    unlist(str_split(ab_string, " |\n|;|,|-|\\/")) %>%
        tolower(.)
}

special_abs <- function(ab_vec) {
    ab_vec <- tolower(ab_vec)

    ab_vec <- ab_vec %>%
        str_replace(., "clavulanate|clavulanic|sulbactam|tazobactam|monosulfate|acid|sodium|salt|hydrate|and", "")

    ab_vec <- case_when(
        ab_vec == "imipenem" ~ "ertapenem",
        ab_vec == "fusidic" ~ "fusidic acid",
        ab_vec == "nalidixic" ~ "nalidixic acid",
        ab_vec == "oxolinic" ~ "oxolinic acid",
        ab_vec == "tetracyclin" ~ "tetracycline",
        ab_vec == "polymixin" ~ "polymixin B",
        ab_vec %in% c("sulfamethoxazol", "sulphamethoxazole") ~ "sulfamethoxazole",
        ab_vec %in% c("co-amoxiclav", "coamoxiclav") ~ "amoxicillin",
        TRUE ~ ab_vec 
    )

    return(ab_vec)
}

clean_ab_vec <- function(ab_vec) {
    tolower(ab_vec) %>%
        str_replace("[[:punct:]]", "") %>%
        str_replace("[0-9]", "") %>%
        unique() %>%
        .[!(nchar(.)==0)]
}

match_to_cat <- function(ab_vec) {
    atc_grouped <- unlist(sapply(ab_vec, function(a) which(str_detect(a, ab_groups$AB_name)))) %>%
        ab_groups$Level_5[.] %>% unique
    
    class_groups <- ab_vec[!(ab_vec %in% ab_groups$AB_name)] %>%
        sapply(., function(a) which(str_detect(a, ab_groups$Level_4))) %>%
        unlist()

    other_groups <- case_when(
        ab_vec == "esbl" ~ "ESBL", 

        ab_vec %in% c("macrolide", "macrolides", "tylosine", "tylosin") ~ "Macrolides",

        ab_vec %in% c("sulfonamide", "sulfonamides", "sulfamerazin", "sulfisoxazole") ~ "Sulfonamides",

        ab_vec %in% c("beta", "lactams", "lactamase", "lactamases") ~ "Beta lactams",

        ab_vec %in% c("carbapenems", "carbapenem") ~ "Carbapenems",

        ab_vec %in% c("penicillin", "penicillins") ~ "Penicillins",

        ab_vec %in% c("fluoroquinolone", "fluoroquinolones") ~ "Quinolones",

        ab_vec %in% c("quinolone", "quinolones") ~ "Quinolones",

        ab_vec %in% c("aminoglycoside", "aminoglycosides") ~ "Aminoglycosides",

        ab_vec %in% c("lincosamide", "lincosamides") ~ "Lincosamides",

        ab_vec %in% c("streptogramin", "streptogramins", "dalfopristin", "quinupristin", "virginiamycin") ~ "Streptogramins",

        ab_vec %in% c("rifampicin", "rifampin") ~ "TB antibiotics",

        ab_vec %in% c("all") ~ "All (WGS, MG, large PCR panel)",

        ab_vec %in% c("secondgencephalosporins", "cefuroxime") ~ "Second-Generation Cephalosporins",

        ab_vec %in% c("thirdgencephalosporins", "cefpodoxime", "ceftiofur")  ~ "Third-Generation Cephalosporins",

        ab_vec %in% c("furazolidone") ~ "Nitrofuran derivatives",

        ab_vec %in% c("polymixin", "polymyxin") ~ "Polymixin",

        ab_vec %in% c("ceftaroline") ~ "Other cephalosporins and penems",

        ab_vec %in% c("oleomycin", "avilamycin", "sulfazoritrim", "monensin") ~ "No ATC group found",

       TRUE ~ ""
    )
    
    unmatched <- which(sapply(1:length(ab_vec), function(a) {
        all(!(str_detect(ab_vec[a], ab_groups$AB_name)) 
                    & !(str_detect(ab_vec[a], ab_groups$Level_4)) 
                    & other_groups[a] == "")
    }))
    unmatched2 <- paste0(ab_vec[unmatched], "_unmatched")

    return(unique(c(atc_grouped, other_groups, unmatched2)))
}

abs_ls <- lapply(1:nrow(all_data), function(r) {
    ab_oi <- all_data$phenorestype[r]
    ab_vec <- split_abs(ab_oi)
    ab_vec2 <- special_abs(ab_vec) %>%
        clean_ab_vec
    match_to_cat(ab_vec2)
})

all_abs_types <- unlist(abs_ls) %>%
    unique() %>%
    .[!(str_detect(., "_unmatched"))] %>%
    .[!(nchar(.)==0)]

abs_df <- lapply(1:length(abs_ls), function(elem) {
        out <- c(study_id[elem],
            as.numeric(unlist(sapply(all_abs_types, function(i) i %in% abs_ls[[elem]]))))
        data.frame(t(out))
    }) %>%
        bind_rows() %>%
        setNames(., c("study_id", all_abs_types))

abs_direction <- bind_cols(abs_df, dir = all_data$direction, finding = all_data$finding)

#Organism studied
org_only <- subset(all_data, str_detect(finding, "ositive|egative")) %>%
    select(., study_id, org) %>% unique

all_orgs <- sapply(org_only$org, function(o) {
    str_split(o, ";|,| and ") %>%
        unlist %>%
        stringi::stri_remove_empty(.) %>%
        trimws
}) %>% unlist %>% unique

org_study_id <- lapply(1:nrow(org_only), function(i) {
    orgs <- str_split(org_only[i,"org"], ";|,| and ") %>%
        unlist %>%
        stringi::stri_remove_empty(.) %>%
        trimws
    out <- c(org_only$study_id[i],
            as.numeric(unlist(sapply(all_orgs, function(n) n %in% orgs))))
        data.frame(t(out))
}) %>% bind_rows %>% setNames(., c("study_id", all_orgs))
