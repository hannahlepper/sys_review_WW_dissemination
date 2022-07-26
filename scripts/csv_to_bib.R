#read in CSV
library(tidyverse)
entries <- read_csv("data/SavedRecords-FullRecord.csv")

#Get rid of the xlmns crap

entries$Title <- gsub(" xmlns=\"http://www.w3.org/1999/xhtml\"", "", entries$Title)
entries[,"Abstract Text"] <- gsub(" xmlns=\"http://www.w3.org/1999/xhtml\"", "", entries[,"Abstract Text"])

select(entries, Title, `Item Types`, Authors, Volume, `Page Numbers`, `Document Title`)

get_cat <- function(cat_string) {
    out <- str_match(cat_string, "article|Book|Thesis")
    if (is.na(out)) {
        out <- "misc"
    }
    return(out)
}

entries_2 <- data.frame(TITLE = entries$Title, 
    JOURNAL = entries$`Document title`,
    AUTHOR = entries$Authors,
    BIBTEXKEY = entries$PAN,
    YEAR = entries$`Year of Publication`,
    VOLUME = entries$Volume,
    PAGES = entries$`Page Numbers`, 
    CATEGORY = toupper(sapply(entries$`Item Types`, get_cat)),
    NUMBER = entries$`Bulletin / Issue / Part`,
    ABSTRACT = entries$`Abstract Text`)

#Convert df to a bib tex file and save
library(bib2df)
df2bib(entries_2, file = "data/cab_direct_bibs.bib")

#Convert T and A selected bibs to CSV
library(bibtex)
T_A_bibs <- read.bib("data/My Collection.bib")
