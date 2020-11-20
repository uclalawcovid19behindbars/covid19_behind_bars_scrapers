rm(list=ls())
library(tidyverse)
library(R6)
library(tryCatchLog)
library(futile.logger)

# initiate all scrapers in the production folders
sc_files <- list.files("production/scrapers", full.names = TRUE)

sapply(sc_files, source)

ids_to_restructure <- c(
    "alaska", "colorado", "hawaii", "indiana", "maine",
    "maricopa_county", "minnesota", "nebraska", "north_dakota", "orange_county",
    "polk_county", "tennessee", "texas", "west_virginia", "wisconsin"
)

tests_unclear <- c(
    "allegheny_county", "idaho", "maryland"
)

for(i in ids_to_restructure){
    results_files <- list.files(
        "results/extracted_data", full.names = TRUE,
        pattern = str_c("\\d+-\\d+-\\d+_", i, ".csv"))
    
    for(j in results_files){
        df_ <- read_csv(j, col_types = cols())
        n_ <- names(df_)
        if("Residents.Tested" %in% n_ & !("Residents.Tadmin" %in% n_)){
            # df_ %>%
            #     rename(Residents.Tadmin = "Residents.Tested") %>%
            #     write_csv(j)
        }
        else{
            warning(paste0("File does not meet data requirements: ", j))
        }
    }
}
