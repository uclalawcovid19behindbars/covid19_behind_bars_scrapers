# Script to fix some of the issues with the past scrapes that have been fixed 
# in updated scrapers but the old scrape data needs to be adjusted. These should
# be coded in such a way that if ever this scrip is run the new saved data should
# be what we are looking for. 
rm(list=ls())
library(tidyverse)

EXTRACT_SOURCE <- "/srv/shiny-server/scraper_data/extracted_data"
DRY_RUN <- TRUE

get_files <- function(scraper_name){
    src_str <- str_c("\\d+-\\d+-\\d+_", scraper_name, ".csv")
    list.files(EXTRACT_SOURCE, pattern = src_str, full.names = TRUE)
}

# Fix the bonehead error of placing Milwaukee in MN instead of WI
mil_files <- get_files("milwaukee_county")

for(m in mil_files){
    df_ <- read_csv(m, col_types = cols())
    if(any(df_$State == "MN")){
        cat(
        "This file had the state variable wrong and will be updated: ", m, "\n")
        df_$State <- "WI"
        if(!DRY_RUN){
            write_csv(df_, m)
        }
    }
}

# Make sure TX jails all have jails at the end of their name
# Fix the bonehead error of placing Milwaukee in MN instead of WI
txj_files <- get_files("texas_jails")

for(tj in txj_files){
    df_ <- read_csv(tj, col_types = cols())
    if(all(!str_ends(df_$Name, " Jail"))){
        cat("This file had the name wrong and will be updated:", tj, "\n")
        df_$Name <- str_squish(str_c(df_$Name, " Jail"))
        if(!DRY_RUN){
            write_csv(df_, tj)
        }
    }
    else if(any(!str_ends(df_$Name, " Jail"))){
        warning(
            "The following file has unorthdox names and should be inspected: ",
            txj)
    }
}

# change state in ice facilities from Federal to federal
ice_files <- get_files("ice")
for(ice in ice_files){
    df_ <- read_csv(ice, col_types = cols())
    if(all((df_$State == "Federal"))){
        cat("This file had the jurisdiction wrong and will be updated:", ice, "\n")
        df_$State <- "federal"
        if(!DRY_RUN){
            write_csv(df_, ice)
        }
    }
    else if(any(df_$State == "Federal")){
        warning(
            "The following file has unorthdox names and should be inspected: ",
            ice)
    }
}

# change DC jurisdiction from state to county
dc_files <- get_files("dc")
for(dcf in dc_files){
    df_dc <- read_csv(dcf, col_types = cols())
    if(all((df_dc$jurisdiction == "state")) & all((df_dc$Name == "State-Wide"))){
        cat("This file had the jurisdiction wrong and will be updated:", dcf, "\n")
        df_dc$jurisdiction <- "county"
        df_dc$Name <- "COUNTY WIDE"
        if(!DRY_RUN){
            write_csv(df_dc, dcf)
        }
    }
}
