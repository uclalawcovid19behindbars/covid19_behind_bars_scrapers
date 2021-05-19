rm(list=ls())
library(tidyverse)
library(lubridate)

DRY_RUN <- TRUE

wa_files <- list.files(
    "/srv/shiny-server/scraper_data/extracted_data",
    "\\d+-\\d+-\\d+_washington.csv", full.names = TRUE
    )

for(waf in wa_files){
    wa_df <- read_csv(waf, col_types = cols())
    
    change <- FALSE

    if(all(wa_df$Date == ymd("2020-12-02"))){
        print("Changing AIRWAY HEIGHTS value to 30 for date of 2020-12-02")
        wa_df$Residents.Confirmed[
            wa_df$Name == "Airway Heights Corrections Center"] <- 30
        change <- TRUE
    }

    if(all(wa_df$Date > ymd("2020-12-02"))){
        print(paste0(
            "Changing AIRWAY HEIGHTS value to NA for date of ",
            as.character(wa_df$Date[1])))
        wa_df$Residents.Confirmed[
            wa_df$Name == "Airway Heights Corrections Center"] <- NA
        change <- TRUE
    }

    if(change){
        wa_df %>%
            filter(str_starts(Name, "(?i)Airway")) %>%
            select(Name, Date, Residents.Confirmed) %>%
            print()
    }

    if(!DRY_RUN & change){
        write_csv(wa_df, waf)
    }

}
