#!/usr/bin/Rscript
remotes::install_github(
    "uclalawcovid19behindbars/behindbarstools", upgrade = "never")
library(tidyverse)
library(data.table)
library(behindbarstools)

ind_vars <- c("Date", "Name", "State")

# Update aggregated_data.csv
list.files("/srv/shiny-server/scraper_data/extracted_data", full.names = T) %>%
    lapply(function(x){
        df_ <- fread(x)
        if(nrow(df_) == 0){
            df_ <- data.table()
        }
        if("Date" %in% names(df_)){
            df_[,Date := lubridate::as_date(Date)]
        }
        df_
    }) %>%
    rbindlist(fill=TRUE, use.names = TRUE) %>%
    select(-Resident.Deaths) %>%
    # remove values if they are missing a data name or state
    filter(!is.na(Date) & !is.na(Name) & State != "") %>%
    # order the names alphabetically
    select(!!sort(names(.))) %>%
    # put the indicator variables first
    select(!!ind_vars, !!(names(.)[!(names(.) %in% ind_vars)])) %>%
    filter(!is.na(id), !is.na(jurisdiction)) %>%
    write_csv("/srv/shiny-server/scraper_data/summary_data/aggregated_data.csv")

# Update time series scraped data 
read_scrape_data(all_dates = TRUE) %>% 
    write_csv(
        "/srv/shiny-server/scraper_data/summary_data/scraped_time_series.csv")
