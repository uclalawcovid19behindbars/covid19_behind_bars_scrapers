#!/usr/bin/Rscript

library(tidyverse)

ind_vars <- c("Date", "Name", "State")
    
# Update aggregated_data.csv
list.files("/srv/shiny-server/scraper_data/extracted_data", full.names = TRUE) %>%
    lapply(function(x){
        df_ <- read_csv(x, col_types = cols())
        if("Date" %in% names(df_)){
            df_ <- df_ %>%
                mutate(Date = lubridate::as_date(Date)) %>%
		mutate_at(vars(starts_with("Residents")), as.numeric) %>%
		mutate_at(vars(starts_with("Staff")), as.numeric)
        }
        df_
    }) %>%
    bind_rows() %>%
    select(-Resident.Deaths) %>%
    # remove values if they are missing a data name or state
    filter(!is.na(Date) & !is.na(Name) & State != "") %>%
    # order the names alphabetically
    select(!!sort(names(.))) %>%
    # put the indicator variables first
    select(!!ind_vars, !!(names(.)[!(names(.) %in% ind_vars)])) %>%
    filter(!is.na(id), !is.na(jurisdiction)) %>%
    write_csv("/srv/shiny-server/scraper_data/summary_data/aggregated_data.csv")

# Updated aggregated_data_clean.csv
behindbarstools::read_scrape_data(all_dates = TRUE, coalesce = TRUE) %>% 
    write_csv("/srv/shiny-server/scraper_data/summary_data/scraped_time_series.csv")
