rm(list=ls())
library(lubridate)
library(tidyverse)
library(R6)
library(tryCatchLog)
library(futile.logger)
# need to explicitly authenticate for this rsession
options(tryCatchLog.include.full.call.stack = FALSE)

# initiate all scrapers in the production folders
sapply(list.files("production/scrapers", full.names = TRUE), source)

# read in the config file of all the wayback to 
json_config <- "./production/historical_rescrape/wayback.json" %>%
    jsonlite::read_json() %>%
    lapply(as_tibble) %>%
    bind_rows()

# run through exh scraper described in the config
for(i in 1:nrow(json_config)){
    # only run if we get the run flag
    if(!json_config$run[[i]]){
        next
    }

    # grab the run parameters
    STARTDATE <- ymd(json_config$start_date[i])
    ENDDATE <- ymd(json_config$end_date[i])
    scraper_name <- json_config$scraper[i]

    # check to make sure dates are valid
    if(ENDDATE < STARTDATE){
        stop("End date must be later than start date.")
    }

    d <- STARTDATE

    cat("Starting wayback run for", scraper_name, "\n")

    while(d <= ENDDATE){
        # sunday is 1 and we only want SUN, MON, WED, FRI
        if(wday(d) %in% c(1, 2, 4, 6)){
            cat("On date", as.character(d), "\n")
            
            # run step by step but dont write to perma.cc
            scraper <- get(scraper_name)$new(log = TRUE)
            # reset the date to date of scrape
            scraper$reset_date(date = d)
            # pull from waybackmachine
            scraper$pull_wayback_raw()
            # rest of the process is normal
            scraper$save_raw()
            scraper$restruct_raw()
            scraper$extract_from_raw()
            scraper$validate_extract()
            scraper$save_extract()
            
            Sys.sleep(30)
        }
        
        # increment to the next day
        d <- d + 1
    }
    
}
