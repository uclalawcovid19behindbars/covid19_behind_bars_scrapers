rm(list = ls())
library(tidyverse)

# read in configuration file to see what historical scrapers to run
config_df <- read_csv(
    "./production/historical_scrape/historical_config.csv", col_types = cols())

# load all the historical scrapers
sapply(list.files(
    "production/historical_scrape/historical_scrapers/", full.names = TRUE), 
    source)

# loop through the historical scraper config df and run the scraper
for(i in 1:nrow(config_df)){
    # get the date from the config
    date_ <- config_df$Date[i]
    # get the scraper to run from the config
    sn <- config_df$scraper[i]
    # initialize
    scraper <- get(sn)$new(log = TRUE)
    # set the historic date and print details
    scraper$reset_date(date_)
    print(scraper)
    cat("  Date: ", as.character(scraper$date), "\n")
    # pull data from an external file
    scraper$pull_raw(date = scraper$date, .dated_pull = TRUE)
    # do the rest of the process normally
    scraper$restruct_raw(date = scraper$date)
    scraper$extract_from_raw(date = scraper$date)
    scraper$validate_extract()
    scraper$save_extract()
}

