#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(behindbarstools))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(R6))
suppressPackageStartupMessages(library(tryCatchLog))
suppressPackageStartupMessages(library(futile.logger))
# need to explicitly authenticate for this rsession
suppressPackageStartupMessages(library(googlesheets4))
gs4_auth("ucla.law.covid.staff@gmail.com")
options(tryCatchLog.include.full.call.stack = FALSE)
source("R/utilities.R")

parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument(
    "-c", "--config",
    help=paste0(
        "A path to a csv file with three columns. ",
        "Scraper: a character column of a valid historical scraper. ",
        "Date: YYYY-MM-DD date of which day to run the hsitorical scraper.",
        "File: a file that is associated with a date, may be NA if not required."
        ))

args <- parser$parse_args()

# read in configuration file to see what historical scrapers to run
config_df <- read_csv(
    args$config, col_types = c(Scraper = "c", Date = "D", File = "c"))
if(any(!(c("Date", "Scraper", "File") %in% names(config_df)))){
    stop("The config file must include the column names Date, Scraper, and File")
}

# load all the historical scrapers
sapply(list.files(
    "production/historical_scrape/historical_scrapers/", full.names = TRUE), 
    source)

# loop through the historical scraper config df and run the scraper
for(i in 1:nrow(config_df)){
    # get the date from the config
    date_ <- config_df$Date[i]
    # get the scraper to run from the config
    sn <- config_df$Scraper[i]
    # initialize
    scraper <- get(sn)$new(log = TRUE)
    # set the historic date and print details
    scraper$reset_date(date_)
    print(scraper)
    cat("  Date: ", as.character(scraper$date), "\n")
    # pull data from an external file
    scraper$pull_raw(
        date = scraper$date, file = config_df$File[i], .dated_pull = TRUE)
    # save the historical file
    scraper$save_raw()
    scraper$restruct_raw(date = scraper$date)
    scraper$extract_from_raw(date = scraper$date)
    scraper$validate_extract()
    scraper$save_extract()
}

# push the historical file to the remote server after successful run
hist_config_update(config_df)