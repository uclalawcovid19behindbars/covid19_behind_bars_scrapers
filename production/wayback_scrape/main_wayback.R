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
# need to explicitly authenticate for this rsession
options(tryCatchLog.include.full.call.stack = FALSE)

scraper_name_vec <- get_scraper_vec()

parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument(
    "-sc", "--scraper",
    help="The name of the scraper to rerun")
parser$add_argument(
    "-st", "--start",
    help="The date to start the redo scraper run should be in YYYY-MM-DD format")
parser$add_argument(
    "-en", "--end",
    help="The date to stop the redo scraper run should be in YYYY-MM-DD format")

args <- parser$parse_args()

st_date <- lubridate::ymd(args$start)
en_date <- lubridate::ymd(args$end)

if(!(args$scraper %in% names(scraper_name_vec))){
    stop("scraper name provided is not a valid option")
}

source(str_c("production/scrapers/", args$scraper, ".R"))

if(is.na(st_date)){
    stop("Start date format is invalid")
}

if(is.na(en_date)){
    stop("End date format is invaliud")
}

if(st_date > en_date){
    stop("Start date can not be later than end date.")
}

current_date <- st_date


# run through the scraper

cat("Starting wayback run for", args$scraper, "\n")

while(current_date <= en_date){
    # sunday is 1 and we only want SUN, MON, WED, FRI
    # if(lubridate::wday(current_date) %in% c(1, 2, 4, 6)){
        
    cat("On date", as.character(current_date), "\n")
    
    # run step by step but dont write to perma.cc
    scraper <- get(scraper_name_vec[args$scraper])$new(log = TRUE)
    # reset the date to date of scrape
    scraper$reset_date(date = current_date)
    # pull from waybackmachine
    scraper$pull_wayback_raw()
    # rest of the process is normal
    scraper$save_raw()
    scraper$restruct_raw()
    scraper$extract_from_raw()
    # scraper$validate_extract()
    scraper$save_extract()
    
    Sys.sleep(30)
    # }
    
    # increment to the next day
    current_date <- current_date + 1
}
