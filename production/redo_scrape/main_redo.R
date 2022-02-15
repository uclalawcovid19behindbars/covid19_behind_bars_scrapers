#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(behindbarstools))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(R6))
suppressPackageStartupMessages(library(tryCatchLog))
suppressPackageStartupMessages(library(futile.logger))
# need to explicitly authenticate for this rsession
suppressPackageStartupMessages(library(googlesheets4))
gs4_auth("ucla.law.covid.staff@gmail.com", scopes = "https://www.googleapis.com/auth/drive")
options(tryCatchLog.include.full.call.stack = FALSE)
source("R/utilities.R")

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

valid_dates <- lubridate::ymd(list_remote_data(
    "raw_files", args$scraper, dates_only = TRUE))

while(current_date <= en_date){
    
    if(current_date %in% valid_dates){
        scraper <- get(scraper_name_vec[args$scraper])$new(log = TRUE)
        scraper$reset_date(current_date)
        print(scraper)
        cat("  Date: ", as.character(scraper$date), "\n")
        scraper$pull_raw()
        scraper$restruct_raw()
        scraper$extract_from_raw()
        scraper$validate_extract()
        scraper$save_extract()
    }
    
    current_date <- current_date + 1
}
