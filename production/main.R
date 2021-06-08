#!/usr/bin/env Rscript
rm(list=ls())
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(R6))
suppressPackageStartupMessages(library(tryCatchLog))
suppressPackageStartupMessages(library(futile.logger))
suppressPackageStartupMessages(library(googlesheets4))
gs4_auth("ucla.law.covid.staff@gmail.com")
options(tryCatchLog.include.full.call.stack = FALSE)

parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument(
    "-r", "--raw_only", action="store_true", default=FALSE,
    help="only pull raw files")

args <- parser$parse_args()

# initiate all scrapers in the production folders
sapply(list.files("production/scrapers", full.names = TRUE, pattern = ".R"), source)

# grab only the scrapers and put their names in a vector
scraper_name_vec <- get_scraper_vec()

if(!args$raw_only){
    scraper_list <- lapply(scraper_name_vec, function(sn){
        # initialize an instance of the scraper
        scraper <- get(sn)$new(log = TRUE)
        print(scraper)
        # run everything
        scraper$run_all()
        # return the scraper
        scraper
    })
}else{
    scraper_list <- lapply(scraper_name_vec, function(sn){
        # initialize an instance of the scraper
        scraper <- get(sn)$new(log = TRUE)
        print(scraper)
        # run everything
        scraper$pull_raw()
        scraper$save_raw()
        # return the scraper
        scraper
    })
}