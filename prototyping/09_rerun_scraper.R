rm(list=ls())
library(tidyverse)
library(R6)
library(tryCatchLog)
library(futile.logger)

scraper <- "south_dakota"
date <- "2020-12-14"

source(paste0("./production/scrapers/", scraper, ".R"))
scraper <- get(paste0(scraper, "_scraper"))$new(log = TRUE)
scraper$reset_date(date = date)
scraper$pull_raw()
scraper$restruct_raw()
scraper$restruct_data
scraper$extract_from_raw()
scraper$extract_data
read_csv(scraper$extract_dest)
scraper$validate_extract()
scraper$save_extract()
