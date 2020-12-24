# singularity run --app Rscript init/singularity-r.simg production/main.R
rm(list=ls())
library(tidyverse)
library(R6)
library(tryCatchLog)
library(futile.logger)
options(tryCatchLog.include.full.call.stack = FALSE)

# initiate all scrapers in the production folders
sapply(list.files("production/scrapers", full.names = TRUE), source)

# grab only the scrapers and put their names in a vector
scraper_name_vec <- get_scraper_vec()

scraper_list <- lapply(scraper_name_vec, function(sn){
    # initialize an instance of the scraper
    scraper <- get(sn)$new(log = TRUE)
    print(scraper)
    # run everything
    scraper$run_all()
    # return the scraper
    scraper
})
