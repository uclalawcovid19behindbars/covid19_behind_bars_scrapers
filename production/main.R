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
obj_space <- sapply(sapply(ls(), get), class)
scraper_space <- sapply(obj_space, function(x) "R6ClassGenerator" %in% x)
scraper_name_vec <- names(scraper_space[scraper_space]) %>%
    .[. != "generic_scraper"]
names(scraper_name_vec) <- str_remove(scraper_name_vec, "_scraper")

scraper_list <- lapply(scraper_name_vec, function(sn){
    # initialize an instance of the scraper
    scraper <- get(sn)$new(log = TRUE)
    print(scraper)
    # run everything
    scraper$run_all()
    # return the scraper
    scraper
})
