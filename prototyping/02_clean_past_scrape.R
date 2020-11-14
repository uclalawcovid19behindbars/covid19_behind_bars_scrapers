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

for(i in names(scraper_name_vec)){
    results_files <- list.files(
        "results/extracted_data", full.names = TRUE,
        pattern = str_c("\\d+-\\d+-\\d+_", i, ".csv"))
    
    scraper <- get(scraper_name_vec[i])$new(log = FALSE)
    
    for(j in results_files){
        print(j)
        df_ <- read_csv(j, col_types = cols()) %>%
            mutate(id = scraper$id) %>%
            mutate(source = scraper$url) %>%
            mutate(jurisdiction = scraper$jurisdiction)
        
        write_csv(df_, j)
    }
}
