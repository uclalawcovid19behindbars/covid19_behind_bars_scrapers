library(tidyverse)
library(R6)
library(tryCatchLog)
library(futile.logger)

generic_scraper <- R6Class(
    "generic_scraper",
    list(
        # URL where the data lives can be a url that holds an iframe
        # but the pull function will use this as the first argument
        url=NULL,
        # how we are referencing this scraper, usually a state name
        id=NULL,
        # one of either html, img, json, or pdf. Whatever the raw data
        # format is
        type = NULL,
        log = NULL,
        # how to pull the data using the URL provided above
        pull_func = NULL,
        # restructure the data into a data frame like object
        restruct_func = NULL,
        # extract the data to have the appropriate names
        extract_func = NULL,
        # the date of the run (usually auto populated)
        date = Sys.Date(),
        # no need to populate the remaining cells, they will be populated later
        raw_data = NULL,
        restruct_data = NULL,
        extract_data = NULL,
        state = NULL,
        err_log = NULL,
        raw_dest = NULL,
        extract_dest = NULL,
        initialize = function(
            url, id, pull_func, type, restruct_func, extract_func, log, state){

            valid_types <- c(
                html = ".html", img = ".png", json = ".json", pdf = ".pdf"
            )

            stopifnot(is.character(url), length(url) == 1)
            stopifnot(is.character(id), length(id) == 1)
            stopifnot((type %in% names(valid_types)))
            
            self$log = log
            self$type = type
            self$url = url
            self$id = id
            self$state = state
            self$raw_data = NULL
            self$restruct_data = NULL
            self$extract_data = NULL
            self$pull_func = pull_func
            self$restruct_func = restruct_func
            self$extract_func = extract_func
            self$err_log = paste0(
                "./results/log_files/", self$date, "_", id, ".log")

            if(file.exists(self$err_log)){
                file.remove(self$err_log)
            }

            self$raw_dest = paste0(
                "./results/raw_files/", self$date, "_", id, valid_types[type])
            self$extract_dest = paste0(
                "./results/extracted_data/", self$date, "_", id, ".csv")
            
            
            
            # initiate logger
            flog.appender(appender.file(self$err_log))
            flog.threshold(WARN)
        },

        pull_raw = function(url = self$url, ...){
            if(self$log){
                tryLog(self$raw_data <- self$pull_func(url, ...))
            }
            else{
                self$raw_data <- self$pull_func(url, ...)
            }
            invisible(self)
        },

        save_raw = function(dest=self$raw_dest){
            valid_types <- list(
                html = xml2::write_html, img = magick::image_write, 
                json = jsonlite::write_json, pdf = utils::download.file
            )
            
            if(self$log){
                tryLog(valid_types[[self$type]](self$raw_data, dest))
            }
            else{
                valid_types[[self$type]](self$raw_data, dest)
            }
            invisible(self)
        },

        restruct_raw = function(raw = self$raw_data, ...){
            if(self$log){
                tryLog(self$restruct_data <- self$restruct_func(raw, ...))
            }
            else{
                self$restruct_data <- self$restruct_func(raw, ...)
            }
            invisible(self)
        },
        
        extract_from_raw = function(raw = self$restruct_data, ...){
            if(self$log){
                tryLog(self$extract_data <- self$extract_func(raw, ...))
            }
            else{
                self$extract_data <- self$extract_func(raw, ...)
            }
            invisible(self)
        },
        
        save_extract = function(extract = self$extract_data){
            if(self$log){
                tryLog(write_csv(self$extract_data, self$extract_dest))
            }
            else{
                write_csv(self$extract_data, self$extract_dest)
            }
            invisible(self)
        },
        
        last_update = function(){
            list.files("./results/extracted_data") %>%
                {.[str_ends(., str_c(self$id, ".csv"))]} %>%
                str_extract("\\d+-\\d+-\\d+") %>%
                lubridate::as_date() %>%
                max()
        },
        
        
        validate_extract = function(){
            
        },
        
        run_all = function(){
            self$pull_raw()
            self$save_raw()
            self$restruct_raw()
            self$extract_from_raw()
            self$save_extract()
        }
    )
)
