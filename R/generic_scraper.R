library(tidyverse)
library(R6)
library(tryCatchLog)
library(behindbarstools)
library(futile.logger)

UCLABB_MAIN_VARIABLES <- c(
    "Name", "Staff.Confirmed", "Residents.Confirmed",
    "Staff.Deaths", "Residents.Deaths",
    "Staff.Recovered", "Residents.Recovered",
    "Staff.Tested", "Residents.Tested", "Residents.Tadmin",
    "Staff.Negative", "Residents.Negative",
    "Staff.Pending", "Residents.Pending",
    "Staff.Quarantine", "Residents.Quarantine",
    "Residents.Population", "Residents.Active"
)

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
        permalink = NULL,
        jurisdiction = NULL,
        initialize = function(
            url, id, pull_func, type, restruct_func, extract_func, log,
            state, jurisdiction){

            valid_types <- c(
                html = ".html", img = ".png", json = ".json", pdf = ".pdf",
                csv = ".csv", manual = ".csv")

            stopifnot(is.character(url), length(url) == 1)
            stopifnot(is.character(id), length(id) == 1)
            stopifnot((type %in% names(valid_types)))
            stopifnot(jurisdiction %in% c("state", "county", "federal", "immigration"))
            
            self$log = log
            self$type = type
            self$url = url
            self$id = id
            self$state = state
            self$raw_data = NULL
            self$restruct_data = NULL
            self$extract_data = NULL
            self$permalink = NULL
            self$pull_func = pull_func
            self$restruct_func = restruct_func
            self$extract_func = extract_func
            self$jurisdiction = jurisdiction
            self$err_log = paste0(
                "./results/log_files/", self$date, "_", id, ".log")

            if(file.exists(self$err_log)){
                file.remove(self$err_log)
            }

            self$raw_dest = paste0(
                "./results/raw_files/", self$date, "_", id, valid_types[type])
            self$extract_dest = paste0(
                "./results/extracted_data/", self$date, "_", id, ".csv")
            
            if(!dir.exists("./results/")){
                dir.create("./results/")
            }
            
            if(!dir.exists("./results/extracted_data/")){
                dir.create("./results/extracted_data/")
            }
            
            if(!dir.exists("./results/raw_files/")){
                dir.create("./results/raw_files/")
            }
            
            if(!dir.exists("./results/log_files")){
                dir.create("./results/log_files")
            }
            
            # initiate logger
            flog.appender(appender.file(self$err_log))
            flog.threshold(WARN)
        },
        
        reset_date = function(date){
            
            valid_types <- c(
                html = ".html", img = ".png", json = ".json", pdf = ".pdf",
                csv = ".csv", manual = ".csv"
            )
            
            self$date <- as.Date(date)
            
            if(file.exists(self$err_log)){
                file.remove(self$err_log)
            }
            
            self$err_log = paste0(
                "./results/log_files/", self$date, "_", self$id, ".log")
            
            self$raw_dest = paste0(
                "./results/raw_files/", self$date, "_", self$id,
                valid_types[self$type])

            self$extract_dest = paste0(
                "./results/extracted_data/", self$date, "_", self$id, ".csv")
            
            # initiate logger
            flog.appender(appender.file(self$err_log))
            flog.threshold(WARN)

            invisible(self)
        },

        pull_raw = function(url = self$url, ...){date

            valid_types <- list(
                html = xml2::read_html, img = magick::image_read, 
                json = jsonlite::read_json, pdf = function(x) x, 
                csv = read_csv, manual = read_csv
            )

            if(self$log){
                if(self$date != Sys.Date()){
                    tryLog(self$raw_data <- valid_types[[self$type]](self$raw_dest))
                }
                else{
                    tryLog(self$raw_data <- self$pull_func(url, ...))
                }
            }

            else{
                if(self$date != Sys.Date()){
                    self$raw_data <- valid_types[[self$type]](self$raw_dest)
                }
                else{
                    self$raw_data <- self$pull_func(url, ...)
                }
            }
            invisible(self)
        },
        
        pull_wayback_raw = function(url = self$url, ...){
            archives <- wayback_archives(url)
            
            wb_url <- archives %>%
                filter(timestamp <= self$date) %>%
                filter(timestamp == max(timestamp)) %>%
                pull(url)
            
            if(self$log){
                tryLog(self$raw_data <- self$pull_func(wb_url, ...))
            }
            
            else{
                self$raw_data <- self$pull_func(wb_url, ...)
            }
            invisible(self)

        },

        save_raw = function(dest=self$raw_dest){
            
            pdf_save <- function(x, y){
                if(file.exists(x)){
                    file.copy(x, y)
                }
                else{
                    utils::download.file(x, y)
                }
            }
            
            
            valid_types <- list(
                html = xml2::write_html, img = magick::image_write, 
                json = jsonlite::write_json, pdf = pdf_save, csv = write_csv,
                manual = write_csv
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
        
        save_extract = function(){
            if(self$log){
                tryLog(self$extract_data %>%
                           mutate(State = self$state, Date = self$date) %>%
                           mutate(id = self$id, source = self$url) %>%
                           mutate(jurisdiction = self$jurisdiction) %>%
                           write_csv(self$extract_dest))
            }
            else{
                self$extract_data %>%
                    mutate(State = self$state, Date = self$date) %>%
                    mutate(id = self$id, source = self$url) %>%
                    mutate(jurisdiction = self$jurisdiction) %>%
                    write_csv(self$extract_dest)
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
        
        get_previous_run_dates = function(){
            valid_types <- c(
                html = ".html", img = ".png", json = ".json", pdf = ".pdf",
                csv = ".csv", manual = ".csv")
            
            m_str = paste0("\\d+-\\d+-\\d+_", self$id, valid_types[self$type])
            
            list.files("./results/raw_files", pattern = m_str, full.names = T)
            
        },
        
        perma_save = function(tries = 3){
            if(self$log){
                # sometimes this wont work on the first try so give it a
                # couple goes
                attempts <- 0
                while(attempts < tries){
                    tryLog(pcc <- save_perma_cc(
                        arc_url = self$url, scraper_id = self$id,
                        state_ = self$state, date_ = self$date, api = NULL))
                    if(is.null(pcc)){
                        attempts <- attempts + 1
                    }
                    else{
                        attempts <- Inf
                    }
                }
            }
            else{
                pcc <- save_perma_cc(
                    arc_url = self$url, scraper_id = self$id,
                    state_ = self$state, date_ = self$date, api = NULL)
            }
            
            self$permalink <- str_c("perma.cc/", pcc)
            
            invisible(self)
        },
        
        validate_process = function(){
            valid_columns <- UCLABB_MAIN_VARIABLES
            
            for(i in names(self$extract_data)){
                if(!(i %in% valid_columns)){
                    warning(str_c(i, " not a valid column name. Being removed."))
                    self$extract_data <- self$extract_data %>%
                        select(-!!i)
                }
                
                else{
                    if(i != "Name" & !is.numeric(self$extract_data[[i]])){
                        warning(str_c(i, " is not numeric must convert."))
                        self$extract_data[[i]] <- as.numeric(
                            self$extract_data[[i]])
                    }
                    
                    if(any(self$extract_data[,i] < 0, na.rm = TRUE)){
                        warning(str_c(i, " has negative values. Being removed."))
                        self$extract_data[,i] <- ifelse(
                            self$extract_data[,i] < 0, NA, self$extract_data[,i])
                    }
                }
            }
            
            ext_names <- names(self$extract_data)
            
            if(!("Name" %in% ext_names)){
                stop(str_c(
                    "No 'Name' column present. Did you forget to add ",
                    "the indicator 'State-Wide'?"))
            }
            
            less_check <- function(gr, ls){
                if(all(c(gr, ls) %in% ext_names)){
                    gr_vec <- self$extract_data[[gr]]
                    ls_vec <- self$extract_data[[ls]]
                    comp_vec <- gr_vec < ls_vec
                    comp_vec[is.na(comp_vec)] <- FALSE
                    if(any(comp_vec)){
                        bad_names <- self$extract_data$Name[comp_vec]
                        warning(str_c(
                            "The following facilities had ", gr,
                            " values that were less than ", ls, " values: ",
                            str_c(bad_names, collapse = ", ")
                        ))
                    }
                }
            }

            ### sanity checks no changes made only warnings thrown
            less_check("Staff.Confirmed", "Staff.Recovered")
            less_check("Residents.Confirmed", "Residents.Recovered")
            less_check("Staff.Confirmed", "Staff.Deaths")
            less_check("Residents.Confirmed", "Residents.Deaths")
            less_check("Staff.Tested", "Staff.Confirmed")
            less_check("Residents.Tested", "Residents.Confirmed")
            less_check("Staff.Tested", "Staff.Negative")
            less_check("Residents.Tested", "Residents.Negative")
        },
        
        validate_extract = function(){
            if(self$log){
                tryLog(self$validate_process())
            }
            else{
                self$validate_process()
            }
            invisible(self)
        },
        
        print = function(...) {
            cat("  ID: ", self$id, "\n", sep = "")
            cat("  State: ", self$state, "\n", sep = "")
            cat("  Jurisdiction:  ", self$jurisdiction, "\n", sep = "")
            cat("  URL:  ", self$url, "\n", sep = "")
            cat("  Type:  ", self$type, "\n", sep = "")
            invisible(self)
        },
        
        manual_change = function(column, facility_name, new_value){
            if(is.null(self$extract_data)){
                stop("Must have extracted data already")
            }

            if(!(column %in% names(self$extract_data))){
                stop("column supplied is not a valid coulmn")
            }

            if(!(facility_name %in% self$extract_data$Name)){
                stop("facilty name supplied is not present in data")
            }

            old_value <- self$extract_data[
                self$extract_data$Name == facility_name, column]

            if(is.na(old_value)){
                old_value <- "NA"
            }

            self$extract_data[
                self$extract_data$Name == facility_name, column] <- new_value

            out_mes <- paste0(
                "The value of ", column, " for facilty ", facility_name,
                " was manually changed from ", old_value, " to ",
                new_value, "."
            )

            # always log this part, sorry not sorry
            tryLog(warning(out_mes))

            invisible(self)
        },

        run_all = function(){
            self$perma_save()
            self$pull_raw()
            self$save_raw()
            self$restruct_raw()
            self$extract_from_raw()
            self$validate_extract()
            self$save_extract()
        }
    )
)
