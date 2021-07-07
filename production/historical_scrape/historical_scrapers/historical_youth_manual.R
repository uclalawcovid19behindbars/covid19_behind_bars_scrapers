source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_youth_manual_pull <- function(x){
    ## ran this once and then downloaded it to avoid rate limiting
    manual_sheet <- googlesheets4::read_sheet(ss = "17mC-uHp1jhMQO8JGqn4is6pJLrKHP0G0TR57R01MxrY",
                                              sheet = "Permanent", 
                                              col_types = "c")
    return(manual_sheet)
}

historical_youth_manual_restruct <- function(x){
    restruct <- x %>%
        mutate(Date = lubridate::mdy(`Date of last positive case/last update`)) %>%
        filter(!is.na(Date)) %>%
        mutate(Name = `County/Name of Facility`,
               Name = str_glue('{Name} YOUTH FACILITY {State}'),
               Residents.Confirmed = string_to_clean_numeric(`Confirmed Cases (Youth)`),
               Residents.Active = string_to_clean_numeric(`Confirmed Cases (Youth)`),
               Staff.Confirmed = string_to_clean_numeric(`Active Cases (Youth)`),
               Staff.Deaths = string_to_clean_numeric(`Confirmed Deaths (Staff)`),
               Staff.Active = string_to_clean_numeric(`Active Cases Staff`),
               Residents.Population = string_to_clean_numeric(`Youth Population`)) %>%
        relocate(Date,
                 Name,
                 any_of(starts_with("Residents.")),
                 any_of(starts_with("Staff."))) %>%
        rowwise() %>%
        mutate(total_numeric = sum_na_rm(c_across(Residents.Confirmed:Staff.Active))) %>%
        ## filter out rows where all numeric vars are NA 
        filter(!is.na(total_numeric)) %>%
        filter(!is.na(Name))
    
    return(restruct)
}

historical_youth_manual_extract <- function(x){
    extract <- x %>%
        select(Name, Date, 
               any_of(starts_with("Residents.")),
               any_of(starts_with("Staff."))) %>%
        mutate(id = 'historical_youth_manual',
               jurisdiction = 'youth',
               State = '',
               source = 'Manual data collection')
    
    return(extract)
}

historical_youth_manual_validate_save <- function(x){
    part_valid_columns <- UCLABB_MAIN_VARIABLES
    historical_manual_youth_vars <- c("Name", "Date", "id", "jurisdiction", "source", "State")
    valid_columns <- c(part_valid_columns, historical_manual_youth_vars)
    
    for(i in names(x)){
        if(!(i %in% valid_columns)){
            warning(str_c(i, " not a valid column name. Being removed."))
            x <- x %>%
                select(-!!i)
        }
        
        else{
            if((i %in% historical_manual_youth_vars == FALSE) & !is.numeric(x[[i]])){
                warning(str_c(i, " is not numeric must convert."))
                x[[i]] <- as.numeric(
                    x[[i]])
            }
        }
    }
    
    ext_names <- names(x)
    
    if(!("Name" %in% ext_names)){
        stop(str_c(
            "No 'Name' column present. Did you forget to add ",
            "the indicator 'State-Wide'?"))
    }
    
    less_check <- function(gr, ls){
        if(all(c(gr, ls) %in% ext_names)){
            gr_vec <- x[[gr]]
            ls_vec <- x[[ls]]
            comp_vec <- gr_vec < ls_vec
            comp_vec[is.na(comp_vec)] <- FALSE
            if(any(comp_vec)){
                bad_names <- x$Name[comp_vec]
                warning(str_c(
                    "The following facilities had ", gr,
                    " values that were less than ", ls, " values: ",
                    str_c(bad_names, collapse = ", ")
                ))
            }
        }
    }
    
    ## sanity checks no changes made only warnings thrown
    less_check("Staff.Confirmed", "Staff.Recovered")
    less_check("Residents.Confirmed", "Residents.Recovered")
    less_check("Staff.Confirmed", "Staff.Deaths")
    less_check("Residents.Confirmed", "Residents.Deaths")
    less_check("Staff.Tested", "Staff.Confirmed")
    less_check("Residents.Tested", "Residents.Confirmed")
    less_check("Staff.Tested", "Staff.Negative")
    less_check("Residents.Tested", "Residents.Negative")
    less_check("Residents.Initiated", "Residents.Completed")
    less_check("Staff.Initiated", "Staff.Completed")
    less_check("Residents.Vadmin", "Residents.Initiated")
    less_check("Staff.Vadmin", "Staff.Initiated")
    
    write_csv(x, "results/extracted_data/historical_youth_manual.csv")
}

#' Scraper class for general historical_youth_manual COVID data
#' 
#' @name historical_youth_manual_scraper
#' @description Reads google sheet with back-log of youth manual data, cleans it lightly, and 
#' saves it in a CSV. Note that the output contains a mix of states, which get resolved in 
#' `clean_facility_name()`
#' \describe{
#'   \item{Name}{The facility name}
#' }

historical_youth_manual_scraper <- R6Class(
    "historical_youth_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Manual data collection",
            id = "historical_youth_manual",
            type = "csv",
            state = "",
            jurisdiction = "youth",
            check_date = NULL,
            pull_func = historical_youth_manual_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = historical_youth_manual_restruct,
            # Rename the columns to appropriate database names
            extract_func = historical_youth_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    historical_youth_manual <- historical_youth_manual_scraper$new(log = TRUE)
    # historical_youth_manual$reset_date("DATE")
    historical_youth_manual$raw_data
    historical_youth_manual$pull_raw()
    historical_youth_manual$raw_data
    historical_youth_manual$save_raw()
    historical_youth_manual$restruct_raw()
    historical_youth_manual$restruct_data
    historical_youth_manual$extract_from_raw() 
    historical_youth_manual$extract_data
    historical_youth_manual$historical_youth_manual_validate_save()
}

