source("./R/generic_scraper.R")
source("./R/utilities.R")

youth_manual_pull <- function(x){
    manual_sheet <- googlesheets4::read_sheet(ss = "17mC-uHp1jhMQO8JGqn4is6pJLrKHP0G0TR57R01MxrY",
                                              sheet = "Permanent", 
                                              col_types = "c")
    return(manual_sheet)
}

youth_manual_restruct <- function(x){
    out <- x %>%
        mutate(Date = lubridate::mdy(`Date of last positive case/last update`)) %>%
        ## filter to dates within the last month, & no dates in the future 
        filter(Date  >= lubridate::today() - lubridate::days(30),
               Date <= lubridate::today()) %>%
        mutate(Name = `County/Name of Facility`,
               Name = str_glue('{Name} YOUTH FACILITY'),
               Residents.Confirmed = string_to_clean_numeric(`Confirmed Cases (Youth)`),
               Residents.Active = string_to_clean_numeric(`Confirmed Cases (Youth)`),
               Staff.Confirmed = string_to_clean_numeric(`Active Cases (Youth)`),
               Staff.Deaths = string_to_clean_numeric(`Confirmed Deaths (Staff)`),
               Staff.Active = string_to_clean_numeric(`Active Cases Staff`),
               Residents.Population = string_to_clean_numeric(`Youth Population`)) %>%
        relocate(any_of(starts_with("Residents.")),
                 any_of(starts_with("Staff."))) %>%
        rowwise() %>%
        mutate(total_numeric = sum_na_rm(c_across(Residents.Confirmed:Staff.Active))) %>%
        ## filter out rows where all numeric vars are NA 
        filter(!is.na(total_numeric))
    
    return(out)
}

youth_manual_extract <- function(x){
    x %>%
        select(
            Date, 
            Name,
            State, 
            Jurisdiction, 
            Residents.Confirmed,
            Residents.Active,
            Residents.Population,
            Staff.Confirmed,
            Staff.Active,
            Staff.Deaths
        ) 
}

#' Scraper class for general youth_manual COVID data
#' 
#' @name youth_manual_scraper
#' @description This will be a description of youth_manual data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

youth_manual_scraper <- R6Class(
    "youth_manual_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "Youth team manual data collection",
            id = "youth_manual",
            type = "csv",
            state = "",
            jurisdiction = "state",
            pull_func = youth_manual_pull,
            restruct_func = youth_manual_restruct,
            extract_func = youth_manual_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    youth_manual <- youth_manual_scraper$new(log=TRUE)
    youth_manual$pull_raw()
    youth_manual$raw_data
    youth_manual$save_raw()
    youth_manual$restruct_raw()
    youth_manual$restruct_data
    youth_manual$extract_from_raw()
    youth_manual$extract_data
    youth_manual$validate_extract()
    youth_manual$save_extract()
}

