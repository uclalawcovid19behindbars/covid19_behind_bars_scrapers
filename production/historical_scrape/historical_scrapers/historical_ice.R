source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_ice_str_word_detect <- function(string, pattern){
    str_detect(string, str_c(" ", pattern, " ")) |
        str_starts(string, str_c(pattern, " ")) |
        str_ends(string, str_c(" ", pattern))
}

historical_ice_pull <- function(x, date = NULL){
    wide_df <- "https://raw.githubusercontent.com/ivnagpal/ICE-COVID19/" %>%
        # the specific commit we are pulling historical data from
        str_c("596de9a76c564ce48ab6aae39d5a9aa4b6566a94/imm_df.csv") %>%
        read_csv(col_types = cols())
    
    wide_df %>%
        pivot_longer(-`Custody/AOR/Facility`) %>%
        mutate(Date = lubridate::mdy(str_split_fixed(name, ":", 2)[,1])) %>%
        mutate(Measure = str_split_fixed(name, ":", 2)[,2]) %>%
        select(-name) %>%
        pivot_wider(names_from = "Measure", values_from = "value")
}

historical_ice_restruct <- function(x, date = NULL){
    x %>%
        rename(
            Name = "Custody/AOR/Facility",
            Residents.Deaths = "Detainee deaths",
            Residents.Confirmed = "Total confirmed COVID-19 cases",
            Residents.Active =
                "Confirmed cases currently under isolation or monitoring",
            Staff.Confirmed = "Staff Confirmed Cases") %>%
        mutate(no_data = is.na(Residents.Deaths) & is.na(Residents.Confirmed) &
                   is.na(Staff.Confirmed) & is.na(Residents.Active)) %>%
        filter(!no_data)
}

historical_ice_extract <- function(x, date){
    x %>%
        mutate(Name = clean_fac_col_txt(Name, TRUE)) %>%
        mutate(Name = ifelse(
            historical_ice_str_word_detect(Name, "ICE"),
            Name,
            str_c("ICE ", Name))) %>%
        filter(Date == date) %>%
        select(-Date, -no_data)
}

#' Scraper class for ice COVID data
#' 
#' @name historical_ice_scraper
#' @description This scraper pulls html data from the ice page which reports on
#' the variables listed below. Unlike all other scrapers their are total column
#' values that we want to keep which do not corresponds to a state but rather
#' ICE as a whole. In addition we have found that facility names frequently
#' change and require updates to the facility spellings sheet. Dates for this
#' scraper should correspond to all dates which are present in the github sheet
#' columns where the data is being pulled up to when the main scraper started
#' pulling ice data 2021-01-12.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Confirmed cases currently under isolation}{Residents with active inf}
#'   \item{Detainee deaths}{Resident deaths}
#'   \item{Total confirmed COVID-19}{Residents cconfirmed cumulative}
#'   \item{Detained Population}{Current Resident Population}
#'   \item{Population Tested}{Tested}
#' }

historical_ice_scraper <- R6Class(
    "historical_ice_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.ice.gov/coronavirus",
            # should not have historical in the name as it matches
            # with places where we had data prior
            id = "ice",
            type = "csv",
            state = "federal",
            jurisdiction = "immigration",
            pull_func = historical_ice_pull,
            restruct_func = historical_ice_restruct,
            extract_func = historical_ice_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_ice <- historical_ice_scraper$new(log=TRUE)
    historical_ice$reset_date("SET_DATE_HERE")
    historical_ice$raw_data
    historical_ice$pull_raw(date = scraper$date, .dated_pull = TRUE)
    historical_ice$raw_data
    historical_ice$save_raw()
    historical_ice$restruct_raw(date = scraper$date)
    historical_ice$restruct_data
    historical_ice$extract_from_raw(date = scraper$date)
    historical_ice$extract_data
    historical_ice$validate_extract()
    historical_ice$save_extract()
}