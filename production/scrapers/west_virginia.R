source("./R/generic_scraper.R")
source("./R/utilities.R")

west_virginia_check_date <- function(url, date = Sys.Date()){
    
    tsv_src <- get_src_by_attr(url, "a", attr = "href", 
                               attr_regex = "COVID19_DCR") %>% 
        first()
    
    tsv_src %>% 
        str_split("DCR_") %>% # get the part of the url with the date
        .[[1]] %>% 
        .[2] %>%
        lubridate::ymd() %>% 
        error_on_date(date)
}

west_virginia_pull <- function(url){
    tsv_src <- get_src_by_attr(url, "a", attr = "href", 
                               attr_regex = "COVID19_DCR") %>% 
        first()

    dev_null <- suppressWarnings(pull_data <- read_tsv(
        tsv_src, skip = 1, col_types = cols()))
    
    return(pull_data)
}

west_virginia_restruct <- function(pull_data){
    restruct_data <- pull_data
    
    replace_col <- unname(unlist(restruct_data[1,]))
    rcol_idx <- which(!is.na(replace_col))
    
    names(restruct_data)[rcol_idx] <- replace_col[rcol_idx]
    
    return(restruct_data)
}

west_virginia_extract <- function(restruct_data){
    exp_cols <- c(
        Name = "Regional jails", 
        Drop.County = "County", 
        Residents.Population = "Pop.",
        Residents.Active = "Active cases",
        Residents.Recovered = "Recovered cases",
        Residents.Confirmed = "Positive test results",
        Residents.Negative = "Negative test results",
        Tests.Intake.Drop = "Tests at intake",
        Positive.Intake.Drop = "Positive intake results",
        Tests.Drop = "Tests at release",
        Positive.Release.Drop = "Positive release results",
        Residents.Pending = "Results pending (all tests)",
        Residents.Tadmin = "Total tests",
        Residents.Quarantine = "Quarantine"
    )
    
    check_names(restruct_data, exp_cols)
    names(restruct_data) <- names(exp_cols)
    
    fac_df <- restruct_data %>%
        filter(
            !is.na(Residents.Population) & 
                !str_detect(Residents.Population, "(?i)total") &
                !str_detect(Name, "(?i)total")) %>%
        select(-Drop.County) %>%
        .[1:(which(str_detect(.$Name, "(?i)employee")) - 1),] %>%
        clean_scraped_df() %>%
        mutate(
            Residents.Quarantine = Residents.Quarantine + Residents.Pending) %>%
        select(-starts_with("Drop"))
    
    # get deaths
    resident_deaths <- restruct_data %>%
        filter(
            str_starts(Name, "(?i)inmate deaths") | 
                str_starts(Name, "(?i)confirmed inmate deaths")) %>%
        pull(Name) %>%
        # remove items in parenthesis
        str_remove_all("\\([^)]*\\)") %>%
        str_split("\\.") %>%
        unlist() %>%
        .[1] %>%
        # remove commas
        str_remove_all(",") %>%
        str_extract_all("[0-9]+") %>%
        unlist() %>%
        string_to_clean_numeric() %>%
        sum()
    
    # see which row starts employee data
    emp_idx <- first(which(str_detect(restruct_data$Name, "(?i)employee")))
    staff_df <- restruct_data[emp_idx:nrow(restruct_data),] 
    names(staff_df) <- str_replace(names(staff_df), "Residents", "Staff")
    
    extract_data <- staff_df %>% 
        select(-starts_with("Drop"), -Staff.Population, Staff.Active) %>%
        filter(!is.na(Staff.Confirmed)) %>% 
        filter(!str_detect(Staff.Confirmed, "(?i)cumulative")) %>% 
        filter(!str_detect(Name, "(?i)total")) %>% 
        mutate(Name = str_c(clean_fac_col_txt(Name), " staff total")) %>% 
        select(-ends_with(".Drop")) %>%
        clean_scraped_df() %>%
        bind_rows(fac_df) %>%
        rename(Staff.Tested = Staff.Tadmin) %>% 
        bind_rows(tibble(Name = "State-Wide", Residents.Deaths = resident_deaths)) %>%
        select(-ends_with(".Drop")) %>%
        clean_scraped_df() 
    
    return(extract_data)
}

#' Scraper class for general West Virginia COVID data
#' 
#' @name west_virginia_scraper
#' @description WV data resides in a tsv file hosted on the doc website. Staff
#' information is only provided at the state wide table and a number of coding
#' steps are necessary to extract the information.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{County}{County location}
#'   \item{Pop}{Resident population}
#'   \item{Negative}{Residents negative}
#'   \item{Positive active}{Residents currently infected}
#'   \item{Pending}{Resident tests pending, all inmates/residents awaiting test results remain in isolation}
#'   \item{Total}{cumulative tests refers to number of tests performed and completed, NOT the number of inmates or residents tested}
#'   \item{Recovered}{Residents recovered}
#'   \item{Cumulative Cases}{Residents cumulative cases}
#'   \item{Quarantine}{Residents in quarantine}
#'   \item{Staff Negative}{Statewide only}
#'   \item{Staff Positive active}{Statewide only}
#'   \item{Staff Pending}{Statewide only}
#'   \item{Staff Total}{Statewide only}
#'   \item{Staff Recovered}{Statewide only}
#'   \item{Staff Cumulative}{Statewide only}
#'   \item{Resident Deaths}{Statewide only, deaths of positive inmates are not included in this data when medical opinion does not conclude COVID-19 caused or contributed to the death, pending any further medical determination. Deaths not included per this criteria: MOCC, 6}
#' }

west_virginia_scraper <- R6Class(
    "west_virginia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://dhhr.wv.gov/COVID-19/Pages/Case-Reports-2022.aspx",
            id = "west_virginia",
            type = "csv",
            state = "WV",
            jurisdiction = "state",
            check_date = west_virginia_check_date,
            # pull the JSON data directly from the API
            pull_func = west_virginia_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = west_virginia_restruct,
            # Rename the columns to appropriate database names
            extract_func = west_virginia_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    west_virginia <- west_virginia_scraper$new(log=TRUE)
    west_virginia$run_check_date()
    west_virginia$raw_data
    west_virginia$pull_raw()
    west_virginia$raw_data
    west_virginia$save_raw()
    west_virginia$restruct_raw()
    west_virginia$restruct_data
    west_virginia$extract_from_raw()
    west_virginia$extract_data
    west_virginia$validate_extract()
    west_virginia$save_extract()
}
