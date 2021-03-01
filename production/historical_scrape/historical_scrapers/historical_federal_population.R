source("./R/generic_scraper.R")
source("./R/utilities.R")

historical_fed_pop_pull <- function(x, date, file = NULL){
    
    if(date > lubridate::ymd("2021-01-02")){
        stop(
            "Historical federal pop scraper should not be run past 2021-01-02 as to ",
            "not overlap with federal pop scraper.")
    }
    
    date <- as.Date(date, format = "%Y-%m-%d")
    year4 <- format.Date(date, "%Y")
    month <- format.Date(date, "%m")
    day <- format.Date(date, "%d")
    
    stringr::str_c(
        "https://web.archive.org/web/", 
        year4, month, day, 
        "if_/https://www.bop.gov/PublicInfo/execute/popreport?todo=query&output=json") %>%
        jsonlite::read_json(simplifyVector = TRUE)
}

historical_fed_pop_restruct <- function(x, date = NULL){
    bind_rows(
        x$BOP, 
        x$PRIVATE, 
        x$CCM
    ) %>% as_tibble() 
}

historical_fed_pop_extract <- function(x, date = NULL){
    
    df_ <- x 
    
    exp_names <- c(
        Drop.code = "code", 
        Name = "Name", 
        Drop.state = "state", 
        Residents.Population = "popCount", 
        Drop.groupDescription = "groupDescription", 
        Drop.sortKey = "sortKey", 
        Drop.sortNameKey = "sortNameKey", 
        Drop.sortpzapSequence = "sortpzapSequence", 
        Drop.indentationIndicator = "indentationIndicator", 
        Drop.recordtype = "recordtype"
    )
    
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    df_ %>%
        select(-starts_with("Drop")) %>%
        mutate(Name = clean_fac_col_txt(Name, to_upper = TRUE))
}

#' Scraper class for Federal prison population data
#' 
#' @name historical_federal_pop_scraper
#' @description Data pulled from wayback archives and federal population API 
#' \describe{
#'   \item{state}{US State not always provided}
#'   \item{name}{Name of institution}
#'   \item{popCount}{ Current Poulation}
#'   \item{groupDescription}{Description of group type}
#'   \item{sortKey}{}
#'   \item{sortNameKey}{}
#'   \item{sortpzapSequence}{}
#'   \item{indentationIndicator}{}
#'   \item{recordType}{}
#' }

historical_fed_pop_scraper <- R6Class(
    "historical_federal_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.bop.gov/mobile/about/population_statistics.jsp",
            id = "historical_federal_pop",
            type = "json",
            state = "federal",
            jurisdiction = "federal",
            pull_func = historical_fed_pop_pull,
            restruct_func = historical_fed_pop_restruct,
            extract_func = historical_fed_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_fed_pop <- historical_fed_pop_scraper$new(log=TRUE)
    historical_fed_pop$reset_date("2021-01-09")
    historical_fed_pop$raw_data
    # API returns json closest to date queried  
    historical_fed_pop$pull_raw(date = historical_fed_pop$date, .dated_pull = TRUE)
    historical_fed_pop$raw_data
    historical_fed_pop$date
    # Reset date based on date modified in raw file 
    historical_fed_pop$reset_date(lubridate::mdy(historical_fed_pop$raw_data$DATEMODIFIED))
    historical_fed_pop$date
    historical_fed_pop$save_raw()
    historical_fed_pop$restruct_raw()
    historical_fed_pop$restruct_data
    historical_fed_pop$extract_from_raw()
    historical_fed_pop$extract_data
    historical_fed_pop$validate_extract()
    historical_fed_pop$save_extract()
}
