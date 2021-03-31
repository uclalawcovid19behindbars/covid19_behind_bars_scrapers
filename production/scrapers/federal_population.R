source("./R/generic_scraper.R")
source("./R/utilities.R")

federal_population_check_date <- function(x, date = Sys.Date()) {
    "https://www.bop.gov/PublicInfo/execute/popreport?todo=query&output=json" %>% 
        jsonlite::read_json(simplifyVector = TRUE) %>%
        {.$DATEMODIFIED} %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

federal_population_pull <- function(x) {
    "https://www.bop.gov/PublicInfo/execute/popreport?todo=query&output=json" %>% 
        jsonlite::read_json(simplifyVector = TRUE)
}

federal_population_restruct <- function(x) {
    bind_rows(
        x$BOP, 
        x$PRIVATE, 
        x$CCM
    ) %>% as_tibble()
}

federal_population_extract <- function(x) {
    
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
#' @name federal_population_scraper
#' @description Data pulled from federal pouplation api
#' Brief description of api can be found here 
#' http://prisondb.github.io/bopapidocs.html
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

federal_population_scraper <- R6Class(
    "federal_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.bop.gov/mobile/about/population_statistics.jsp",
            id = "federal_population",
            type = "json",
            state = "federal",
            jurisdiction = "federal",
            # pull the JSON data directly from the API
            pull_func = federal_population_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = federal_population_restruct,
            # Rename the columns to appropriate database names
            extract_func = federal_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    federal_population <- federal_population_scraper$new(log=TRUE)
    federal_population$raw_data
    federal_population$pull_raw()
    federal_population$raw_data
    federal_population$save_raw()
    federal_population$restruct_raw()
    federal_population$restruct_data
    federal_population$extract_from_raw()
    federal_population$extract_data
    federal_population$validate_extract()
    federal_population$save_extract()
}
