source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_population_pull <- function(x){
    stop_defunct_scraper(x)
    # xml2::read_html(x)
}

pennsylvania_population_restruct <- function(x){
    stop_defunct_scraper("https://www.cor.pa.gov/Pages/COVID-19.aspx")
    x %>%
        rvest::html_node("table") %>%
        rvest::html_table(header = TRUE) %>%
        as_tibble()
}

pennsylvania_population_extract <- function(x){
    
    df_ <- x
    
    exp_names <- c(
        Name = "INSTITUTION",
        Residents.Population = "TODAY'S POPULATION",
        Drop.Residents.Release = "REPRIEVE RELEASES",
        Drop.Pop.After.Release = "TODAY'S POPULATION AFTER REPRIEVE RELEASES",
        Drop.Delta.Day = "INCREASE/ DECREASE FROM YESTERDAY",
        Drop.Delta.Week = "INCREASE/ DECREASE FROM LAST WEEK",
        Drop.Delta.Month = "INCREASE/ DECREASE FROM LAST MONTH"
    )
    
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    bad_names <- c(
        "INSTITUTION", "SCI TOTAL", "CCC/F East", "CCC/F Central",
        "CCC/F West", "CCC/F TOTAL")
    
    df_ %>%
        select(-starts_with("Drop")) %>%
        filter(!(Name %in% bad_names)) %>%
        clean_scraped_df()
}

#' Scraper class for general Pennsylvania population COVID data
#' 
#' @name pennsylvania_population_scraper
#' @description PN also reports separately info on the population of each
#' facility. We collect that information from an html table here. You can
#' ignore the warnings about missingness below.
#' \describe{
#'   \item{INSTITUTION}{The facility name.}
#'   \item{TODAY'S POPULATION}{Residnet population}
#'   \item{REPRIEVE RELEASES}{Residents released}
#'   \item{TODAY'S POPULATION AFTER REPRIEVE RELEASES}{New res population}
#'   \item{INCREASE/DECREASE FROM YESTERDAY}{Resident change from yesterday}
#'   \item{INCREASE/DECREASE FROM LAST WEEK}{Resident change from last week}
#'   \item{INCREASE/DECREASE FROM LAST MONTH}{Resident change from last month}
#' }

pennsylvania_population_scraper <- R6Class(
    "pennsylvania_population_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cor.pa.gov/Pages/COVID-19.aspx",
            id = "pennsylvania_population",
            type = "html",
            state = "PA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = pennsylvania_population_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = pennsylvania_population_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_population_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania_population <- pennsylvania_population_scraper$new(log=TRUE)
    pennsylvania_population$raw_data
    pennsylvania_population$pull_raw()
    pennsylvania_population$raw_data
    pennsylvania_population$save_raw()
    pennsylvania_population$restruct_raw()
    pennsylvania_population$restruct_data
    pennsylvania_population$extract_from_raw()
    pennsylvania_population$extract_data
    pennsylvania_population$validate_extract()
    pennsylvania_population$save_extract()
}

