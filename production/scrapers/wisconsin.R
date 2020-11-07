source("./R/generic_scraper.R")
source("./R/utilities.R")

wisconsin_pull <- function(x){
    # need to have chron job with rselenium running in background
    # we should fix this soon
    base <- "http://nmmarquez.twilightparadox.com:3838/WI_covid_files/"

    get_src_by_attr(
        base, "a", attr = "href", attr_regex = "*",
        date_regex = "\\d+-\\d+-\\d+", date_format = "ymd")

}

wisconsin_restruct <- function(x){
    pdf_area <- list(c(104.64249, 43.36788, 431.90674, 566.58031))
    
    tabulizer::extract_tables(
        x, pages = 1, area = pdf_area)[[1]]
    
}

wisconsin_extract <- function(x){
    
    list_dat <- x
    
    col_names <- apply(list_dat[1:2,], 2, str_c, collapse=" ") %>%
        str_trim() %>%
        # Facility doesnt have a name
        {ifelse(. == "", "Facility", .)} %>%
        str_replace_all(" ", ".")
    
    colnames(list_dat) <- col_names
    
    wis_df <- list_dat[-(1:2),] %>%
        as_tibble() %>%
        rename(Name=Facility) %>%
        clean_scraped_df()
    
    exp_names <- c(
        "Name", "Positive.Tests", "Negative.Tests", "Total.Tests",
        "Released.Positive.Cases", "Active.Positive.Cases",
        "Inactive.PositiveCases")
    
    check_names(wis_df, exp_names)
    
    ext_df <- wis_df
    names(ext_df) <- c(
        "Name", "Residents.Confirmed", "Residents.Negative",
        "Residents.Tested", "Drop.Release", "Drop.Active", "Residents.Recovered"
    )
    
    ext_df %>%
        select(-contains("Drop")) %>%
        filter(!str_detect(Name, "(?i)total"))
}

#' Scraper class for general wisconsin COVID data
#' 
#' @name wisconsin_scraper
#' @description WI comes from a tableau table which is downloaded as a pdf.
#' Currently the downloading of this pdf is done on a private server run by
#' marquezn at law.ucla.edu because of RSelenium requirements. This should be
#' changed when a dedicated server is built. 
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Positive tests}{Cumulative positive test count for residents.}
#'   \item{Negative tests}{Cumulative negative test count for residents.}
#'   \item{Total tests}{Cumulative tests administered for residents.}
#'   \item{Released Positive Cases}{Cumulative residents released while positive.}
#'   \item{Active Positive Cases}{Residents with active cases.}
#'   \item{Inactive Positive Cases}{Residents recovered but the wording changed 10/9 from recovered to inactive.}
#' }

wisconsin_scraper <- R6Class(
    "wisconsin_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "https://doc.wi.gov/Pages/COVID19%28Coronavirus%29/COVID19TestingDashboard.aspx",
            id = "wisconsin",
            type = "pdf",
            state = "WI",
            # pull the JSON data directly from the API
            pull_func = wisconsin_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wisconsin_restruct,
            # Rename the columns to appropriate database names
            extract_func = wisconsin_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin <- wisconsin_scraper$new(log=TRUE)
    wisconsin$raw_data
    wisconsin$pull_raw()
    wisconsin$raw_data
    wisconsin$save_raw()
    wisconsin$restruct_raw()
    wisconsin$restruct_data
    wisconsin$extract_from_raw()
    wisconsin$extract_data
    wisconsin$validate_extract()
    wisconsin$save_extract()
}

