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
        "Recovered.Positive.Cases", "Released.Positive.Cases", 
        "Active.PositiveCases"
    )
    
    check_names(wis_df, exp_names)
    
    ext_df <- wis_df
    names(ext_df) <- c(
        "Name", "Residents.Confirmed", "Residents.Negative",
        "Residents.Tested", "Residents.Recovered", "Drop.Release", "Drop.Active"
    )
    
    ext_df %>%
        select(-contains("Drop"))
}

#' Scraper class for general wisconsin COVID data
#' 
#' @name wisconsin_scraper
#' @description This will be a description of wisconsin data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

wisconsin_scraper <- R6Class(
    "wisconsin_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = str_c(
                "https://doc.wi.gov/Pages/COVID19%28Coronavirus%29/",
                "COVID19TestingDashboard.aspx"),
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

