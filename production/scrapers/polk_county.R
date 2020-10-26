source("./R/generic_scraper.R")
source("./R/utilities.R")

polk_county_pull <- function(x){
    get_src_by_attr(x, "img", attr = "src", attr_regex = "covid") %>%
        magick::image_read()
}

polk_county_restruct <- function(x){
    ExtractTable(x)
}

polk_county_extract <- function(x){
    
    col_name <- matrix(c(
        "Residents.Tested", "0", "Inmate Tests Administered",
        "Residents.Confirmed", "1", "Inmates Positive",
        "Staff.Tested", "2", "Staff Tests Administered",
        "Staff.Confirmed", "3", "Staff Positive"
        ), ncol = 3, nrow = 4, byrow = TRUE)
    
    colnames(col_name) <- c("clean", "raw", "check")
    col_name_df <- as_tibble(col_name)
    
    df_ <- as.data.frame(x[[1]])
    
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>%
        filter(!str_detect(Residents.Tested, "(?i)inmate")) %>%
        mutate(Name = "Polk County Jail") %>%
        clean_scraped_df() %>%
        as_tibble()
}

#' Scraper class for general Polk county COVID data
#' 
#' @name polk_county_scraper
#' @description Polk county reports testing and confirmed data for staff and
#' residents in an image hosted on the web page. Test number appear to be tests
#' administered not individuals tested.
#' \describe{
#'   \item{Inmate Tests Administered}{Number of tests administered to residents}
#'   \item{Inmates Positive}{Number of Inmates Confirmed}
#'   \item{Staff Tests Administered}{Number of tests administered to staff}
#'   \item{Staff Positive}{Number of staff confirmed}
#' }

polk_county_scraper <- R6Class(
    "polk_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.polkcountyiowa.gov/county-sheriff/news-press-releases/covid-19-quick-stats/",
            id = "polk_county",
            type = "img",
            state = "IA",
            # pull the JSON data directly from the API
            pull_func = polk_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = polk_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = polk_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    polk_county <- polk_county_scraper$new(log=TRUE)
    polk_county$raw_data
    polk_county$pull_raw()
    polk_county$raw_data
    polk_county$save_raw()
    polk_county$restruct_raw()
    polk_county$restruct_data
    polk_county$extract_from_raw()
    polk_county$extract_data
    polk_county$validate_extract()
    polk_county$save_extract()
}

