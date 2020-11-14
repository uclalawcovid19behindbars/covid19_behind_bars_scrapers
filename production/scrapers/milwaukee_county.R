source("./R/generic_scraper.R")
source("./R/utilities.R")

milwaukee_county_pull <- function(x){
    get_src_by_attr(x, "img", attr = "src", attr_regex = "HOCdashboard.png") %>%
        magick::image_read()
}

milwaukee_county_restruct <- function(x){
    x %>%
        magick::image_crop("800x120+0+437") %>%
        ExtractTable()
}

milwaukee_county_extract <- function(x){
    df_ <- as.data.frame(t(x[[1]]))

    col_name <- matrix(c(
        "Name", "0", "",
        "Drop.Residents.Active", "1", "Current Cases",
        "Residents.Pending", "2", "Pending Tests",
        "Residents.Negative", "3", "Negative Tests",
        "Drop.Residents.Hospital", "4", "Hospitalized",
        "Residents.Deaths", "5", "Deaths"
    ), ncol = 3, nrow = 6, byrow = TRUE)
    
    colnames(col_name) <- c("clean", "raw", "check")
    col_name_df <- as_tibble(col_name)
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>%
        filter(Name != "" & Name != "-") %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general Milwaukee county COVID data
#' 
#' @name milwaukee_county_scraper
#' @description Milwaukee reports resident data in an image hosted on the
#' website. No information is given about total administered tests or the
#' cumulative number of confirmed cases.
#' \describe{
#'   \item{Facility_Name}{The facility name}
#'   \item{Current Cases}{Residents with active infections}
#'   \item{Pending Tests}{Residents with pending tests}
#'   \item{Negative Tests}{Residents with negative tests}
#'   \item{Hospitalized}{Residents hospitalized}
#'   \item{Deaths}{Resident deaths}
#' }

milwaukee_county_scraper <- R6Class(
    "milwaukee_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://county.milwaukee.gov/EN/COVID-19/Individuals-in-County-Care",
            id = "milwaukee_county",
            type = "img",
            state = "WI",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = milwaukee_county_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = milwaukee_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = milwaukee_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    milwaukee_county <- milwaukee_county_scraper$new(log=TRUE)
    milwaukee_county$raw_data
    milwaukee_county$pull_raw()
    milwaukee_county$raw_data
    milwaukee_county$save_raw()
    milwaukee_county$restruct_raw()
    milwaukee_county$restruct_data
    milwaukee_county$extract_from_raw()
    milwaukee_county$extract_data
    milwaukee_county$validate_extract()
    milwaukee_county$save_extract()
}

