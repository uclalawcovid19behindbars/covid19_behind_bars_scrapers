source("./R/generic_scraper.R")
source("./R/utilities.R")

kentucky_pull <- function(x){
    get_src_by_attr(x, "img", attr = "src", attr_regex = "(?i)cases") %>%
        magick::image_read()
}

kentucky_restruct <- function(x){
    ExtractTable(x)
}

kentucky_extract <- function(x){
    df_ <- as.data.frame(x[[1]])
    
    col_name_mat <- matrix(c(
        "Institution", "0", "Name", 
        "Active Staff", "1", "Drop.Staff.Active",
        "Total Staff", "2", "Staff.Confirmed",
        "Total Staff Deaths", "3", "Staff.Deaths", 
        "Active Inmates", "4", "Residents.Active",
        "Total Inmates", "5", "Residents.Confirmed",
        "Total Inmate Deaths", "6", "Residents.Deaths"
    ), ncol = 3, nrow = 7, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>%
        filter(Name != "Institution" & Name != "Total") %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop")) %>%
        as_tibble()
}

#' Scraper class for general Kentucky COVID data
#' 
#' @name kentucky_scraper
#' @description KY data is hosted on an image on the url but only contains
#' information about confirmed and deaths, no testing. More information about
#' transfers and recoveries is sometimes embedded in the text of the website
#' but it is inconsistent.
#' \describe{
#'   \item{Institution}{The facility name}
#'   \item{Staff}{staff confirmed}
#'   \item{Staff Deaths}{Staff deaths}
#'   \item{Inmates}{Residents confirmed}
#'   \item{Inmates Deaths}{Resident deaths}
#' }

kentucky_scraper <- R6Class(
    "kentucky_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.ky.gov/Facilities/Pages/covid19.aspx",
            id = "kentucky",
            type = "img",
            state = "KY",
            jurisdiction = "state",
            pull_func = kentucky_pull,
            restruct_func = kentucky_restruct,
            # Rename the columns to appropriate database names
            extract_func = kentucky_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    kentucky <- kentucky_scraper$new(log=TRUE)
    kentucky$raw_data
    kentucky$pull_raw()
    kentucky$raw_data
    kentucky$save_raw()
    kentucky$restruct_raw()
    kentucky$restruct_data
    kentucky$extract_from_raw()
    kentucky$extract_data
    kentucky$validate_extract()
    kentucky$save_extract()
}

