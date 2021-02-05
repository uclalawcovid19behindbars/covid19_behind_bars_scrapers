source("./R/generic_scraper.R")
source("./R/utilities.R")

utah_pull <- function(x){
    get_src_by_attr(x, "img", attr = "src", attr_regex = "(?i)update") %>%
        .[[1]] %>%
        magick::image_read()
}

utah_restruct <- function(x){
    ExtractTable(x)
}

utah_extract <- function(x){
    col_name_mat1 <- matrix(c(
        "Location", "0", "Name", 
        "Total Offenders Tested", "1", "Residents.Tested",
        "Offenders Confirmed", "2", "Residents.Confirmed",
        "Offenders Negative", "3", "Residents.Negative", 
        "Offenders Recovered", "4", "Residents.Recovered",
        "Offender Deaths", "5", "Residents.Deaths"
        ), ncol = 3, nrow = 6, byrow = TRUE)
    
    colnames(col_name_mat1) <- c("check", "raw", "clean")
    col_name_df1 <- as_tibble(col_name_mat1)
    
    check_names_extractable(x[[1]], col_name_df1)
    
    col_name_mat2 <- matrix(c(
        "Location", "0", "Name",
        "Tests Administered", "1", "Residents.Tadmin",
        "Inmates Confirmed", "2", "Residents.Confirmed",
        "Inmates Negative", "3", "Residents.Negative",
        "Inmates Recovered", "4", "Residents.Recovered",
        "Inmate Deaths", "5", "Residents.Deaths"
        ), ncol = 3, nrow = 6, byrow = TRUE)
    
    colnames(col_name_mat2) <- c("check", "raw", "clean")
    col_name_df2 <- as_tibble(col_name_mat2)
    
    x2 <- x[[2]]
    if(x2[1,1] == "Correctional Facilities"){
        x2 <- x2[2:nrow(x2),]
    }

    check_names_extractable(x2, col_name_df2)
    
    bad_names <- c(
        "Location", "Correctional Facilities", "Total",
        "Community Correctional Centers")
    
    bind_rows(
        rename_extractable(x2, col_name_df2),
        rename_extractable(x[[1]], col_name_df1)) %>%
        filter(!(Name %in% bad_names)) %>% 
        clean_scraped_df() %>%
        as_tibble()
    
}

#' Scraper class for general Utah COVID data
#' 
#' @name utah_scraper
#' @description UT has changed the way that they present data several times
#' but the latest iteration is presented via an image hosted on the page. 
#' Currently the image is grabbed by position which is not sustainable. The
#' test number appears to be residents tested rather than tests administered.
#' \describe{
#'   \item{Location}{The facility name.}
#'   \item{Total Inmates Tested}{Cumulative resients tested not tests administered}
#'   \item{Inmates Confirmed}{Residents confirmed}
#'   \item{Inmates Negative}{Residents negative}
#'   \item{Inmates Recovered}{Residents recovered}
#'   \item{Inmate Deaths}{Resident deaths}
#' }

utah_scraper <- R6Class(
    "utah_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://corrections.utah.gov/index.php/home/alerts-2/1237-udc-coronavirus-updates",
            id = "utah",
            type = "img",
            state = "UT",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = utah_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = utah_restruct,
            # Rename the columns to appropriate database names
            extract_func = utah_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    utah <- utah_scraper$new(log=TRUE)
    utah$raw_data
    utah$pull_raw()
    utah$raw_data
    utah$save_raw()
    utah$restruct_raw()
    utah$restruct_data
    utah$extract_from_raw()
    utah$extract_data
    utah$validate_extract()
    utah$save_extract()
}

