source("./R/generic_scraper.R")
source("./R/utilities.R")

michigan_staff_date_check <- function(x, date = Sys.Date()){
    base_html <- rvest::read_html(x)
    
    base_html %>% 
        rvest::html_nodes("h2") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)updated")]} %>% 
        last() %>% 
        str_split("–") %>% 
        unlist() %>% 
        {.[str_detect(., "(?i)updated")]} %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

michigan_staff_pull <- function(x){
    mi_html <- xml2::read_html(x)
    
    img2 <- mi_html %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        .[9] %>%
        magick::image_read()
    
    img2
}

michigan_staff_restruct <- function(x){
    ExtractTable(x)
}

michigan_staff_extract <- function(x){
    col_name_mat <- matrix(c(
        "Location", "0", "Name",
        "Staff confirmed", "1", "Staff.Confirmed",
        "Staff deaths", "2" , "Staff.Deaths"
    ), ncol = 3, nrow = 3, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x[[1]], col_name_df)
    
    rename_extractable(x[[1]], col_name_df) %>%
        as_tibble() %>%
        filter(Name != "Location" & !grepl(Name, pattern = "Tota")) %>%
        clean_scraped_df() %>%
        mutate(Staff.Deaths = ifelse(is.na(Staff.Deaths), 0, Staff.Deaths),
               Name = str_remove(Name, "^Location ")) 
}

#' Scraper class for general Michigan staff COVID data
#' 
#' @name michigan_staff_scraper
#' @description MI staff data scraper, this information is pulled from a
#' separate table from resident data and though it says we are missing the
#' resident data below we shouldnt be concerned about that. Note that there does
#' not appear to be any sort of consistency or the position of this image
#' and as such this scraper is extremely error prone. Fixing this should be a
#' priority.
#' \describe{
#'   \item{Location}{The facility name}
#'   \item{Staff confirmed}{Staff confirmed positive}
#'   \item{Staff deaths}{staff deaths}
#' }

michigan_staff_scraper <- R6Class(
    "michigan_staff_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://medium.com/@MichiganDOC/mdoc-takes-steps-to-prevent-spread-of-coronavirus-covid-19-250f43144337",
            id = "michigan_staff",
            type = "img",
            state = "MI",
            jurisdiction = "state",
            check_date = michigan_staff_date_check,
            # pull the JSON data directly from the API
            pull_func = michigan_staff_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = michigan_staff_restruct,
            # Rename the columns to appropriate database names
            extract_func = michigan_staff_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    michigan_staff <- michigan_staff_scraper$new(log=TRUE)
    michigan_staff$run_check_date()
    michigan_staff$raw_data
    michigan_staff$pull_raw()
    michigan_staff$raw_data
    michigan_staff$save_raw()
    michigan_staff$restruct_raw()
    michigan_staff$restruct_data
    michigan_staff$extract_from_raw()
    michigan_staff$extract_data
    michigan_staff$validate_extract()
    michigan_staff$save_extract()
}

