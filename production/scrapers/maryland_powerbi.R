source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

maryland_powerbi_pull <- function(x){
    src_url <- str_c(
        "https://app.powerbigov.us/view?r=",
        "eyJrIjoiMjdlZjZmNzAtOGYxNS00ODA4LThhMzktOGYyZDEw",
        "YTMwMDZkIiwidCI6IjYwYWZlOWUyLTQ5Y2QtNDliMS04ODUx",
        "LTY0ZGYwMjc2YTJlOCJ9&pageName=ReportSection")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(src_url)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()
    
    remDr$quit()
    
    xml2::read_html(base_html[[1]])
}

maryland_powerbi_restruct <- function(x){
    tab <- x %>%
        rvest::html_node(".tableEx") %>%
        rvest::html_node(".innerContainer")
    
    col_dat <- tab %>%
        rvest::html_node(".bodyCells") %>%
        rvest::html_node("div") %>%
        rvest::html_children()
    
    dat_df <- do.call(rbind, lapply(col_dat, function(p){
        sapply(rvest::html_children(p), function(z){
            z %>% 
                rvest::html_nodes("div") %>%
                rvest::html_attr("title")})})) %>%
        as.data.frame()
    
    names(dat_df) <- tab %>%
        rvest::html_node(".columnHeaders") %>%
        rvest::html_node("div") %>%
        rvest::html_nodes("div") %>% 
        rvest::html_attr("title") %>%
        na.omit() %>%
        as.vector()
    
    dat_df %>%
        rename(Name = "Facility Name") %>%
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        as_tibble() %>%
        mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
        filter(!str_detect(Name, "(?i)total"))
}

maryland_powerbi_extract <- function(x){
    x %>%
        rename(
            Residents.Recovered = "Inmate Recoveries",
            Residents.Confirmed = "Inmate Cases",
            Residents.Deaths = "Inmate Deaths",
            Staff.Recovered = "Staff Recoveries",
            Staff.Confirmed = "Staff Cases",
            Staff.Deaths = "Staff Deaths"
        )
}

#' Scraper class for general Maryland COVID data
#' 
#' @name maryland_powerbi_scraper
#' @description Data from MD is pulled from a image hosted in the DOC website
#' which is run the OCR. The data posted has been fairly consistent.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Region}{Greater region of facility}
#'   \item{Staff Tests}{Tests administered to staff not sure if test administered or individuals tested}
#'   \item{Staff Positive}{Number of confirmed staff}
#'   \item{Staff Recovered}{Number of recovered staff}
#'   \item{Staff Deaths}{Number of staff deaths}
#'   \item{Inmates Tested}{Residents tested not sure if test administered or individuals tested}
#'   \item{Inmates Positive}{Number of confirmed residents}
#'   \item{Inmates Recovered}{Number of recovered residents}
#'   \item{Inmates Deaths}{Number of resident deaths}
#' }

maryland_powerbi_scraper <- R6Class(
    "maryland_powerbi_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://news.maryland.gov/dpscs/covid-19/",
            id = "maryland_powerbi",
            type = "html",
            state = "MD",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = maryland_powerbi_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maryland_powerbi_restruct,
            # Rename the columns to appropriate database names
            extract_func = maryland_powerbi_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    maryland_powerbi <- maryland_powerbi_scraper$new(log=TRUE)
    maryland_powerbi$run_check_date()
    maryland_powerbi$raw_data
    maryland_powerbi$pull_raw()
    maryland_powerbi$raw_data
    maryland_powerbi$save_raw()
    maryland_powerbi$restruct_raw()
    maryland_powerbi$restruct_data
    maryland_powerbi$extract_from_raw()
    maryland_powerbi$extract_data
    maryland_powerbi$validate_extract()
    maryland_powerbi$save_extract()
}

