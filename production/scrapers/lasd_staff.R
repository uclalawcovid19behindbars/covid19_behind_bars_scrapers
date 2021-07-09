source("./R/generic_scraper.R")
source("./R/utilities.R")

lasd_staff_date_check <- function(x, date = Sys.Date()){
    lasd_html <- xml2::read_html(x)
    
    app_source <- get_src_by_attr(x, "iframe", attr="src", attr_regex = "app")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_source)
    Sys.sleep(6)
    
    x <- remDr$getPageSource() %>%
        {xml2::read_html(.[[1]])}
    
    rvest::html_nodes(x, ".visualContainer") %>% 
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)date")]} %>%
        str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
        lubridate::mdy() %>%
        error_on_date(date)
}

lasd_staff_pull <- function(x, wait = 5){
    lasd_html <- xml2::read_html(x)
    
    app_source <- get_src_by_attr(x, "iframe", attr="src", attr_regex = "app")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_source)
    Sys.sleep(6)
    
    remDr$getPageSource() %>%
        {xml2::read_html(.[[1]])}
}

lasd_staff_restruct <- function(x){
    tables <- rvest::html_nodes(x, ".pivotTable")
    
    col_names <- tables[[2]] %>%
        rvest::html_nodes(".columnHeaders") %>%
        rvest::html_nodes(".pivotTableCellWrap") %>%
        rvest::html_text() %>%
        str_squish()
    
    row_vals <- tables[[2]] %>%
        rvest::html_nodes(".floatingBodyCells") %>%
        rvest::html_nodes(".pivotTableCellWrap") %>%
        rvest::html_text() %>%
        str_squish() %>%
        string_to_clean_numeric()
    
    col_names1 <- tables[[1]] %>%
        rvest::html_nodes(".columnHeaders") %>%
        rvest::html_nodes(".pivotTableCellWrap") %>%
        rvest::html_text() %>%
        str_squish()
    
    row_vals1 <- tables[[1]] %>%
        rvest::html_nodes(".bodyCells") %>%
        rvest::html_nodes(".pivotTableCellWrap") %>%
        rvest::html_text() %>%
        str_squish() %>%
        string_to_clean_numeric()
    
    df_ <- as_tibble(as.data.frame(t(c(row_vals, row_vals1))))
    names(df_) <- c(col_names, col_names1)
    
    df_
}

lasd_staff_extract <- function(x){
    x %>%
        select(
            Staff.Confirmed = Total,
            Staff.Quarantine = `Personnel Currently Quarantined`
        ) %>%
        mutate(Name = "LA Jail")
}

#' Scraper class for general LASD staff COVID data
#' 
#' @name lasd_staff_scraper
#' @description Info comes from a Microsoft power bi app that can be
#' temperamental as load times vary and Selenium can not tell when the DOM is
#' ready. May need to run a couple of times to get data.
#' \describe{
#'   \item{Personnel Currently Quarantined}{}
#'   \item{Personnel Currently High Risk}{}
#'   \item{Returned to Work}{Not neccesarily positive}
#'   \item{Personnel Affected Since Inception}{}
#'   \item{Prof. Confirmed}{Staff type distinction of confirmed cases}
#'   \item{Sworn Confirmed}{Staff type distinction of confirmed cases}
#'   \item{Total Confirmed}{Total staff confirmed cases}
#' }

lasd_staff_scraper <- R6Class(
    "lasd_staff_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://lasd.org/covid19updates/",
            id = "lasd_staff",
            type = "html",
            state = "CA",
            jurisdiction = "county",
            check_date = lasd_staff_date_check,
            # pull the JSON data directly from the API
            pull_func = lasd_staff_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = lasd_staff_restruct,
            # Rename the columns to appropriate database names
            extract_func = lasd_staff_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    lasd_staff <- lasd_staff_scraper$new(log=TRUE)
    lasd_staff$run_check_date()
    lasd_staff$raw_data
    lasd_staff$pull_raw()
    lasd_staff$raw_data
    lasd_staff$save_raw()
    lasd_staff$restruct_raw()
    lasd_staff$restruct_data
    lasd_staff$extract_from_raw()
    lasd_staff$extract_data
    lasd_staff$validate_extract()
    lasd_staff$save_extract()
}

