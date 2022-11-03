source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

california_check_date <- function(x, date = Sys.Date()){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96")
    
    remDr <- initiate_remote_driver()
    
    remDr$open(silent = TRUE)
    remDr$navigate(y)
    
    Sys.sleep(10)
    
    base_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    site_date <- base_page %>%
        rvest::html_nodes("title") %>%
        rvest::html_text() %>%
        {.[str_starts(., "(?i)data last updated")]} %>%
        str_remove("(?i)data last updated: ") %>%
        lubridate::mdy_hm() %>%
        lubridate::floor_date(unit="day") %>%
        as.Date()
    
    remDr$close()
    
    error_on_date(site_date, date)
}

california_pull <- function(x, wait = 20){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    out_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    remDr$close()
    
    out_html
}

california_pull_col <- function(html,num) {
    header_front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[2]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[2]/div[2]/div['
    header_end_xpath <- ']/div'
    front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[2]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    middle_xpath <- ']/div['
    end_xpath <- ']'
    header <- html %>%
        rvest::html_nodes(xpath = str_c(header_front_xpath, num, header_end_xpath)) %>%
        rvest::html_text() %>%
        str_squish()
    column <- do.call(rbind, lapply(1:22, function(x) html %>% rvest::html_nodes(xpath = str_c(front_xpath, x, middle_xpath, num+1, end_xpath)) %>% rvest::html_text())) %>% as.data.frame()
    colnames(column) <- header
    return(column)
    
}

california_restruct <- function(x, date = Sys.Date()){
    
    name.abb <- x %>%
        california_pull_col(html = ., num = 1) %>%
        mutate(merge.no = 1:22)
    name.full <- x %>%
        california_pull_col(html = ., num = 2)%>%
        mutate(merge.no = 1:22)
    res.confirmed <- x %>%
        california_pull_col(html = ., num = 3)%>%
        mutate(merge.no = 1:22)
    res.new <- x %>%
        california_pull_col(html = ., num = 4)%>%
        mutate(merge.no = 1:22)
    res.active <- x %>%
        california_pull_col(html = ., num = 5)%>%
        mutate(merge.no = 1:22)
    res.release.active <- x %>%
        california_pull_col(html = ., num = 6)%>%
        mutate(merge.no = 1:22)
    res.resolved <- x %>%
        california_pull_col(html = ., num = 7)%>%
        mutate(merge.no = 1:22)
    res.deaths <- x %>%
        california_pull_col(html = ., num = 8)%>%
        mutate(merge.no = 1:22)
    
    out_data <- name.abb %>%
        left_join(name.full, by = 'merge.no') %>%
        left_join(res.confirmed, by = 'merge.no') %>%
        left_join(res.new, by = 'merge.no') %>%
        left_join(res.active, by = 'merge.no') %>%
        left_join(res.release.active, by = 'merge.no') %>%
        left_join(res.resolved, by = 'merge.no') %>%
        left_join(res.deaths, by = 'merge.no') %>%
        select(-merge.no) %>%
        select(-Institution) %>%
        rename(Name = "Institution Name") %>%
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        as_tibble()
    
}

california_extract <- function(x){
    ext <- c(
        "Name", "Confirmed", "New In Last 14 Days", "Active In Custody",
        "Released While Active", "Resolved", "Deaths"
    )
    
    check_names(x, ext)
    
    ext_df <- x
    names(ext_df) <- c(
        "Name", "Residents.Confirmed", "Drop.New", "Residents.Active",
        "Drop.Released", "Residents.Recovered", "Residents.Deaths")
    
    ext_df %>%
        select(-contains("Drop"))
}

#' Scraper class for general California COVID data
#' 
#' @name california_scraper
#' @description California data scraped from rendered power BI iframe. Testing
#' data is also available at the facility level but will need to be scraped
#' separately. Note that time series data for the state exists for comparison.
#' \describe{
#'   \item{Institution Name}{The facility name.}
#'   \item{Confirmed}{The number of confimed cases among residents.}
#'   \item{New In Last 14 Days}{The number of new cases among residents.}
#'   \item{Active in Custody}{The number of active cases.}
#'   \item{Released While Active}{Residents released after testing positive.}
#'   \item{Resolved}{Recovered cases.}
#'   \item{Deaths}{Resident Deaths.}
#' }

california_scraper <- R6Class(
    "california_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/updates/",
            id = "california",
            type = "html",
            state = "CA",
            jurisdiction = "state",
            check_date = california_check_date,
            # pull the JSON data directly from the API
            pull_func = california_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california <- california_scraper$new(log=TRUE)
    california$run_check_date()
    california$raw_data
    california$pull_raw()
    california$raw_data
    california$save_raw()
    california$restruct_raw()
    california$restruct_data
    california$extract_from_raw()
    california$extract_data
    california$validate_extract()
    california$save_extract()
}

