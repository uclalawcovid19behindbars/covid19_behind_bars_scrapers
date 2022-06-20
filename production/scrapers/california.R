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

california_pull <- function(x, wait = 10){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI",
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&",
            "pageName=ReportSection90204f76f18a02b19c96")
    
    remDr <- initiate_remote_driver()
    # Create Subdirectory for CA HTML output
    sub_dir <- str_c("./results/raw_files/", Sys.Date(), "_california")
    dir.create(sub_dir, showWarnings = FALSE)
    html_list <- list()
    # Open RemDr
    remDr$open(silent = TRUE)
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    remDr$screenshot(TRUE)
    webEls <- remDr$findElements(value="//div[@title='Institution']")
    webEls[[1]]$clickElement()
    Sys.sleep(wait)
    remDr$screenshot(TRUE)
    html_list[["first"]] <- xml2::read_html(remDr$getPageSource()[[1]])
    
    webEls <- remDr$findElements(value="//div[@title='Institution']")
    webEls[[1]]$clickElement()
    Sys.sleep(wait)
    remDr$screenshot(TRUE)
    html_list[["last"]] <- xml2::read_html(remDr$getPageSource()[[1]])
    
    fns <- str_c(sub_dir, "/", names(html_list), ".html")
    
    Map(xml2::write_html, html_list, fns)
    
    iframes <- lapply(str_remove(fns, "/results/raw_files"), function(fn)
        htmltools::tags$iframe(
            src = fn, 
            style="display:block", 
            height="500", width="1200"
        )  
    )
    
    x <- htmltools::tags$html(
        htmltools::tags$body(
            iframes
        )
    )
    
    tf <- tempfile(fileext = ".html")
    
    htmltools::save_html(x, file = tf)
    
    remDr$close()
    
    xml2::read_html(tf)
    
}

pull_california_column <- function(html, length, name, colno) {
    xbase <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[2]/transform/div/div[3]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/'   
    info <- do.call(rbind, lapply(1:length, function(p, x) {
        x <- html
        select <- xbase %>%
            str_c(., 'div[', p, ']/div[', colno, ']') # 3 designates the name column
        out <- x %>%
            rvest::html_nodes(xpath = select) %>%
            rvest::html_text()})) %>%
        as.data.frame() 
    
    colnames(info) <- name
    info <- info %>%
        mutate(rowid = 1:length)
    
    info
    
}

california_restruct <- function(x, date = Sys.Date()){
    baseraw <- str_c('./results/raw_files/', Sys.Date(), '_california/')
    firsttable <- baseraw %>%
        str_c(., 'first.html') %>%
        rvest::read_html()
    lasttable <- baseraw %>%
        str_c(., 'last.html') %>%
        rvest::read_html()
    # Pull First Table Section
    first.name <- pull_california_column(html = firsttable, length = 22, name = 'Name', colno = 3)
    first.confirmed <- pull_california_column(html = firsttable, length = 22, name = 'Residents.Confirmed', colno = 4)
    first.dropnew <- pull_california_column(html = firsttable, length = 22, name = 'Drop.New', colno = 5)
    first.active <- pull_california_column(html = firsttable, length = 22, name = 'Residents.Active', colno = 6)
    first.ractive <- pull_california_column(html = firsttable, length = 22, name = 'Released.Active', colno = 7)
    first.recovered <- pull_california_column(html = firsttable, length = 22, name = 'Residents.Recovered', colno = 8)
    first.deaths <- pull_california_column(html = firsttable, length = 22, name = 'Residents.Deaths', colno = 9)
    dat_first <- first.name %>%
        merge(first.confirmed, by = 'rowid') %>%
        merge(first.dropnew, by = 'rowid') %>%
        merge(first.active, by = 'rowid') %>%
        merge(first.ractive, by = 'rowid') %>%
        merge(first.recovered, by = 'rowid') %>%
        merge(first.deaths, by = 'rowid') %>%
        select(-c(rowid)) %>%
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        as_tibble()
    # Pull Last Table Section
    last.name <- pull_california_column(html = lasttable, length = 22, name = 'Name', colno = 3)
    last.confirmed <- pull_california_column(html = lasttable, length = 22, name = 'Residents.Confirmed', colno = 4)
    last.dropnew <- pull_california_column(html = lasttable, length = 22, name = 'Drop.New', colno = 5)
    last.active <- pull_california_column(html = lasttable, length = 22, name = 'Residents.Active', colno = 6)
    last.ractive <- pull_california_column(html = lasttable, length = 22, name = 'Released.Active', colno = 7)
    last.recovered <- pull_california_column(html = lasttable, length = 22, name = 'Residents.Recovered', colno = 8)
    last.deaths <- pull_california_column(html = lasttable, length = 22, name = 'Residents.Deaths', colno = 9)
    dat_last <- last.name %>%
        merge(last.confirmed, by = 'rowid') %>%
        merge(last.dropnew, by = 'rowid') %>%
        merge(last.active, by = 'rowid') %>%
        merge(last.ractive, by = 'rowid') %>%
        merge(last.recovered, by = 'rowid') %>%
        merge(last.deaths, by = 'rowid') %>%
        select(-c(rowid)) %>%
        mutate_at(vars(-Name), string_to_clean_numeric) %>%
        as_tibble()
    
    dat_combined <- dat_first %>%
        rbind(dat_last) %>%
        unique()
    
    pull_check <- dat_combined %>%
        row.names() %>%
        length()
    
    if(pull_check < 35) {
        warning('DANGER, WILL ROBINSON!: Fewer than  35 facility observations were pulled for CA - typically 35 facility obs are pulled for this scraper.')
    }
    
    dat_combined
    
}

california_extract <- function(x){
    
    ext_df <- x
    
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

