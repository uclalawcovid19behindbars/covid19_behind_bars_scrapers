source("./R/generic_scraper.R")
source("./R/utilities.R")

alaska_vaccine_check_date <- function(x, date = Sys.Date()){
    fprof <- RSelenium::makeFirefoxProfile(list(
        browser.startup.homepage = "about:blank",
        startup.homepage_override_url = "about:blank",
        startup.homepage_welcome_url = "about:blank",
        startup.homepage_welcome_url.additional = "about:blank",
        browser.download.dir = "/home/seluser/Downloads",
        browser.download.folderList = 2L,
        browser.download.manager.showWhenStarting = FALSE,
        browser.download.manager.focusWhenStarting = FALSE,
        browser.download.manager.closeWhenDone = TRUE,
        browser.helperApps.neverAsk.saveToDisk = 
            "application/pdf, application/octet-stream",
        pdfjs.disabled = TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = 99L))
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox",
        extraCapabilities=fprof
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(x)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()
    
    base_page <- xml2::read_html(base_html[[1]])
    
    vacc_tab <- base_page %>%
        rvest::html_node("#vaccine_tracker") %>%
        rvest::html_table()
    
    site_date <- names(vacc_tab)[1] %>%
        str_remove("(?i)as of ") %>%
        lubridate::mdy()
    
    error_on_date(date, site_date)
}

alaska_vaccine_pull <- function(x){
    fprof <- RSelenium::makeFirefoxProfile(list(
        browser.startup.homepage = "about:blank",
        startup.homepage_override_url = "about:blank",
        startup.homepage_welcome_url = "about:blank",
        startup.homepage_welcome_url.additional = "about:blank",
        browser.download.dir = "/home/seluser/Downloads",
        browser.download.folderList = 2L,
        browser.download.manager.showWhenStarting = FALSE,
        browser.download.manager.focusWhenStarting = FALSE,
        browser.download.manager.closeWhenDone = TRUE,
        browser.helperApps.neverAsk.saveToDisk = 
            "application/pdf, application/octet-stream",
        pdfjs.disabled = TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = 99L))
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox",
        extraCapabilities=fprof
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(x)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()
    
    xml2::read_html(base_html[[1]])
}

alaska_vaccine_restruct <- function(x){
    vacc_tab <- x %>%
        rvest::html_node("#vaccine_tracker") %>%
        rvest::html_table()
    
    # remove the name columns
    names(vacc_tab) <- unlist(vacc_tab[1,])
    
    as_tibble(vacc_tab[-1,])
}

alaska_vaccine_extract <- function(x){
    x %>%
        rename(
            Name = Facility,
            Residents.Initiated = `# inmates at least 1 dose`,
            Residents.Completed = `# inmates completed series`, 
            Residents.Population = `# inmates per facility`) %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        clean_scraped_df()
}

#' Scraper class for general alaska_vaccine COVID data
#' 
#' @name alaska_vaccine_scraper
#' @description Alaska vaccine data does not show up in the static html and
#' needs to be loaded through selenium. Not sure why thats the case and cant
#' tell where in the the network calls this is happening. Nevertheless, the
#' data provided is detailed and covers all vars we want to report on.
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Vax given}{Residents.Vadmin}
#'   \item{# individual inmates}{Residents.Initiated}
#'   \item{# completed series}{Residents.Completed}
#' }

alaska_vaccine_scraper <- R6Class(
    "alaska_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.alaska.gov/covid-19",
            id = "alaska_vaccine",
            type = "html",
            state = "AK",
            jurisdiction = "state",
            check_date = alaska_vaccine_check_date,
            # pull the JSON data directly from the API
            pull_func = alaska_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = alaska_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = alaska_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    alaska_vaccine <- alaska_vaccine_scraper$new(log=TRUE)
    alaska_vaccine$run_check_date()
    alaska_vaccine$perma_save()
    alaska_vaccine$raw_data
    alaska_vaccine$pull_raw()
    alaska_vaccine$raw_data
    alaska_vaccine$save_raw()
    alaska_vaccine$restruct_raw()
    alaska_vaccine$restruct_data
    alaska_vaccine$extract_from_raw()
    alaska_vaccine$extract_data
    alaska_vaccine$validate_extract()
    alaska_vaccine$save_extract()
}

