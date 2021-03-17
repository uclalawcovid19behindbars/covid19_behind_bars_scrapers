source("./R/generic_scraper.R")
source("./R/utilities.R")

maryland_vaccine_pull <- function(x){
    src_url <- str_c(
        "https://app.powerbigov.us/view?r=", 
        "eyJrIjoiMjdlZjZmNzAtOGYxNS00ODA4LThhMzktOGYyZDEw", 
        "YTMwMDZkIiwidCI6IjYwYWZlOWUyLTQ5Y2QtNDliMS04ODUx", 
        "LTY0ZGYwMjc2YTJlOCJ9&pageName=ReportSection&pageName=ReportSection", 
        "4b1b308960abe0bc0ce7")

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
    remDr$navigate(src_url)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()
    
    xml2::read_html(base_html[[1]])
}

maryland_vaccine_restruct <- function(x){
        
    tab <- x %>% 
        rvest::html_node(xpath = "//*[contains(@title,'Staff Vaccinations')]/parent::*") %>% 
        # Help :(
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
    
    dat_df 
}

maryland_vaccine_extract <- function(x){
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
#' @name maryland_vaccine_scraper
#' @description Vaccine data from MD is pulled from the fourth table on their Power BI 
#' dashboard. Statewide totals for first and second shot for staff and incarcerated 
#' people are reported. The dashboard also lists vaccine requirement status, 
#' eligibility group, and the number of eligible inmates. 
#' \describe{
#'   \item{Staff Vaccinations First Shot}{}
#'   \item{Staff Vaccinations Second Shot}{}
#'   \item{Inmate Vaccinations First Shot}{}
#'   \item{Inmate Vaccinations SEcond Shot}{}
#' }

maryland_vaccine_scraper <- R6Class(
    "maryland_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://news.maryland.gov/dpscs/covid-19/",
            id = "maryland_vaccine",
            type = "html",
            state = "MD",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = maryland_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maryland_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = maryland_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    maryland_vaccine <- maryland_vaccine_scraper$new(log=TRUE)
    maryland_vaccine$raw_data
    maryland_vaccine$pull_raw()
    maryland_vaccine$raw_data
    maryland_vaccine$save_raw()
    maryland_vaccine$restruct_raw()
    maryland_vaccine$restruct_data
    maryland_vaccine$extract_from_raw()
    maryland_vaccine$extract_data
    maryland_vaccine$validate_extract()
    maryland_vaccine$save_extract()
}

