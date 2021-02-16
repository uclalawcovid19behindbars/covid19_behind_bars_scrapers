source("./R/generic_scraper.R")
source("./R/utilities.R")

rhode_island_pull <- function(x){
    
    app_source <- xml2::read_html(x) %>%
        rvest::xml_nodes("iframe") %>%
        rvest::html_attr("src")
        
    
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
            "text/csv",
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
    remDr$navigate(app_source)
    Sys.sleep(6)
    
    out_file <- "/tmp/sel_dl/Grid view.csv"
    
    if(file.exists(out_file)){
        file.remove(out_file)
    }
    
    remDr$findElement("xpath", "//*[text()='Download CSV']")$clickElement()
    Sys.sleep(6)
    
    if(!file.exists(out_file)){
        stop("RI unable to download csv")
    }

    return(read_csv(out_file, col_types = cols()))
}

rhode_island_restruct <- function(x){
    x %>%
        extract(`Deaths`, into = c("n_deaths", "death_group"), "([0-9]+)\\s+\\(([^)]+)") %>%
        mutate(`Facility staff` = string_to_clean_numeric(`Facility staff`)) %>%
        mutate(`Facility residents` = 
                   string_to_clean_numeric(`Facility residents`)) %>%
        filter(!is.na(`Facility residents`)) %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        mutate(Residents.Deaths = ifelse(str_detect(death_group, "(?i)inmate"), n_deaths, NA),
               Residents.Deaths = string_to_clean_numeric(Residents.Deaths),
               Staff.Deaths = ifelse(str_detect(death_group, "(?i)staff"), n_deaths, NA),
               Staff.Deaths = string_to_clean_numeric(Staff.Deaths))
}

rhode_island_extract <- function(x){
    x %>%
        select(
            Name, 
            Residents.Confirmed = `Facility residents`,
            Staff.Confirmed = `Facility staff`,
            Residents.Deaths,
            Staff.Deaths)
}

#' Scraper class for general rhode_island COVID data
#' 
#' @name rhode_island_scraper
#' @description This will be a description of rhode_island data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

rhode_island_scraper <- R6Class(
    "rhode_island_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.doc.ri.gov/covid-19/",
            id = "rhode_island",
            type = "csv",
            state = "RI",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = rhode_island_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = rhode_island_restruct,
            # Rename the columns to appropriate database names
            extract_func = rhode_island_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    rhode_island <- rhode_island_scraper$new(log=TRUE)
    rhode_island$raw_data
    rhode_island$pull_raw()
    rhode_island$raw_data
    rhode_island$save_raw()
    rhode_island$restruct_raw()
    rhode_island$restruct_data
    rhode_island$extract_from_raw()
    rhode_island$extract_data
    rhode_island$validate_extract()
    rhode_island$save_extract()
}

