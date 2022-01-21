source("./R/generic_scraper.R")
source("./R/utilities.R")

colorado_check_date <- function(url, date = Sys.Date()){
    base_page <- xml2::read_html(url)
    
    site_date <- base_page %>%
        rvest::html_node("article") %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>%
        {.[str_detect(., "(?i)last updated") & str_detect(., "(?i)covid")]} %>%
        str_split("(?i)updated") %>%
        unlist() %>%
        last() %>%
        str_squish() %>%
        lubridate::mdy() 
    
    error_on_date(site_date, date)
}


colorado_pull <- function(url){
  
    # these are the names of the csv tabs we want dowloaded
    download_labs <- c(
        "COVIDCNTSDEATHRFAC", "COVIDCNTSACTVFAC", "COVIDCNTSTESTRFAC",
        "COVIDCNTSTOTPOSFAC", "COVIDINMVAC", "COVIDCNTSDEATHRFAC (2)"
    )
    
    # go back and check to see if old files exist and delete them if tehy do
    for(dls in download_labs){
        out_file <- stringr::str_c("/tmp/sel_dl/", dls, ".csv")
        if(file.exists(out_file)){
            file.remove(out_file)
        }
    }
    
    # need this string to tell selenium we just want to download the csv
    csv_no_display_str <- str_c(
      "application/csv,application/excel,application/vnd.ms-excel,",
      "application/vnd.msexcel,text/anytext,text/comma-separated-values,",
      "text/csv,text/plain,text/x-csv,application/x-csv,",
      "text/x-comma-separated-values,text/tab-separated-values,",
      "data:text/csv,application/xml,text/plain,text/xml,image/jpeg,",
      "application/octet-stream,data:text/csv"
    )
    
    # set up the ff profile to tell selenium just to download csvs and not 
    # bring up a prompt or display them
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
        browser.helperApps.neverAsk.saveToDisk = csv_no_display_str,
        browser.helperApps.neverAsk.openFile = csv_no_display_str,
        pdfjs.disabled = TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = "99.0"))
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox",
        extraCapabilities=fprof
    )
    
    # Suppress terminal output
    capture.output(remDr$open())
    remDr$navigate(url)
    Sys.sleep(2)
    
    tableau_url <- remDr$findElement(
      "xpath", "//iframe")$getElementAttribute('src')[[1]]
    remDr$navigate(tableau_url)
    Sys.sleep(2)
    
    # we want to loop through the dialogue options and get all the csvs
    # downloaded
    for(dls in download_labs){
        remDr$findElement(
            "css", "[id='download-ToolbarButton']")$clickElement()
        Sys.sleep(2)
        remDr$findElement(
            "css", "[data-tb-test-id='DownloadCrosstab-Button']")$clickElement()
        Sys.sleep(2)
        remDr$findElement(
          using = 'xpath',
          stringr::str_c("//div[@title='", dls, "']"))$clickElement()
        Sys.sleep(2)
        remDr$findElement(
          using = 'xpath',
          "//label[@data-tb-test-id='crosstab-options-dialog-radio-csv-Label']"
          )$clickElement()
        Sys.sleep(2)
        remDr$findElement(
            using = 'xpath', "//button[contains(text(),'Download')]"
            )$clickElement()
        Sys.sleep(4)
    }

    # Tquit our current selenium instance
    remDr$quit()
    
    # lets aggregate the files into one readable file,
    # its annoying because they are actually tsvs with weird encodings but 
    # we can get around the weirdness by setting the encoding to UTF-16LE
    fac_df <- bind_rows(lapply(
      download_labs[str_detect(download_labs, "FAC")], function(dls){
      read_tsv(
        stringr::str_c("/tmp/sel_dl/", dls, ".csv"),
        skip = 1, locale = locale(encoding="UTF-16LE"), col_names = FALSE)
    })) %>%
      bind_rows(
          read_tsv(
            stringr::str_c("/tmp/sel_dl/COVIDINMVAC.csv"),
            skip = 2, locale = locale(encoding="UTF-16LE"), col_names = F) %>%
            mutate(X1 = "STATEWIDE", X2 = "Vaccinated")) %>%
      pivot_wider(names_from = X2, values_from = X3) %>%
      rename(Name = "X1")
    
    return(fac_df)
}

colorado_restruct <- function(scraped_csv){
    scraped_csv
}

colorado_extract <- function(restructured_data){
    
    df_ <- restructured_data
    
    exp_names <- c(
        Name = "Name",
        Deaths.Due.Drop = "Deaths",
        Residents.Active = "Active",
        Residents.Tadmin = "Tested",
        Residents.Confirmed = "Positive",
        Deaths.Among.Drop = "Deaths Among", 
        Residents.Initiated = "Vaccinated"
    )
    
    check_names(df_, exp_names)
    
    names(df_) <- names(exp_names)
    
    df_ %>%
        mutate(Residents.Deaths = Deaths.Due.Drop + Deaths.Among.Drop, 
               Residents.Recovered = 
                   Residents.Confirmed - Residents.Active - Residents.Deaths) %>% 
      select(-ends_with("Drop"))
}

#' Scraper class for general Colorado COVID data
#' 
#' @name colorado_scraper
#' @description Colorado data is pulled from a tableau app which is known
#' to be temperamental. Sometimes code will need to be run several times in
#' order to work as the page has variable load times and selenium can not
#' tell when the DOM is ready. Data is downloaded through selenium into multiple
#' tsvs which are then collated into a single data object which is saved as a
#' csv.
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Tests}{Total tests administered.}
#'   \item{Positives}{Residents with psoitive tests, not neccesarily by DOC.}
#'   \item{Active}{Inidviduals with active cases.}
#'   \item{Deaths}{Symptoms or positive test, not coroners office.}
#'   \item{Offenders Tested}{Number of residents tested.}
#' }

colorado_scraper <- R6Class(
    "colorado_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://cdoc.colorado.gov/resources/covid-19-faq-and-updates",
            id = "colorado",
            type = "csv",
            state = "CO",
            jurisdiction = "state",
            check_date = colorado_check_date,
            # pull the JSON data directly from the API
            pull_func = colorado_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = colorado_restruct,
            # Rename the columns to appropriate database names
            extract_func = colorado_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    colorado <- colorado_scraper$new(log=F)
    colorado$run_check_date()
    colorado$raw_data
    colorado$pull_raw()
    colorado$raw_data
    colorado$save_raw()
    colorado$restruct_raw()
    colorado$restruct_data
    colorado$extract_from_raw()
    colorado$extract_data
    colorado$validate_extract()
    colorado$save_extract()
}
