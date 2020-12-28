source("./R/generic_scraper.R")
source("./R/utilities.R")

oregon_testing_pull <- function(x){
    
    app_url <- "https://public.tableau.com/views/ODOCCovid-19TestResultDates" %>%
        str_c(
            "/ODOCCOVID-19Testing?%3Aembed=y&%3AshowVizHome=no&%3A",
            "display_count=y&%3Adisplay_static_image=y&%3A",
            "bootstrapWhenNotified=true&%3Alanguage=en&publish=yes&:",
            "embed=y&:showVizHome=n&:apiID=host0#navType=0&navSrc=Parse")
    
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
            "text/plain,text/csv,application/csv,application/download,application/octet-stream",
        pdfjs.disabled = TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = 99L))
    
    out_file <- "/tmp/sel_dl/ODOC_COVID-19_Testing_data.csv"
    
    if(file.exists(out_file)){
        file.remove(out_file)
    }
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox",
        extraCapabilities=fprof
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_url)
    Sys.sleep(6)
    
    old_windows <- unlist(remDr$getWindowHandles())
    
    remDr$findElement(
        "css", "[id='download-ToolbarButton']")$clickElement()
    Sys.sleep(10)
    remDr$findElement(
        "css", "[data-tb-test-id='DownloadData-Button']")$clickElement()
    Sys.sleep(10)
    
    new_window <- setdiff(unlist(remDr$getWindowHandles()), old_windows)
    remDr$switchToWindow(new_window)
    Sys.sleep(10)
    
    remDr$findElement(
        "css", "[class='csvLink_summary']")$clickElement()
    Sys.sleep(10)
    
    if(!file.exists(out_file)){
        stop("OR testing unable to download csv")
    }
    
    return(read_csv(out_file, col_types = cols()))
}

oregon_testing_restruct <- function(x){
    x
}

oregon_testing_extract <- function(x){
    
    exp_names <- c(
        "Name" = "Institution",
        "Type" = "Type",
        "Date" = "Month, Day, Year of Test Result Date",
        "Value" = "Count of Sheet1"
    )
    
    df_ <- x
    check_names(df_, exp_names)
    names(df_) <- names(exp_names)
    
    df_ %>%
        group_by(Name, Type) %>%
        summarize(Value = sum(Value)) %>%
        ungroup() %>%
        pivot_wider(names_from = Type, values_from = "Value") %>%
        rename(Staff.Tested = Employee, Residents.Tadmin = AIC)
}

#' Scraper class for general oregon_testing COVID data
#' 
#' @name oregon_testing_scraper
#' @description This will be a description of oregon_testing data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

oregon_testing_scraper <- R6Class(
    "oregon_testing_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://public.tableau.com/profile/josh4372#!/vizhome/ODOCCovid-19TestResultDates/ODOCCOVID-19Testing?publish=yes",
            id = "oregon_testing",
            type = "csv",
            state = "OR",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = oregon_testing_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = oregon_testing_restruct,
            # Rename the columns to appropriate database names
            extract_func = oregon_testing_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    oregon_testing <- oregon_testing_scraper$new(log=TRUE)
    oregon_testing$raw_data
    oregon_testing$pull_raw()
    oregon_testing$raw_data
    oregon_testing$save_raw()
    oregon_testing$restruct_raw()
    oregon_testing$restruct_data
    oregon_testing$extract_from_raw()
    oregon_testing$extract_data
    oregon_testing$validate_extract()
    oregon_testing$save_extract()
}

