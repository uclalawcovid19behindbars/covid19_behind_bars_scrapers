source("./R/generic_scraper.R")
source("./R/utilities.R")

wisconsin_pull <- function(x){
   app_src <- "https://public.tableau.com/views/WIDOCCOVID19/COVID-19Table?" %>%
       str_c(
           "%3Aembed=y&%3AshowVizHome=no&%3Ahost_url=https%3A%2F%2F",
           "public.tableau.com%2F&%3Aembed_code_version=3&%3Atabs=yes",
           "&%3Atoolbar=no&%3Aanimate_transition=yes&%3A",
           "display_static_image=no&%3Adisplay_spinner=no",
           "&%3Adisplay_overlay=yes&%3Adisplay_count=yes&%3Alanguage=en",
           "&%3AloadOrderID=0")
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
   remDr$navigate(app_src)
   Sys.sleep(6)
   
   out_file <- "/tmp/sel_dl/COVID-19 Table.pdf"
   
   if(file.exists(out_file)){
       file.remove(out_file)
   }
   
   remDr$findElement(
       "css", "[id='download-ToolbarButton']")$clickElement()
   Sys.sleep(10)
   remDr$findElement(
       "css", "[data-tb-test-id='DownloadPdf-Button']")$clickElement()
   Sys.sleep(10)
   remDr$findElement( 
       "css", "[data-tb-test-id='PdfDialogCreatePdf-Button']")$clickElement()
   Sys.sleep(10)
   
   if(!file.exists(out_file)){
       stop("WI unable to download image")
   }
   
   return(out_file)
   
}

wisconsin_restruct <- function(x){
    pdf_area <- list(c(104.64249, 43.36788, 431.90674, 566.58031))
    
    tabulizer::extract_tables(
        x, pages = 1, area = pdf_area)[[1]]
    
}

wisconsin_extract <- function(x){
    
    list_dat <- x
    
    col_names <- apply(list_dat[1:2,], 2, str_c, collapse=" ") %>%
        str_trim() %>%
        # Facility doesnt have a name
        {ifelse(. == "", "Facility", .)} %>%
        str_replace_all(" ", ".")
    
    colnames(list_dat) <- col_names
    
    wis_df <- list_dat[-(1:2),] %>%
        as_tibble() %>%
        rename(Name=Facility) %>%
        clean_scraped_df()
    
    exp_names <- c(
        "Name", "Positive.Tests", "Negative.Tests", "Total.Tests",
        "Released.Positive.Cases", "Active.Positive.Cases",
        "Inactive.PositiveCases")
    
    check_names(wis_df, exp_names)
    
    ext_df <- wis_df
    names(ext_df) <- c(
        "Name", "Residents.Confirmed", "Residents.Negative",
        "Residents.Tadmin", "Drop.Release", "Residents.Active", "Residents.Recovered"
    )
    
    ext_df %>%
        select(-contains("Drop")) %>%
        filter(!str_detect(Name, "(?i)total"))
}

#' Scraper class for general wisconsin COVID data
#' 
#' @name wisconsin_scraper
#' @description WI comes from a tableau table which is downloaded as a pdf.
#' Currently the downloading of this pdf is done on a private server run by
#' marquezn at law.ucla.edu because of RSelenium requirements. This should be
#' changed when a dedicated server is built. 
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Positive tests}{Cumulative positive test count for residents.}
#'   \item{Negative tests}{Cumulative negative test count for residents.}
#'   \item{Total tests}{Cumulative tests administered for residents.}
#'   \item{Released Positive Cases}{Cumulative residents released while positive.}
#'   \item{Active Positive Cases}{Residents with active cases.}
#'   \item{Inactive Positive Cases}{Residents recovered but the wording changed 10/9 from recovered to inactive.}
#' }

wisconsin_scraper <- R6Class(
    "wisconsin_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "https://doc.wi.gov/Pages/COVID19%28Coronavirus%29/COVID19TestingDashboard.aspx",
            id = "wisconsin",
            type = "pdf",
            state = "WI",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = wisconsin_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wisconsin_restruct,
            # Rename the columns to appropriate database names
            extract_func = wisconsin_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin <- wisconsin_scraper$new(log=TRUE)
    wisconsin$raw_data
    wisconsin$pull_raw()
    wisconsin$raw_data
    wisconsin$save_raw()
    wisconsin$restruct_raw()
    wisconsin$restruct_data
    wisconsin$extract_from_raw()
    wisconsin$extract_data
    wisconsin$validate_extract()
    wisconsin$save_extract()
}

