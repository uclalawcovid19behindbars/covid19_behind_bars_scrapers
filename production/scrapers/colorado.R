source("./R/generic_scraper.R")
source("./R/utilities.R")

colorado_pull <- function(x){
    
    app_src <- xml2::read_html(x) %>%
        rvest::xml_node("iframe") %>%
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
    
    out_file <- "/tmp/sel_dl/Story 1.pdf"
    
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
        stop("CO unable to download pdf")
    }
    
    return(out_file)
}

colorado_restruct <- function(x){
    magick::image_read_pdf(x) %>%
        ExtractTable()
}

colorado_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "TESTS", "0", "OName.Residents.Tested",
        "", "1", "Residents.Tested",
        "POSITIVES", "2", "OName.Residents.Confirmed",
        "", "3", "Residents.Confirmed",
        "RECOVERED", "4", "OName.Residents.Recovered",
        "", "5", "Residents.Recovered",
        "DEATHS", "6", "OName.Residents.Deaths",
        "", "7", "Residents.Deaths",
        "TOTAL TESTED", "8", "Drop"
    ), ncol = 3, nrow = 9, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    check_names_extractable(x[[1]], col_name_df)
    
    lean_df <- rename_extractable(x[[1]], col_name_df) %>%
        as_tibble() %>%
        select(-Drop) %>%
        .[3:nrow(.),] %>%
        mutate_at(names(.)[str_starts("OName", names(.))], clean_fac_col_txt)
    
    CO_names <- lean_df %>%
        select(starts_with("OName")) %>%
        as.matrix()
    
    co_test <- apply(CO_names, 1, function(x) length(unique(x)) != 1)
    
    for(i in 1:length(co_test)){
        if(co_test[i]){
            bnames <- unname(CO_names[i,])
            warning(str_c(
                "Inconsistent names found: ", str_c(bnames, collapse = ", ")))
        }
    }
    
    lean_df %>%
        mutate(Name = OName.Residents.Tested) %>%
        select(-starts_with("OName")) %>%
        clean_scraped_df()
}

#' Scraper class for general Colorado COVID data
#' 
#' @name colorado_scraper
#' @description Colorado data is pulled from a tableau app which is known
#' to be temperamental. Sometimes code will need to be run several times in
#' order to work as the page has variable load times and selenium can not
#' tell when the DOM is ready. Data is downloaded through selenium in a pdf form
#' from which OCR is run.
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Tests}{Total tests administered.}
#'   \item{Positives}{Residents with psoitive tests, not neccesarily by DOC.}
#'   \item{Recovered}{Inidviduals who have recovered after positive test.}
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
            url = "https://www.colorado.gov/pacific/cdoc/covid-19-faq-and-updates",
            id = "colorado",
            type = "pdf",
            state = "CO",
            # pull the JSON data directly from the API
            pull_func = colorado_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = colorado_restruct,
            # Rename the columns to appropriate database names
            extract_func = colorado_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    colorado <- colorado_scraper$new(log=TRUE)
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

