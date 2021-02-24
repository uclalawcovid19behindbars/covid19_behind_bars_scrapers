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
    z <-  magick::image_read_pdf(x, pages = 1)
    
    col_names <- clean_fac_col_txt(c(
        z %>%
            magick::image_crop("423x50+177+484") %>%
            magick::image_ocr(),
        z %>%
            magick::image_crop("423x50+616+484") %>%
            magick::image_ocr(),
        z %>%
            magick::image_crop("423x50+1055+484") %>%
            magick::image_ocr(),
        z %>%
            magick::image_crop("423x50+1494+484") %>%
            magick::image_ocr()
        ))

    # extract tables struggles to pull some of this data so
    # by cropping it first we can ensure greater consistency.
    # that is as long as CO keeps the same formatting.
    col_vals <- cbind(
        z %>%
            magick::image_crop("213x1080+387+1030") %>%
            ExtractTable() %>%
            unlist() %>%
            string_to_clean_numeric(),

        z %>%
            magick::image_crop("213x1080+824+1030") %>%
            ExtractTable() %>%
            unlist() %>%
            string_to_clean_numeric(),

        z %>%
            magick::image_crop("213x1080+1264+1030") %>%
            ExtractTable() %>%
            unlist() %>%
            string_to_clean_numeric(),

        z %>%
            magick::image_crop("213x1080+1700+1030") %>%
            ExtractTable() %>%
            unlist() %>%
            string_to_clean_numeric())
    
    colnames(col_vals) <- col_names
    
    vacc_check <- magick::image_crop(z, "400x50+1940+1585") %>%
        magick::image_ocr() %>%
        str_detect("(?i)inmate vaccination")
    
    if(!vacc_check){
        warning("Vaccination data not as expected, please inspect")
    }
    
    vac_num <- magick::image_crop(z, "156x50+2075+1790") %>%
        magick::image_ocr() %>%
        string_to_clean_numeric()
    
    col_vals %>%
        as_tibble() %>%
        mutate(Name = z %>%
                   magick::image_crop("213x1080+177+1030") %>%
                   ExtractTable() %>%
                   unlist()) %>%
        bind_rows(tibble(Name = "STATEWIDE", Residents.Vadmin = vac_num))
}

colorado_extract <- function(x){
    
    df_ <- x
    
    exp_names <- c(
        Residents.Tadmin = "TESTS",
        Residents.Confirmed = "TOTAL POSITIVE",
        Residents.Active = "ACTIVE CASES",
        Residents.Deaths = "DEATHS",
        Name = "Name",
        Residents.Vadmin = "Residents.Vadmin"
        )
    
    check_names(df_, exp_names)
    
    names(df_) <- names(exp_names)
    
    df_ %>%
        mutate(Residents.Recovered = 
                   Residents.Confirmed - Residents.Active - Residents.Deaths)
}

#' Scraper class for general Colorado COVID data
#' 
#' @name colorado_scraper
#' @description Colorado data is pulled from a tableau app which is known
#' to be temperamental. Sometimes code will need to be run several times in
#' order to work as the page has variable load times and selenium can not
#' tell when the DOM is ready. Data is downloaded through selenium in a pdf form
#' from which OCR is run. Colorado staff numbers in the early stages puzzling
#' but pretty clear now that positive means the total, while active is the
#' active total, so you can take the positive active to get recoveries. 
#' It is less clear what is happening in the early days, where recoveries are 
#' dropping, but mathematically this makes the most sense since otherwise the
#' cumulative staff positives would be highest in early May.
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
            type = "pdf",
            state = "CO",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = colorado_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = colorado_restruct,
            # Rename the columns to appropriate database names
            extract_func = colorado_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
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

