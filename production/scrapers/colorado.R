source("./R/generic_scraper.R")
source("./R/utilities.R")

colorado_vec_return_white <- function(vec, threshold = 200, buffer = 1){
    start_idx <- first(which(vec < threshold))
    sub_vec <- vec
    sub_vec[1:start_idx] <- 0
    
    first(which(sub_vec >= threshold)) + buffer
}

colorado_extract_cell <- function(cell){
    target <- drop(as.integer(cell[[1]]))
    mid_idx <- round((dim(target)[1]+1)/2)
    
    new_wstart <- colorado_vec_return_white(target[mid_idx,])
    
    old_width <- magick::image_info(cell)$width
    old_height <- magick::image_info(cell)$height
    
    cell %>%
        magick::image_crop(
            str_c(old_width, "x", old_height, "+", new_wstart, "+0")) %>%
        magick::image_ocr() %>%
        # sometimes ocr converts 4's to a's
        str_replace_all("a", "4") %>% 
        string_to_clean_numeric()
}

colorado_extract_col <- function(sub_col, numeric = TRUE){
    
    idxs <- seq(0, 47*20, by = 47)
    
    sapply(1:length(idxs), function(idx){
        if(idx == 21){
            crp <- "213x40+0+940"
        }else{
            crp <- str_c("213x47+0+", idxs[idx])
        }
        
        if(numeric){
            out <- sub_col %>%
                magick::image_crop(crp) %>%
                colorado_extract_cell()
        }
        else{
            out <- sub_col %>%
                magick::image_crop(crp) %>%
                magick::image_ocr() %>%
                str_replace_all("\n", " ") %>%
                str_squish()
        }
        out
    })
}

colorado_pull <- function(x){
    
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
    
    app_src <- xml2::read_html(base_html[[1]])%>%
        rvest::html_element("iframe") %>%
        rvest::html_attr("src")
    
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
    
    sub_col_list <- list(
        z %>%
            magick::image_crop("213x987+387+1100") %>%
            magick::image_convert(type = 'Bilevel'),
        z %>%
            magick::image_crop("213x987+823+1100") %>%
            magick::image_convert(type = 'Bilevel'),
        z %>%
            magick::image_crop("213x987+1264+1100") %>%
            magick::image_convert(type = 'Bilevel'),
        z %>%
            magick::image_crop("213x987+1700+1100") %>%
            magick::image_convert(type = 'Bilevel')
    )
    
    col_vals <- sapply(sub_col_list, function(sc){
        colorado_extract_col(sc)
    })
    
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
    
    fac_col <- z %>%
        magick::image_crop("213x987+177+1100") %>%
        magick::image_convert(type = 'Bilevel')

    fac_names <- colorado_extract_col(fac_col, FALSE)
  
    col_vals %>%
        as_tibble() %>%
        mutate(Name =fac_names) %>%
        bind_rows(tibble(Name = "STATEWIDE", Residents.Initiated = vac_num))
}

colorado_extract <- function(x){
    
    df_ <- x
    
    exp_names <- c(
        Residents.Tadmin = "TESTS",
        Residents.Confirmed = "TOTAL POSITIVE",
        Residents.Active = "ACTIVE CASES",
        Residents.Deaths = "DEATHS",
        Name = "Name",
        Residents.Initiated = "Residents.Initiated"
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
