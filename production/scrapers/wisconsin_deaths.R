source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

wisconsin_deaths_check_date <- function(x, date = Sys.Date()){
    app_src <- "https://public.tableau.com/views/WIDOCCOVID19/" %>%
        str_c(
            "COVID-19RelatedDeaths?%3Aembed=y&%3AshowVizHome=no&%3A",
            "host_url=https%3A%2F%2Fpublic.tableau.com%2F&%3A",
            "embed_code_version=3&%3Atabs=yes&%3Atoolbar=no&%3A",
            "animate_transition=yes&%3Adisplay_static_image=no&%3A",
            "display_spinner=no&%3Adisplay_overlay=yes&%3A",
            "display_count=yes&%3Alanguage=en&%3AloadOrderID=0")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(app_src)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()
    
    base_page <- xml2::read_html(base_html[[1]])
    
    remDr$close()
    
    base_page %>%
        rvest::html_node(xpath ="//span[contains(text(),'Updated')]") %>%
        rvest::html_text() %>%
        str_remove_all("Updated: |\\*") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

wisconsin_deaths_pull <- function(x){
    
    app_src <- "https://public.tableau.com/views/WIDOCCOVID19/" %>%
        str_c(
            "COVID-19RelatedDeaths?%3Aembed=y&%3AshowVizHome=no&%3A",
            "host_url=https%3A%2F%2Fpublic.tableau.com%2F&%3A",
            "embed_code_version=3&%3Atabs=yes&%3Atoolbar=no&%3A",
            "animate_transition=yes&%3Adisplay_static_image=no&%3A",
            "display_spinner=no&%3Adisplay_overlay=yes&%3A",
            "display_count=yes&%3Alanguage=en&%3AloadOrderID=0")

    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(app_src)
    Sys.sleep(6)
    
    out_file <- "/tmp/sel_dl/COVID-19 Related Deaths.pdf"
    
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
        "css", "[data-tb-test-id='export-pdf-export-Button']")$clickElement()
    Sys.sleep(10)
    
    if(!file.exists(out_file)){
        stop("WI unable to download image")
    }
    
    remDr$close()
    
    return(out_file)
}

wisconsin_deaths_restruct <- function(x){
    z <-  magick::image_read_pdf(x, pages = 1)
    
    list(
        Name = z %>%
            magick::image_crop("1000x780+0+332") %>%
            magick::image_ocr() %>%
            str_split("\\n") %>%
            unlist() %>%
            str_squish() %>%
            .[. != ""],
        
        Residents.Deaths = z %>%
            magick::image_crop("200x778+2200+332") %>%
            magick::image_ocr() %>%
            str_split("\\n") %>%
            unlist() %>%
            str_squish() %>%
            .[. != ""] %>%
            as.numeric())
    
    
}

wisconsin_deaths_extract <- function(x){
    
    lapply(x, function(l){
        if(any(is.na(l))){
            warning("Value is NA that was not expected")
        }
    })
    
    as_tibble(x) %>% 
        filter(!str_detect(Name, "(?i)total"))
}

#' Scraper class for general COVID data
#' 
#' @name wisconsin_deaths_scraper
#' @description 
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Deaths}{}
#' }

wisconsin_deaths_scraper <- R6Class(
    "wisconsin_deaths_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.wi.gov/Pages/COVID19(Coronavirus)/COVID19TestingDashboard.aspx",
            id = "wisconsin_deaths",
            type = "pdf",
            state = "WI",
            jurisdiction = "state",
            check_date = wisconsin_deaths_check_date,
            # pull the JSON data directly from the API
            pull_func = wisconsin_deaths_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wisconsin_deaths_restruct,
            # Rename the columns to appropriate database names
            extract_func = wisconsin_deaths_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    wisconsin_deaths <- wisconsin_deaths_scraper$new(log=TRUE)
    wisconsin_deaths$run_check_date()
    wisconsin_deaths$raw_data
    wisconsin_deaths$pull_raw()
    wisconsin_deaths$raw_data
    wisconsin_deaths$save_raw() 
    wisconsin_deaths$restruct_raw()
    wisconsin_deaths$restruct_data
    wisconsin_deaths$extract_from_raw()
    wisconsin_deaths$extract_data
    wisconsin_deaths$validate_extract()
    wisconsin_deaths$save_extract()
}

