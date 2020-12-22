source("./R/generic_scraper.R")
source("./R/utilities.R")

florida_url_pull <- function(x){
    html_page <- xml2::read_html(x)
    
    data_imgs <- html_page %>%
        rvest::html_nodes("ul") %>%
        .[[2]] %>%
        rvest::html_nodes("img") %>%
        rvest::html_attr("src") %>%
        xml2::url_absolute(x)
    
    magick::image_append(
        c(
            magick::image_read(data_imgs[1]),
            magick::image_read(data_imgs[2]),
            magick::image_read(data_imgs[3]),
            magick::image_read(data_imgs[4]),
            magick::image_read(data_imgs[5])
        ), stack = TRUE)
    
}

process_FL_cell <- function(x, crop, enhancement = 500, debug = FALSE){
    
    sub_img <- x %>%
        # crop out a specific cell
        magick::image_crop(crop)
    
    if(debug){
        return(sub_img)
    }
    
    suppressWarnings(out_ <- sub_img %>%
                         # convert to gray-scale
                         magick::image_convert(type = 'Grayscale') %>%
                         # enhance image
                         magick::image_resize(paste(enhancement, "x")) %>%
                         magick::image_ocr() %>%
                         stringr::str_remove_all(",|\\n") %>%
                         as.numeric() %>%
                         # sometimes when extraction fails it spits a
                         # nonsensical negative number
                         {ifelse(. < 0, NA, .)})
    
    return(out_)
}

process_FL_image <- function(base_image, ...){
    tibble(
        Residents.Confirmed = process_FL_cell(base_image, "300x42+0+300", ...),
        Staff.Confirmed = process_FL_cell(base_image, "300x42+0+430", ...),
        Residents.Recovered = process_FL_cell(base_image, "300x42+700+300", ...),
        Staff.Recovered = process_FL_cell(base_image, "300x42+700+430", ...),
        Residents.Deaths = process_FL_cell(base_image, "400x105+0+545", ...)
    )
}

florida_url_restruct <-  function(x, enhancements = seq(100, 500, by = 50)){
    base_image <- x
    
    dat_df <- bind_rows(lapply(enhancements, function(e){
        process_FL_image(base_image, enhancement = e)})) %>%
        # we should write a test here if more than one value is returned as right
        # now the function will just return NA if there is more than one value
        # or there is no values extracted
        summarize_all(function(z){
            ifelse(length(unique(na.omit(z))) == 1, unique(na.omit(z)), NA)
        })
    
    for(i in names(dat_df)){
        if(any(is.na(dat_df[[i]]))){
            warning(paste0(
                "NA value extracted tho not expected for FL column ", i))
        }
    }
    
    dat_df
}

florida_url_extract <- function(x){
    x %>%
        mutate(Name = "State-Wide") %>%
        clean_scraped_df()
}

#' Scraper class for general Florida COVID data
#' 
#' @name florida_url_scraper
#' @description Florida has an html table for reporting results at the facilty
#' level which has been consistent but they have recently stopped reporting
#' facility level deaths. The death data is now reported weekly in a table
#' further down on the web page.
#' \describe{
#'   \item{Facility}{The faciilty name.}
#'   \item{Resident Security Quarantine}{}
#'   \item{Resident Medical Quarantine}{}
#'   \item{Resident Medical Isolation}{}
#'   \item{Resident Pending Tests}{}
#'   \item{Resident Negative Tests}{}
#'   \item{Resident Positive Tests}{}
#'   \item{Staff Positive Tests}{}
#' }

florida_url_scraper <- R6Class(
    "florida_url_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://www.dc.state.fl.us/comm/covid-19.html",
            id = "florida_url",
            type = "img",
            state = "FL",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = florida_url_pull,
            restruct_func = florida_url_restruct,
            # Rename the columns to appropriate database names
            extract_func = florida_url_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    florida_url <- florida_url_scraper$new(log=FALSE)
    florida_url$raw_data
    florida_url$pull_raw()
    florida_url$raw_data
    florida_url$restruct_raw()
    florida_url$restruct_data
    florida_url$extract_from_raw()
    florida_url$extract_data
    florida_url$validate_extract()
    florida_url$save_extract()
}

