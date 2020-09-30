source("./R/generic_scraper.R")
source("./R/utilities.R")

#' Crop out a cell in the AR image and extract the numeric value
#' 
#' @description the data for AR numbers come from a PNG with consistent
#' formatting. the goal of this function is to isolate the cell with a single
#' numeric value using the crop argument, enhance the image, and then extract
#' the number value using OCR. Note that converting to gray-scale will help
#' this process and if we can invert the colors that would probably be even 
#' better. enhancement 
#' 
#' @param base_image image loaded into R using the magick package
#' @param crop the area to crop the image
#' @param enhancement what size to blow up the image to
#' 
#' @return numeric value of specified cell.
process_AR_cell <- function(base_image, crop, enhancement = 500){
    suppressWarnings(out_ <- base_image %>%
                         # crop out a specific cell
                         magick::image_crop(crop) %>%
                         # convert to gray-scale
                         magick::image_convert(type = 'Grayscale') %>%
                         # enhance image
                         magick::image_resize(paste(enhancement, "x")) %>%
                         magick::image_ocr() %>%
                         stringr::str_remove_all(",|\\n") %>%
                         as.numeric())
}

#' Extract information from all cells in an AR image.
#' 
#' @description Get all information from an AR image cells and place in tibble.
#' NOTE: THIS ONLY WORKS WIH A PARTICULAR IMAGE FORMAT AND ANOTHER SET
#' OF CROPPPING AREAS WILL LIKELY BE NEEDED IF THIS CHANGES.
#' 
#' @param image loaded into R using the magick package
#' @param ... other arguments to pass to process_AR_cell
#' 
#' @return data frame with values extracted from image
process_AR_image <- function(base_image, ...){
    tibble(
        Residents.Tested = process_AR_cell(base_image, "93x30+599+360", ...),
        Residents.Recovered = process_AR_cell(base_image, "93x30+599+400", ...),
        Residents.Not.Recovered = process_AR_cell(
            base_image, "93x30+599+440", ...),
        Staff.Tested = process_AR_cell(base_image, "93x30+599+565", ...),
        Staff.Recovered = process_AR_cell(base_image, "93x30+599+605", ...),
        Staff.Not.Recovered = process_AR_cell(base_image, "93x30+599+647", ...)
    )
}

#' Process an image multiple times to increase chance of successful extraction
#' 
#' @description Sometimes an image cannot be extracted with the numbers placed
#' in a cell with the default values for extraction. In this case we loop over
#' several values of extraction parameters to try and get the cell values.
#' 
#' @param url_ the url of the image
#' @param enhancements list of enhancements to use for extraction
#' 
#' @return data frame with values extracted from image
arkansas_restruct <- function(img, enhancements = seq(100, 500, by = 50)){
    base_image <- img
    
    dat_df <- bind_rows(lapply(enhancements, function(e){
        process_AR_image(base_image, enhancement = e)})) %>%
        # we should write a test here if more than one value is returned as right
        # now the function will just return NA if there is more than one value
        # or there is no values extracted
        summarize_all(function(x){
            ifelse(length(unique(na.omit(x))) == 1, unique(na.omit(x)), NA)
        })
    
    for(i in names(dat_df)){
        if(any(is.na(dat_df[[i]]))){
            warning(paste0(
                "NA value extracted tho not expected for AR column ", i))
        }
    }
    
    dat_df
}

arkansas_scraper <- R6Class(
    "arkansas_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://adc.arkansas.gov/coronavirus-covid-19-updates",
            id = "arkansas",
            type = "img",
            # restructuring the data means pulling out the data portion of the json
            pull_func = function(x) {
                magick::image_read(get_src_by_attr(
                    base = x,
                    css = "a",
                    attr = "href",
                    attr_regex = "(?i)stats_update",
                    date_regex = "\\d+-\\d+-\\d+"
                ))
                
            },
            # restructuring the data means pulling out the data portion of the json
            restruct_func = function(x, ...) arkansas_restruct(x, ...),
            # Rename the columns to appropriate database names
            extract_func = function(x) mutate(x, Name = "state-wide")){
            super$initialize(
                url, id, pull_func, type, restruct_func, extract_func, log)
        }
    )
)

if(sys.nframe() == 0){
    arkansas <- arkansas_scraper$new(log=FALSE)
    arkansas$raw_data
    arkansas$pull_raw()
    arkansas$raw_data
    arkansas$save_raw()
    arkansas$restruct_raw(enhancements = seq(100, 500, by = 50))
    arkansas$restruct_data
    arkansas$extract_from_raw()
    arkansas$extract_data
}