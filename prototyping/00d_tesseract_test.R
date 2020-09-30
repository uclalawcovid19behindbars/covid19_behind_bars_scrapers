#' Data from the state of Arkansas comes in the form of structured PNGs
#' The format is very similar each time they update but they have an 
#' inconsistent naming scheme for the URL to the PNG so some work and
#' assumptions are required in order to get the new data url. Checking
#' on this website scraping weekly is advised.
#' TODO: Write Test Specialized test functions
rm(list=ls())
library(tidyverse)
#library(tesseract)
library(magick)

#' Crop out a cell in the AK image and extract the numeric value
#' 
#' @description the data for AK numbers come from a PNG with consistent
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
process_AK_cell <- function(base_image, crop, enhancement = 500){
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

#' Extract information from all cells in an AK image.
#' 
#' @description Get all information from an AK image cells and place in tibble.
#' NOTE!!! THIS ONLY WORKS WIH A PARTICULAR IMAGE FORMAT AND ANOTHER SET
#' OF CROPPPING AREAS WILL LIKELY BE NEEDED IF THIS CHANGES.
#' 
#' @param image loaded into R using the magick package
#' @param ... other arguments to pass to process_AK_cell
#' 
#' @return data frame with values extracted from image
process_AK_image <- function(base_image, ...){
    tibble(
        Residents.Tested = process_AK_cell(base_image, "93x30+599+360", ...),
        Residents.Recovered = process_AK_cell(base_image, "93x30+599+400", ...),
        Residents.Not.Recovered = process_AK_cell(
            base_image, "93x30+599+440", ...),
        Staff.Tested = process_AK_cell(base_image, "93x30+599+565", ...),
        Staff.Recovered = process_AK_cell(base_image, "93x30+599+605", ...),
        Staff.Not.Recovered = process_AK_cell(base_image, "93x30+599+647", ...)
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

multiprocess_AK_image <- function(url_, enhancements = seq(100, 900, by = 100)){
    base_image <- image_read(url_)

    bind_rows(lapply(enhancements, function(e){
        process_AK_image(base_image, enhancement = e)
    })) %>%
    # we should write a test here if more than one value is returned as right
    # now the function will just return NA if there is more than one value
    # or there is no values extracted
    summarize_all(function(x){
        ifelse(length(unique(na.omit(x))) == 1, unique(na.omit(x)), NA)
    })
}



pull_raw_ak <- function(
    base = "https://adc.arkansas.gov/coronavirus-covid-19-updates"){
    web_page_imgs <- xml2::read_html(base) %>%
        rvest::html_nodes("a")
    
    srcs <- rvest::html_attr(web_page_imgs, "href") %>%
        stringr::str_to_lower()
    
    condition_df <- tibble::tibble(
        src_string = srcs,
        match_grep = grepl("stats_update", srcs),
        date = stringr::str_extract(srcs, "\\d+-\\d+-\\d+") %>%
            lubridate::parse_date_time("mdy")) %>%
        dplyr::mutate(val = as.numeric(date)) %>%
        dplyr::mutate(val = ifelse(match_grep, val, -Inf))
    
    im_pos <- which(condition_df$val == max(condition_df$val))
    url_portion <- rvest::html_attr(web_page_imgs, "href")[[im_pos]]
    
    xml2::url_absolute(url_portion, base)
    
    condition_df$val[cond]

    test <- srcs[grepl("stats_update", srcs)]
    
    stringr::str_extract(srcs, "\\d+-\\d+-\\d+") %>%
        lubridate::parse_date_time("mdy")

}

# TESTING
url_ <- paste0(
    "https://adc.arkansas.gov/images/uploads/",
    "COVID-19_-_Stats_Update_9-9-2020_social.jpg")

image_read(url_)
multiprocess_AK_image(url_)

url_<- paste0(
    "https://adc.arkansas.gov/images/uploads/",
    "COVID-19_-_Stats_Update_9-1-2020_social.jpg")
image_read(url_)
multiprocess_AK_image(url_)


url_<- paste0(
    "https://adc.arkansas.gov/images/uploads/",
    "COVID-19_-_Stats_Update_8-18-2020_social.jpg")
image_read(url_)
multiprocess_AK_image(url_)
