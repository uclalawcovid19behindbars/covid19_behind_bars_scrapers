source("./R/generic_scraper.R")
source("./R/utilities.R")

lasd_crop <- function(img, crop, detect = "", rimg = FALSE){
    sub_img <- img %>%
        magick::image_crop(crop)
    
    if(rimg){
        return(sub_img)
    }
    
    sub_txt <- sub_img %>%
        magick::image_ocr() %>%
        str_remove_all("(?i)covid.?19") %>%
        clean_fac_col_txt()
    
    if(!str_detect(sub_txt, detect)){
        warning("Field does mot match expected text")
    }
    
    as.numeric(str_c(unlist(str_extract_all(sub_txt, "[0-9]+")), collapse = ""))
}

lasd_pull <- function(x, wait = 5){
   get_src_by_attr(
       x, "img", attr = "src", attr_regex = "(?i)covid.?fact.?sheet") %>%
        magick::image_read()
}

lasd_restruct <- function(x){
    tibble(
        Residents.Confirmed = lasd_crop(x, "570x30+620+410", "(?i)total pos"),
        Residents.Recovered = lasd_crop(x, "570x30+620+670", "(?i)recover"),
        Residents.Deaths = lasd_crop(x, "570x30+620+745", "(?i)deaths"),
        Residents.Quarantine = lasd_crop(x, "570x30+620+1015", "(?i)total"),
        drop.neg.asymp = lasd_crop(x, "570x25+620+572", "(?i)negative"),
        drop.neg.symp = lasd_crop(x, "570x25+20+572", "(?i)negative"),
        drop.pos.asymp = lasd_crop(x, "500x25+620+518", "(?i)current"),
        drop.pos.symp = lasd_crop(x, "562x25+20+518", "(?i)current"),
        Residents.Population = lasd_crop(x, "562x25+20+219", "(?i)jail pop"))
}

lasd_extract <- function(x){
    x %>%
        mutate(Residents.Negative = drop.neg.asymp + drop.neg.symp) %>%
        mutate(Residents.Active = drop.pos.asymp + drop.pos.symp) %>%
        select(-starts_with("drop")) %>%
        mutate(Name = "LA Jail")
}

#' Scraper class for general LASD staff COVID data
#' 
#' @name lasd_scraper
#' @description Info comes from a Microsoft power bi app that can be
#' temperamental as load times vary and Selenium can not tell when the DOM is
#' ready. May need to run a couple of times to get data.
#' \describe{
#'   \item{Personnel Currently Quarantined}{}
#'   \item{Personnel Currently High Risk}{}
#'   \item{Returned to Work}{Not neccesarily positive}
#'   \item{Personnel Affected Since Inception}{}
#'   \item{Prof. Confirmed}{Staff type distinction of confirmed cases}
#'   \item{Sworn Confirmed}{Staff type distinction of confirmed cases}
#'   \item{Total Confirmed}{Total staff confirmed cases}
#' }

lasd_scraper <- R6Class(
    "lasd_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://lasd.org/covid19updates/",
            id = "lasd",
            type = "img",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = lasd_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = lasd_restruct,
            # Rename the columns to appropriate database names
            extract_func = lasd_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    lasd <- lasd_scraper$new(log=TRUE)
    lasd$raw_data
    lasd$pull_raw()
    lasd$raw_data
    lasd$save_raw()
    lasd$restruct_raw()
    lasd$restruct_data
    lasd$extract_from_raw()
    lasd$extract_data
    lasd$validate_extract()
    lasd$save_extract()
}

