source("./R/generic_scraper.R")
source("./R/utilities.R")

nyc_jails_pull <- function(x){
    html_page <- xml2::read_html(x)
    
    html_page %>%
        rvest::html_nodes("a") %>%
        .[str_squish(rvest::html_text(.)) == "CHS COVID-19 Data Snapshot"] %>%
        rvest::html_attr("href")
}

nyc_jails_restruct <- function(x){
    txt_ext <- x %>%
        magick::image_read_pdf(pages = 1) %>%
        magick::image_crop("2550x685+0+365") %>%
        magick::image_ocr()
    
    line_results <- txt_ext %>%
        str_split("\n") %>%
        unlist()
    
    comb_txt <- c() 
    j <- 1
    
    for(i in line_results){
        if(length(comb_txt) < j){
            comb_txt[j] <- ""
        }
        comb_txt[j] <- str_c(comb_txt[j], i, sep  = " ")
        if(str_detect(i, ":")){
            j <- j + 1
        }
    }
    
    txt_mat <- comb_txt %>%
        str_split_fixed(":", 2)
    
    colnames(txt_mat) <- c("text", "value")
    
    as_tibble(txt_mat)
}

nyc_jails_extract <- function(x){
    
    count_df <- x %>%
        #remove rate rows
        filter(!str_detect(text, "(?i)rate"))
    
    idx_list <- list(
        tests = which(str_detect(
            count_df$text, "(?i)Total number of tests completed")),
        confirmed = which(str_detect(
            count_df$text, "(?i)positive tests completed among patients")),
        active = which(str_detect(
            count_df$text, "(?i)in custody with active infection"))
    )
    
    if(any(sapply(idx_list, length) != 1)){
        stop("Extraction not as expected please inspect further.")
    }
    
    tibble(
        Name = "NEW YORK CITY JAILS",
        Residents.Confirmed = count_df$value[idx_list$confirmed],
        Residents.Active = count_df$value[idx_list$active],
        Residents.Tadmin = count_df$value[idx_list$tests]) %>%
        clean_scraped_df()
}

#' Scraper class for general nyc_jails COVID data
#' 
#' @name nyc_jails_scraper
#' @description This will be a description of nyc_jails data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

nyc_jails_scraper <- R6Class(
    "nyc_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.nychealthandhospitals.org/correctionalhealthservices/",
            id = "nyc_jails",
            type = "pdf",
            state = "NY",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = nyc_jails_pull,
            restruct_func = nyc_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = nyc_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    nyc_jails <- nyc_jails_scraper$new(log=TRUE)
    nyc_jails$perma_save()
    nyc_jails$raw_data
    nyc_jails$pull_raw()
    nyc_jails$raw_data
    nyc_jails$save_raw()
    nyc_jails$restruct_raw()
    nyc_jails$restruct_data
    nyc_jails$extract_from_raw()
    nyc_jails$extract_data
    nyc_jails$validate_extract()
    nyc_jails$save_extract()
}

