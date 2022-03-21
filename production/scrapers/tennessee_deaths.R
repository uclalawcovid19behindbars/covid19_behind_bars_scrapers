source("./R/generic_scraper.R")
source("./R/utilities.R")

tennessee_deaths_check_date <- function(url, date = Sys.Date()){
    z <- get_src_by_attr(
        url, "a", attr = "href", attr_regex = "COVIDDeaths.pdf$") %>% 
        magick::image_read_pdf(pages = 1)
    
    z %>%
        magick::image_crop("600x80+550+280") %>%
        magick::image_ocr() %>%
        str_remove("(?i)updated") %>%
        str_remove_all("\\||\n") %>%
        lubridate::mdy() %>%
        error_on_date(date)
}

tennessee_deaths_pull <- function(url){
    tn_deaths_pdf <- get_src_by_attr(
        url, "a", attr = "href", attr_regex = "COVIDDeaths.pdf$")
    return(tn_deaths_pdf)
}

tennessee_deaths_restruct <- function(tn_deaths_pdf){
    extract <- tabulizer::extract_tables(tn_deaths_pdf)
}

tennessee_deaths_extract <- function(extract){
    
    col_name_mat <- matrix(c(
        "Name", "V1", "By Location",
        "Residents.Deaths", "V3", "Deaths"
    ), ncol = 3, nrow = 2, byrow = TRUE)
    
    colnames(col_name_mat) <- c("clean", "raw", "check")
    col_name_df <- as_tibble(col_name_mat)
    
    tab1 <- extract[[1]][4:nrow(extract[[1]]),] 
    cleaner_tab <- tab1 %>%
        as.data.frame() %>%
        select(-V2) %>%
        as_tibble()
    
    check_names_extractable(cleaner_tab, col_name_df)
    fac_deaths_df <- rename_extractable(cleaner_tab, col_name_df) %>%
        filter(!str_detect(Name, "(?i)region|managed|location|total")) %>%
        filter(Name != "") %>%
        clean_scraped_df() %>%
        as_tibble() %>% 
        mutate(Residents.Deaths = ifelse(
            is.na(Residents.Deaths), 0, Residents.Deaths)) 
    
    return(fac_deaths_df)
}

#' Scraper class for general Tennessee COVID data
#' 
#' @name tennessee_deaths_scraper
#' @description Data is pulled from a pdf at a static location that is
#' updated. We need to find better documentation on what variables mean.
#' \describe{
#'   \item{Location}{The facility name}
#'   \item{Residents Tested}{Tests administred not actually residents tested}
#'   \item{Residents Positive}{Seems to be active rather than cumulative positive}
#'   \item{Residents Negative}{Negative tests not residents negative}
#'   \item{Residents Pending}{Residents currently pending}
#'   \item{Residents Recovered}{Residents recovered}
#'   \item{Residents Deaths}{Residents deaths}
#'   \item{Staff confirmed}{cant tell if cumulative}
#'   \item{Staff returned to work}{seems to be recovered}
#'   \item{Staff Deaths}{Staff deaths}
#' }

tennessee_deaths_scraper <- R6Class(
    "tennessee_deaths_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.tn.gov/correction.html",
            id = "tennessee",
            type = "pdf",
            state = "TN",
            jurisdiction = "state",
            check_date = tennessee_deaths_check_date,
            # pull the JSON data directly from the API
            pull_func = tennessee_deaths_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = tennessee_deaths_restruct,
            # Rename the columns to appropriate database names
            extract_func = tennessee_deaths_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    tennessee_deaths <- tennessee_deaths_scraper$new(log=TRUE)
    tennessee_deaths$run_check_date()
    tennessee_deaths$raw_data
    tennessee_deaths$pull_raw()
    tennessee_deaths$raw_data
    tennessee_deaths$save_raw()
    tennessee_deaths$restruct_raw()
    tennessee_deaths$restruct_data
    tennessee_deaths$extract_from_raw()
    tennessee_deaths$extract_data
    tennessee_deaths$validate_extract()
    tennessee_deaths$save_extract()
}

