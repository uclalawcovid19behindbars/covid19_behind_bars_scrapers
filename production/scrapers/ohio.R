source("./R/generic_scraper.R")
source("./R/utilities.R")

ohio_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)covid")
}

ohio_restruct <- function(x){
    oh_pgs <- magick::image_read_pdf(x)
    
    tmp_files <- sapply(oh_pgs, function(li){
        f <- tempfile(fileext = ".png")
        li %>% 
            magick::image_write(f, format = "png")
        f
    })
    
    restruct_results <- lapply(tmp_files, ExtractTable)
    
    restruct_results
}

ohio_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "Institution", "0", "Name",
        "Housing Type (cell, open bay, combo)", "1", "Housing.Type",
        "# of Staff who have Reported Positive Tests", "2", "Staff.Confirmed",
        "# of COVID- 19 Related Staff Deaths", "3", "Staff.Deaths",
        "# of Staff who have Recovered", "4", "Staff.Recovered",
        "Units in Quarantine", "5", "Units.Quarantine",
        "# of Inmates in Quarantine", "6", "Residents.Quarantine",
        "# of Inmates in Isolation", "7", "Residents.Isolation",
        "# of inmates currently Positive for COVID-19", "8", "Residents.Confirmed",
        "# of Probable COVID-19 Related Inmate Deaths", "9", "Resident.Probable.Deaths",
        "# of Confirmed COVID-19 Related Inmate Deaths", "10", "Residents.Deaths",
        "# of Inmates who have Pending Results",  "11", "Residents.Pending",
        "# of current Inmates who have Recovered", "12", "Residents.Recovered"
    ), ncol = 3, nrow = 13, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    Ohio <- bind_rows(lapply(x, function(li){
        df_ <- as_tibble(li[[1]])
        check_names_extractable(df_, col_name_df)
        renamed_df <- rename_extractable(df_, col_name_df) %>%
            select(-Units.Quarantine, -Housing.Type) %>%
            filter(Name != "Institution" & Name != "Totals")})) %>%
        clean_scraped_df()
    
    Ohio$Residents.Deaths <- (Ohio$Residents.Deaths)+
        (Ohio$Resident.Probable.Deaths)
    Ohio$Residents.Confirmed  <- (Ohio$Residents.Confirmed)+
        (Ohio$Residents.Deaths)+(Ohio$Residents.Recovered)
    Ohio$Residents.Quarantine <- (Ohio$Residents.Quarantine)+
        (Ohio$Residents.Isolation)

    Ohio <- select(
        Ohio, -Residents.Pending, -Residents.Isolation, -Resident.Probable.Deaths)
    
    Ohio
}

#' Scraper class for general Ohio COVID data
#' 
#' @name ohio_scraper
#' @description Data come from a pdf which is updated periodically. The link
#' to the pdf itself does not change only the data within the pdf. We should be
#' periodically checking to see if alternative sources are available as the data
#' collected are sometimes fickle.
#' \describe{
#'   \item{Institution}{}
#'   \item{Housing Type (cell, open bay, combo)}{}
#'   \item{# of Staff who have Reported Positive Tests}{}
#'   \item{# of COVID- 19 Related Staff Deaths}{}
#'   \item{# of Staff who have Recovered}{}
#'   \item{Units in Quarantine}{}
#'   \item{# of Inmates in Quarantine}{}
#'   \item{# of Inmates in Isolation}{}
#'   \item{# of inmates currently Positive for COVID-19}{}
#'   \item{# of Probable COVID-19 Related Inmate Deaths}{}
#'   \item{# of Confirmed COVID-19 Related Inmate Deaths}{}
#'   \item{# of Inmates who have Pending Results}{}
#'   \item{# of current Inmates who have Recovered}{}
#' }

ohio_scraper <- R6Class(
    "ohio_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://drc.ohio.gov/Organization/Research/Reports/COVID-19-Information",
            id = "ohio",
            type = "pdf",
            state = "OH",
            jurisdiction = "state",
            pull_func = ohio_pull,
            restruct_func = ohio_restruct,
            extract_func = ohio_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        })
)

if(sys.nframe() == 0){
    ohio <- ohio_scraper$new(log=TRUE)
    ohio$raw_data
    ohio$pull_raw()
    ohio$save_raw()
    ohio$raw_data
    ohio$restruct_raw()
    ohio$restruct_data
    ohio$extract_from_raw()
    ohio$extract_data
    ohio$validate_extract()
    ohio$save_extract()
}