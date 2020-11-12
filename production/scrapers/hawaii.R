source("./R/generic_scraper.R")
source("./R/utilities.R")

hawaii_extract <- function(x){
    
    col_name_mat <- matrix(c(
        "Facilities", "0", "Name",
        "Tested", "1", "Residents.Tested",
        "Results Pending", "2", "Residents.Pending",
        "Negative", "3", "Residents.Negative",
        "Inconclusive", "4", "Residents.Inconclusive",
        "Total Tested Positive", "5", "Residents.Confirmed",
        "Active Positive", "6", "Residents.Active",
        "Number of Persons in Medical Isolation", "7", "Residents.Isolation",
        "Number of Persons in Quarantine", "8", "Residents.Quarantine",
        "Hospitalization", "9", "Residents.Hospitalized",
        "Recovered", "10", "Residents.Recovered",
        "Deaths", "11", "Residents.Deaths"
    ), ncol = 3, nrow = 12, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    df_ <- as_tibble(x[[1]])
    check_names_extractable(df_, col_name_df)
    renamed_df <- rename_extractable(df_, col_name_df)

    hw <- renamed_df %>%
        filter(Name!= "Facilities") %>%
        clean_scraped_df() %>%
        mutate(Residents.Quarantine = Residents.Quarantine + 
                   Residents.Isolation) %>%
        select(
            -Residents.Hospitalized,
            -Residents.Isolation, 
            -Residents.Inconclusive) %>%
        as_tibble()
    
    hw
}

#' Scraper class for general Hawaii COVID data
#' 
#' @name hawaii_scraper
#' @description Data comes from an image which is uploaded to extractable
#' servers for analysis.
#' \describe{
#'   \item{Facilities}{The faciilty name}
#'   \item{Tested}{Cumulative residents tested. No indication as to who 
#'   gets tested.}
#'   \item{Results Pending}{Current Residents who are tested but no outcome 
#'   yet reported.}
#'   \item{Negative}{Cumulative number of residents who are negative.}
#'   \item{Inconclusive}{Cumulative number of residents who have neither
#'   positive nor negative results.}
#'   \item{Positive}{Cumulative number of residents who are positive.}
#'   \item{Number of Persons in Medical Isolation}{Residents in isolation.}
#'   \item{Number of Persons in Quarantine}{Residents in quarantine.}
#'   \item{Hospitilization}{Current residents hospitalized.}
#'   \item{Recovered}{Cumulative residents recovered.}
#'   \item{Deaths}{Cumulative residents who have died.}
#' }

hawaii_scraper <- R6Class(
    "hawaii_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = 
                "https://dps.hawaii.gov/blog/2020/03/17/coronavirus-covid-19-information-and-resources/",
            id = "hawaii",
            state = "HI",
            type = "img",
            # restructuring the data means pulling out the data portion of the json
            pull_func = function(x) {
                get_src_by_attr(
                    x, "img", attr = "src", attr_regex = "(?i)Inmate-Test-Report") %>%
                    {gsub("-[0-9]+x[0-9]+", "", .)} %>%
                    magick::image_read()
            },
            # TODO: we are not currently extracting the last updated section
            # but it looks somewhat scrape-able.
            restruct_func = function(x) ExtractTable(x),
            # Rename the columns to appropriate database names and do some minor
            # minor cleaning
            extract_func = hawaii_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    hawaii <- hawaii_scraper$new(log=TRUE)
    hawaii$raw_data
    hawaii$pull_raw()
    hawaii$raw_data
    hawaii$restruct_raw()
    hawaii$restruct_data
    hawaii$extract_from_raw()
    hawaii$extract_data
    hawaii$validate_extract()
    hawaii$save_extract()
}