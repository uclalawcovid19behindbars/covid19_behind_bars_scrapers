source("./R/generic_scraper.R")
source("./R/utilities.R")

virginia_beach_pull <- function(x){
    xml2::read_html(x)
}

virginia_beach_restruct <- function(x){
    x %>%
        rvest::html_nodes("table") %>%
        rvest::html_table() %>%
        lapply(function(z){
            df_ <- as.data.frame(t(z))[2,]
            names(df_) <- clean_fac_col_txt(z[[1]])
            df_
        })
}

virginia_beach_extract <- function(x){
    exp_names_rez <- c(
        Residents.Tadmin = "INMATE TESTS",
        Residents.Negative = "INMATE NEGATIVE RESULTS",
        Residents.Confirmed = "INMATE POSITIVE RESULTS",
        Residents.Pending = "INMATE RESULTS PENDING",
        Residents.Active = "ACTIVE INMATE CASES",
        Residents.Recovered = "INMATES CLEARED OR RELEASED**",
        Residents.Deaths = "INMATE DEATHS DUE TO COVID-19",
        Residents.Population = "TOTAL INMATES (CURRENT JAIL POPULATION)"
    )

    exp_names_staff <- c(
        Staff.Confirmed = "DEPUTY POSITIVE RESULTS",
        Staff.Active ="ACTIVE DEPUTY CASES",
        Staff.Recovered = "DEPUTIES CLEARED",
        Staff.Population = "TOTAL DEPUTIES"
    )
    
    df_staff <- x[[1]]
    df_rez <- x[[2]]
    
    check_names(df_staff, exp_names_staff)
    check_names(df_rez, exp_names_rez)

    names(df_staff) <- names(exp_names_staff)
    names(df_rez) <- names(exp_names_rez)
    
    bind_cols(df_staff, df_rez) %>%
        mutate(Name = "Virginia Beach Jail") %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        select(-starts_with("Drop"))
}

#' Scraper class for general virginia_beach COVID data
#' 
#' @name virginia_beach_scraper
#' @description Virginia Beach reports data in two html tables one for staff
#' and another for residents. Data is reported regularly and the columns 
#' reported has changed over time.
#' \describe{
#'   \item{INMATES TESTED}{}
#'   \item{INMATE NEGATIVE RESULTS}{}
#'   \item{INMATE POSITIVE RESULTS}{}
#'   \item{INMATE RESULTS PENDING}{}
#'   \item{ACTIVE INMATE CASES}{}
#'   \item{INMATES CLEARED OR RELEASED}{}
#'   \item{INMATE DEATHS DUE TO COVID-19}{}
#'   \item{TOTAL INMATES (CURRENT JAIL POPULATION)}{}
#'   \item{DEPUTIES TESTED}{}
#'   \item{DEPUTY NEGATIVE RESULTS}{}
#'   \item{DEPUTY POSITIVE RESULTS}{}
#'   \item{DEPUTY RESULTS PENDING}{}
#'   \item{ACTIVE DEPUTY CASES}{}
#'   \item{DEPUTIES CLEARED}{}
#'   \item{TOTAL DEPUTIES}{}
#' }

virginia_beach_scraper <- R6Class(
    "virginia_beach_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.vbso.net/coronavirus",
            id = "virginia_beach",
            type = "html",
            state = "VA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = virginia_beach_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = virginia_beach_restruct,
            # Rename the columns to appropriate database names
            extract_func = virginia_beach_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    virginia_beach <- virginia_beach_scraper$new(log=TRUE)
    virginia_beach$raw_data
    virginia_beach$pull_raw()
    virginia_beach$raw_data
    virginia_beach$save_raw()
    virginia_beach$restruct_raw()
    virginia_beach$restruct_data
    virginia_beach$extract_from_raw()
    virginia_beach$extract_data
    virginia_beach$validate_extract()
    virginia_beach$save_extract()
}

