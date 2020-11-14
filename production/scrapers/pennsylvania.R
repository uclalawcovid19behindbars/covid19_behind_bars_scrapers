source("./R/generic_scraper.R")
source("./R/utilities.R")

pennsylvania_pull <- function(x){
    tf_ <- tempfile()
    # grab xlsx directly
    "https://www.cor.pa.gov/Documents/PA-DOC-COVID-19-Testing.xlsx" %>%
        download.file(destfile=tf_)
    readxl::read_excel(tf_)
}

pennsylvania_restruct <- function(x){
    pa <- x
    pa <- pa[-c(1,nrow(pa)),c(1:11,14,15)]
    pa %>%
        rename(
            Name = LOCATION,
            Staff.Confirmed = "Staff Testing",
            Staff.Negative = ...3, 
            Staff.Pending = ...4,
            Staff.Deaths = ...5,
            Staff.Recovered = ...6,
            Residents.Confirmed = "Inmate Testing",
            Residents.Negative = ...8,
            Residents.Pending = ...9,
            Residents.Deaths = ...10,
            Residents.Recovered = ...11,
            Residents.Released1 = ...14,
            Residents.Released2 = ...15)
}

pennsylvania_extract <- function(x){
    pa <- x
    pa <- clean_scraped_df(pa)
    stopifnot(!any(is.na(pa$Name)))
    
    # they report zeros as blanks in their table
    pa[is.na(pa)] <- 0
    
    ## Adding Releases Together | Two release columns release and release (plus)
    # <- possibly released while positive/possible double counting
    pa$Residents.Released <- pa$Residents.Released1 + pa$Residents.Released2
    ## Adding confirmed, negative and pending to get testing numbers
    pa$Residents.Tested   <- pa$Residents.Confirmed+pa$Residents.Negative+pa$Residents.Pending
    ## Same as residents tested
    pa$Staff.Tested       <- pa$Staff.Confirmed+pa$Staff.Negative+pa$Staff.Pending
    
    pa <- subset(pa,select=-c(
        Residents.Released1,Residents.Released2,Residents.Released))
    
    pa
}

#' Scraper class for general Pennsylvania COVID data
#' 
#' @name pennsylvania_scraper
#' @description PN data come from an xlsx sheet which is frequently updated.
#' Note that this data is also available on the PN DOC website through a
#' Microsoft BI app.
#' \describe{
#'   \item{LOCATION}{The facility name.}
#'   \item{Staff Positive}{}
#'   \item{Staff Negative}{}
#'   \item{Staff Pending}{}
#'   \item{Staff Death}{}
#'   \item{Staff Recovered}{}
#'   \item{Residents Positive}{}
#'   \item{Residents Negative}{}
#'   \item{Residents Pending}{}
#'   \item{Residents Death}{}
#'   \item{Residents Recovered}{}
#'   \item{Resident Transfers}{}
#'   \item{Resident Transfers while Positive}{}
#'   \item{Resident Release}{}
#'   \item{Resident Release while Positive}{}
#'   \item{Resident Hospital}{}
#'   \item{Resident Hospital while Positive}{}
#'   \item{Resident Surveilance}{}
#'   \item{Resident Surveilance while Positive}{}
#'   \item{Resident Symptomatic}{}
#'   \item{Resident Symptomatic while Positive}{}
#' }

pennsylvania_scraper <- R6Class(
    "pennsylvania_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cor.pa.gov/Pages/COVID-19.aspx",
            id = "pennsylvania",
            type = "csv",
            state = "PA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = pennsylvania_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = pennsylvania_restruct,
            # Rename the columns to appropriate database names
            extract_func = pennsylvania_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    pennsylvania <- pennsylvania_scraper$new(log=TRUE)
    pennsylvania$raw_data
    pennsylvania$pull_raw()
    pennsylvania$raw_data
    pennsylvania$save_raw()
    pennsylvania$restruct_raw()
    pennsylvania$restruct_data
    pennsylvania$extract_from_raw()
    pennsylvania$extract_data
    pennsylvania$validate_extract()
    pennsylvania$save_extract()
}

