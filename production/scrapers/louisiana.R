source("./R/generic_scraper.R")
source("./R/utilities.R")

louisiana_pull <- function(x){
    xml2::read_html(x) 
}

louisiana_restruct <- function(x){
    list(
        la1 = x %>%
            rvest::html_nodes('table') %>%
            .[[1]] %>%
            rvest::html_table(),
        la2 = x %>%
            rvest::html_nodes('table') %>%
            .[[3]] %>%
            rvest::html_table())
}

louisiana_extract <- function(x){
    rez_exp <- c(
        Name = "Prisons", 
        Drop.Res.Symp = "Current Positive Symptomatic",
        Drop.Res.Asymp = "Current Positive Asymptomatic",
        Residents.Confirmed = "Total Tested Positive Cases",
        Drop.Res.Pos.Symp = "Tested Positive Symptomatic Cases",
        Drop.Res.Pos.Asymp = "Tested Positive Asymptomatic Cases",
        Residents.Tested = "Total # of Inmates Tested",
        Drop.Res.Retests = "# of Inmate Retests Processed",
        Drop.Res.Stepdown = "Step Down",
        Residents.Recovered = "Recovered",
        Drop.Res.Release = "Released",
        Drop.Res.Death.Und = "COVID-19 Deaths (Underlying Medical Conditions)",
        Drop.Res.Death.Main = "COVID-19 Deaths",
        Residents.Deaths = "Total Deaths")
    
    staff_exp <- c(
        Name = "Prisons",
        Staff.Confirmed = "Positive",
        Staff.Recovered = "Recovered", 
        Staff.Tested = "Staff Tested (Self Reported & DOC Conducted)",
        Staff.Deaths = "Deaths")
    
    check_names(x$la1, rez_exp)
    check_names(x$la2, staff_exp)
    
    la_rez <- x$la1
    names(la_rez) <- names(rez_exp)
    
    la_staff <- x$la2
    names(la_staff) <- names(staff_exp)
    
    la_rez <- la_rez %>%
        clean_scraped_df() %>%
        mutate(Residents.Tadmin = Residents.Tested + Drop.Res.Retests, 
               Residents.Active = Drop.Res.Symp + Drop.Res.Asymp) %>%
        select(
            Name, Residents.Confirmed, Residents.Tested, Residents.Deaths,
            Residents.Recovered, Residents.Tadmin, Residents.Active) %>%
        filter(Name != "TOTAL")
    
    la_staff <- la_staff %>%
        filter(Name != "TOTAL") %>%
        mutate(Name = ifelse(
            Name == "Louisiana Correctional Institute for Women",
            "Louisiana Correctional Institute for Women - Hunt",
            Name))
    
    full_join(la_rez, la_staff, by="Name") %>%
        clean_scraped_df()
}

#' Scraper class for general Louisiana COVID data
#' 
#' @name louisiana_scraper
#' @description Louisiana scraper extracts information from two html tables, one
#' for staff and another for residents. Table for residents is more detailed.
#' Not that the testing numbers indicate the number of individuals who are
#' tested and not the total number of tests administered. 
#' \describe{
#'   \item{Prisons}{The facility name.}
#'   \item{Current Positive Symptomatic}{Residents active Positive who are sympotomatic}
#'   \item{Current Positive Asymptomatic}{Residents active Positive who are asympotomatic}
#'   \item{Total Tested Positive Cases}{Total residents confirmed}
#'   \item{Tested Positive Symptomatic Cases}{Residents Testing Positive who are sympotomatic}
#'   \item{Tested Positive Asymptomatic Cases}{Residents Testing Positive who are asympotomatic}
#'   \item{Total # of Inmates Tested}{Number of residents tested, not tests administered.}
#'   \item{# of Inmate Retests Processed}{Residents re-tested}
#'   \item{Step Down}{Not sure}
#'   \item{Recovered}{Residents recovered}
#'   \item{Released}{Residents released}
#'   \item{COVID-19 Deaths (Underlying Medical Conditions)}{Residents passing w/ covid and other underlying conditions}
#'   \item{COVID-19 Deaths}{Residents passing w/ covid and w/o other underlying conditions}
#'   \item{Total Deaths}{Total resident deaths related to covid}
#'   \item{Staff Positive}{Staff who are confirmed}
#'   \item{Staff Recovered}{Staff who are recovered}
#'   \item{Staff Tested (Self Reported & DOC Conducted)}{Staff who have been tested}
#'   \item{Staff Deaths}{Staff deaths}
#' }

louisiana_scraper <- R6Class(
    "louisiana_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.louisiana.gov/doc-covid-19-testing/",
            id = "louisiana",
            type = "html",
            state = "LA",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = louisiana_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = louisiana_restruct,
            # Rename the columns to appropriate database names
            extract_func = louisiana_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    louisiana <- louisiana_scraper$new(log=TRUE)
    louisiana$run_check_date()
    louisiana$raw_data
    louisiana$pull_raw()
    louisiana$raw_data
    louisiana$save_raw()
    louisiana$restruct_raw()
    louisiana$restruct_data
    louisiana$extract_from_raw()
    louisiana$extract_data
    louisiana$validate_extract()
    louisiana$save_extract()
}

