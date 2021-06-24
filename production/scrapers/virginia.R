source("./R/generic_scraper.R")
source("./R/utilities.R")

virginia_pull <- function(x){
    xml2::read_html(x)
}

virginia_restruct <- function(x){
    
    list(
        web = x %>%
            rvest::html_node("table") %>%
            rvest::html_table() %>%
            as_tibble(),
        
        sw = tibble(
            Name = "STATEWIDE",
            
            Staff.Deaths =  x %>%
                rvest::html_nodes(
                    xpath = "//h4[contains(text(), 'Staff')]/parent::div") %>%
                rvest::html_nodes(
                    xpath = str_c(
                        ".//span[contains(text(), 'deaths')]/",
                        "preceding-sibling::span")) %>%
                rvest::html_text(),
    
            Residents.Initiated = x %>%
                rvest::html_nodes(
                    xpath = str_c(
                        "//span[contains(text(), 'inmate vaccinations')]/",
                        "following-sibling::span")) %>%
                rvest::html_text(),
        
            Staff.Initiated = x %>%
                rvest::html_nodes(
                    xpath = str_c(
                        "//span[contains(text(), 'staff vaccinations')]/",
                        "following-sibling::span")) %>%
                rvest::html_text()))
}

virginia_extract <- function(x){
    exp_names <- c(
        Name = "Location",
        Residents.Active = "Inmates on-site active cases",
        Drop.Active2 = "Inmates in hospitals active cases",
        Residents.Deaths = "Death of COVID-19 positive inmates",
        Residents.Confirmed = "Total positive inmatesonsite hospital deaths releases recovered transfers in - transfers out",
        Staff.Active = "Staff active cases including employees & contractors"
    )

    check_names(x$web, exp_names)
    df_ <- x$web
    names(df_) <- names(exp_names)
    
    df_ %>%
        clean_scraped_df() %>%
        select(-starts_with("Drop")) %>%
        bind_rows(clean_scraped_df(x$sw))
}

#' Scraper class for general Virginia COVID data
#' 
#' @name virginia_scraper
#' @description VA cumulative confirmed does not account for transfers out as
#' listed in the table. data is pulled from the html table directly
#' \describe{
#'   \item{Location}{The facility name.}
#'   \item{Offenders on site}{Infected and at facility}
#'   \item{Offenders in hospital}{Currently hospitalized}
#'   \item{Death of COVID-19 positive offender}{Residents deaths}
#'   \item{Total positive offenders on site}{No transfers but cumulative}
#'   \item{Staff active}{Staff currently infected}
#'   \item{Residents Vaccinations Initiated}{Number of inmates that have received at least one dose of a vaccine}
#'   \item{Staff Vaccinations Initiated}{Number of staff that have received at least one dose of a vaccine}
#' }

virginia_scraper <- R6Class(
    "virginia_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://vadoc.virginia.gov/news-press-releases/2020/covid-19-updates/",
            id = "virginia",
            type = "html",
            state = "VA",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = virginia_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = virginia_restruct,
            # Rename the columns to appropriate database names
            extract_func = virginia_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    virginia <- virginia_scraper$new(log=TRUE)
    virginia$run_check_date()
    virginia$raw_data
    virginia$pull_raw()
    virginia$raw_data
    virginia$save_raw()
    virginia$restruct_raw()
    virginia$restruct_data
    virginia$extract_from_raw()
    virginia$extract_data
    virginia$validate_extract()
    virginia$save_extract()
}

