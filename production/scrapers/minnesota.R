source("./R/generic_scraper.R")
source("./R/utilities.R")

minnesota_pull <- function(x, wait = 5){
    
    app_source <- "https://app.smartsheet.com/b/publish?EQBCT=4fffc0afb455414da7680411f796b64c"
        # xml2::read_html(x) %>%
        # rvest::xml_nodes("iframe") %>%
        # rvest::html_attr("src")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_source)
    Sys.sleep(wait)
    
    xml2::read_html(remDr$getPageSource()[[1]])
}

minnesota_restruct <- function(x){
    
    df_res <- x %>%
        rvest::xml_nodes("table") %>%
        .[[4]] %>%
        rvest::html_table(fill=T) %>%
        .[,5:ncol(.)] %>%
        filter(!is.na(.[,1])) %>%
        filter(!str_detect(.[,1], "(?i)total"))
    
    names(df_res) <- x %>%
        rvest::xml_nodes("table") %>%
        .[[3]] %>%
        rvest::html_table() %>%
        .[,2:ncol(.)] %>%
        unlist() %>%
        unname() %>%
        # remove values in parentheses
        str_remove_all('\\([^)]*\\)') %>%
        str_squish()
    
    exp_res <- c(
        Name = "Primary",
        Residents.Active = "Currently Positive",
        Residents.Tadmin = "Total Tests Administered",
        Residents.Negative = "Confirmed Negative",
        Residents.Confirmed = "Confirmed Positive",
        Residents.Pending = "Tests Pending",
        Resident.Hospital.Drop = "Hospitalized",
        Residents.Recovered = "Recovered",
        Residents.Deaths = "Total Deceased"
    )
    
    exp_staff <- c(
        Name = "Primary",
        Staff.Confirmed = "Confirmed Positive Test",
        Presumed.Drop = "Presumed Positive",
        Staff.Recovered = "Returned to Work"
    )
    
    exp_pop <- c(
        Name = "Work Location",
        Residents.Population = "Facility Population"
    )
    
    df_pop <- x %>%
        rvest::xml_nodes("table") %>%
        .[[2]] %>%
        rvest::html_table(fill=T) %>%
        .[,2:ncol(.)] %>%
        filter(!is.na(.[,1])) %>%
        filter(!str_detect(.[,1], "(?i)total"))
    
    names(df_pop) <- x %>%
        rvest::xml_nodes("table") %>%
        .[[1]] %>%
        rvest::html_table() %>%
        .[,2:ncol(.)] %>%
        unlist() %>%
        unname() %>%
        # remove values in parentheses
        str_remove_all('\\([^)]*\\)') %>%
        str_squish()
    
    df_staff <- x %>%
        rvest::xml_nodes("table") %>%
        .[[18]] %>%
        rvest::html_table(fill=T) %>%
        .[,5:ncol(.)] %>%
        filter(!is.na(.[,1])) %>%
        filter(!str_detect(.[,1], "(?i)total"))
    
    names(df_staff) <- x %>%
        rvest::xml_nodes("table") %>%
        .[[17]] %>%
        rvest::html_table() %>%
        .[,2:ncol(.)] %>%
        unlist() %>%
        unname() %>%
        # remove values in parentheses
        str_remove_all("\\([^)]*\\)") %>%
        str_squish()
    
    basic_check(names(df_res), exp_res)
    basic_check(names(df_staff), exp_staff)
    basic_check(names(df_pop), exp_pop)
    
    names(df_res) <- names(exp_res)
    names(df_staff) <- names(exp_staff)
    names(df_pop) <- names(exp_pop)
    
    full_join(
        df_pop %>% 
            mutate(Residents.Population = as.numeric(Residents.Population)) %>%
            mutate(Name = ifelse(
                str_starts(Name, "MCF- "), Name, str_c("MCF-", Name))) %>%
            mutate(Name = str_replace(Name, "(?i)st\\.* cloud", "Saint Cloud")),
        clean_scraped_df(df_res) %>%
            mutate(Name = str_replace(Name, "(?i)st\\.* cloud", "Saint Cloud")),
        by = "Name") %>%
        full_join(
        clean_scraped_df(df_staff) %>%
            mutate(Name = str_replace(Name, "(?i)st\\.* cloud", "Saint Cloud")),
        by = "Name") %>%
        as_tibble()
}

minnesota_extract <- function(x){
    x %>%
        mutate(Staff.Confirmed = Staff.Confirmed + Presumed.Drop) %>%
        select(!contains("Drop"))
}

#' Scraper class for general Minnesota COVID data
#' 
#' @name minnesota_scraper
#' @description MN data comes from an iframe that is loaded from smart sheets.
#' The main data sources come from two primary tables, one for staff and
#' another for residents.
#' \describe{
#'   \item{Primary}{Facility Name}
#'   \item{Resident Total Tests Administered}{}
#'   \item{Resident Confirmed Negative}{}
#'   \item{Resident Confirmed Positive}{}
#'   \item{Resident Tests Pending}{}
#'   \item{Resident Hospitalized}{}
#'   \item{Resident Recovered}{}
#'   \item{Resident Deceased}{}
#'   \item{Staff Confirmed Positive Test}{}
#'   \item{Staff Hospitalized}{}
#'   \item{Staff Presumed Positive}{}
#'   \item{Staff Returned to Work}{}
#' }

minnesota_scraper <- R6Class(
    "minnesota_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://mn.gov/doc/about/covid-19-updates/",
            id = "minnesota",
            type = "html",
            state = "MN",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = minnesota_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = minnesota_restruct,
            # Rename the columns to appropriate database names
            extract_func = minnesota_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    minnesota <- minnesota_scraper$new(log=TRUE)
    minnesota$raw_data
    minnesota$pull_raw()
    minnesota$raw_data
    minnesota$restruct_raw()
    minnesota$restruct_data
    minnesota$extract_from_raw()
    minnesota$extract_data
    minnesota$validate_extract()
    minnesota$save_extract()
}

