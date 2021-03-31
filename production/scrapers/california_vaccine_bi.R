source("./R/generic_scraper.R")
source("./R/utilities.R")

california_vaccine_bi_pull <- function(x, wait = 10){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>% 
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI", 
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&", 
            "pageName=ReportSection1d82f52cafdcc3e76847")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    xml2::read_html(remDr$getPageSource()[[1]])
}

get_california_vaccine_bi_table <- function(x, idx){
    tab <- x %>%
        rvest::html_nodes(".tableEx") %>%
        # Assumes first table is incarcerated people, second is staff 
        # Should check this in a better way 
        .[idx] %>% 
        rvest::html_node(".innerContainer")
    
    col_dat <- tab %>%
        rvest::html_node(".bodyCells") %>%
        rvest::html_node("div") %>%
        rvest::html_children()
    
    # This is only returning the first 20 rows for some reason??  
    dat_df <- do.call(rbind, lapply(col_dat, function(p){
        sapply(rvest::html_children(p), function(z){
            z %>% 
                rvest::html_nodes("div") %>%
                rvest::html_attr("title")})})) %>%
        as.data.frame()
    
    names(dat_df) <- tab %>%
        rvest::html_node(".columnHeaders") %>%
        rvest::html_node("div") %>%
        rvest::html_nodes("div") %>% 
        rvest::html_attr("title") %>%
        na.omit() %>%
        as.vector()
    
    dat_df
}

california_vaccine_bi_restruct <- function(x){
    res_tab <- get_california_vaccine_bi_table(x, 1) %>%
        rename("Name" = "Institution", 
               "Residents.Population" = "Current Population", 
               "Residents.Initiated" = "Partially Vaccinated", 
               "Residents.Completed" = "Fully Vaccinated") %>%
        select(Name, starts_with("Res"))
    
    staff_tab <- get_california_vaccine_bi_table(x, 2) %>%
        rename("Name" = "Institution", 
               "Drop.Staff.Population" = "Current Population", 
               "Staff.Initiated" = "Partially Vaccinated", 
               "Staff.Completed" = "Fully Vaccinated") %>%
        select(Name, starts_with("Staff"))
    
    full_join(res_tab, staff_tab, by = "Name") 
}


california_vaccine_bi_extract <- function(x){
    x %>% 
        mutate_at(vars(-Name), string_to_clean_numeric) %>% 
        as_tibble() %>% 
        clean_scraped_df()
}

#' Scraper class for general California vaccine COVID data from dashboard
#' 
#' @name california_vaccine_bi_scraper
#' @description California vaccine and population data scraped from rendered 
#' power BI iframe. All variables (including population) are reported for 
#' incarcerated people and staff. 
#' 
#' Dashboard says "Reported vaccination rates for staff may underrepresent actual 
#' vaccination rates, as personnel may receive vaccinations from community health 
#' providers and are not required to report vaccination status". Dashboard also 
#' says data updated once/day at 9am. 
#' 
#' \describe{
#'   \item{Institution Name}{}
#'   \item{Current Population}{}
#'   \item{Partially Vaccinated}{}
#'   \item{Fully Vaccinated}{}
#'   \item{% Fully Vaccinated}{}
#' }

california_vaccine_bi_scraper <- R6Class(
    "california_vaccine_bi_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/covid19/updates/",
            id = "california_vaccine_bi",
            type = "html",
            state = "CA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = california_vaccine_bi_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_vaccine_bi_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_vaccine_bi_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    california_bi_vaccine <- california_vaccine_bi_scraper$new(log=TRUE)
    california_bi_vaccine$raw_data
    california_bi_vaccine$pull_raw()
    california_bi_vaccine$raw_data
    california_bi_vaccine$restruct_raw()
    california_bi_vaccine$restruct_data
    california_bi_vaccine$extract_from_raw()
    california_bi_vaccine$extract_data
    california_bi_vaccine$validate_extract()
    california_bi_vaccine$save_extract()
}

