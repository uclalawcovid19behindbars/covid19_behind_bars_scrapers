source("./R/generic_scraper.R")
source("./R/utilities.R")

vermont_html_check_date <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    date_txt <- rvest::html_nodes(base_html, 
                                  xpath="//*[@id='block-uswds-base-subtheme-content']/article/div/div/table[1]/tbody/tr[1]/td[2]/span/span/span/span/span/span") %>%
        rvest::html_text()
    
    date_txt %>%
        lubridate::mdy() %>%
        error_on_date(expected_date = date)
}

vermont_html_pull <- function(x){
    xml2::read_html(x)
}

vermont_html_restruct <- function(x){
    ## table 1 
    in_state_tab <- rvest::html_nodes(x, 
                                      xpath = "//*[@id='block-uswds-base-subtheme-content']/article/div/div/table[1]/tbody") %>%
        lapply(rvest::html_table) %>%
        .[[1]] 
    if(!str_detect(in_state_tab$X1[1], "(?i)in-state incarcerated individuals")){
        warning("Check first table, unexpected value received")
    }
    ## table 2
    out_state_tab <- rvest::html_nodes(x, 
                                       xpath = "//*[@id='block-uswds-base-subtheme-content']/article/div/div/table[2]/tbody") %>%
        lapply(rvest::html_table) %>%
        .[[1]] 
    if(!str_detect(out_state_tab$X1[1], "(?i)out-of-state incarcerated individuals")){
        warning("Check second table, unexpected value received")
    }
    ## table 3 
    facility_tab <- rvest::html_nodes(x, 
                                       xpath = "//*[@id='block-uswds-base-subtheme-content']/article/div/div/table[3]/tbody") %>%
        lapply(rvest::html_table) %>%
        .[[1]] %>%
        janitor::row_to_names(2)
    ## table 4
    staff_tab <- rvest::html_nodes(x, 
                                      xpath = "//*[@id='block-uswds-base-subtheme-content']/article/div/div/table[6]/tbody") %>%
        lapply(rvest::html_table) %>%
        .[[1]] %>%
        janitor::row_to_names(2)
    out <- list(in_state = in_state_tab,
                out_state = out_state_tab,
                facility = facility_tab,
                staff = staff_tab)
    return(out)
}

vermont_html_extract <- function(x){
    in_state_tab_extract <- x$in_state %>%
        mutate(Name = "State-wide for non-facility",
               Residents.Confirmed = subset(., X1 == "Unique Positive Cases*", select = X2),
               Residents.Tested = subset(., X1 == "Total Incarcerated Individuals Tested", select = X2),
               Residents.Tadmin = subset(., X1 == "Total Tests Conducted", select = X2)) %>%
        select(Name, starts_with("Residents.")) %>%
        unique() %>%
        unnest(cols = c(Residents.Confirmed, Residents.Tested, Residents.Tadmin))
    out_state_tab_extract <- x$out_state %>%
        mutate(Name = "Out-of-State",
               Residents.Confirmed = subset(., X1 == "Unique Positive Cases*", select = X2),
               Residents.Tested = subset(., X1 == "Total Incarcerated Individuals Tested", select = X2),
               Residents.Tadmin = subset(., X1 == "Total Tests Conducted", select = X2)) %>%
        select(Name, starts_with("Residents.")) %>%
        unique() %>%
        unnest(cols = c(Residents.Confirmed, Residents.Tested, Residents.Tadmin))
    facility_tab_extract <- x$facility %>%
        select(Name = Facility,
            Residents.Active = `# Currently Positive`,
            Residents.Confirmed = `Unique Positive Cases`) %>%
        filter(Name != "Out of State",
               Name != "Total",
               !str_detect(Name, "(?i)NOTE:"))
    staff_tab_extract <- x$staff %>%
        select(Name = Location,
               Staff.Active = `# Currently Positive`,
               Staff.Confirmed = `Unique Positive Cases`) %>%
        filter(Name != "Total",
               !str_detect(Name, "(?i)NOTE:"))
    fac_data <- facility_tab_extract %>%
        full_join(staff_tab_extract, by = "Name") 
    
    out <- in_state_tab_extract %>%
        bind_rows(out_state_tab_extract) %>%
        bind_rows(fac_data) %>%
        clean_scraped_df()
    
    return(out)
}

#' Scraper class for general vermont COVID data
#' 
#' @name vermont_html_scraper
#' @description HTML Vermonth scraper! no more pics
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

vermont_html_scraper <- R6Class(
    "vermont_html_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.vermont.gov/covid-19-information-page",
            id = "vermont_html",
            type = "html",
            state = "VT",
            jurisdiction = "state",
            check_date = vermont_html_check_date,
            # pull the JSON data directly from the API
            pull_func = vermont_html_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = vermont_html_restruct,
            # Rename the columns to appropriate database names
            extract_func = vermont_html_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    vermont <- vermont_html_scraper$new(log=TRUE)
    vermont$run_check_date()
    vermont$raw_data
    vermont$pull_raw()
    vermont$raw_data
    vermont$save_raw()
    vermont$restruct_raw()
    vermont$restruct_data
    vermont$extract_from_raw()
    vermont$extract_data
    vermont$validate_extract()
    vermont$save_extract()
}

