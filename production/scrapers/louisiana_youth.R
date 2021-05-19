source("./R/generic_scraper.R")
source("./R/utilities.R")

louisiana_youth_pull <- function(x){
    xml2::read_html(x)
}

louisiana_youth_restruct <- function(x){
    youth <- x %>%
        rvest::html_nodes('.wp-block-table') %>%
        .[[1]] %>%
        rvest::html_table() 
    
    staff <- x %>%
        rvest::html_nodes('.wp-block-table') %>% 
        .[[3]] %>%     
        rvest::html_table()
    
    out <- bind_rows(youth, staff)
    return(out)
}

louisiana_youth_extract <- function(x){
    la <- x %>%
        janitor::row_to_names(row_number = 1) 
    
    la_staff_row_start <- which(la$`# of Youth Positive` == "# of Employees Positive")
    
    ## separate and merge dfs by name due to different facility name order
    la_staff <- la %>%
        slice(la_staff_row_start:n()) %>%
        janitor::row_to_names(row_number = 1) 
    
    la_youth <- la %>%
        slice(1:(la_staff_row_start - 1)) 
    
    youth_merged <- full_join(la_youth, la_staff, by = c("Secure Care Facility"))
    
    check_names(youth_merged, 
                c("Secure Care Facility", 
                  "# of Youth Positive", 
                  "# of Youth Recovered",
                  "# of Employees Positive", 
                  "# of Employees Recovered"))
    
    la_cln <- youth_merged %>%
        select(Name = `Secure Care Facility`, 
               Residents.Confirmed = `# of Youth Positive`,
               Residents.Recovered = `# of Youth Recovered`,
               Staff.Confirmed = `# of Employees Positive`,
               Staff.Recovered = `# of Employees Recovered`) %>%
        mutate(Name = str_c(toupper(Name), " YOUTH"))
    
    out <- la_cln %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        filter(!str_detect(Name, "(i?)TOTAL"))
    
    return(out)
}

#' Scraper class for general Louisiana youth COVID data
#' 
#' @name louisiana_youth_scraper
#' @description LA data self contained within html table. 
#' \describe{
#'   \item{Location}{The facilty name}
#'   \item{Residents.Confirmed}{Youth Confirmed}
#'   \item{Residents.Recovered}{Youth Recovered}
#'   \item{Staff.Confirmed}{Staff Confirmed}
#'   \item{Staff.Recovered}{Staff Recovered}
#' }

louisiana_youth_scraper <- R6Class(
    "louisiana_youth_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://ojj.la.gov/ojj-covid-19-information/",
            id = "louisiana_youth",
            type = "html",
            state = "LA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = louisiana_youth_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = louisiana_youth_restruct,
            # Rename the columns to appropriate database names
            extract_func = louisiana_youth_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    louisiana_youth <- louisiana_youth_scraper$new(log=TRUE)
    louisiana_youth$raw_data
    louisiana_youth$pull_raw()
    louisiana_youth$raw_data
    louisiana_youth$save_raw()
    louisiana_youth$restruct_raw()
    louisiana_youth$restruct_data
    louisiana_youth$extract_from_raw()
    louisiana_youth$extract_data
    louisiana_youth$validate_extract()
    louisiana_youth$save_extract()
}

