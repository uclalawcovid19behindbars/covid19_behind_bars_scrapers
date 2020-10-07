source("./R/generic_scraper.R")

cook_county_restruct <- function(x){
    
    Staff.Txt <- x %>%
        html_nodes(., 'ul+ p') %>%
        html_text()

    Staff.Active1 <- Staff.Txt %>%
        str_extract("[0-9]+ correctional officers are positive for COVID") %>%
        str_remove_all("[^0-9]") %>%
        as.numeric()

    Staff.Active2 <- Staff.Txt %>%
        str_extract("as are [0-9]+ other Cook County Sheriff") %>%
        str_remove_all("[^0-9]") %>%
        as.numeric()
    
    tibble(
        Residents.Recovered = x %>%
            html_nodes(., '.col-md-8 li:nth-child(2)') %>%
            html_text() %>%
            str_replace_all('(?<=\\d),(?=\\d)', '') %>%
            str_extract("[0-9]+") %>%
            as.numeric(),
        
        Residents.Deaths = x %>%
            html_nodes(., '.col-md-8 li:nth-child(4)') %>%
            html_text() %>%
            str_replace_all('(?<=\\d),(?=\\d)', '') %>%
            str_extract("[0-9]+") %>%
            as.numeric(),
        
        Residents.Active = x %>%
            html_node(., 'p+ ul li:nth-child(1)') %>%
            html_text() %>%
            str_replace_all('(?<=\\d),(?=\\d)', '') %>%
            str_extract("[0-9]+") %>%
            as.numeric(),
        
        Residents.Negative = x %>%
            html_nodes(., '.col-md-8 li:nth-child(3)') %>%
            html_text() %>%
            str_replace_all('(?<=\\d),(?=\\d)', '') %>%
            str_extract("[0-9]*") %>%
            as.numeric(),
        
        Staff.Active = Staff.Active1 + Staff.Active2,
        
        Staff.Recovered = x %>%
            html_nodes(., 'p:nth-child(12)') %>%
            html_text() %>%
            str_replace_all('(?<=\\d),(?=\\d)', '') %>%
            gsub("[^0-9]", "", .) %>%
            as.numeric(),
        
        Staff.Deaths = x %>%
            html_nodes(., 'p:nth-child(13)') %>%
            html_text() %>%
            str_replace_all('(?<=\\d),(?=\\d)', '') %>%
            str_remove_all("COVID-19") %>%
            str_extract_all("[0-9]+") %>%
            unlist() %>%
            as.numeric() %>%
            sum()
        )
}

cook_county_extract <- function(x){
    x %>%
        mutate(Residents.Confirmed = 
                   Residents.Recovered + Residents.Deaths + Residents.Active) %>%
        mutate(Staff.Confirmed = 
                   Staff.Recovered + Staff.Deaths + Staff.Active) %>%
        select(-Residents.Active, -Staff.Active) %>%
        mutate(Name = "Cook County Jail")
        
}

#' Scraper class for general Cook County COVID data
#' 
#' @name cook_county_scraper
#' @description This will be a description of Cook County data and what the
#' scraper does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

cook_county_scraper <- R6Class(
    "cook_county_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cookcountysheriff.org/covid-19-cases-at-ccdoc/",
            id = "cook_county",
            type = "html",
            state = "IL",
            # pull the JSON data directly from the API
            pull_func = xml2::read_html,
            # restructuring the data
            restruct_func = cook_county_restruct,
            # Rename the columns to appropriate database names
            extract_func = cook_county_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    cook_county <- cook_county_scraper$new(log=FALSE)
    cook_county$raw_data
    cook_county$pull_raw()
    cook_county$raw_data
    cook_county$save_raw()
    cook_county$restruct_raw()
    cook_county$restruct_data
    cook_county$extract_from_raw()
    cook_county$extract_data
}

