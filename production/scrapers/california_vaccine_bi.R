source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

california_vaccine_bi_pull <- function(x){
    # scrape from the power bi iframe directly
    src_url <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI", 
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&", 
            "pageName=ReportSection1d82f52cafdcc3e76847")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(src_url)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()[[1]]
    
    remDr$close()
    
    out_html <- xml2::read_html(base_html)
    
    out_html
    
}

california_vaccine_bi_pull_col <- function(html, num, container) {
    
    header_front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container['
    header_middle_xpath <- ']/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[2]/div[2]/div['
    header_end_xpath <- ']/div'
    
    front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container['
    second_xpath <- ']/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    third_xpath <- ']/div['
    end_xpath <- ']'
    
    header <- html %>%
        rvest::html_nodes(xpath = str_c(header_front_xpath, container, header_middle_xpath, num, header_end_xpath)) %>%
        rvest::html_text() %>%
        str_squish()
    
    column <- do.call(rbind, lapply(1:20, function(x) html %>% 
                                        rvest::html_nodes(xpath = str_c(front_xpath, container, second_xpath, x, third_xpath, num+1, end_xpath)) %>% 
                                        rvest::html_text())) %>% 
        as.data.frame()
    
    colnames(column) <- header
    
    return(column)
    
}

california_vaccine_bi_restruct <- function(x, date = Sys.Date()){
    
    resident.fac <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 1, container = 22) %>%
        mutate(merge.no = 1:20)
    resident.population <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 2, container = 22)%>%
        mutate(merge.no = 1:20)
    resident.initiated.pct <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 3, container = 22)%>%
        mutate(merge.no = 1:20)
    resident.completed.pct <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 4, container = 22)%>%
        mutate(merge.no = 1:20)
    staff.fac <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 1, container = 23)%>%
        mutate(merge.no = 1:20)
    staff.population <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 2, container = 23)%>%
        mutate(merge.no = 1:20)
    staff.initiated.pct <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 3, container = 23)%>%
        mutate(merge.no = 1:20)
    staff.completed.pct <- x %>%
        california_vaccine_bi_pull_col(html = ., num = 4, container = 23)%>%
        mutate(merge.no = 1:20)

    res.data <- resident.fac %>%
        left_join(resident.population, by = 'merge.no') %>%
        left_join(resident.initiated.pct, by = 'merge.no') %>%
        left_join(resident.completed.pct, by = 'merge.no') %>%
        select(-merge.no) %>%
        as_tibble() %>%
        rename(
            Name = "Institution",
            Residents.Population = "Current Population",
            Residents.Initiated.Pct = "% Partially Vaccinated",
            Residents.Completed.Pct = "% Primary Series Complete",
        )
    
    staff.data <- staff.fac %>%
        left_join(staff.population, by = 'merge.no') %>%
        left_join(staff.initiated.pct, by = 'merge.no') %>%
        left_join(staff.completed.pct, by = 'merge.no') %>%
        select(-merge.no) %>%
        as_tibble() %>%
        rename(
            Name = "Institution",
            Staff.Population = "Current Population",
            Staff.Initiated.Pct = "% Partially Vaccinated",
            Staff.Completed.Pct = "% Primary Series Complete",
        )
    
    out_data <- merge(res.data, staff.data, by = "Name", all = TRUE)
    
}


california_vaccine_bi_extract <- function(x){
    x %>% 
        mutate_at(vars(-Name), string_to_clean_numeric) %>% 
        mutate_at(vars(contains('Pct')), funs(./100)) %>%
        mutate(Residents.Initiated.Pct = Residents.Initiated.Pct + Residents.Completed.Pct) %>%
        mutate(Staff.Initiated.Pct = Staff.Initiated.Pct + Staff.Completed.Pct) %>%
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
#'   \item{% Partially Vaccinated}{}
#'   \item{% Primary Series Complete}{}
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
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = california_vaccine_bi_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = california_vaccine_bi_restruct,
            # Rename the columns to appropriate database names
            extract_func = california_vaccine_bi_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    california_bi_vaccine <- california_vaccine_bi_scraper$new(log=TRUE)
    california_bi_vaccine$run_check_date()
    california_bi_vaccine$raw_data
    california_bi_vaccine$pull_raw()
    california_bi_vaccine$raw_data
    california_bi_vaccine$save_raw()
    california_bi_vaccine$restruct_raw()
    california_bi_vaccine$restruct_data
    california_bi_vaccine$extract_from_raw()
    california_bi_vaccine$extract_data
    california_bi_vaccine$validate_extract()
    california_bi_vaccine$save_extract()
}

