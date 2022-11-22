source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

maryland_vaccine_pull <- function(url){
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    
    remDr$navigate(url)
    Sys.sleep(2)
    next_node <- remDr$findElement("xpath", "//button[@aria-label='Next Page']")
    next_node$clickElement()
    Sys.sleep(2)
    next_node$clickElement()
    Sys.sleep(2)
    base_html <- remDr$getPageSource()[[1]]
    
    out_html <- xml2::read_html(base_html)
    
    remDr$close()
    
    out_html
}

maryland_vaccine_pull_col <- function(html, num) {
    
    header_front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[4]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[2]/div[2]/div['
    header_end_xpath <- ']/div'
    front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[4]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    middle_xpath <- ']/div['
    end_xpath <- ']'
    
    header <- html %>%
        rvest::html_nodes(xpath = str_c(header_front_xpath, num, header_end_xpath)) %>%
        rvest::html_text() %>%
        str_squish()
    
    column <- do.call(rbind, lapply(1:22, function(x) html %>% 
                                        rvest::html_nodes(xpath = str_c(front_xpath, x, middle_xpath, num+1, end_xpath)) %>% 
                                        rvest::html_text())) %>% 
        as.data.frame()
    
    colnames(column) <- header
    
    return(column)
    
}

maryland_staff_vaccine_pull_col <- function(staff_html, staff_num) {
    
    staff_header_front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[5]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[2]/div[2]/div['
    staff_header_end_xpath <- ']/div'
    staff_front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[5]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div/div['
    staff_end_xpath <- ']'
    
    staff_header <- staff_html %>%
        rvest::html_nodes(xpath = str_c(staff_header_front_xpath, staff_num, staff_header_end_xpath)) %>%
        rvest::html_text() %>%
        str_squish()
    
    staff_column <- staff_html %>%
        rvest::html_nodes(xpath = str_c(staff_front_xpath, staff_num+1, staff_end_xpath)) %>%
        rvest::html_text() %>%
        as.data.frame()
    
    colnames(staff_column) <- staff_header
    
    return(staff_column)
    
}

maryland_vaccine_restruct <- function(x, date = Sys.Date()){
    
    fac.name <- x %>%
        maryland_vaccine_pull_col(html = ., num = 1) %>%
        mutate(merge.no = 1:22)
    first.doses.given <- x %>%
        maryland_vaccine_pull_col(html = ., num = 2)%>%
        mutate(merge.no = 1:22)
    first.dose.refusals <- x %>%
        maryland_vaccine_pull_col(html = ., num = 3)%>%
        mutate(merge.no = 1:22)
    second.doses.given <- x %>%
        maryland_vaccine_pull_col(html = ., num = 4)%>%
        mutate(merge.no = 1:22)
    second.dose.refusals <- x %>%
        maryland_vaccine_pull_col(html = ., num = 5)%>%
        mutate(merge.no = 1:22)
    boosters.given <- x %>%
        maryland_vaccine_pull_col(html = ., num = 6)%>%
        mutate(merge.no = 1:22)
    booster.refusals <- x %>%
        maryland_vaccine_pull_col(html = ., num = 7)%>%
        mutate(merge.no = 1:22)
    
    res_data <- fac.name %>%
        left_join(first.doses.given, by = 'merge.no') %>%
        left_join(first.dose.refusals, by = 'merge.no') %>%
        left_join(second.doses.given, by = 'merge.no') %>%
        left_join(second.dose.refusals, by = 'merge.no') %>%
        left_join(boosters.given, by = 'merge.no') %>%
        left_join(booster.refusals, by = 'merge.no') %>%
        select(-merge.no) %>%
        rename(Name = "Facility (Administered)") %>%
        mutate_at(vars(-Name), as.numeric) %>%
        as_tibble()
    
    exp_names <- c(
        Name = "Name", 
        Residents.Initiated =  "First Doses Given", 
        Residents.First.Refusal.Drop = "First Dose Refusals", 
        Residents.Completed = "Second Doses Given", 
        Residents.Second.Refusal.Drop = "Second Dose Refusals",
        Residents.Booster.Drop = "Boosters Given",
        Residents.Booster.Refusal.Drop = "Booster Refusals"
    )
    
    check_names(res_data, exp_names)
    
    names(res_data) <- names(exp_names)
    
    staff.first.dose <- x %>%
        maryland_staff_vaccine_pull_col(staff_html = ., staff_num = 1)
    staff.second.dose <- x %>%
        maryland_staff_vaccine_pull_col(staff_html = ., staff_num = 2)
    staff.booster <- x %>%
        maryland_staff_vaccine_pull_col(staff_html = ., staff_num = 3)
    
    staff_data <- cbind(staff.first.dose, staff.second.dose, staff.booster) %>%
        mutate(Name = "STATEWIDE") %>%
        mutate_at(vars(-Name), string_to_clean_numeric)
    
    exp_staff_names <- c(
        Staff.Initiated = "First Shot",
        Staff.Completed = "Second Shot", 
        Staff.Booster.Drop = "Booster Shots",
        Name = "Name"
    )
    
    check_names(staff_data, exp_staff_names)
    names(staff_data) <- names(exp_staff_names)
    
    restructered_data <- bind_rows(res_data, staff_data)
    
}

maryland_vaccine_extract <- function(restructured_data){
    restructured_data %>%
        select(-ends_with(("Drop"))) %>% 
        clean_scraped_df() 
}

#' Scraper class for Maryland vaccine data
#' 
#' @name maryland_vaccine_scraper
#' @description Vaccine data from MD is pulled from the fourth table on their Power BI 
#' dashboard. Statewide totals for first and second shot for staff and facility 
#' data (as of June 2021) for incarcerated people are reported. 
#' The dashboard also lists first and second dose refusals by facility, vaccine 
#' requirement status, eligibility group, and the number of eligible inmates. 
#' \describe{
#'   \item{Staff Vaccinations First Shot}{}
#'   \item{Staff Vaccinations Second Shot}{}
#'   \item{Inmate Vaccinations First Shot}{}
#'   \item{Inmate Vaccinations Second Shot}{}
#' }

maryland_vaccine_scraper <- R6Class(
    "maryland_vaccine_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = str_c(
                "https://app.powerbigov.us/view?r=", 
                "eyJrIjoiMjdlZjZmNzAtOGYxNS00ODA4LThhMzktOGYyZDEw", 
                "YTMwMDZkIiwidCI6IjYwYWZlOWUyLTQ5Y2QtNDliMS04ODUx", 
                "LTY0ZGYwMjc2YTJlOCJ9&pageName=ReportSection"),
            id = "maryland_vaccine",
            type = "html",
            state = "MD",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = maryland_vaccine_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maryland_vaccine_restruct,
            # Rename the columns to appropriate database names
            extract_func = maryland_vaccine_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    maryland_vaccine <- maryland_vaccine_scraper$new(log=TRUE)
    # The update date is stored on a landing page with no dashboard data
    maryland_vaccine$run_check_date("https://news.maryland.gov/dpscs/covid-19/")
    maryland_vaccine$raw_data
    maryland_vaccine$pull_raw()
    maryland_vaccine$raw_data
    maryland_vaccine$save_raw()
    maryland_vaccine$restruct_raw()
    maryland_vaccine$restruct_data
    maryland_vaccine$extract_from_raw()
    maryland_vaccine$extract_data
    maryland_vaccine$validate_extract()
    maryland_vaccine$save_extract()
}

