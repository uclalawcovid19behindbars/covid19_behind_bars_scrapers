source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

maryland_powerbi_pull <- function(x){
    src_url <- str_c(
        "https://app.powerbigov.us/view?r=",
        "eyJrIjoiMjdlZjZmNzAtOGYxNS00ODA4LThhMzktOGYyZDEw",
        "YTMwMDZkIiwidCI6IjYwYWZlOWUyLTQ5Y2QtNDliMS04ODUx",
        "LTY0ZGYwMjc2YTJlOCJ9&pageName=ReportSection")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(src_url)
    Sys.sleep(6)
    
    base_html <- remDr$getPageSource()[[1]]
    
    remDr$close()
    
    out_html <- xml2::read_html(base_html)
    
    out_html
}

maryland_powerbi_pull_col <- function(html, num) {
    
    header_front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[4]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[2]/div[2]/div['
    header_end_xpath <- ']/div'
    front_xpath <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[4]/transform/div/div[2]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    middle_xpath <- ']/div['
    end_xpath <- ']'
    
    header <- html %>%
        rvest::html_nodes(xpath = str_c(header_front_xpath, num, header_end_xpath)) %>%
        rvest::html_text() %>%
        str_squish()
    
    column <- do.call(rbind, lapply(1:21, function(x) html %>% 
                                        rvest::html_nodes(xpath = str_c(front_xpath, x, middle_xpath, num+1, end_xpath)) %>% 
                                        rvest::html_text())) %>% 
        as.data.frame()
    
    colnames(column) <- header
    
    return(column)
    
}

maryland_powerbi_restruct <- function(x, date = Sys.Date()){
    fac.name <- x %>%
        maryland_powerbi_pull_col(html = ., num = 1) %>%
        mutate(merge.no = 1:21)
    staff.confirmed <- x %>%
        maryland_powerbi_pull_col(html = ., num = 2)%>%
        mutate(merge.no = 1:21)
    staff.recoveries <- x %>%
        maryland_powerbi_pull_col(html = ., num = 3)%>%
        mutate(merge.no = 1:21)
    staff.deaths <- x %>%
        maryland_powerbi_pull_col(html = ., num = 4)%>%
        mutate(merge.no = 1:21)
    res.confirmed <- x %>%
        maryland_powerbi_pull_col(html = ., num = 5)%>%
        mutate(merge.no = 1:21)
    res.recoveries <- x %>%
        maryland_powerbi_pull_col(html = ., num = 6)%>%
        mutate(merge.no = 1:21)
    res.deaths <- x %>%
        maryland_powerbi_pull_col(html = ., num = 7)%>%
        mutate(merge.no = 1:21)
    
    out_data <- fac.name %>%
        left_join(staff.confirmed, by = 'merge.no') %>%
        left_join(staff.recoveries, by = 'merge.no') %>%
        left_join(staff.deaths, by = 'merge.no') %>%
        left_join(res.confirmed, by = 'merge.no') %>%
        left_join(res.recoveries, by = 'merge.no') %>%
        left_join(res.deaths, by = 'merge.no') %>%
        select(-merge.no) %>%
        rename(Name = "Facility Name") %>%
        mutate_at(vars(-Name), as.numeric) %>%
        as_tibble()
    
}

maryland_powerbi_extract <- function(x){
    x %>%
        rename(
            Residents.Recovered = "Inmate Recoveries",
            Residents.Confirmed = "Inmate Cases",
            Residents.Deaths = "Inmate Deaths",
            Staff.Recovered = "Staff Recoveries",
            Staff.Confirmed = "Staff Cases",
            Staff.Deaths = "Staff Deaths"
        )
}

#' Scraper class for general Maryland COVID data
#' 
#' @name maryland_powerbi_scraper
#' @description Data from MD is pulled from a image hosted in the DOC website
#' which is run the OCR. The data posted has been fairly consistent.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Region}{Greater region of facility}
#'   \item{Staff Tests}{Tests administered to staff not sure if test administered or individuals tested}
#'   \item{Staff Positive}{Number of confirmed staff}
#'   \item{Staff Recovered}{Number of recovered staff}
#'   \item{Staff Deaths}{Number of staff deaths}
#'   \item{Inmates Tested}{Residents tested not sure if test administered or individuals tested}
#'   \item{Inmates Positive}{Number of confirmed residents}
#'   \item{Inmates Recovered}{Number of recovered residents}
#'   \item{Inmates Deaths}{Number of resident deaths}
#' }

maryland_powerbi_scraper <- R6Class(
    "maryland_powerbi_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://news.maryland.gov/dpscs/covid-19/",
            id = "maryland_powerbi",
            type = "html",
            state = "MD",
            jurisdiction = "state",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = maryland_powerbi_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = maryland_powerbi_restruct,
            # Rename the columns to appropriate database names
            extract_func = maryland_powerbi_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    maryland_powerbi <- maryland_powerbi_scraper$new(log=TRUE)
    maryland_powerbi$run_check_date()
    maryland_powerbi$raw_data
    maryland_powerbi$pull_raw()
    maryland_powerbi$raw_data
    maryland_powerbi$save_raw()
    maryland_powerbi$restruct_raw()
    maryland_powerbi$restruct_data
    maryland_powerbi$extract_from_raw()
    maryland_powerbi$extract_data
    maryland_powerbi$validate_extract()
    maryland_powerbi$save_extract()
}
