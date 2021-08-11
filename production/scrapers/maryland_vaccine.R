source("./R/generic_scraper.R")
source("./R/utilities.R")

maryland_vaccine_date_check <- function(x, date = Sys.Date()){
    base_html <- xml2::read_html(x)
    
    base_html %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() %>% 
        {.[str_detect(., "(?i)updated")]} %>% 
        {.[str_detect(., "21")]} %>% 
        str_split("(?i)next") %>% 
        unlist() %>% 
        {.[!str_detect(., "(?i)schedule")]} %>% 
        lubridate::mdy() %>% 
        error_on_date(date)
}

maryland_vaccine_pull <- function(x){
    src_url <- str_c(
        "https://app.powerbigov.us/view?r=", 
        "eyJrIjoiMjdlZjZmNzAtOGYxNS00ODA4LThhMzktOGYyZDEw", 
        "YTMwMDZkIiwidCI6IjYwYWZlOWUyLTQ5Y2QtNDliMS04ODUx", 
        "LTY0ZGYwMjc2YTJlOCJ9&pageName=ReportSectionac5a5322ab94d53d5fca")

    fprof <- RSelenium::makeFirefoxProfile(list(
        browser.startup.homepage = "about:blank",
        startup.homepage_override_url = "about:blank",
        startup.homepage_welcome_url = "about:blank",
        startup.homepage_welcome_url.additional = "about:blank",
        browser.download.dir = "/home/seluser/Downloads",
        browser.download.folderList = 2L,
        browser.download.manager.showWhenStarting = FALSE,
        browser.download.manager.focusWhenStarting = FALSE,
        browser.download.manager.closeWhenDone = TRUE,
        browser.helperApps.neverAsk.saveToDisk = 
            "application/pdf, application/octet-stream",
        pdfjs.disabled = TRUE,
        plugin.scan.plid.all = FALSE,
        plugin.scan.Acrobat = 99L))
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox",
        extraCapabilities=fprof
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(src_url)
    Sys.sleep(15)
    
    base_html <- remDr$getPageSource()
    
    xml2::read_html(base_html[[1]])
}

get_maryland_facility_vaccine_table <- function(x){
    tab <- x %>%
        rvest::html_nodes(".tableEx") %>%
        rvest::html_nodes(".innerContainer")
    
    col_dat <- tab %>%
        rvest::html_nodes(".bodyCells") %>%
        rvest::html_nodes("div") %>%
        rvest::html_children()
    
    dat_list <- lapply(col_dat, function(p){
        sapply(rvest::html_children(p), function(z){
            z %>% 
                rvest::html_nodes("div") %>%
                rvest::html_attr("title")})})

    dat_df <- do.call(rbind, dat_list[sapply(dat_list, is.matrix)]) %>%
        as.data.frame()

    names(dat_df) <- tab %>%
        rvest::html_node(".columnHeaders") %>%
        rvest::html_node("div") %>%
        rvest::html_nodes("div") %>% 
        rvest::html_attr("title") %>%
        na.omit() %>%
        as.vector() %>% 
        head(ncol(dat_df))
    
    dat_df %>%
        rename(Name = "Facility (Administered)") %>%
        filter(!str_detect(Name, "(?i)total")) %>% 
        mutate_all(as.character) %>% 
        mutate_at(vars(-Name), string_to_clean_numeric) %>% 
        filter(Name != "character(0)") %>% 
        mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
        filter(!str_detect(Name, "(?i)total"))
}

get_maryland_vaccine_table <- function(x, str){
    tab <- x %>% 
        rvest::html_node(xpath = stringr::str_c(
            "//*[contains(@title,", str, ")]/parent::*")
        ) %>% 
        rvest::html_node(".tableEx") %>%
        rvest::html_node(".innerContainer")
    
    col_dat <- tab %>%
        rvest::html_node(".bodyCells") %>%
        rvest::html_node("div") %>%
        rvest::html_children()
    
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

maryland_vaccine_restruct <- function(x){
        
    # Need single quotes for the xpath 
    staff_total <- get_maryland_vaccine_table(x, "'Staff Vaccinations'") 
    res_total <- get_maryland_vaccine_table(x, "'Inmate Vaccinations'")

    exp_staff_names <- c(
        Staff.Initiated = "First Shot",
        Staff.Completed = "Second Shot", 
        Staff.Single.Drop = "Single Shot"
    )
    
    exp_res_names <- c(
        Residents.Initiated =  "First Shot", 
        Residents.Completed = "Second Shot"
    )
    
    check_names(staff_total, exp_staff_names)
    names(staff_total) <- names(exp_staff_names)
    
    check_names(res_total, exp_res_names)
    names(res_total) <- names(exp_res_names)
    
    staff_total <- mutate_all(staff_total, string_to_clean_numeric) %>% 
        mutate(Name = "STATEWIDE")
    res_total <- mutate_all(res_total, string_to_clean_numeric)

    res_facility <- get_maryland_facility_vaccine_table(x)
    
    exp_names <- c(
        Name = "Name", 
        Residents.Initiated =  "First Doses Administered", 
        Residents.First.Refusal.Drop = "First Dose Refusals", 
        Residents.Completed = "Second Doses Adminstered", 
        Residents.Second.Refusal.Drop = "Second Dose Refusals"
    )
    
    check_names(res_facility, exp_names)
    names(res_facility) <- names(exp_names)
    
    if (sum_na_rm(res_facility$Residents.Initiated) != res_total$Residents.Initiated){
        warning("Sum of facilities does not match total. Check dashboard for changes.")
    }
    
    if (sum_na_rm(res_facility$Residents.Completed) != res_total$Residents.Completed){
        warning("Sum of facilities does not match total. Check dashboard for changes.")
    }
    
    bind_rows(res_facility, staff_total)
}

maryland_vaccine_extract <- function(x){
    x %>%
        mutate(Staff.Initiated = Staff.Initiated + Staff.Single.Drop, 
               Staff.Completed = Staff.Completed + Staff.Single.Drop) %>%  
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
#'   \item{Staff Vaccinations Single Shot}{}
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
            url = "https://news.maryland.gov/dpscs/covid-19/",
            id = "maryland_vaccine",
            type = "html",
            state = "MD",
            jurisdiction = "state",
            check_date = maryland_vaccine_date_check,
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
    maryland_vaccine$run_check_date()
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

