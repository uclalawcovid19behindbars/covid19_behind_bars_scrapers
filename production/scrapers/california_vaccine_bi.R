source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

california_vaccine_bi_pull <- function(x, wait = 20){
    # scrape from the power bi iframe directly
    y <- "https://app.powerbigov.us/view?r=" %>%
        str_c(
            "eyJrIjoiODBjZjExNDktYWUxNi00NmM1LTllODMtY2VkMDM1MjlkODRiIiwidCI", 
            "6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9&", 
            "pageName=ReportSection1d82f52cafdcc3e76847")
    
    remDr <- initiate_remote_driver()
    sub_dir <- str_c("./results/raw_files/", Sys.Date(), "_california_vaccine")
    dir.create(sub_dir, showWarnings = FALSE)
    html_list <- list()
    
    remDr$open(silent = TRUE)
    remDr$navigate(y)
    
    Sys.sleep(wait)
    
    # only 20 rows appear in each table at a time in order to get all rows we
    # have to first sort by facility name ascending and grab the first 20 rows
    
    webEls <- remDr$findElements(value="//div[@title='Institution']")
    
    webEls[[1]]$clickElement()
    webEls[[2]]$clickElement()
    
    Sys.sleep(wait)
    
    html_list[["first"]] <- xml2::read_html(remDr$getPageSource()[[1]])
    
    # then we sort by descending and grab the first 20 rows
    # this strategy only works if there is less than or equal to 40 facilities
    # and right now there is 39 :/. Hopefully CDCR doesnt add more but there
    # is a warning below to check this.
    
    webEls <- remDr$findElements(value="//div[@title='Institution']")
    webEls[[1]]$clickElement()
    webEls[[2]]$clickElement()
    
    Sys.sleep(wait)

    html_list[["last"]] <- xml2::read_html(remDr$getPageSource()[[1]])

    fns <- str_c(sub_dir, "/", names(html_list), ".html")

    Map(xml2::write_html, html_list, fns)
    
    iframes <- lapply(str_remove(fns, "/results/raw_files"), function(fn)
        htmltools::tags$iframe(
            src = fn, 
            style="display:block", 
            height="500", width="1200"
        )  
    )
    
    x <- htmltools::tags$html(
        htmltools::tags$body(
            iframes
        )
    )
    
    tf <- tempfile(fileext = ".html")
    
    htmltools::save_html(x, file = tf)
    
    remDr$close()
    
    xml2::read_html(tf)
}

pull_california_vaccine_column <- function(html, length, name, colno, typeno) {
    xbase1 <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container['
    xbase2 <- ']/transform/div/div[3]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    hold <- html
    out <- do.call(rbind, lapply(1:length, function(p, html) {
        html <- hold
        select <- xbase1 %>%
            str_c(., typeno, xbase2, p, ']/div[', colno, ']')
        out <- html %>%
            rvest::html_nodes(xpath = select) %>%
            rvest::html_text()
    })) %>%
        as.data.frame() 
    colnames(out) <- name
    out <- out %>%
        mutate(rowid = 1:length)
    
    out
}

get_california_vaccine_bi_residents <- function(x) {
    baseraw <- str_c('./results/raw_files/', Sys.Date(), '_california_vaccine/')
    xbase <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[18]/transform/div/div[3]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    firsttable <- baseraw %>%
        str_c(., 'first.html') %>%
        rvest::read_html()
    lasttable <- baseraw %>%
        str_c(., 'last.html') %>%
        rvest::read_html()
    
    ## Pull First Resident Table
    firstresname <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Name', colno = 2, typeno = 18)
    firstrespop <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Residents.Population', colno = 3, typeno = 18)
    firstrespvax <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Residents.Initiated.Pct', colno = 4, typeno = 18)
    firstresfvax <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Residents.Completed.Pct', colno = 5, typeno = 18)
    
    firstres <- firstresname %>%
        merge(firstrespop, by = 'rowid') %>%
        merge(firstrespvax, by = 'rowid') %>%
        merge(firstresfvax, by = 'rowid') 
    
    ## Pull Last Resident Table
    lastresname <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Name', colno = 2, typeno = 18)
    lastrespop <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Residents.Population', colno = 3, typeno = 18)
    lastrespvax <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Residents.Initiated.Pct', colno = 4, typeno = 18)
    lastresfvax <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Residents.Completed.Pct', colno = 5, typeno = 18)
    
    lastres <- lastresname %>%
        merge(lastrespop, by = 'rowid') %>%
        merge(lastrespvax, by = 'rowid') %>%
        merge(lastresfvax, by = 'rowid') 
    
    res_table <- firstres %>%
        rbind(lastres) %>%
        select(-c(rowid)) %>%
        as.data.frame() %>%
        mutate(Residents.Initiated.Pct = as.character(str_replace_all(Residents.Initiated.Pct, '%', '')),
               Residents.Completed.Pct = as.character(str_replace_all(Residents.Completed.Pct, '%', '')),
               Residents.Initiated.Pct = ifelse(str_length(Residents.Initiated.Pct)==1, str_c('0.0', Residents.Initiated.Pct), Residents.Initiated.Pct),
               Residents.Initiated.Pct = ifelse(str_length(Residents.Initiated.Pct)==2, str_c('0.', Residents.Initiated.Pct), Residents.Initiated.Pct),
               Residents.Completed.Pct = ifelse(str_length(Residents.Completed.Pct)==1, str_c('0.0', Residents.Completed.Pct), Residents.Completed.Pct),
               Residents.Completed.Pct = ifelse(str_length(Residents.Completed.Pct)==2, str_c('0.', Residents.Completed.Pct), Residents.Completed.Pct),
               Residents.Population = string_to_clean_numeric(Residents.Population)
        )
    
    res_table
    
}

get_california_vaccine_bi_staff <- function(x) {
    baseraw <- str_c('./results/raw_files/', Sys.Date(), '_california_vaccine/')
    xbase <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[18]/transform/div/div[3]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    firsttable <- baseraw %>%
        str_c(., 'first.html') %>%
        rvest::read_html()
    lasttable <- baseraw %>%
        str_c(., 'last.html') %>%
        rvest::read_html()
    
    ## Pull First Resident Table
    firstname <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Name', colno = 2, typeno = 19)
    firstpop <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Staff.Population', colno = 3, typeno = 19)
    firstpvax <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Staff.Initiated.Pct', colno = 4, typeno = 19)
    firstfvax <- pull_california_vaccine_column(html = firsttable, length = 20, name = 'Staff.Completed.Pct', colno = 5, typeno = 19)
    
    first_staff <- firstname %>%
        merge(firstpop, by = 'rowid') %>%
        merge(firstpvax, by = 'rowid') %>%
        merge(firstfvax, by = 'rowid') 
    
    ## Pull Last Resident Table
    lastname <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Name', colno = 2, typeno = 19)
    lastpop <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Staff.Population', colno = 3, typeno = 19)
    lastpvax <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Staff.Initiated.Pct', colno = 4, typeno = 19)
    lastfvax <- pull_california_vaccine_column(html = lasttable, length = 20, name = 'Staff.Completed.Pct', colno = 5, typeno = 19)
    
    last_staff <- lastname %>%
        merge(lastpop, by = 'rowid') %>%
        merge(lastpvax, by = 'rowid') %>%
        merge(lastfvax, by = 'rowid') 
    
    staff_table <- first_staff %>%
        rbind(last_staff) %>%
        select(-c(rowid))%>%
        mutate(Staff.Initiated.Pct = as.character(str_replace_all(Staff.Initiated.Pct, '%', '')),
               Staff.Completed.Pct = as.character(str_replace_all(Staff.Completed.Pct, '%', '')),
               Staff.Initiated.Pct = ifelse(str_length(Staff.Initiated.Pct)==1, str_c('0.0', Staff.Initiated.Pct), Staff.Initiated.Pct),
               Staff.Initiated.Pct = ifelse(str_length(Staff.Initiated.Pct)==2, str_c('0.', Staff.Initiated.Pct), Staff.Initiated.Pct),
               Staff.Completed.Pct = ifelse(str_length(Staff.Completed.Pct)==1, str_c('0.0', Staff.Completed.Pct), Staff.Completed.Pct),
               Staff.Completed.Pct = ifelse(str_length(Staff.Completed.Pct)==2, str_c('0.', Staff.Completed.Pct), Staff.Completed.Pct),
               Staff.Population = string_to_clean_numeric(Staff.Population)
        )
    
    staff_table
    
}

get_california_vaccine_bi_statewide <- function(x) {
    baseraw <- str_c('./results/raw_files/', Sys.Date(), '_california_vaccine/')
    xbase <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[18]/transform/div/div[3]/div/visual-modern/div/div/div[2]/div[1]/div[4]/div/div['
    xres.pct <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[10]/transform/div/div[3]/div/visual-modern/div/svg/g[2]/text'
    xstaff.pct <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[12]/transform/div/div[3]/div/visual-modern/div/svg/g[2]/text'
    firsttable <- baseraw %>%
        str_c(., 'first.html') %>%
        rvest::read_html()
    
    res.pct <- firsttable %>%
        rvest::html_nodes(xpath = xres.pct) %>%
        rvest::html_text() %>%
        str_replace_all(., '%', '') %>%
        str_c('0.', .)
    
    staff.pct <- firsttable %>%
        rvest::html_nodes(xpath = xstaff.pct) %>%
        rvest::html_text() %>%
        str_replace_all(., '%', '')%>%
        str_c('0.', .)
    
    out <- data.frame('Name' = c('STATEWIDE'),
                      'Residents.Initiated.Pct' = res.pct,
                      'Staff.Initiated.Pct' = staff.pct)
    
    out
    
    
}

california_vaccine_bi_restruct <- function(x){
    staff_table <- get_california_vaccine_bi_staff() %>%
        unique()
    residents_table <- get_california_vaccine_bi_residents() %>%
        unique()
    statewide <- get_california_vaccine_bi_statewide()
    
    data_table <- staff_table %>%
        left_join(residents_table, by = 'Name') %>%
        plyr::rbind.fill(statewide) 
    
    data_table
    
}


california_vaccine_bi_extract <- function(x){
    out <- x %>% 
        #mutate_at(vars(-Name), string_to_clean_numeric) %>% 
        mutate_at(vars(-Name), as.numeric) %>% 
        mutate(Residents.Initiated.Pct = ifelse(Name != 'STATEWIDE', round(Residents.Initiated.Pct+Residents.Completed.Pct, 2), Residents.Initiated.Pct),
               Staff.Initiated.Pct = ifelse(Name != 'STATEWIDE', round(Staff.Initiated.Pct+Staff.Completed.Pct, 2), Staff.Initiated.Pct)) %>%
        as_tibble() #%>% 
    #clean_scraped_df()
    
    out
    
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

