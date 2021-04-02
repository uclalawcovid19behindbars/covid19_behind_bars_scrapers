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
    
    sub_dir <- str_c("./results/raw_files/", Sys.Date(), "_california_vaccine")
    dir.create(sub_dir, showWarnings = FALSE)
    html_list <- list()
    
    del_ <- capture.output(remDr$open())
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
    
    xml2::read_html(tf)
}

get_california_vaccine_bi_table <- function(x, idx){
    tab <- x %>%
        rvest::html_nodes(".tableEx") %>%
        .[idx] %>% 
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

california_vaccine_bi_restruct <- function(x){
    sub_files <- x %>%
        rvest::html_nodes("iframe") %>%
        rvest::html_attr("src") %>%
        str_replace("\\./", "./results/raw_files/")
    
    lhtml <- lapply(sub_files, xml2::read_html)
    
    span_text <- lhtml[[1]] %>%
        rvest::html_nodes("span") %>%
        rvest::html_text() %>%
        str_remove_all(" ") %>%
        str_to_lower()
    
    staff_idx <- which(str_detect(span_text, "staffvaccination"))
    rez_idx <- which(str_detect(span_text, "patientvaccination"))
    
    # make sure the tables are talking about who we think they are talking about
    if(staff_idx < rez_idx){
        warning("Staff table may appear before prisoner table. Please inspect.")
    }
    
    vac_list <- lapply(1:2, function(j){
        unique(bind_rows(lapply(lhtml, function(z){
            if(j == 1){
                s_tab <- get_california_vaccine_bi_table(z, 1) %>%
                    rename("Name" = "Institution", 
                           "Residents.Population" = "Current Population", 
                           "Residents.Initiated" = "Partially Vaccinated", 
                           "Residents.Completed" = "Fully Vaccinated") %>%
                    select(Name, starts_with("Res"))
            }
            if(j == 2){
                s_tab <- get_california_vaccine_bi_table(z, 2) %>%
                    rename("Name" = "Institution", 
                           "Staff.Population" = "Current Population", 
                           "Staff.Initiated" = "Partially Vaccinated", 
                           "Staff.Completed" = "Fully Vaccinated") %>%
                    select(Name, starts_with("Staff"))
            }
            s_tab
        })))
    })
    
    # if we have 40 or more observations odds are good we are not catching
    # everything
    if(any(lapply(vac_list, nrow) >= 40)){
        warning(
            "The hacky strategy we are using of sorting by facility may no ",
            "longer work. Please inspect.")
    }
    
    as_tibble(full_join(vac_list[[1]], vac_list[[2]], by = "Name"))
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
    california_bi_vaccine$save_raw()
    california_bi_vaccine$restruct_raw()
    california_bi_vaccine$restruct_data
    california_bi_vaccine$extract_from_raw()
    california_bi_vaccine$extract_data
    california_bi_vaccine$validate_extract()
    california_bi_vaccine$save_extract()
}

