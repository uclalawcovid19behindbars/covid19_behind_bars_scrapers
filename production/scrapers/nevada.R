source("./R/generic_scraper.R")
source("./R/utilities.R")

nevada_pull <- function(x){
    app_source <- str_c(
        "https://app.powerbigov.us/view?r=",
        "eyJrIjoiNDMwMDI0YmQtNmUyYS00ZmFjLWI0MGItZDM0OTY1Y2Y0YzNhIiwidCI6Im",
        "U0YTM0MGU2LWI4OWUtNGU2OC04ZWFhLTE1NDRkMjcwMzk4MCJ9")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(app_source)
    Sys.sleep(6)
    
    remDr$screenshot(file = "~/Desktop/test.png")
    
    remDr$findElement(
        "xpath", 
        str_c(
            "//div[@aria-label='Facility Type Slicer Drop down box to ",
            "select one or more facility types.']"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(
        "xpath", 
        str_c("//div[@aria-label='Facility Type Slicer Drop down box to ",
              "select one or more facility types.']"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(
        "xpath", 
        str_c("//div[@class='slicerItemContainer']",
              "/span[@title='Correctional']"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(
        "xpath", "//div[@aria-label='Facility Name,  All']")$clickElement()
    Sys.sleep(1)
    
    sub_dir <- str_c("./results/raw_files/", Sys.Date(), "_nevada")
    dir.create(sub_dir, showWarnings = FALSE)
    
    
    more_options <- TRUE
    html_list <- list()
    i <- 0
    
    while(more_options){
        i <- i + 1
        if(i > 200){
            break
        }
        
        nv_page <- xml2::read_html(remDr$getPageSource()[[1]])
        
        box_options <- nv_page %>%
            rvest::html_nodes(".slicerText") %>%
            rvest::html_text()
        
        valid_prison_options <- box_options %>%
            str_replace_all("[^a-zA-Z0-9 -]", "") %>%
            str_replace_all(" ", "") %>%
            {grepl("^[[:upper:]]+$", .)} %>%
            which()
        
        names(valid_prison_options) <- box_options[valid_prison_options]
        
        new_options <- valid_prison_options[
            !(names(valid_prison_options) %in% names(html_list))]
        
        remDr$findElements(
            "css", ".glyphicon.checkbox")[[new_options[1]]]$clickElement()
        
        Sys.sleep(5)
        
        html_list[[names(new_options[1])]] <- xml2::read_html(
            remDr$getPageSource()[[1]])
        
        more_options <- any(!(
            names(valid_prison_options) %in% names(html_list)))
    }
    
    # Not gonna work need to find a permanent location
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

nevada_restruct <- function(x){
    
    sub_files <- x %>%
        rvest::html_nodes("iframe") %>%
        rvest::html_attr("src") %>%
        str_replace("\\./", "./results/raw_files/")
    
    bind_rows(lapply(sub_files, function(hl){
    
        op_page <- xml2::read_html(hl)
        
        facility <- hl %>%
            str_split("/") %>%
            unlist() %>%
            last() %>%
            str_remove("\\.html")
    
        recoveries <- op_page %>%
            rvest::html_nodes(
                xpath = str_c(
                    "//div[@aria-label=",
                    "' Card Total number of recovered COVID-19 cases.']")) %>%
            rvest::html_nodes("tspan") %>%
            rvest::html_text() %>%
            matrix(nrow = 2, ncol=2) %>%
            t()
    
        basic_check(recoveries[,2], c("Residents/Patients", "Staff"))
    
        resident_deaths <- op_page %>%
            rvest::html_node(xpath = str_c(
                "//div[@aria-label=",
                "' Card Total number of COVID-19 resident deaths.']")) %>%
            rvest::html_nodes("tspan") %>%
            rvest::html_text()
    
        staff_deaths <- op_page %>%
            rvest::html_node(xpath = str_c(
                "//div[@aria-label=", 
                "' Card Total number of COVID-19 staff deaths.']")) %>%
            rvest::html_nodes("tspan") %>%
            rvest::html_text()
        
        tabid <- str_c(
            "Confirmed COVID-19 Cases Clustered column chart Graph with the ",
            "total number of confirmed COVID-19 cases, ",
            "broken down by resident and staff.")
    
        confirmed_table <- op_page %>%
            rvest::html_nodes(xpath = str_c(
                "//div[@aria-label='", tabid, "']")) %>%
            rvest::html_nodes(".label") %>%
            rvest::html_text()
    
        confirmed_labels <- op_page %>%
            rvest::html_nodes(xpath = str_c(
                "//div[@aria-label='", tabid, "']")) %>%
            rvest::html_nodes(".legend-item-text") %>%
            rvest::html_text()
    
        basic_check(
            confirmed_labels, c("Residents/Patients", "Staff", "Imported"))
    
        tibble(
            Name = facility,
            Residents.Confirmed = confirmed_table[1], 
            Residents.Recovered = 
                recoveries[which(recoveries[,2] == "Residents/Patients"),1],
            Residents.Deaths = resident_deaths[1],
            Staff.Confirmed = confirmed_table[2],
            Staff.Recovered = 
                recoveries[which(recoveries[,2] == "Staff"),1],
            Staff.Deaths = staff_deaths[1]) %>%
            clean_scraped_df()
    }))
}

nevada_extract <- function(x){
    x %>%
        mutate(Name = clean_fac_col_txt(Name))
}

#' Scraper class for general nevada COVID data
#' 
#' @name nevada_scraper
#' @description The NV Scraper works with a standalone Microsoft Power BI
#' app and extracts data from all facilities listed as correctional by saving
#' a copy of the html after selecting a single facility. Because an html
#' document is saved for every selection the 'raw' data is simply an html
#' file which embeds each of these saved html files as an iframe. The scraper
#' is not very robust to changes in the names of labels and as such should be
#' accurate but sensitive to changes to the design of the app. On several
#' occasions data for the number of residents confirmed has been less than the
#' number of residents recovered which should not happen in our own records.
#' Note that the dashboard also contains information on facilities other than
#' for incarcerated populations.
#' \describe{
#'   \item{Facility Name}{The facility name}
#'   \item{Residents Confirmed}{Sometimes can be less than recovered}
#'   \item{Residents Recoveries}{Individuals recovered from COVID-19}
#'   \item{Residents Deaths}{Resident deaths}
#'   \item{Staff Confirmed}{Sometimes can be less than recovered}
#'   \item{Staff Recoveries}{Individuals recovered from COVID-19}
#'   \item{Staff Deaths}{Staff deaths}
#' }

nevada_scraper <- R6Class(
    "nevada_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://doc.nv.gov/About/Press_Release/covid19_updates/",
            id = "nevada",
            type = "html",
            state = "NV",
            # pull the JSON data directly from the API
            pull_func = nevada_pull,
            # 
            restruct_func = nevada_restruct,
            # Rename the columns to appropriate database names
            extract_func = nevada_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state)
        }
    )
)

if(sys.nframe() == 0){
    nevada <- nevada_scraper$new(log=TRUE)
    nevada$raw_data
    nevada$pull_raw()
    nevada$raw_data
    nevada$save_raw()
    nevada$restruct_raw()
    nevada$restruct_data
    nevada$extract_from_raw()
    nevada$extract_data
    nevada$validate_extract()
    nevada$save_extract()
}

