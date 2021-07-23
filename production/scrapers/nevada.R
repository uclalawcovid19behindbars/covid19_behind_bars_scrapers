source("./R/generic_scraper.R")
source("./R/utilities.R")

nevada_clean_fac_text <- function(x){
    str_remove(x, "(?i)NDOC -") %>%
        clean_fac_col_txt()
}

nevada_check_date <- function(x, date = Sys.Date()){
    # scrape from the power bi iframe directly
    y <- str_c(
            "https://app.powerbigov.us/view?r=",
            "eyJrIjoiNDMwMDI0YmQtNmUyYS00ZmFjLWI0MGItZDM0OTY1Y2Y0YzNhIiwidCI6Im",
            "U0YTM0MGU2LWI4OWUtNGU2OC04ZWFhLTE1NDRkMjcwMzk4MCJ9")
    
    remDr <- RSelenium::remoteDriver(
        remoteServerAddr = "localhost",
        port = 4445,
        browserName = "firefox"
    )
    
    del_ <- capture.output(remDr$open())
    remDr$navigate(y)
    
    Sys.sleep(10)
    
    base_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    site_date <- base_page %>%
        rvest::html_nodes(xpath="//*[@id=\"pvExplorationHost\"]/div/div/exploration/div/explore-canvas-modern/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-modern[1]/transform/div/div[3]/div/visual-modern/div/div/div/p[2]/span[1]") %>%
        rvest::html_text() %>%
        {.[str_detect(., "(?i)21")]} %>%
        str_remove("(?i)data were last updated on ") %>%
        lubridate::mdy_hm() %>%
        lubridate::floor_date(unit="day") %>%
        as.Date()
    
    error_on_date(site_date, date)
}

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
    
    remDr$findElement(
        "xpath", 
        str_c(
            "//div[@aria-label='Facility Type Slicer Drop down box to ",
            "select one or more facility types.']"))$clickElement()
    Sys.sleep(10)
    remDr$findElement(
        "xpath", 
        str_c("//div[@class='slicerItemContainer']",
              "/span[@title='Correctional']"))$clickElement()
    Sys.sleep(10)
    remDr$findElement(
        "xpath", "//div[@aria-label='Facility Name,  All']")$clickElement()
    Sys.sleep(10)

    nv_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    box_options <- nv_page %>%
        rvest::html_nodes(".slicerText") %>%
        rvest::html_text()
    
    valid_prison_options <- box_options %>%
        str_replace_all("[^a-zA-Z0-9 -]", "") %>%
        str_remove_all("-") %>%
        str_replace_all(" ", "") %>%
        {grepl("^[[:upper:]]+$", .)} %>%
        which()
    
    # TODO: Need to find a way of this is selected or not given that
    # isElementSelected() is not retuening expected results
    
    # deselect_index <- max(which(box_options == "Select all"))
    # 
    # remDr$findElements(
    #     "css", ".glyphicon.checkbox")[[deselect_index]]$clickElement()
    # Sys.sleep(3)

    sub_dir <- str_c("./results/raw_files/", Sys.Date(), "_nevada")
    dir.create(sub_dir, showWarnings = FALSE)
    
    new_labels <- TRUE
    html_list <- list()
    iters <- 0
    
    while(new_labels & iters < 10){
        # after we have initiated the NV page to only show correction facilities
        # grab the labels that are currently visible and accessible
        for(j in valid_prison_options){
            fac_name <- nevada_clean_fac_text(box_options[j])
            if(!(fac_name %in% names(html_list))){
                
                src_str <- str_c(
                    "//div[@class=\"slicerItemContainer\" and @aria-label=\"",
                    box_options[j], "\"]/div")
                
                elCB <- remDr$findElement("xpath", src_str)
                
                if(elCB$isElementDisplayed()[[1]]){
                    
                    elCB$clickElement()
                    Sys.sleep(7)
                    
                    html_list[[fac_name]] <- xml2::read_html(
                        remDr$getPageSource()[[1]])
                }
            }
        }
        
        # after we have grabbed all the data scroll down on the bar a little
        # bit to make new elements appear
        elSB <- remDr$findElements(
            "xpath", "//div[@class='scroll-bar']")[[4]]
        loc <- elSB$getElementLocation()
        remDr$mouseMoveToLocation(webElement = elSB)
        remDr$buttondown()
        Sys.sleep(1)
        # i have no idea how sustainable the 25 value is here might need
        # to be changed in the future. We want to scroll and y-25 seems
        # to give just enough scroll to show new options if they exist
        # while not skipping over others
        remDr$mouseMoveToLocation(x = loc$x, y=loc$y-25)
        remDr$buttonup()
        
        nv_page <- xml2::read_html(remDr$getPageSource()[[1]])
        
        box_options <- nv_page %>%
            rvest::html_nodes(".slicerText") %>%
            rvest::html_text()
        
        valid_prison_options <- box_options %>%
            str_replace_all("[^a-zA-Z0-9 -]", "") %>%
            str_remove_all("-") %>%
            str_replace_all(" ", "") %>%
            {grepl("^[[:upper:]]+$", .)} %>%
            which()
        
        # is there any new facility info to grab? if yes run this whole loop
        # again
        new_valid_options <- nevada_clean_fac_text(
            box_options[valid_prison_options])
        new_labels <- !all(new_valid_options %in% names(html_list))
        
        # if there were no overlapping labels then we might have skipped
        # some facilities which is bad!
        if(!any(new_valid_options %in% names(html_list))){
            warning(
                "There was no overlap in labels. Check to make sure no ",
                "facilities were skipped.")
        }
        
        # avoid the infinite loop
        iters <- iters + 1
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
        
        # get the values of confirmed
        confirmed <- op_page %>%
            rvest::html_nodes(".labelGraphicsContext") %>%
            .[[3]] %>%
            rvest::html_nodes("text") %>%
            rvest::html_text() %>%
            as.numeric()
        # make sure labels match what we expect
        confirmed_labels <- op_page %>%
            rvest::html_nodes(".legend-item-container") %>%
            .[[2]] %>%
            rvest::html_nodes("text") %>%
            rvest::html_text()
        names(confirmed) <- confirmed_labels
        
        basic_check(
            confirmed_labels, c("Residents/Patients", "Staff", "Imported"))

        svg_cards <- op_page %>%
            rvest::html_nodes(".card")
        
        resident_deaths <- svg_cards %>%
            rvest::html_attr("aria-label") %>%
            # get the card that has resident in it but isnt a percentage
            {which(str_detect(., "Resident") & !str_detect(., "%"))} %>%
            {svg_cards[.]} %>%
            rvest::html_node("title") %>%
            rvest::html_text() %>%
            as.numeric()
        
        staff_deaths <- svg_cards %>%
            rvest::html_attr("aria-label") %>%
            # get the card that has staff in it but isnt a percentage
            {which(str_detect(., "Staff") & !str_detect(., "%"))} %>%
            {svg_cards[.]} %>%
            rvest::html_node("title") %>%
            rvest::html_text() %>%
            as.numeric()
        
        if(any(is.null(c(confirmed, staff_deaths, resident_deaths)))){
            warning(
                "NA values extracted where there should not be. Please inspect")
        }
    
        tibble(
            Name = facility,
            Residents.Confirmed = confirmed["Residents/Patients"],
            Residents.Deaths = resident_deaths,
            Staff.Confirmed = confirmed["Staff"],
            Staff.Deaths = staff_deaths) %>%
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
            jurisdiction = "state",
            check_date = nevada_check_date,
            # pull the JSON data directly from the API
            pull_func = nevada_pull,
            restruct_func = nevada_restruct,
            # Rename the columns to appropriate database names
            extract_func = nevada_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    nevada <- nevada_scraper$new(log=TRUE)
    nevada$run_check_date()
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

