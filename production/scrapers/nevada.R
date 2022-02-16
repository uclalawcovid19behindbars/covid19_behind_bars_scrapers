source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

nevada_clean_fac_text <- function(x){
    str_remove(x, "(?i)NDOC -") %>%
        clean_fac_col_txt()
}

nevada_check_date <- function(url, date = Sys.Date()){
    remDr <- initiate_remote_driver()
    
    remDr$open(silent = TRUE)
    remDr$navigate(url)
    
    site_date <- remDr$findElement(using = "css", "transform")$getElementText() %>% 
        unlist() %>%
        {.[str_detect(., "20")]} %>% # look for year 20xx
        lubridate::mdy()

    remDr$close()
    
    error_on_date(site_date, date)
}

nevada_pull <- function(x){
    app_source <- str_c(
        "https://app.powerbigov.us/view?r=",
        "eyJrIjoiNDMwMDI0YmQtNmUyYS00ZmFjLWI0MGItZDM0OTY1Y2Y0YzNhIiwidCI6Im",
        "U0YTM0MGU2LWI4OWUtNGU2OC04ZWFhLTE1NDRkMjcwMzk4MCJ9")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    remDr$navigate(app_source)
    Sys.sleep(10)
    # open up the facility drop down menu
    remDr$findElement(
        "xpath", 
        str_c(
            "//div[@aria-label='Facility Type Final']"))$clickElement()
    Sys.sleep(10)
    # find the div that has the select all option and see if it is marked
    select_all_ss <- "//span[@title='Select all']/parent::div"
    select_all_node <- remDr$findElement("xpath", select_all_ss)

    # if it is marked then we need to unmark it so we can only select
    # correctional facilities
    if(unlist(select_all_node$getElementAttribute("aria-checked")) == "true"){
        select_all_node$clickElement()
        Sys.sleep(10)
    }

    # find the correctional facilities tab and mark it
    remDr$findElement(
        "xpath", 
        str_c("//span[@title='Correctional']/parent::div"))$clickElement()
    Sys.sleep(10)
    # close the dropdown for facility types
    remDr$findElement(
        "xpath", 
        str_c(
            "//div[@aria-label='Facility Type Final']"))$clickElement()
    Sys.sleep(10)
    # open the dropdown for specific facilities
    remDr$findElement(
        "xpath", 
        str_c(
            "//div[@aria-label='Facility Name']"))$clickElement()
    Sys.sleep(10)
    
    # same thing here if select all is selected we need to unmark it
    fac_select_all_node <- remDr$findElement("xpath", select_all_ss)
    
    if(unlist(
        fac_select_all_node$getElementAttribute("aria-checked")) == "true"){
        fac_select_all_node$clickElement()
        Sys.sleep(10)
    }
    
    # get a static version of the page
    nv_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    # get all the box options in this current drop down menu
    box_options <- nv_page %>%
        rvest::html_nodes(".slicerItemContainer") %>%
        rvest::html_text()
    
    # find the ones that are valid prison options
    valid_prison_options <- box_options %>%
        str_replace_all("[^a-zA-Z0-9 -]", "") %>%
        str_remove_all("-") %>%
        str_replace_all(" ", "") %>%
        {grepl("^[[:upper:]]+$", .)} %>%
        which()
    

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
                # find the box option that matches the current facility
                box_option_j <- box_options[which(str_detect(box_options, fac_name))]
                
                # make sure it only matches one argumnet
                if(length(box_option_j) != 1){
                    stop("Webpage not as expected please inspect names of facilities")
                }
                
                # find the corresponding selector
                src_str <- str_c(
                    "//span[@title='", box_option_j, "']/parent::div")
                elCB <- remDr$findElement("xpath", src_str)
                
                # check that it is actually displayed and not just phantom displayed
                if(elCB$isElementDisplayed()[[1]]){
                    
                    # click the box
                    elCB$clickElement()
                    Sys.sleep(7)
                    
                    # save the webpage
                    html_list[[fac_name]] <- xml2::read_html(
                        remDr$getPageSource()[[1]])
                }
            }
        }
        
        # after we have grabbed all the data scroll down on the bar a little
        # bit to make new elements appear
        elSB <- remDr$findElements(
            "xpath", "//div[@class='scroll-bar']")[[2]]
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
    
    # lets post process all teh individuals pages and stich them 
    # together into a single web page
    fns <- str_c(sub_dir, "/", names(html_list), ".html")
    
    Map(xml2::write_html, html_list, fns)
    
    iframes <- lapply(str_remove(fns, "/results/raw_files"), function(fn)
        htmltools::tags$iframe(
            src = fn, 
            style="display:block", 
            height="1000", width="1200"
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

nevada_restruct <- function(x){
    
    sub_files <- x %>%
        rvest::html_nodes("iframe") %>%
        rvest::html_attr("src") %>%
        str_replace("\\./", "./results/raw_files/")
    
    out.data <- bind_rows(lapply(sub_files, function(hl){
    
        op_page <- xml2::read_html(hl)
        
        facility <- hl %>%
            str_split("/") %>%
            unlist() %>%
            last() %>%
            str_remove("\\.html")
        element.front <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[9]/transform/div/div[3]/div/visual-modern/div/div/div/p['
        element.end.value1 <- ']/span[1]'
        element.end.title.val <- ']/span[2]'
        element.end.title1 <- ']/span[4]'
        
        ## pres //*[@id="pvExplorationHost"]/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[9]/transform/div/div[3]/div/visual-modern/div/div/div/p[4]/span[4]
        ## pres /html/body/div[1]/root/div/div/div[1]/div/div/div/exploration-container/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[9]/transform/div/div[3]/div/visual-modern/div/div/div/p[4]/span[2]
        ## ires /html/body/div[1]/root/div/div/div[1]/div/div/div/exploration-container/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[9]/transform/div/div[3]/div/visual-modern/div/div/div/p[7]/span[4]
        ## pstaff /html/body/div[1]/root/div/div/div[1]/div/div/div/exploration-container/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[9]/transform/div/div[3]/div/visual-modern/div/div/div/p[5]/span[1]

        ## Pull Values and Titles
        residents.confirmed.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 2, element.end.value1)) %>%
            rvest::html_text() %>%
            str_replace_all(., ',', '') %>%
            as.numeric()
        residents.confirmed.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 2, element.end.title.val)) %>%
            rvest::html_text()
        residents.probable.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 4, element.end.title.val)) %>%
            rvest::html_text() %>%
            as.numeric()
        residents.probable.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 4, element.end.title1)) %>%
            rvest::html_text()
        residents.imported.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 7, element.end.title.val)) %>%
            rvest::html_text() %>%
            as.numeric()
        residents.imported.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 7, element.end.title1)) %>%
            rvest::html_text()
        staff.confirmed.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 3, element.end.value1)) %>%
            rvest::html_text()%>%
            as.numeric()
        staff.confirmed.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 5, element.end.title.val)) %>%
            rvest::html_text()
        staff.probable.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 5, element.end.value1)) %>%
            rvest::html_text()%>%
            as.numeric()
        staff.probable.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 5, element.end.title.val)) %>%
            rvest::html_text()
        residents.deaths.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 10, element.end.title.val)) %>%
            rvest::html_text()%>%
            as.numeric()
        residents.deaths.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 10, element.end.title1)) %>%
            rvest::html_text()
        staff.deaths.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 11, element.end.title.val)) %>%
            rvest::html_text()%>%
            as.numeric()
        staff.deaths.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front, 11, element.end.title1)) %>%
            rvest::html_text()
        
        if(any(is.null(c(residents.confirmed.value, staff.deaths.value, residents.deaths.value)))){
            warning(
                "NA values extracted where there should not be. Please inspect")
        }
        
        if(!str_detect(residents.confirmed.title, 'PATIENT CASES') | !str_detect(staff.confirmed.title, 'STAFF CASES') | !str_detect(residents.deaths.title, 'PATIENT DEATHS') | !str_detect(staff.deaths.title, 'STAFF DEATHS') |
           !str_detect(residents.probable.title, 'PROBABLE') | !str_detect(residents.imported.title, 'IMPORTED') | !str_detect(staff.probable.title, 'PROBABLE')){
            warning(
                "Pulled Element Titles are not as expected. The order of elements may have changed. Please inspect"
            )
        }
        
        residents.confirmed.total <- sum(residents.confirmed.value, residents.probable.value, residents.imported.value)
        staff.confirmed.total <- sum(staff.confirmed.value, staff.probable.value)
    
        tibble(
            Name = facility,
            Residents.Confirmed = residents.confirmed.total,
            Residents.Deaths = residents.deaths.value,
            Staff.Confirmed = staff.confirmed.total,
            Staff.Deaths = staff.deaths.value) %>%
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
            # DOC press releases are here: http://doc.nv.gov/About/Press_Release/covid19_updates/
            url = str_c(
                "https://app.powerbigov.us/view?r=",
                "eyJrIjoiNDMwMDI0YmQtNmUyYS00ZmFjLWI0MGItZDM0OTY1Y2Y0YzNhIiwidCI6Im",
                "U0YTM0MGU2LWI4OWUtNGU2OC04ZWFhLTE1NDRkMjcwMzk4MCJ9"),
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

