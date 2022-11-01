source("./R/generic_scraper.R")
source("./R/utilities.R")
source("./R/selenium_driver.R")

vermont_check_date <- function(url, date = Sys.Date()){
    remDr <- initiate_remote_driver()
    
    remDr$open(silent = TRUE)
    remDr$navigate(url)
    Sys.sleep(5)
    
    date.page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    # using xpath to locate date of last update
    site_date <- date.page %>%
        rvest::html_nodes(xpath = '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[2]/transform/div/div[2]/div/visual-modern/div/svg/g[1]/text/tspan') %>%
        rvest::html_text() %>%
        lubridate::mdy_hms() %>%
        as.Date()
    
    remDr$close()
    
    error_on_date(site_date, date)
}

vermont_pull <- function(x){
    app_source <- str_c(
        "https://app.powerbigov.us/view?r=", 
        "eyJrIjoiMWVkOTk2MWEtNmQzNC00M2M5LWIyMzItZDk3N2RmYmMwNmIxIiwid",
        "CI6IjIwYjQ5MzNiLWJhYWQtNDMzYy05YzAyLTcwZWRjYzc1NTljNiJ9")
    
    remDr <- initiate_remote_driver()
    remDr$open(silent = TRUE)
    Sys.sleep(2)
    
    remDr$navigate(app_source)
    Sys.sleep(10)
    
    # find the div that has the select all option and see if it is marked
    select_all_ss <- "//span[@title='Select all']/parent::div"
    select_all_node <- remDr$findElement("xpath", select_all_ss)
    
    # if it is marked then we need to unmark it so that we select individual facilities
    if(unlist(select_all_node$getElementAttribute("aria-checked")) == "true"){
        select_all_node$clickElement()
        Sys.sleep(10)
    }
    
    # get a static version of the page
    res_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    # get all the options for facilities
    box_options <- res_page %>%
        rvest::html_nodes(".slicerItemContainer") %>%
        rvest::html_text()
    
    res_list <- list()
    
    # capture page of each facility and index into list
    for(i in 2:length(box_options)){
        box_option_i <- box_options[i]
        src_str <- str_c("//span[@title='", box_option_i, "']/parent::div")
        
        elCB <- remDr$findElement("xpath", src_str)
        Sys.sleep(2)
        
        elCB$clickElement()
        Sys.sleep(2)
        
        res_list[[box_options[i]]] <- xml2::read_html(
            remDr$getPageSource()[[1]])
        
    }
    
    Sys.sleep(2)
    
    # access second sheet to scrape staff data
    next_node <- remDr$findElement("xpath", "//button[@aria-label='Next Page']")
    next_node$clickElement()
    
    Sys.sleep(2)
    
    fac.front <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container[5]/transform/div/div[2]/div/visual-modern/div/svg/svg/g[1]/g[1]/g['
    fac.end <- ']/text/title'
    fac_names <- character()
    
    staff_page <- xml2::read_html(remDr$getPageSource()[[1]])
    
    # capture facility names from x-axis of bar chart
    for(i in 1:20){
        
        fac_names[i] <- staff_page %>%
            rvest::html_nodes(xpath = str_c(fac.front, i, fac.end)) %>%
            rvest::html_text()
        
    }
    
    # iteratively click through each bar of bar graph to capture individual facility data
    staff_list = list()
    
    for(i in 1:length(fac_names)) {
        
        # css seems to work here better than xpath
        bar_ss <- str_c('#pvExplorationHost > div > div > exploration > div > explore-canvas > div > div.canvasFlexBox > div > div.displayArea.disableAnimations.fitToPage > div.visualContainerHost.visualContainerOutOfFocus > visual-container-repeat > visual-container:nth-child(5) > transform > div > div.visualContent > div > visual-modern > div > svg > svg > g.axisGraphicsContext.columnChart > g.columnChartUnclippedGraphicsContext > svg > g > rect:nth-child(', i, ')')
        bar_node <- remDr$findElement("css", bar_ss)
        Sys.sleep(2)
        bar_node$clickElement()
        Sys.sleep(2)
        staff_list[[fac_names[i]]] <- xml2::read_html(
            remDr$getPageSource()[[1]])
        
    }
    
    sub_dir <- str_c("./results/raw_files/", Sys.Date(), "_vermont")
    
    dir.create(sub_dir, showWarnings = FALSE)
    
    # let's post process all the individuals pages for staff and residents
    # and stitch them together into a single web page
    
    fns_staff <- str_c(sub_dir, "/", names(staff_list), "_staff", ".html")
    fns_res <- str_c(sub_dir, "/", names(res_list), "_residents", ".html")
    
    Map(xml2::write_html, res_list, fns_res)
    Map(xml2::write_html, staff_list, fns_staff)
    
    iframes <- lapply(str_remove(c(fns_staff, fns_res), "/results/raw_files"), function(fn)
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

vermont_restruct <- function(x){
    
    sub_files <- x %>%
        rvest::html_nodes("iframe") %>%
        rvest::html_attr("src") %>%
        str_replace("\\./", "./results/raw_files/")
    
    # identify files by ending
    staff_files <- sub_files[grepl("staff.html", sub_files)]
    res_files <- sub_files[grepl("residents.html", sub_files)]
    
    res.data <- bind_rows(lapply(res_files, function(hl){
        
        op_page <- xml2::read_html(hl)
        
        facility <- hl %>%
            str_split("/") %>%
            unlist() %>%
            last() %>%
            str_remove("_residents.html")
        
        element.front.res <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container['
        element.end.res <- ']/transform/div/div[2]/div/visual-modern/div/svg/g[1]/text/tspan[1]'
        element.end.title.res <- ']/transform/div/div[2]/div/visual-modern/div/svg/g[2]/text/tspan[1]'
        
        ## Pull Values and Titles
        
        residents.confirmed.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front.res, 3, element.end.res)) %>%
            rvest::html_text() %>%
            as.numeric()
        
        residents.active.value <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front.res, 4, element.end.res)) %>%
            rvest::html_text() %>%
            as.numeric()
        
        residents.deaths.value <-  op_page %>%
            rvest::html_nodes(xpath = str_c(element.front.res, 11, element.end.res)) %>%
            rvest::html_text() %>%
            as.numeric()
        
        residents.confirmed.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front.res, 3, element.end.title.res)) %>%
            rvest::html_text() 
        
        residents.active.title <- op_page %>%
            rvest::html_nodes(xpath = str_c(element.front.res, 4, element.end.title.res)) %>%
            rvest::html_text() 
        
        residents.deaths.title <-  op_page %>%
            rvest::html_nodes(xpath = str_c(element.front.res, 11, element.end.title.res)) %>%
            rvest::html_text() 
        
        if(any(is.null(c(residents.confirmed.value, residents.active.value, residents.deaths.value)))){
            warning(
                "NA values extracted where there should not be. Please inspect")
        }
        
        if(!str_detect(residents.confirmed.title, 'Unique Positive Cases') | !str_detect(residents.active.title, 'Currently Incarcerated Positiv') | 
           !str_detect(residents.deaths.title, 'COVID-Related Deaths')){
            warning(
                "Pulled Element Titles are not as expected. The order of elements may have changed. Please inspect"
            )
        }
        
        tibble(
            Name = facility,
            Residents.Confirmed = residents.confirmed.value,
            Residents.Deaths = residents.deaths.value,
            Residents.Active = residents.active.value) %>%
            clean_scraped_df()
    }))
    
    staff.data <- bind_rows(lapply(staff_files, function(hf){
        
        op_page.staff <- xml2::read_html(hf)
        
        facility.staff <- hf %>%
            str_split("/") %>%
            unlist() %>%
            last() %>%
            str_remove("_staff.html")
        
        element.front.staff <- '/html/body/div[1]/report-embed/div/div/div[1]/div/div/div/exploration-container/div/div/docking-container/div/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container['
        element.end.staff <- ']/transform/div/div[2]/div/visual-modern/div/svg/g[1]/text/tspan[1]'
        element.end.title.staff <- ']/transform/div/div[2]/div/visual-modern/div/svg/g[2]/text/tspan[1]'
        
        ## Pull Values and Titles
        
        staff.active.value <- op_page.staff %>%
            rvest::html_nodes(xpath = str_c(element.front.staff, 3, element.end.staff)) %>%
            rvest::html_text() %>%
            as.numeric()
        
        staff.confirmed.value <- op_page.staff %>%
            rvest::html_nodes(xpath = str_c(element.front.staff, 6, element.end.staff)) %>%
            rvest::html_text() %>%
            as.numeric()
        
        staff.deaths.value <-  op_page.staff %>%
            rvest::html_nodes(xpath = str_c(element.front.staff, 8, element.end.staff)) %>%
            rvest::html_text() %>%
            as.numeric()
        
        staff.active.title <- op_page.staff %>%
            rvest::html_nodes(xpath = str_c(element.front.staff, 3, element.end.title.staff)) %>%
            rvest::html_text() 
        
        staff.confirmed.title <- op_page.staff %>%
            rvest::html_nodes(xpath = str_c(element.front.staff, 6, element.end.title.staff)) %>%
            rvest::html_text() 
        
        staff.deaths.title <-  op_page.staff %>%
            rvest::html_nodes(xpath = str_c(element.front.staff, 8, element.end.title.staff)) %>%
            rvest::html_text() 
        
        if(any(is.null(c(staff.confirmed.value, staff.active.value, staff.deaths.value)))){
            warning(
                "NA values extracted where there should not be. Please inspect")
        }
        
        if(!str_detect(staff.confirmed.title, 'Unique Positive Cases') | !str_detect(staff.active.title, 'Current Staff Positives') | 
           !str_detect(staff.deaths.title, 'COVID-Related Deaths')){
            warning(
                "Pulled Element Titles are not as expected. The order of elements may have changed. Please inspect"
            )
        }
        
        tibble(
            Name = facility.staff,
            Staff.Confirmed = staff.confirmed.value,
            Staff.Deaths = staff.deaths.value,
            Staff.Active = staff.active.value) %>%
            clean_scraped_df()
    }))
    
    # left join staff and resident data to create final dataset
    out.data <- merge(x = staff.data, y = res.data, by = "Name", all.x = TRUE) %>%
        clean_scraped_df()
}

vermont_extract <- function(x){
    x %>%
        mutate(Name = clean_fac_col_txt(Name))
}

#' Scraper class for general Vermont COVID data
#' 
#' @name vermont_scraper
#' @description The VT Scraper works with a standalone Microsoft Power BI
#' app and extracts data from all facilities listed as correctional by saving
#' a copy of the html after selecting a single facility. Because an html
#' document is saved for every selection the 'raw' data is simply an html
#' file which embeds each of these saved html files as an iframe. The data
#' for residents exist on the first tab of the dashboard while staff data
#' exist on the second tab. Navigation for the two tabs is slightly different
#' but scraping methodology is similar. The scraper is not very robust to 
#' changes in the names of labels and as such should be accurate but 
#' sensitive to changes to the design of the app.
#' 
#' \describe{
#'   \item{Facility Name}{The facility name}
#'   \item{Staff Confirmed}
#'   \item{Staff Deaths}
#'   \item{Staff Active}
#'   \item{Residents Deaths}
#'   \item{Residents Confirmed} 
#'   \item{Residents Active}
#' }

vermont_scraper <- R6Class(
    "vermont_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
        log,
        url = str_c(
            "https://app.powerbigov.us/view?r=",
            "eyJrIjoiMWVkOTk2MWEtNmQzNC00M2M5LWIyMzItZDk3N2RmYmMwNmIxIiwidCI6IjIwYjQ5MzNiLWJhYWQtNDMzYy05YzAyLTcwZWRjYzc1NTljNiJ9"),
        id = "vermont",
        type = "html",
        state = "VT",
        jurisdiction = "state",
        check_date = vermont_check_date,
        # pull the JSON data directly from the API
        pull_func = vermont_pull,
        restruct_func = vermont_restruct,
        # Rename the columns to appropriate database names
        extract_func = vermont_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    vermont <- vermont_scraper$new(log=TRUE)
    vermont$run_check_date()
    vermont$raw_data
    vermont$pull_raw()
    vermont$raw_data
    vermont$save_raw()
    vermont$restruct_raw()
    vermont$restruct_data
    vermont$extract_from_raw()
    vermont$extract_data
    vermont$validate_extract()
    vermont$save_extract()
}