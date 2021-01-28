source("./R/generic_scraper.R")
source("./R/utilities.R")

washington_pull <- function(x){
    xml2::read_html(x)
}

washington_restruct <- function(x){
    x %>%
        rvest::html_nodes(., css = 'table')
}

washington_extract <- function(x){
    wa_tables <- x
    
    res.tab <- rvest::html_table(wa_tables[1], fill = TRUE) %>%
        as.data.frame() %>%
        rename(
            "Name" = "Location",
            "Residents.Confirmed" = "Number.Confirmed.Cases",
            "Drop" = "Number.of.New.Positive.Cases.Confirmed..last.30.days.",
            "Residents.Deaths" = "Number.of.Deaths..also.counted.in.Confirmed.Cases.") %>%
        # remove rows that are actually subheaders
        filter(
            !(Name %in% c("Work Release",
                          "Prisons",
                          "Other",
                          "Totals",
                          "All Locations"))) %>%
        select(-Drop) %>% 
        clean_scraped_df()
    
    
    # get people in RCFs; they count as confirmed resident cases
    rcf.tab <- rvest::html_table(wa_tables[[8]], fill = TRUE) %>% 
        as.data.frame() %>%
        rename(
            "Name" = "Regional Care Facility",
            "Residents.Confirmed_rcf" = "Incarcerated Individuals Housed") %>%
        clean_scraped_df()
    
    # add the RCF counts
    res.tab <- full_join(rcf.tab, res.tab, by = "Name")
    res.tab[is.na(res.tab)] <- 0
    
    res.tab$Residents.Confirmed <- res.tab$Residents.Confirmed + 
        res.tab$Residents.Confirmed_rcf
    res.tab.lean <- select(res.tab, -Residents.Confirmed_rcf)
    
    bad_names <- c(
        "Business & Training Offices",
        "Prisons",
        "Work Release",
        "All Locations",
        "Totals",
        "Other",
        str_c(
            "Community Corrections(See Community Facilities Map (pdf) ",
            "for section designations)"))
    
    # staff
    staff.tab <- rvest::html_table(wa_tables[3], fill = TRUE) %>%
        as.data.frame() %>%
        rename(
            "Name" = "Location",
            "Staff.Confirmed" = "Number.Confirmed.Cases",
            "Staff.Deaths"    = "Number.of.Deaths") %>%
        filter(!(Name %in% bad_names)) %>%
        clean_scraped_df()
    
    wa <- full_join(staff.tab, res.tab.lean, by = "Name")
    
    # statewide testing counts
    # no longer reported
    # test.tab <- rvest::html_table(wa_tables[[9]], fill = TRUE) %>%
    #     as.data.frame() %>%
    #     select(-c(`Individuals Tested`,`Positive Results`)) %>%
    #     rename(
    #         "Residents.Tested" = "Tests Completed",
    #         "Residents.Negative" = "Negative Results",
    #         "Residents.Pending" = "Pending Lab Results") 
    # test.tab$Name <- "State-Wide"
    # test.tab <- clean_scraped_df(test.tab)
    
    # statewide quarantine counts and add to the statewide table
    quar.tab <- rvest::html_table(wa_tables[[9]], fill = TRUE) %>%
        as.data.frame() %>%
        rename("isolated"  = "Incarcerated Individuals in Isolation",
               "quarantine" = "Incarcerated Individuals in Quarantine")
    quar.tab$Name <-  "State-Wide"
    quar.tab <- clean_scraped_df(quar.tab)
    quar.tab$Residents.Quarantine <- quar.tab$isolated + quar.tab$quarantine
    quar.tab <- select(quar.tab, -isolated, -quarantine)
    
    
    state.tab <- quar.tab #full_join(test.tab, quar.tab, by = "Name")
    
    dplyr::bind_rows(state.tab, wa) %>%
        as_tibble()
}

#' Scraper class for general washington COVID data
#' 
#' @name washington_scraper
#' @description WA has a number of html tables on their page with both facility
#' specific and state-wide information. As of 1-22-21 testing data stopped being
#' reported.
#' \describe{
#'   \item{Residents Number Confirmed Cases}{Facility specific}
#'   \item{Residents Number of Deaths}{Facility specific}
#'   \item{Resident Active cases}{state-wide}
#'   \item{Resident recovered cases}{state-wide}
#'   \item{Resident age}{state wide}
#'   \item{Resident confirmed age}{state wide}
#'   \item{Resident race}{state wide}
#'   \item{Resident confirmed race}{state wide}
#'   \item{Resident ethicity}{state wide}
#'   \item{Resident confirmed ethnicity}{state wide}
#'   \item{Resident confirmed in regional care}{facility specific}
#'   \item{Resident quarantined/isolated}{state wide}
#'   \item{Staff Number Confirmed Cases}{Facility specific}
#'   \item{Staff Number of Deaths}{Facility specific}
#' }

washington_scraper <- R6Class(
    "washington_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.doc.wa.gov/corrections/covid-19/data.htm",
            id = "washington",
            type = "html",
            state = "WA",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = washington_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = washington_restruct,
            # Rename the columns to appropriate database names
            extract_func = washington_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    washington <- washington_scraper$new(log=TRUE)
    washington$raw_data
    washington$pull_raw()
    washington$raw_data
    washington$save_raw()
    washington$restruct_raw()
    washington$restruct_data
    washington$extract_from_raw()
    washington$extract_data
    washington$validate_extract()
    washington$save_extract()
}

