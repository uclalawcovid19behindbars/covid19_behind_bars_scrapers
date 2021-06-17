source("./R/generic_scraper.R")
source("./R/utilities.R")

texas_jails_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)report.pdf") %>%
        first()
}

texas_jails_restruct <- function(x){
    npages <- tabulizer::get_n_pages(x)
    
    res_exp <- c(
        Residents.Active = 
            "Number of inmates with active positive test confirmation",
        Residents.Pending = "Number of inmates pending test results",
        Residents.Quarantine = 
            "Number of inmates quarantined/isolated, not active",
        Drop.Res.Treated =
            "Number of inmates being treated offsite for active, COVID-19",
        Drop.Res.Known = "Number of confirmed deaths related to COVID-19",
        Drop.Res.Sus = "Number of suspected deaths related to COVID-19"
    )
    
    staff_exp <- c(
        Staff.Active = 
            "Number of jailers with active positive test confirmation",
        Drop.Staff.Quarantine =
            "Number of jailers quarantined/isolated pending test results"
    )
    
    
    TXjaildata_ <- NULL
    TXjailnames_ <- NULL
    TXjail_ <- NULL
    
    for(i in 1:npages) {
        res_names <- unlist(tabulizer::extract_tables(
            x, page = i, area = list(c(194, 110, 314, 395)), guess = FALSE))
        staff_names <- unlist(tabulizer::extract_tables(
            x, page = i, area = list(c(338, 110, 370, 380)), guess = FALSE))
        
        basic_check(res_names, res_exp)
        basic_check(staff_names, staff_exp)
        
        # sometime data gets spread over two columns so we want to only
        # grab the first column and remove the empty columns
        TXjaildata_[[i]] <- tabulizer::extract_tables(
            x, page = i, area = list(c(188, 404, 372, 452)), guess = F)[[1]] %>%
            # data we care about is in the first column
            .[,1] %>%
            # we only want valid numeric data
            as.numeric() %>%
            # remove NAs
            na.omit() %>%
            # convert back to character
            as.character()

        TXjailnames_[[i]] <- tabulizer::extract_tables(
            x, page = i, area = list(c(130, 160, 148, 256)), guess = FALSE)
        
        if(length(staff_names) != length(staff_exp)){
            warning(str_c(
                "Names not of expected length. Received: ",
                str_c(staff_names, collapse = ", "),
                ". Expected: ",
                str_c(staff_exp, collapse = ", "),
                "for facility ", TXjailnames_[[i]], " on page ", i
                ))
        }
        
        if(length(res_names) != length(res_exp)){
            warning(str_c(
                "Names not of expected length. Received: ",
                str_c(res_names, collapse = ", "),
                ". Expected: ",
                str_c(res_exp, collapse = ", "),
                "for facility ", TXjailnames_[[i]], " on page ", i
            ))
        }
    }

    df_ <- as.data.frame(
        t(sapply(1:npages, function(i){
            c(TXjailnames_[[i]][[1]], TXjaildata_[[i]])})))
    names(df_) <- c("Name", names(res_exp), names(staff_exp))
    
    as_tibble(df_)
}

texas_jails_extract <- function(x){
    x %>%
        clean_scraped_df() %>%
        mutate(Residents.Deaths = Drop.Res.Known + Drop.Res.Sus) %>%
        select(-starts_with("Drop")) %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        mutate(Name = str_c(Name, " Jail"))
}

#' Scraper class for general texas_jails COVID data
#' 
#' @name texas_jails_scraper
#' @description Texas jails report the data below in a pdf where information is
#' reported for most facilities in the state. Each page of the pdf should have
#' the same information.
#' \describe{
#'   \item{Facility_Name}{The facility name.}
#'   \item{Number of inmates with active positive test confirmation}{Does not look cumulative, needs checking.}
#'   \item{Number of inmates pending test results}{}
#'   \item{Number of inmates quarantined/isolated, not active}{}
#'   \item{Number of inmates being treated offsite for active, COVID-19}{}
#'   \item{Number of confirmed deaths related to COVID-19}{}
#'   \item{Number of suspected deaths related to COVID-19}{}
#'   \item{Number of jailers with active positive test confirmation}{}
#'   \item{Number of jailers quarantined/isolated pending test results}{}
#' }

texas_jails_scraper <- R6Class(
    "texas_jails_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.tcjs.state.tx.us/",
            id = "texas_jails",
            type = "pdf",
            state = "TX",
            jurisdiction = "county",
            check_date = NULL,
            # pull the JSON data directly from the API
            pull_func = texas_jails_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = texas_jails_restruct,
            # Rename the columns to appropriate database names
            extract_func = texas_jails_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction,
                check_date = check_date)
        }
    )
)

if(sys.nframe() == 0){
    texas_jails <- texas_jails_scraper$new(log=TRUE)
    texas_jails$run_check_date()
    texas_jails$raw_data
    texas_jails$pull_raw()
    texas_jails$raw_data
    texas_jails$save_raw()
    texas_jails$restruct_raw()
    texas_jails$restruct_data
    texas_jails$extract_from_raw()
    texas_jails$extract_data
    texas_jails$validate_extract()
    texas_jails$save_extract()
}

