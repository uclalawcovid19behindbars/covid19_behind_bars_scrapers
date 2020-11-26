source("./R/utilities.R")
library(behindbarstools)

get_scraper_info <- function(){
    sc_files <- list.files("production/scrapers", full.names = TRUE)
    
    bind_rows(lapply(sc_files, function(x){
        source(x)
        sn <- str_c(
            str_remove(last(unlist(str_split(x, "/"))), ".R"), "_scraper")
        scraper <- get(sn)$new(log = TRUE)
        
        tibble(
            id = scraper$id,
            state = scraper$state,
            jurisdiction = scraper$jurisdiction,
            type = scraper$type,
            url = scraper$url,
            last_update = scraper$last_update()
        )
    }))
}

state_reporting <- function(){
    tmp_df <- load_latest_data()
    tmp_df %>%
        filter(jurisdiction == "state") %>%
        select(State, starts_with("Residents"), starts_with("Staff")) %>%
        group_by(State) %>%
        summarise_all(function(x) any(!is.na(x)))
}

list(
    scraper_df = get_scraper_info(),
    report_df = state_reporting(),
    current_data = load_latest_data(),
    hist_data = load_latest_data(TRUE, coalesce = TRUE) %>%
        filter(!is.na(id), !is.na(jurisdiction)),
    current_state = "https://api.covidtracking.com/v1/states/current.csv" %>%
        read_csv(),
    date = Sys.Date()) %>%
    write_rds("./reports/scrape_details.rds")


# api call for states
# read_csv("https://api.covidtracking.com/v1/states/current.csv")
