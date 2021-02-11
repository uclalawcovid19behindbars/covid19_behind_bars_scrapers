rm(list=ls())
source("./production/scrapers/new_york.R")
library(lubridate)

scraper_name_vec <- get_scraper_vec()

st_date <- ymd("2020-05-10")
en_date <- ymd("2020-10-19")

current_date <- st_date

# run through the scraper

cat("Starting wayback run for", "new_york", "\n")

while(current_date <= en_date){
    # sunday is 1 and we only want SUN, MON, WED, FRI
    if(lubridate::wday(current_date) %in% c(1, 2, 4, 6)){
        cat("On date", as.character(current_date), "\n")
        
        # run step by step but dont write to perma.cc
        scraper <- get(scraper_name_vec["new_york"])$new(log = FALSE)
        # reset the date to date of scrape
        scraper$reset_date(date = current_date)
        # pull from waybackmachine
        scraper$pull_wayback_raw()
        scraper$raw_data <- scraper$raw_data %>%
            .[[1]] %>%
            str_split("doccs.ny.gov") %>%
            unlist() %>%
            .[2] %>%
            {str_c("https://doccs.ny.gov", .)}
        # rest of the process is normal
        scraper$save_raw()
        Sys.sleep(30)
    }
    
    # increment to the next day
    current_date <- current_date + 1
}
