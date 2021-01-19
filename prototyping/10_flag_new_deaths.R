##Define package list
Packages<-c("tidyverse", "devtools")
.packages = Packages
##Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
##Load packages into session 
lapply(.packages, require, character.only=TRUE)
###Install UCLA Law COVID-19 Behind Bars team package
devtools::install_github("uclalawcovid19behindbars/behindbarstools")
help(package=behindbarstools)

## pull latest data from our server
latest_data <- behindbarstools::read_scrape_data(all_dates = TRUE)

## get date of last scrape, and the previous scrape to compare it to 
date_last_scraped <- max(latest_data$Date)
date_last_scraped

date_before_last_scrape <- max( latest_data$Date[latest_data$Date != max(latest_data$Date)] )
date_before_last_scrape

## calculate change in deaths from previous date scraped
check <- latest_data %>%
    filter(Date == date_last_scraped | Date == date_before_last_scrape) %>%
    group_by(Name, State, Jurisdiction) %>%
    mutate(previous_death_value = dplyr::lag(Residents.Deaths, order_by = Date)) %>%
    ungroup() %>%
    mutate(change_in_deaths = Residents.Deaths - previous_death_value,
           flag_change_deaths = ifelse(change_in_deaths != 0, TRUE, FALSE)) %>%
    arrange(-flag_change_deaths, -change_in_deaths)

check %>% 
    filter(change_in_deaths != 0) %>%
    select(State, Name, Date, Residents.Deaths, previous_death_value, change_in_deaths) %>% 
    View()






