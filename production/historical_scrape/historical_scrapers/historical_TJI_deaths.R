# The TJI facility-level population data on the state's open 
# data portal. This script extracts ALL data since 6/20/21 through 9/5/21 
# cleaned csv file. There's some facility name aggregation necessary to 
#' match how the DOC reports COVID data. The portal shows that it's updated 
#' almost daily, although the most recent date is almost a month old. 
#' \describe{
#'   \item{Date}{}
#'   \item{Facility Name}{Name}
#'   \item{Residents.Deaths}{}
#' }

source("./R/generic_scraper.R")
source("./R/utilities.R")

write_to_server <- FALSE

raw <-  "1mOS1wggvyRUOpI-u2VabmnQ1yJPPEgOc2zdZjWxbAwQ" %>% 
    googlesheets4::read_sheet(
        sheet = "Inmate Deaths", 
        col_types = "cDDccccccccccccccc")

x_ <- raw %>% 
    filter(str_detect(FacilityType, "(?i)state|county")) 

fac_types <- x_ %>% 
    distinct(FacilityType) %>% 
    pull()

for (i in fac_types){
    if (!i %in% c("State Prison", "State Jail", "County Jail")){
        stop(paste("Unexpected facility type:", i))
    }
}

if (length(fac_types) != 3){
    stop(paste("Should have pulled State Prison and State Jail facilities.", 
               "Instead pulled:", toString(fac_types)))
}

data_date_agg <- x_ %>% 
    mutate(Facility = paste(Facility, FacilityType)) %>% 
    group_by(DateofDeath, Facility) %>% 
    summarise(new_deaths_by_fac = n()) %>%
    ungroup() %>%
    group_by(Facility) %>%
    arrange(DateofDeath) %>% 
    mutate(Residents.Deaths = cumsum(new_deaths_by_fac)) %>%
    ungroup() %>%
    filter(DateofDeath > as.Date("2021-06-27"),
           DateofDeath < as.Date("2021-09-06"))

dat <- data_date_agg %>%
    group_by(Facility) %>%
    filter(DateofDeath == max(DateofDeath)) 

## sum of deaths close to and prior from September 5th  is 105
## this is far from the number we have gathered, 275, so nevermind re: historical data!
sum(dat$Residents.Deaths)
    
        
out <- data_date_agg %>%
    clean_scraped_df() %>%
    select(Date = DateofDeath,
           Name = Facility,
           Residents.Deaths) %>% 
        # Add scraper columns 
        mutate(
            State = "TX", 
            id = "historical_tji_deaths", 
            jurisdiction = "state", 
            source = "https://texasjusticeinitiative.org/publications/covid-deaths-in-texas"
        )
# 
# if (write_to_server){
#     write_csv(out, "results/extracted_data/2022-04-07_historical_TJI_deaths.csv") 
#     sync_remote_files()
# }