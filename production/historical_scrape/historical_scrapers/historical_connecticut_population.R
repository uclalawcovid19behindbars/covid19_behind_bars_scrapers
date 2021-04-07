# Connecticut's DOC posts facility-level population data on the state's open 
# data portal. This script extracts ALL data since 2020-01-01 into a single 
# cleaned csv file. There's some facility name aggregation necessary to 
#' match how the DOC reports COVID data. The portal shows that it's updated 
#' almost daily, although the most recent date is almost a month old. 
#' \describe{
#'   \item{Date}{}
#'   \item{Facility Name}{Name}
#'   \item{Accused/Other Status Count}{}
#'   \item{Sentenced Status Count}{}
#'   \item{Total Facility Population Count}{Residents.Population}
#' }

source("./R/generic_scraper.R")
source("./R/utilities.R")

write_to_server <- FALSE

raw <- read_csv("https://data.ct.gov/api/views/n8x6-s299/rows.csv", 
         col_types = "ccccc") %>% 
    mutate(Date = lubridate::mdy(Date)) %>% 
    filter(Date >= as.Date("2020-01-01"))

check_names(raw, c(
    "Date", 
    "Facility Name",  
    "Accused/Other Status Count", 
    "Sentenced Status Count", 
    "Total Facility Population Count"
))

out <- raw %>% 
    select(
        "Name" = "Facility Name", 
        "Residents.Population" = "Total Facility Population Count", 
        Date
    ) %>% 
    clean_scraped_df() %>% 
    
    # Aggregate names to match COVID data reporting 
    mutate(
        Name = clean_fac_col_txt(Name, to_upper = TRUE), 
        Name = case_when(str_detect(Name, "MACDOUGALL|WALKER") ~ "MACDOUGALL WALKER", 
                         str_detect(Name, "CORRIGAN|RADGOWSKI") ~ "CORRIGAN RADGOWSKI",
                         str_detect(Name, "WILLARD|CYBULSKI") ~ "WILLARD CYBULSKI", 
                         TRUE ~ Name)
    ) %>% 
    group_by(Name, Date) %>% 
    summarise(Residents.Population = sum_na_rm(Residents.Population)) %>% 
    ungroup() %>%
    
    # Add scraper columns 
    mutate(
        State = "CT", 
        id = "historical_ct_pop", 
        jurisdiction = "state", 
        source = stringr::str_c("https://data.ct.gov/Public-Safety/", 
                                "Correctional-Facility-Daily-Population-Count-By-Fa/n8x6-s299")
    )
    
if (write_to_server){
    write_csv(out, "results/extracted_data/2021-03-31_historical_ct_pop.csv") 
    sync_remote_files()
}
