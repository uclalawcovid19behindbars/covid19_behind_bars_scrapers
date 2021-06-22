# Pre-November population data for Oregon comes from public records 
# Post-November data comes from the agency website and historical_oregon_population scraper 
# https://www.oregon.gov/doc/Documents/inmate-profile.pdf

library(tidyverse)
library(behindbarstools)

source("R/utilities.R")

# Load Excel file from public records response 
# Contains data 2020-02-01 through 2020-11-19
raw_pre_nov <- readxl::read_excel("results/raw_files/PR_historical_or_population.xlsx")

# Clean data and add scraper columns 
extracted_pre_nov <- raw_pre_nov %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(!`Count Date`, names_to = "Name", values_to = "Residents.Population") %>% 
    filter(!str_detect(Name, "(?i)total")) %>% 
    clean_scraped_df() %>% 
    mutate(Date = lubridate::ymd(`Count Date`), 
           State = "OR", 
           id = "historical_or_pop",
           jurisdiction = "state", 
           source = "Oregon DOC Public Records Response") %>% 
    select(Name, Residents.Population, State, Date, id, source, jurisdiction) %>% 
    # Limit to pre-regular scraper data 
    filter(Date > "2020-01-01" & Date < "2021-04-30") %>% 
    # Remove date with bad data 
    filter(Date != "2020-09-10")

# Write extracted data 
write_csv(extracted_pre_nov, "results/extracted_data/PR_historical_or_population.csv")
    