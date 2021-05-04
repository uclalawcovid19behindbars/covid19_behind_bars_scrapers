rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(googlesheets4)
source("./R/utilities.R")
gs4_auth("ucla.law.covid.staff@gmail.com")

manual_youth_data_loc <- "17mC-uHp1jhMQO8JGqn4is6pJLrKHP0G0TR57R01MxrY"
youth_sheet_destination <- "1X6uJkXXS-O6eePLxw2e4JeRtM41uPZ2eRcOA_HkPVTk" 

## set column order for row-binding data sets
column_order <- c("State", "Name", "Date", "Residents.Confirmed",
                  "Staff.Confirmed", "Residents.Active", "Residents.Deaths",
                  "Staff.Deaths", "Address", "City",
                  "Facility.ID")

scraped_states <- c("Georgia", "Indiana",
                    "Kansas", "Louisiana", "Maryland", "Missouri",
                    "Montana", "Nebraska", "North Carolina", 
                    "North Dakota", "Pennsylvania", "South Carolina",
                    "Wisconsin", "Maine", "New Mexico")

## convert manually-collected youth facilities to clean data
manual_youth_dat_sheet <- read_sheet(manual_youth_data_loc, 
                                     sheet = "Permanent",
                                     col_types = "c") 

manual_youth_dat <- manual_youth_dat_sheet %>%
    mutate(Residents.Confirmed = string_to_clean_numeric(`Confirmed Cases (Youth)`),
           Staff.Confirmed = string_to_clean_numeric(`Confirmed Cases (Staff)`),
           Facility.ID = NA,
           Address = NA,
           City = NA,
           Residents.Active = NA,
           Residents.Deaths = NA,
           Staff.Deaths = NA
           ) %>%
    dplyr::rename(jurisdiction = Jurisdiction, 
           Name = `County/Name of Facility`,
           Date = `Date of last positive case/last update`
           ) %>%
    filter(!is.na(Name),
           !str_detect(Name, "(?i)total")) %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    select(all_of(column_order)) %>%    
    ## remove manual data if we have a scraper for it
    filter(!State %in% scraped_states) 

all_dat <- read_scrape_data(all_dates = FALSE) 
youth_df <- all_dat %>%
    filter(Age == "Juvenile")

# check for any other youth facilities 
other_youth <- all_dat %>% 
    filter((str_detect(Name, "(?i)juvenile|youth"))) %>%
    filter(Age != "Juvenile" | is.na(Age))

## bind together age-classified and name-searched
all_scraped_youth <- youth_df %>%
    bind_rows(other_youth) 

all_youth <- all_scraped_youth %>%
    ## save only the latest data
    group_by(Facility.ID) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    select(all_of(column_order)) %>%
    bind_rows(manual_youth_dat) %>%
    arrange(State, Name, Date) %>%
    mutate(Date = as.character(Date),
           Name = str_to_title(Name))
    
## add youth total counts
sum_res_confirmed <- sum_na_rm(all_youth$Residents.Confirmed)
sum_res_active <- sum_na_rm(all_youth$Residents.Active)
sum_res_deaths <- sum_na_rm(all_youth$Residents.Deaths)
sum_staff_confirmed <- sum_na_rm(all_youth$Staff.Confirmed)
sum_staff_deaths <- sum_na_rm(all_youth$Staff.Deaths)

all_youth_out <- all_youth %>%
    add_row(Date = "TOTAL", 
            State = "", 
            Name = "",
            Residents.Confirmed = sum_res_confirmed,
            Residents.Active = sum_res_active,
            Residents.Deaths = sum_res_deaths,
            Staff.Confirmed = sum_staff_confirmed,
            Staff.Deaths = sum_staff_deaths,
            .before = 1
                ) %>%
        select(
            `Facility ID` = Facility.ID,
            Name, 
            Address, 
            City, 
            State,
            `Cumulative Confirmed Cases - YOUTH` = Residents.Confirmed,
            `Active Confirmed Cases - YOUTH` = Residents.Active,
            `Confirmed Deaths - YOUTH` = Residents.Deaths,
            `Cumulative Confirmed Cases - STAFF` = Staff.Confirmed,
            `Confirmed Deaths - STAFF` = Staff.Deaths,
            Date
        ) %>%
    relocate(Date, State, Name, ends_with("YOUTH"), ends_with("STAFF"))

## delete current data (except headers, for formatting)
range_flood(ss = youth_sheet_destination,
            sheet = "COVID-19 Youth Correctional Facilities",
            range = "A2:M800", 
            cell = "")

## write new data
range_write(
    data = all_youth_out, 
    ss = youth_sheet_destination, 
    sheet = "COVID-19 Youth Correctional Facilities", 
    reformat = FALSE)