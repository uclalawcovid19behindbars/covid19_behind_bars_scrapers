rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(googlesheets4)
gs4_auth("ucla.law.covid.staff@gmail.com")

## QUESTIONS: 
# - which sheet to use? --> formal sheet 
# - all dates, or just latest scrape? ---> most recent confirmed case number 
# - date last updated? or date last scraped? 
# historical data --> also create a sheet of historical sheets 

youth_sheet <- "1AfqaEPZTMy1hMdnZC8UYMP_JTJrnDlLi_6uh8sxmQWM"

# fac_sheet_df <- read_sheet(fac_sheet, sheet = "Facility TAB") %>%
#     filter(!is.na(`Facility ID`)) %>%
#     select(-starts_with("...") , -ends_with("(Detainees)")) %>%
#     select(-ends_with("Staff)"), -Address, -State)

fac_sheet_df <- behindbarstools::read_fac_info() %>%
    rename(`Facility ID` = Facility.ID)

all_dat <- read_scrape_data(all_dates = FALSE) 
youth_df <- all_dat %>%
    filter(Age == "Juvenile")

# check for any other youth facilities 
other_youth <- all_dat %>% 
    filter((str_detect(Name, "(?i)juvenile|youth"))) %>%
    filter(Age != "Juvenile" | is.na(Age))

# bind together age-classified and name-searched
all_youth <- youth_df %>%
    bind_rows(other_youth)

new_df <- all_youth %>%
    # save only the latest data
    group_by(Facility.ID) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
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
    arrange(State, Name) %>%
    # left_join(fac_sheet_df, 
    #           by = "Facility ID", 
    #           suffix = c(".scrape", "")) %>%
    # select(-ends_with(".scrape"),
    #        -ICE.Field.Office) %>%
    mutate(Date = as.character(Date),
           Name = str_to_title(Name)) %>% 
    relocate(Date, State, Name, ends_with("YOUTH"), ends_with("STAFF")) 

range_write(
    data = new_df, 
    ss = youth_sheet, 
    sheet = "main", 
    reformat = FALSE)


