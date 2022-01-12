rm(list = ls())

library(tidyverse)
library(behindbarstools)
library(googlesheets4)

# ------------------------------------------------------------------------------

GITHUB_PATH <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/"
STATE_JUR_PATH <- "data/master/latest-data/latest_state_jurisdiction_counts.csv"
STATE_PATH <- "data/master/latest-data/latest_state_counts.csv"
SPREADSHEET_LOC <- "1EOHjJttK4OlAoy6sdQsZimgu7JfwC41-CqCm9nmRc4o/edit#gid=0"

# Read raw data 
state_jur_df <- read.csv(str_c(GITHUB_PATH, STATE_JUR_PATH))
state_df <- read_csv(str_c(GITHUB_PATH, STATE_PATH))

# Get state population and federal data
state_df_small <- state_df %>%
    select(State, ends_with(".Population"))

federal_ice_rows <- state_df %>% 
    filter(State %in% c("Federal", "ICE")) %>%
    mutate(Date = "") %>%
    select(State, Date, 
                 Residents.Initiated, 
                 Staff.Initiated, 
                 Residents.Completed,
                 Staff.Completed, ends_with(".Population")) %>%
    mutate(Rate_Residents.Initiated = Residents.Initiated / Residents.Population,
           Rate_Staff.Initiated = Staff.Initiated / Staff.Population,
           Rate_Residents.Completed = Residents.Completed / Residents.Population,
           Rate_Staff.Completed = Staff.Completed / Staff.Population)

# Get counts, rates with date associated 
vax_audit_df <- state_jur_df %>% 
    filter(Web.Group == "Prison") %>%
    # select(-Web.Group) %>%
    filter(Measure %in% c("Residents.Initiated", 
                          "Residents.Completed",
                          "Staff.Initiated", 
                          "Staff.Completed")) %>%
    filter((!is.na(Val)) & (!is.na(Rate))) %>%
    pivot_wider(id_cols = c(State, Date), 
                names_from = Measure, 
                values_from = c(Val, Rate)) %>%
    rename_all(~stringr::str_replace(.,"Val_","")) %>%
    group_by_coalesce(State) %>%
    left_join(state_df_small, by = "State") %>%
    bind_rows(federal_ice_rows) %>%
    mutate(`Source` = "",
           `Does website report resident rates?` = "",
           `Does website report staff rates?` = "",
           `Notes` = "",
           `Volunteer` = "")

# Write data locally
write_csv(vax_audit_df, "~/Desktop/vax_audit.csv", na = "")

# Write data to google sheet
gs4_auth("ucla.law.covid.staff@gmail.com")
range_write(
    data = all_youth_out, 
    ss = SPREADSHEET_LOC, 
    sheet = "Sheet1", 
    reformat = FALSE)
    

