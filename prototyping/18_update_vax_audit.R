rm(list = ls())

library(tidyverse)
library(behindbarstools)

# ------------------------------------------------------------------------------

GITHUB_PATH <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/"
STATE_JUR_PATH <- "data/master/latest-data/latest_state_jurisdiction_counts.csv"
STATE_PATH <- "data/master/latest-data/latest_state_counts.csv"

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
    select(-Web.Group) %>%
    filter(Measure %in% c("Residents.Initiated", 
                          "Residents.Completed",
                          "Staff.Initiated", 
                          "Staff.Completed")) %>%
    pivot_wider(names_from = Measure, values_from = c(Val, Rate)) %>%
    rename_all(~stringr::str_replace(.,"Val_","")) %>%
    left_join(state_df_small, by = "State") %>%
    bind_rows(federal_ice_rows)
    

