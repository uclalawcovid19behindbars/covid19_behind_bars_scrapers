rm(list=ls())
library(tidyverse)
source("R/utilities.R")

translate_state <- function(x, reverse = FALSE){
    state_vec <- c(state.name, "DC")
    names(state_vec) <- c(state.abb, "DC")
    
    if(reverse){
        state_vec <- c(state.abb, "DC")
        names(state_vec) <- c(state.name, "DC")
    }
    
    state_vec[x]
}

facname_df <- "~/Documents/facility_data/data_sheets/fac_spellings.csv" %>%
    read_csv() %>%
    select(State, Facility = facility_name_clean, Name = facility_name_raw) %>%
    mutate(Name = str_to_upper(Name)) %>%
    unique()

new_df <- read_historical_data() %>%
    mutate(Name = str_to_upper(Name)) %>%
    rename(Facility = Name) %>%
    mutate(State = translate_state(State)) %>%
    left_join(facname_df, by = c("Facility", "State")) %>%
    mutate(Name = ifelse(
        str_detect(Facility, "(?i)state-wide"), "STATEWIDE", Name)) %>%
    select(-Facility)

compare_df <- "~/Downloads/UCLA Law Covid-19 Behind Bars Data Project " %>%
    str_c(
        "Professor Sharon Dolovich, Director - COVID-19 Jail_Prison ",
        "Confirmed Cases and Deaths.csv") %>%
    read_csv(skip = 1, col_types = cols()) %>%
    .[2:nrow(.),] %>%
    rename(Name = "County / Name of Facility") %>%
    select(
        Facility, Name, State, Rez.Co.Old = "Confirmed Cases \n(Residents)") %>%
    full_join(
        new_df %>%
            filter(Date == lubridate::ymd("2020-10-30")) %>%
            select(Name, State, Residents.Confirmed)) %>%
    arrange(State, Name)

View(compare_df)
