rm(list=ls())
library(tidyverse)
source("R/utilities.R")

facname_df <- "~/Documents/facility_data/data_sheets/fac_spellings.csv" %>%
    read_csv() %>%
    select(State, Facility = facility_name_clean, Name = facility_name_raw) %>%
    mutate(Name = str_to_upper(Name)) %>%
    unique()

new_df <- load_latest_data()

old_df <- "~/Downloads/UCLA Law Covid-19 Behind Bars Data Project " %>%
    str_c(
        "Professor Sharon Dolovich, Director - COVID-19 Jail_Prison ",
        "Confirmed Cases and Deaths.csv") %>%
    read_csv(skip = 1, col_types = cols()) %>%
    .[2:nrow(.),] %>%
    rename(Name = "County / Name of Facility") %>%
    select(
        Facility, Name, State, 
        Staff.Confirmed.Old = "Confirmed Cases (Staff)")


full_join(
    new_df %>%
        group_by(State) %>%
        summarize(Staff.Confirmed.New = sum(Staff.Confirmed, na.rm = T)),

    old_df %>%
        group_by(State) %>%
        summarize(Staff.Confirmed.Old = sum(Staff.Confirmed.Old, na.rm = T))) %>%
    View()

compare_df <- "~/Downloads/UCLA Law Covid-19 Behind Bars Data Project " %>%
    str_c(
        "Professor Sharon Dolovich, Director - COVID-19 Jail_Prison ",
        "Confirmed Cases and Deaths.csv") %>%
    read_csv(skip = 1, col_types = cols()) %>%
    .[2:nrow(.),] %>%
    rename(Name = "County / Name of Facility") %>%
    select(
        Facility, Name, State, 
        Rez.Co.Old = "Confirmed Cases \n(Residents)") %>%
    full_join(
        new_df %>%
            filter(Date == lubridate::ymd("2020-10-30")) %>%
            select(Name, State, Residents.Confirmed)) %>%
    arrange(State, Name)

View(compare_df)
