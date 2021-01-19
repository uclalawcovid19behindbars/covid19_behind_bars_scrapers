rm(list=ls())
library(tidyverse)
library(behindbarstools)

new_df <- "data/Adult Facility Counts/adult_facility_covid_counts_today_latest.csv" %>%
    read_csv()

old_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/" %>%
    str_c(
        "master/Adult%20Facility%20Counts/",
        "adult_facility_covid_counts_today_latest.csv") %>%
    read_csv()


VAR_COMPARE <- "Residents.Deaths"

bind_rows(
    new_df %>%
        # filter(Jurisdiction %in% c("county", "state")) %>%
        select(VAR_COMPARE, State, jurisdiction) %>%
        mutate(Type = "New"),
    old_df %>%
        # filter(Jurisdiction %in% c("county", "state")) %>%
        select(VAR_COMPARE, State, jurisdiction) %>%
        mutate(Type = "Old")) %>%
    select(-State) %>%
    group_by(jurisdiction, Type) %>%
    summarise_all(sum_na_rm) %>%
    ungroup() %>%
    pivot_wider(names_from = "Type", values_from = VAR_COMPARE) %>%
    mutate(difference = New - Old) %>%
    arrange(difference) %>%
    print(n=Inf)



full_join(
    new_df %>%
        group_by(State) %>%
        summarize(Staff.Confirmed.New = sum(Staff.Confirmed, na.rm = T)),

    old_df %>%
        group_by(State) %>%
        summarize(Staff.Confirmed.Old = sum(Staff.Confirmed.Old, na.rm = T))) %>%
    View()

old_ddf <- "1X6uJkXXS-O6eePLxw2e4JeRtM41uPZ2eRcOA_HkPVTk" %>%
    read_sheet(sheet = 2, skip = 1) %>%
    .[2:nrow(.),] %>%
    rename(Name = "County / Name of Facility") %>%
    select(
        Facility, Name, State, 
        Staff.Deaths.Old = "Confirmed Deaths\n(Staff)")

full_join(
    new_df %>%
        group_by(State) %>%
        summarize(Staff.Deaths.New = sum(Staff.Deaths, na.rm = T)),
    
    old_ddf %>%
        group_by(State) %>%
        summarize(Staff.Deaths.Old = sum(Staff.Deaths.Old, na.rm = T))) %>%
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
