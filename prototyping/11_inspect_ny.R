rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)
library(ggrepel)
library(ggpmisc)
library(plotly)

# read temp historical file
hist_df <- read_csv(
    "~/Documents/cdc_collab/historical-aggregate-counts/hist_df.csv") %>%
    filter(Date < ymd("2020-10-20")) %>%
    filter(State == "New York" & !is.na(Facility.ID) & Jurisdiction == "state")

new_df <- read_scrape_data(TRUE, state = "New York") %>%
    filter(!is.na(Facility.ID) & Jurisdiction == "state")

new_df %>%
    select(Name, Date, Residents.Confirmed) %>%
    bind_rows(
        hist_df %>%
            select(Name, Date, Residents.Confirmed) %>%
            filter(Date < ymd("2020-10-20"))) %>%
    filter(Name == "GREAT MEADOW CORRECTIONAL FACILITY") %>%
    ggplot(aes(x = Date, y = Residents.Confirmed, color="", fill = "")) +
    geom_line(size = 1.5) +
    geom_area(alpha = .5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    labs(y = "") +
    ggtitle("Cumulative COVID-19 Cases", "Great Meadow Correctional Facility") +
    theme(legend.position = "none") +
    scale_x_date(date_labels = "%b %y'") +
    stat_peaks(
        size = 6,
        span = NULL,
        geom = "text_repel",
        mapping = aes(label = paste(..y.label.., ..x.label..)),
        x.label.fmt = "\n of 2/6/20",
        y.label.fmt = "%.0f cases as",
        nudge_x = 10)


new_df %>%
    select(Name, Date, Residents.Confirmed) %>%
    bind_rows(
        hist_df %>%
            select(Name, Date, Residents.Confirmed) %>%
            filter(Date < ymd("2020-10-20"))) %>%
    filter(Name == "ELMIRA CORRECTIONAL FACILITY") %>%
    ggplot(aes(x = Date, y = Residents.Confirmed, color="", fill = "")) +
    geom_line(size = 1.5) +
    geom_area(alpha = .5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    labs(y = "") +
    ggtitle("Cumulative COVID-19 Cases", "Elmira Correctional Facility") +
    theme(legend.position = "none") +
    scale_x_date(date_labels = "%b %y'")

compare_plot <- new_df %>%
    select(Name, Date, Residents.Confirmed) %>%
    bind_rows(
        hist_df %>%
            select(Name, Date, Residents.Confirmed) %>%
            filter(Date < ymd("2020-10-20"))) %>%
    filter(Name != "STATEWIDE") %>%
    ggplot(aes(x = Date, y = Residents.Confirmed, color=Name)) +
    geom_line(size = 1.5) +
    labs(y = "") +
    ggtitle("Cumulative COVID-19 Cases", "NY Prisons") +
    theme(legend.position = "none") +
    scale_x_date(date_labels = "%b %y'") +
    theme_classic()

ggplotly(compare_plot)

comb_df <- new_df %>%
    select(Name, Date, Residents.Confirmed, Residents.Deaths) %>%
    bind_rows(
        hist_df %>%
            select(Name, Date, Residents.Confirmed, Residents.Deaths) %>%
            filter(Date < ymd("2020-10-20"))) %>%
    arrange(Name, Date) %>%
    group_by(Name) %>%
    mutate(Roll.New = diff_roll_sum(Residents.Confirmed, Date, window = 10)) %>%
    mutate(New.Deaths = Residents.Deaths - lag(Residents.Deaths)) %>%
    ungroup()

# great meadow had 43 new cases in the past 10 days
new_cases_df <- comb_df %>%
    filter(Roll.New >= 40)

new_death_df <- bind_rows(lapply(1:nrow(new_cases_df), function(i){
    sub_df <- new_cases_df[i,]
    
    sub_df %>%
        mutate(
            had_new_death = comb_df %>%
                # filter down to that facility and date
                filter(Name == sub_df$Name & Date >= sub_df$Date) %>%
                # did a new death appear in the next 14 days?
                filter(Date <= (sub_df$Date + 14)) %>%
                pull(New.Deaths) %>%
                {sum(., na.rm=T) > 0}
                )
}))

mean(new_death_df$had_new_death)

