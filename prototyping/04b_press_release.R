source("./R/utilities.R")
library(behindbarstools)
library(tidycensus)
library(lubridate)
library(plotly)

hist_data <- load_latest_data(TRUE, TRUE)

new_data <- load_latest_data()

new_data %>%
    filter(jurisdiction == "state" | jurisdiction == "federal") %>%
    mutate(State = ifelse(jurisdiction == "federal", "Federal", State)) %>% 
    group_by(State) %>%
    summarize(Deaths = sum_na_rm(Residents.Deaths)) %>%
    filter(!is.na(Deaths)) %>%
    left_join(
        read_vera_pop(updated = TRUE) %>%
            mutate(State = translate_state(State)) %>%
            mutate(State = ifelse(is.na(State), "Federal", State)),
        by = "State"
        ) %>%
    summarise(sum(Deaths)/sum(Population)) %>%
    unlist() %>%
    `/`(258000/331000000)

"https://raw.githubusercontent.com/themarshallproject/" %>%
    str_c("COVID_prison_data/master/data/covid_prison_cases.csv") %>%
    read_csv() %>%
    mutate(Date = mdy(as_of_date)) %>%
    select(State = name, Deaths = total_prisoner_deaths, Date) %>%
    group_by(State) %>%
    filter(Date == max(Date)) %>%
    left_join(
        read_vera_pop(updated = TRUE) %>%
            mutate(State = translate_state(State)) %>%
            mutate(State = ifelse(is.na(State), "Federal", State)),
        by = "State"
    ) %>%
    filter(!is.na(Deaths)) %>%
    ungroup() %>%
    summarise(sum(Deaths)/sum(Population)) %>%
    unlist() %>%
    `/`(258000/331000000)

hist_data %>%
    filter(
        (State == "Iowa" & Name == "ANAMOSA") |
            (State == "Kentucky" & Name == "LEE ADJUSTMENT CENTER") |
            (State == "California" & Name == "HIGH DESERT STATE PRISON")
    ) %>%
    mutate(Facility = case_when(
        State == "Iowa" ~ "Anamosa State Prison\nIowa",
        State == "Kentucky" ~ "Lee Adjustment Center\nKentucky",
        TRUE ~ "High Desert State Prison\nCalifornia"
    )) %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    ggplot(aes(
        x = Date, y = Residents.Confirmed, color = Facility, fill = Facility)) +
    geom_line(linetype = 2) +
    geom_area() +
    labs(y = "# of People Incarcerated Infected with COVID-19") +
    theme_bw() +
    #ylim(c(0,820)) +
    xlim(c(mdy("10-15-20"), mdy("11-24-20"))) +
    ggtitle("Major Recent COVID-19 Spikes In Prison Facilities")


all_plot <- hist_data %>%
    filter(
        (State == "Iowa" & Name == "ANAMOSA") |
            (State == "Kentucky" & Name == "LEE ADJUSTMENT CENTER") |
            (State == "California" & Name == "HIGH DESERT STATE PRISON")
            ) %>%
    mutate(Facility = case_when(
        State == "Iowa" ~ "Anamosa State Prison\nIowa",
        State == "Kentucky" ~ "Lee Adjustment Center\nKentucky",
        TRUE ~ "High Desert State Prison\nCalifornia"
    )) %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed, color = Facility)) +
    geom_line(linetype = 2) +
    geom_point() +
    labs(y = "# of People Incarcerated Infected with COVID-19") +
    theme_bw() +
    ylim(c(0,820)) +
    xlim(c(mdy("10-15-20"), mdy("11-24-20"))) +
    ggtitle("Major Recent COVID-19 Spikes In Prison Facilities")

ggsave(
    file="~/Downloads/recent_outbreak.svg", plot=all_plot, width=10, height=8)

c(
    "Anamosa" = "Jones", #Iowa
    "Lee" = "Lee",
    "High Desert" = "Lassen"
)

med_age_df <- get_acs("county", "B01002_001", year = 2018)

med_age_df %>%
    mutate(ageq = (rank(estimate) - 1)/nrow(.)) %>% 
    filter(
        NAME %in% c(
            "Lee County, Kentucky", "Lassen County, California",
            "Jones County, Iowa", "Estill County, Kentucky"))


hist_data %>%
    filter(State == "Kentucky") %>%
    mutate(Facility = str_c(State, ": ", Name)) %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed)) +
    geom_line(linetype = 2) +
    geom_point() +
    labs(y = "# of People Incarcerated Infected with COVID-19") +
    theme_bw() +
    # ylim(c(0,900)) +
    # xlim(c(mdy("10-15-20"), mdy("11-23-20"))) +
    ggtitle("Major Recent COVID-19 Spikes In Prison Facilities") +
    facet_wrap(~Facility)


