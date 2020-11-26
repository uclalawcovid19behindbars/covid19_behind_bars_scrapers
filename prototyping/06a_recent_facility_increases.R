source("./R/utilities.R")
library(behindbarstools)

hist_data <- load_latest_data(TRUE, TRUE)

hist_data %>%
    filter(State == "Texas" & jurisdiction == "county") %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    mutate(Title = str_c(State, ": ", Name)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed)) +
    geom_line() +
    facet_wrap(~Title) +
    labs(y = "Residents Confirmed") +
    theme_bw()



fac_delta_df <- hist_data %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    filter(!(str_detect(Name, "(?i)state") & str_detect(Name, "(?i)wide"))) %>%
    filter(Date >= (lubridate::as_date((Sys.Date())) - lubridate::days(7))) %>%
    # i fucked up SD
    filter(State != "South Dakota") %>%
    group_by(Name, State, jurisdiction) %>%
    mutate(delta = last(Residents.Confirmed) - first(Residents.Confirmed)) %>%
    ungroup() %>%
    filter(delta %in% head(sort(unique(delta), decreasing = TRUE), n = 4)) %>%
    select(jurisdiction, State, Name) %>%
    unique() %>%
    left_join(hist_data, by = c("jurisdiction", "State", "Name"))

out_plot <- fac_delta_df %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    mutate(Title = str_c(State, ": ", Name)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed)) +
    geom_line() +
    facet_wrap(~Title) +
    labs(y = "Residents Confirmed") +
    theme_bw() +
    ggtitle(
        "High Outbreak Facilities",
        "Carceral Facilities with Largest Increase In Resident Cases in Past 7 Days"
    ) +
    theme(
        legend.text = element_text(size=13),
        legend.title = element_text(size=15),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        title =  element_text(size=17))


ggsave("./data/Adult Facility Counts/weekly_facility_monitor.png", out_plot)

neg_death_df <- hist_data %>%
    select(jurisdiction, State, Name, Date, Residents.Deaths, Staff.Deaths) %>%
    mutate(Date = lubridate::mdy(Date)) %>%
    arrange(jurisdiction, State, Name, Date) %>%
    group_by(jurisdiction, State, Name) %>% 
    mutate(lag_death = Residents.Deaths - lag(Residents.Deaths)) %>%
    mutate(lag_staff_death = Staff.Deaths - lag(Staff.Deaths)) %>%
    mutate(any_neg = any(lag_death < 0 | lag_staff_death < 0, na.rm = TRUE)) %>%
    filter(any_neg) %>%
    ungroup()

# every once in a while we get negative deaths record all instances here

neg_death_plot <- neg_death_df %>%
    select(-Residents.Deaths, -Staff.Deaths, -any_neg) %>%
    mutate(title = str_c(State, ": ", Name)) %>%
    select(-State, -Name, -jurisdiction) %>%
    pivot_longer(lag_death:lag_staff_death) %>%
    ggplot(aes(x = Date, y = value, color = name, linetype = name)) +
    geom_line() +
    facet_wrap(~title)

ggsave("./data/Adult Facility Counts/neg_death_monitor.png", neg_death_plot)
