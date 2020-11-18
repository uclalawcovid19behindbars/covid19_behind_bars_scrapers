rm(list = ls())
source("./R/utilities.R")

new_df <- load_latest_data()

new_df %>%
    filter(jurisdiction == "state") %>%
    group_by(State) %>%
    summarise(Residents.Confirmed = sum_na_rm(Residents.Confirmed))

la_death_df <- read_historical_data() %>%
    filter(State == "CA" & jurisdiction == "county" & !str_starts(id, 'la')) %>%
    group_by(Date) %>%
    summarise(Residents.Deaths = sum_na(Residents.Deaths))

fl_death_df %>%
    filter(!is.na(Residents.Deaths)) %>% 
    ggplot(aes(x = Date, y = Residents.Deaths)) +
    geom_line()

fl_death_df %>%
    filter(!is.na(Residents.Deaths)) %>%
    mutate(Death.Lag = Residents.Deaths - lag(Residents.Deaths)) %>%
    ggplot(aes(x = Date, y = Death.Lag)) +
    geom_line()
