rm(list = ls())
source("./R/utilities.R")

sum_na <- function(x){
    if(all(is.na(x))){
        return(NA)
    }
    sum(x, na.rm = TRUE)
}

fl_death_df <- read_historical_data() %>%
    filter(State == "FL") %>%
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
