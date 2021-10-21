rm(list = ls())

library(tidyverse)
library(behindbarstools)

# ------------------------------------------------------------------------------

# Only flag when cumulative counts drop by DIFF_THRESHOLD
DIFF_THRESHOLD <- 5 

# AND that decline is more than PCT_THRESHOLD percent 
PCT_THRESHOLD <- 0.1

# ------------------------------------------------------------------------------

GITHUB_PATH <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/"
FAC_PATH <- "data/master/historical-data/historical_facility_counts.csv"
STATE_PATH <- "data/master/historical-data/historical_state_counts.csv"

# Read raw data 
state_df <- read.csv(str_c(GITHUB_PATH, STATE_PATH))
fac_df <- read.csv(str_c(GITHUB_PATH, FAC_PATH))

# Combine facility and statewide data 
combined_df <- bind_rows(
    state_df %>% 
        mutate(Name = "STATEWIDE", 
               Jurisdiction = ifelse(State %in% c("federal", "ice"), State, "state")) %>% 
        select(Jurisdiction, State, Name, Date, ends_with(".Confirmed")), 
    fac_df %>% 
        select(Jurisdiction, State, Name, Date, ends_with(".Confirmed"))
)

# Reshape long (each row is a facility + date + metric combination)
long_df <- combined_df %>% 
    pivot_longer(!(c(Jurisdiction, State, Name, Date)), 
                 names_to = "Measure", 
                 values_to = "Value") %>% 
    filter(!is.na(Value)) %>% 
    arrange(Jurisdiction, State, Name, Measure, Date) %>% 
    group_by(Jurisdiction, State, Name, Measure) %>% 
    mutate(Diff = Value - lag(Value), 
           Pct.Diff = Diff / Value)
    
# Flag issues 
issues_df <- long_df %>% 
    # Multiply by -1 because we want to flag drops 
    filter((Diff < (DIFF_THRESHOLD * -1)) & (Pct.Diff < (PCT_THRESHOLD * -1)))

# Write to csv
write.csv(issues_df, "facility-data-issues.csv",  row.names = FALSE, na = "")
