# singularity run --app Rscript init/singularity-r.simg production/post_run.R
rm(list=ls())
library(tidyverse)
source("./R/utilities.R")

write_historical_data()
system(
    "scp results/summary_data/aggregated_data.csv do_server:Data/summary_data",
    )
write_latest_data()

