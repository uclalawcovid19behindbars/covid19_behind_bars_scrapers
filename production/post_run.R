# singularity run --app Rscript init/singularity-r.simg production/post_run.R
rm(list=ls())
library(tidyverse)
source("./R/utilities.R")

sync_remote_files(TRUE)
Sys.sleep(7*60)
write_latest_data()
document_all_scrapers()

# Warn if running low on ExtractTable credits 
check_credits()
