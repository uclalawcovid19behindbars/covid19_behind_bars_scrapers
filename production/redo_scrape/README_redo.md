# REDO Scraper Pull pipeline

There are instances where we have raw data files on our database sserver for which we wish to re-extract data either because the inital extraction failed, we extracted something incorrectly, or we missed a variable that should be a part of our analysis. To rerun scrapers in our prduction line using the raw files in our data base (as opposed to pulling them from a website) we should use the redo scraper pipeline. The main file to rerun scrapers can be found in `production/redo_scrape/main_redo.R`. The file is set up as an executable file with command line options to rerun a particular scraper for dates which raw files exist. Below is an example of rerunning the `nyc_jails` scraper for the months of November and December 2020. 

## 1. Re-do the scrape
```
Rscript production/redo_scrape/main_redo.R --scraper nyc_jails --start 2020-11-01 --end 2020-12-31
```

This specification tells the redo scraper pipeline to run the `nyc_jails` scraper for any dates which we have raw files on the server in Nov and Dec 2020. To see more details on how to run this file see:

```
production/redo_scrape/main_redo.R --help
```

## 2. Check the logs! 

Look at scraper/results/log_files and make sure there were no big issues with the re-scape.

## 3. Send it up

Go go `post_run.R` and run ONLY THE FOLLOWING LINES in order to sync raw, extracted, and log files.

```
rm(list=ls())
library(tidyverse)
source("./R/utilities.R")
sync_remote_files(FALSE)
summarize_remote_data()
```

We specify `FALSE`, the default setting, here for the `raw` parameter as to avoid re-writing the raw files from which extractions were made. However, it will not break anything if the parameter accidently set to `TRUE`. 
