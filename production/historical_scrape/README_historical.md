# Historical Scraper Pull Pipeline

Sometimes we find data from sources that we have missed in the past but for which some entity hosts time-series data from which we can pull historical numbers and retrospectively populate our data. These might be from a series of pdfs hosted on a webpage or other groups which have combined and collected the data. 

In these cases we need to run a slightly different pipeline which utilizes the `generic_scraper` class, but has the additional constraint that each function passed to a historic scraper takes two arguments: the normal process argument in the scraper pipeline and a date parameter.  

To see an example of this, please view the file in `production/historical_scrape/historical_scrapers/historical_ice.R`. 

**Note**: The historical scraper pipeline is completely separate form the normal scraper pipline. If you find a historical data source, a new scraper will need to be constructed and placed in `production/historical_scrape/historical_scrapers/` even if a scraper that is used in the main production line already exists for it. 

#### Instructions: 

1. Add the scraper to `production/historical_scrape_historical_scrapers`. Each historical scraper needs a `pull`, `restruct`, and `extract` function, and each of these functions must include a date parameter (regardless of whether that parameter is actually used within the function). The pipeline should reflect the following: 
2. Add the scraper you would like to run (e.g. `historical_ice_scraper`) and the date(s) you would like to run it to the `production/historical_scrape/historical_config.csv` file. 
3. Run the historical scrape pipeline by running `production/historical_scrape/main_historical.R`. 
4. Sync your files with the remote database by calling `sync_remote_files(raw = TRUE)` within the console. 
