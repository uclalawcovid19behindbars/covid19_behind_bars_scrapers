# Historical Scraper Pull Pipeline

Sometimes we find data from sources that we have missed in the past but for which some entity hosts time-series data from which we can pull historical numbers and retrospectively populate our data. These might be from a series of pdfs hosted on a webpage or other groups which have combined and collected the data. 

In these cases we need to run a slightly different pipeline which utilizes the `generic_scraper` class, but has the additional constraint that each function passed to a historic scraper takes two arguments: the normal process argument in the scraper pipeline and a date parameter.  In addition the pull function will also require a file argument when the date is not sufficent to determine which file should be downloaded. 

To see an example of this, please view the file in `production/historical_scrape/historical_scrapers/historical_ice.R`. 

**Note**: The historical scraper pipeline is completely separate from the normal scraper pipeline. If you find a historical data source, a new scraper will need to be constructed and placed in `production/historical_scrape/historical_scrapers/` even if a scraper that is used in the main production line already exists for it. 

#### Instructions: 

1. **Build scraper**: Add the scraper to `production/historical_scrape_historical_scrapers`. Each historical scraper needs a `pull`, `restruct`, and `extract` function, and each of these functions must include a date parameter (regardless of whether that parameter is actually used within the function). 
2. **Create a config file**: Write a config file, which is a csv with 2 columns: 

* `Scraper`: a character column of a valid historical scraper (e.g. `historical_ice_scraper`) 
* `Date`: YYYY-MM-DD date of which day to run the historical scraper
* `File`: A file path or url to pull data from, may be NA if not needed
3. **Run historical pipeline**: Run the historical scrape pipeline by running the following from the command line (assuming your config file is in the path given): 
```
RScript production/historical_scrape/main_historical.R --config production/historical_scrape/config.csv
```
This will populate the `results` directory with the raw, extracted, and log files. 
4. **Sync files**: Sync your files with the remote database by calling `sync_remote_files(raw = TRUE)` within the console. 
