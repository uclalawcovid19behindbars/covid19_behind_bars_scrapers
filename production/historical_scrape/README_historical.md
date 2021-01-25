# Historical Scraper Pull pipeline

Sometimes we find data from sources that we have missed in the passed but for which some entitry hosts time-series data from which we can pull historical numbers and retrospectively populate our data. These might be form a a series of pdfs hosted on a webpage or other groups which have combined and collected the data. In these cases we need to run a slightly different pipeline which utilizes the `generic_scraper` class but has the additional contsraint that each function passed to a gistroc scraper must take two arguments, the first normal process argument in teh scraper pipeline and a second date parameter. To see an example this please view the file in `production/historical_scrape/historical_scrapers/historical_ice.R`. Please notethat the historical scraper pipeline is completely separate form the normal scraper pipline and if you find a historical data source a new scraper will need to be constructed and placed in `production/historical_scrape/historical_scrapers/` even if a scraper that is used in the main production line already exists for it.

You can run the historical scrape pipline by running `Rscript production/historical_scrape/historical_main.R` and ensuring that the scraper that you would like to run and the date to run it on appear in the `production/historical_scrape/historical_config.csv` file. 