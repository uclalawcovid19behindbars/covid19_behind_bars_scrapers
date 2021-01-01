# How to run the wayback machine scraper

Sometimes we may miss dates of scraping but we want to leverage the fact that the internet archives have recorded information from websites on given days. We can explictely run a given scraper from a set date range using the `rescrape_wayback.R` script. Note that this process is only advisable for static html type scrapers. To run a scarper from the wayback machine archives you need to add an entry into the `wayback.json`. The entries require the folowing parameters.

```
"1": { # unique identifier
        "scraper": "nyc_jails_scraper", # the scraper name
        "start_date": "2020-11-01", # the day to begin way back machine scraping
        "end_date": "2020-12-16", # last day of way back machine scraping
        "run": true # actually run this? if not its just here for records
    }
```

By adding new entries to the scraper we can retrospectively collect more data from more urls. Note that after you run the wayback scraper once you shoud set the run parameter to false so that we do not continously rescrpae data. Besure to meticulousy check log files as a lot could potentially go wrong here. Also note that for the date range you specificy every SUN, MON, WED, and FRI is selected and the nearest date not exceeding that date is selected for scraping.