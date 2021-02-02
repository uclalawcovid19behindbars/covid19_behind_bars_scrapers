# How to run the wayback machine scraper

Sometimes we may miss dates of scraping but we want to leverage the fact that the internet archives have recorded information from websites on given days. We can explictely run a given scraper from a set date range using the `rescrape_wayback.R` script. Note that this process is only advisable for static html type scrapers. To run a scarper from the wayback machine archives you need to add specify a command line entry as follows.

```
./production/wayback_scrape/main_wayback.R -sc arkansas_html -st 2020-12-12 -en 2021-01-31
```

Be sure to meticulousy check log files as a lot could potentially go wrong here. Also note that for the date range you specificy every SUN, MON, WED, and FRI is selected and the nearest date not exceeding that date is selected for scraping.