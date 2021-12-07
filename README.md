# Scraper Production Code

The [UCLA Law Covid-19 Behind Data Project](http://covid19behindbars.org/), launched in March 2020, tracks the spread and impact of COVID-19 in American carceral facilities and pushes for greater transparency and accountability around the pandemic response of the carceral system.

Part of this project includes scraping state DOC websites, the federal BOP website, and county jail websites. For each of these websites, we pull data use a unique scraper which performs the following tasks: 

1. Save a copy of the website in [perma.cc](perma.cc)
2. Extract the raw source where the data we would like to collect is hosted. 
3. Save this object to a flate file on your system
4. Convert the raw data to a matrix, data frame, vector, or list of those objects 
5. Clean and filter to the variables of concern for the project
6. Do basic data checks
7. Write extracted data to disk

You can find each of our scrapers in the folder `production/scrapers`. More detailed documentation can be found [here](https://uclalawcovid19behindbars.github.io/covid19-behind-bars-public-docs/scraper_documentation/) for each of our scrapers. If you would like to recreate the documentation with the latest information you can run the function `document_all_scrapers()`. In order to run these scrapers, you will need to install the libraries listed at the top of the file `R/generic_scraper.R` as well as our teams own library [behindbarstools](https://github.com/uclalawcovid19behindbars/behindbarstools). Individual scrapers may require additional libraries which are listen in the individual scraper files themselves through explicit library calls.

## Getting started

In order to run the scraper you will need the following:

- Docker (for selenium web scraping) installed and running
- A direct installation of the r package `tabulizer` and its dependencies. To install, run `remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))`
- API keys for the services we utilize, Extractable (`EXTRACTABLE_API_KEY`) and PERMACC (`PERMACC_API_KEY`)
- SSH key for UCLA server if you will be adding scraped data to the server. The following configuration should be added to your local `.ssh/config`:
```
host {{hostname}}
    HostName {{address}}
    IdentityFile {{~/.ssh/some_private_key}}
    User {{username}}
```

## Running the scraper

The following steps should be completed in order to ensure the scraper will run properly.

1. **Pull from remote repositories**: To make sure that you're working off of the latest files, pull before doing anything else.

```
# Pull latest in the main repository 
cd covid19_behind_bars_scrapers
git checkout master
git pull origin master

# Pull latest in the data submodule 
cd data
git pull origin master

# Return the main repository 
cd .. 
```

2. **Set up scraper environment**: You will first need to set up the R package environment. You can do this by running the script `production/pre_run.R` which will check to see if you have the appropriate packages and install them for you if missing, install the latest version of our teams package `behindbarstools`, and check to see if you have enough extractable credits to run the scrapers. 

```
Rscript production/pre_run.R
```

3. **Set up docker image**: You will need set up the selenium docker image to run and be mounted to the directory `/tmp/sel_dl`. This allows for the files that are downloaded through the docker selenium image to also appear on the host system. You can start the image by running: 

```
mkdir /tmp/sel_dl

docker run -d -p 4445:4444 -p 5901:5900 -v /tmp/sel_dl:/home/seluser/Downloads \
    selenium/standalone-firefox:2.53.0
```

**STOP! DID YOU UPDATE THE MANUAL SCRAPER DATA YET?**: Visit the [manual data Google Sheet](https://docs.google.com/spreadsheets/d/1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ/edit#gid=1527592402) and update the sad scrapers for which we must use our own eyes.


4. **Run scraper**: Now we can run the scraper. Ensure that you are in the root directory where `covid19_behind_bars_scrapers.Rproj` lives, and run the following command.

```
Rscript production/main.R
```

**Side Note 1**: If we want to only save a record of the raw COVID data and make a carbon copy of the websites hosting COVID data for that day and not extract the information we can run a limited version of the scraper which only pulls and saves raw data as shown below.

```
Rscript production/main.R --raw_only
```

**Side Note 2**: Sometimes we will attempt to run a scraper and the scraper will be unable to extract a particular value. When this happens we will occasionally want to manually change the value for a particular facility's column value after the extraction has occurred for the scraper. To do this, go to a particular scrapers file and run the individual scraper using the code at the bottom of the file. After running that scraper's `extract_from_raw` method, select the column name and facility for which you would like to manually change the data using the following method. Doing so will log the occurance of the manual change and keep a record of all the changes we are making by hand. Note that this method will only allow you to change data which is stored in the `extract_data` slot. 

```
scraper$manual_change(
    column = "Some.Column", facility_name = "Facility name here", new_value = 9)
```

After calling the `manual_change` method, you will need to re-run the `validate_extract` and `save_extract` methods to update the extracted data. Updates made with the `manual_change` method are the only updates that should NOT be committed. All other code updates for a particular scraper run should be reflected in the commit for that day. 


5. **Run post-scraper**: There will invariably be some scrapers that will need to be rerun, so fix those given the opportunity. Afterwards, we can run the post-run script to add the data to the remote server database as well as write the latest csv to the data submodule. 

```
Rscript production/post_run.R
```

6. **Inspect diagnostics**: Look at the diagnostics file, and stop if there's anything crazy happening!

7. **Commit changes**: Be sure to commit your changes to the master branch of both the `covid19_behind_bars_scrapers` repo and the `data` submodule. Note that this will require two commits.

```
# check the differences between the new data and the old google sheet
# if things looks good then commit all changes

# FIRST commit changes in data submodule
cd data
git add -A 
git commit -m "update: the/current/date run of scraper"
git push origin master

# NEXT commit changes in scraper code repo
cd ..
git add -A 
git commit -m "update: the/current/date run of scraper"
git push origin master
```

8. **Look at GitHub**: Look at the commit hash labeling [the data submodule](https://github.com/uclalawcovid19behindbars/covid19_behind_bars_scrapers) (e.g., "data @ fa1492"). Compare this to the latest commit in [the data repo](https://github.com/uclalawcovid19behindbars/data/). Are these hashes the same? 

