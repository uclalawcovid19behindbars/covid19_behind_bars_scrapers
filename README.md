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

You can find each of our scrapers in the folder `production/scrapers`. More detailed documentation can be found [here](https://uclalawcovid19behindbars.github.io/covid19-behind-bars-public-docs/scraper_documentation/) for each of our scrapers. In order to run these scrapers, you will need to install the libraries listed at the top of the file `R/generic_scraper.R` as well as our teams own library [behindbarstools](https://github.com/uclalawcovid19behindbars/behindbarstools). Individual scrapers may require additional libraries which are listen in the individual scraper files themselves through explicit library calls.

## Running the scraper

In order to run the scraper you will need the following: 

* Renv installed (for R package versioning) and docker (for selenium web scraping) installed and running
* API and SSH keys for the services we utilize, Extractable, PERMACC, and UCLA server

The following steps should be completed in order to ensure the scraper will run properly.

1. **Set up renv**: You will first need to set up the R package environment. 

```
renv::restore()
```

2. **Set up docker image**: You will need set up the selenium docker image to run and be mounted to the directory `/tmp/sel_dl`. This allows for the files that are downloaded through the docker selenium image to also appear on the host system. You can start the image by running: 

```
mkdir /tmp/sel_dl

docker run -d -p 4445:4444 -p 5901:5900 -v /tmp/sel_dl:/home/seluser/Downloads \
    selenium/standalone-firefox-debug:2.53.1
```

3. **Run scraper**: Now we can run the scraper. Ensure that you are in the root directory where `covid19_behind_bars_scrapers.Rproj` lives, and run the following command.

```
Rscript production/main.R
```

**Side Note**: Sometimes we will attempt to run a scraper and the scraper will be unable to extract a particular value. When this happens we will occasionally want to manually change the value for a particular facility's column value after the extraction has occurred for the scraper. To do this, go to a particular scrapers file and run the individual scraper using the code at the bottom of the file. After running that scraper's `extract_from_raw` method, select the column name and facility for which you would like to manually change the data using the following method. Doing so will log the occurance of the manual change and keep a record of all the changes we are making by hand. Note that this method will only allow you to change data which is stored in the `extract_data` slot. 

```
scraper$manual_change(
    column = "Some.Column", facility_name = "Facility name here", new_value = 9)
```

After calling the `manual_change` method, you will need to re-run the `validate_extract` and `save_extract` methods to update the extracted data. Updates made with the `manual_change` method are the only updates that should NOT be committed. All other code updates for a particular scraper run should be reflected in the commit for that day. 


4. **Run post-scraper**: There will invariably be some scrapers that will need to be rerun, so fix those given the opportunity. Afterwards, we can run the post-run script to add the data to the remote server database as well as write the latest csv to the data submodule. 

```
Rscript production/post_run.R
```

5. **Commit changes**: Make sure to compare the totals from the run to what is on [the Google Sheet](https://docs.google.com/spreadsheets/d/1X6uJkXXS-O6eePLxw2e4JeRtM41uPZ2eRcOA_HkPVTk/edit#gid=1641553906) now to make sure nothing funky happened. Lastly, be sure to commit your changes to the master branch of both the `covid19_behind_bars_scrapers` repo and the `data` submodule. Note that this will require two commits. 

```
# go to the data data directory switch to master and pull latest data
# to avoid fatal conflicts later
cd data
git checkout master
git pull origin master
# go back to the main directory
cd ..

# run the post run Rscript
Rscript ./production/post_run.R

# check the differences between the new data and the old google sheet
# if things looks good then commit all changes

# commit changes in data repo
cd data
git add -A 
git commit -m "update: the/current/date run of scraper"
git push origin master

# commit changes in code repo
cd ..
git add -A 
git commit -m "update: the/current/date run of scraper"
git push origin master
```
