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

* Singularity (for the R image) and docker (for selenium web scraping) installed and running
* API and SSH keys for the services we utilize, Extractable, PERMACC, and UCLA server

**Note**: If you do not have singularity running, you can also recreate the R package environment using `renv` and then run `main.R` and `post_run.R` (steps 3 and 4 below) within RStudio directly. You will still need to set up the docker image with this approach. 

The following steps should be completed in order to ensure the scraper will run properly. 

```
renv::restore()
```

1. **Set up singularity image**: In order to ensure that you are using a compatible form of `R`, you need to build the `R` singularity image using the following command on the command line

```
singularity build singularity-r.simg R.Singularity.4.0.2 &> build.log
```

2. **Set up docker image**: You will need set up the selenium docker image to run and be mounted to the directory `/tmp/sel_dl`. This allows for the files that are downloaded through the docker selenium image to also appear on the host system. You can start the image by running: 

```
mkdir /tmp/sel_dl

docker run -d -p 4445:4444 -p 5901:5900 -v /tmp/sel_dl:/home/seluser/Downloads \
    selenium/standalone-firefox-debug:2.53.1
```

3. **Run scraper**: Now we can run the scraper. Ensure that you are in the root directory where `covid19_behind_bars_scrapers.Rproj` lives, and run the following command.

```
singularity run --app Rscript init/singularity-r.simg production/main.R
```

4. **Run post-scraper**: There will invariably be some scrapers that will need to be rerun, so fix those given the opportunity. Afterwards, we can run the post-run script to add the data to the remote server database as well as write the latest csv to the data submodule. 

```
singularity run --app Rscript init/singularity-r.simg production/post_run.R
```

5. **Commit changes**: Make sure to compare the totals from the run to what is on [the Google Sheet](https://docs.google.com/spreadsheets/d/1X6uJkXXS-O6eePLxw2e4JeRtM41uPZ2eRcOA_HkPVTk/edit#gid=1641553906) now to make sure nothing funky happened. Lastly, be sure to commit your changes to the master branch of both the `covid19_behind_bars_scrapers` repo and the `data` submodule. Note that this will require two commits. 
