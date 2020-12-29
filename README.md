# Scarper Production Code

The [UCLA Law Covid-19 Behind Data Project](http://covid19behindbars.org/), launched in March 2020, tracks the spread and impact of Covid-19 in American carceral facilities and pushes for greater transparency and accountability around the pandemic response of the carceral system.

Part of this project includes scraping state DOC websites, the federal BOP webiste, and county jail websites. For ecah of these websites, we pull data use a unique scraper which performs the following tasks

1. Save a copy of the website in [perma.cc](perma.cc)
2. Extract the raw source where the data we would like to collect is hosted. 
3. Save this object to a flate file on your system
4. Convert the raw data to a matrix, data frame, vector, or list of those objects 
5. Clean and filter to the variables of concern for the project
6. Do basic data checks
7. Write extracted data to disk

You can find each of our scrapers in the folder `production/scrapers`. More detailed documentation can be found [here](https://uclalawcovid19behindbars.github.io/covid19-behind-bars-public-docs/scraper_documentation/) for each of our scrapers. In ordder to run these scrapers an installation of the libaries listed at the top of the file `R/generic_scraper.R` as well as our teams own library [behindbarstools](https://github.com/uclalawcovid19behindbars/behindbarstools). Individual scrapers may require additional libraries which are listen in the individual scraper fie itself through explicit library calls.

## Runnning the scraper

In order to run the scraper you will need.

1. Singularity (for the R image) and docker (for selenium web scraping) installed and running
2. API and SSH keys for the services we utilize, Extractable, PERMACC, and UCLA server

The following steps should be completed in order to ensure the scraper will run properly.

First in order to insure that you are using a compatable form of `R` you need to build the `R` singularity image using the following command on the command line

```
singularity build singularity-r.simg R.Singularity.4.0.2 &> build.log
```

Next you will need set up the selenium docker image to run and be mounted to the directory `/tmp/sel_dl` this allows for the files that are downloaded through the docker selenium image to also appear on the host system. You can start the image by running

```
mkdir /tmp/sel_dl

docker run -d -p 4445:4444 -p 5901:5900 -v /tmp/sel_dl:/home/seluser/Downloads \
    selenium/standalone-firefox-debug:2.53.1
```

Now we can run the scraper. Ensure that you are in the root directory where `covid19_behind_bars_scrapers.Rproj` lives and run the following command.

```
singularity run --app Rscript init/singularity-r.simg production/main.R
```

There will invariably be some scrapers that will need to be rerun so fix those given the opportunity. Afterwards we can run the post run scrip to add the data to the remote server database as well as write the latest csv to the data submodule.

```
singularity run --app Rscript init/singularity-r.simg production/post_run.R
```

Make sure to compare the totals from the psot run to what is on the google sheet now to make sure nothing funky happened. Last be sure to commit your changes for bot this and the data subrepo.



