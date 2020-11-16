---
title: "Introduction to Behind Bars Scraper"
author: "UCLA COVID-19 Behind Bars Team"
date: "11/14/2020"
output: html_document
---

## The UCLA COVID-19 Behind Bars Scraper

Since the beggining of the COVID-19 pandemic in the US there have been extreme data gaps acroos the country that have limited are ability to assess the extent of the spread of the virus. This situation is even more extreme for the ~2 million individuals housed in the US incarcerated population. Facilities housing incarcerated indviduals have not been given mandates for COVID-19 testing or precautions, not to mention data reporting for epidemilogical monitoring. For a population who is highly vulnerable and subjected to close quarter living this is extremely problematic.

In an effort to capture the limited data that is available the UCLA COVID-19 Behind Bars Team has been collecting COVID-19 policy, testing, and mortality data for incarcerated populations where available. Our goal is to collect information from state Department of Correction managed facility (prisons), jail information from county sherrifs offices, and federal carceral facilities. 


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
