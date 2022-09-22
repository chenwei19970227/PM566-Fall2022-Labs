lab05-week5
================
chen wei
2022-09-22

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

## R Markdown

``` {r}
library(lubridate)
library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr)
```

## STEP 1. Read in the data

First download and then read in with data.

``` {r}
if(!file.exists("met_all.gz")){
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60)
}
met <- data.table::fread("met_all.gz")
```

Remove temperature less than -17C and change elev 9999 to missing value
code

``` {r}
met <- met[temp>-17][elev == 9999.0,elev :=NA]
```

read in station data

``` {r}
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

``` {r}
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]
```

``` {r}
# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])
```

``` {r}
# Dropping NAs
stations <- stations[!is.na(USAF)]
```

``` {r}
# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

merge the darta MET WITH STATION

``` {r}
met<-
merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 
nrow(met)
```

\#QUESTION 1.representation statition for the US compute mean
temperature, wind speed and atmopheric pressure for each weather
stationland pick the weather station with the average value closet to
the median for the US

``` {r}
station_averages <-
  met[ , .(
    temp      =mean(temp, na.rm=T),
    wind.sp   =mean(wind.sp, na.rm=T),
    atm.press =mean(atm.press,na.rm=T)
  ),by = USAFID]
```

the above computer the mean by weather station.now let’s compute the
median value for each varaible.

``` {r}
stmeds <- station_averages[, .(
  temp50    =median(temp,na.rm = T),
  windsp50  =median(wind.sp,na.rm =T ),
  atmpress50=median(atm.press,na.rm = T)
)]
stmeds
```

a helpful function we might want to use ‘which,min()’.

``` {r}
station_averages[ ,
                  temp_dist50:=abs(temp-stmeds$temp50)][order(temp_dist50)]
```

Let’s use the which,min

``` {r}
station_averages[which.min(temp_dist50)]
```

It matches the result above \##Question 2:Representationve station per
state Just like the previous question, you are asked to identify what is
the most representative, the median,station per state. This time,instead
of looking at one varable at a time, lookat the euclidean distance.If
multiple stations show inthe median, select the one located at the
lowest latitude.

``` {r}
station_averages <- 
     met[ , .(
       temp      = mean(temp, na.rm=T),
       wind.sp   = mean(wind.sp, na.rm=T),
       atm.press = mean(atm.press,na.rm=T)
     ), by = .(USAFID,STATE)]
head(station_averages)
```

``` {r}
statemeds<- station_averages[ , .(
           temp50    = median(temp, na.rm=T), 
           wind.sp50 = median(wind.sp, na.rm=T)
            ), by = STATE]
statemeds
```

``` {r}
station_averages <- 
  merge(
  x = station_averages,
  y = statemeds,
  by.x = "STATE",
  by.y = "STATE",
  all.x = TRUE,
  all.y = FALSE
)
```

``` {r}
station_averages[ , temp_dist_state50   := temp - temp50]
station_averages[ , windsp_dist_state50 := wind.sp - wind.sp50] 
station_averages
```

``` {r}
station_averages[ , eucdist := temp_dist_state50^2 +
                                windsp_dist_state50^2]
```

``` {r}
repstation <- station_averages[ , .(
                    eucdist = min(eucdist, na.rm=T))
                  , by=STATE]
```

``` {r}
merge(
  x = station_averages,
  y = repstation,
  by.x = c("eucdist","STATE"),
  by.y = c("eucdist","STATE"),
  all.x = FALSE,
  all.y = TRUE
)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

`{r cars} summary(cars)`

Knit the doc and save it on GitHub. Note that the `echo = FALSE`
parameter was added to the code chunk to prevent printing of the R code
that generated the plot.
