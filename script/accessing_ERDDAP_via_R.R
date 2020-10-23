
## Prepared by Cyril Michel on 2019-07-10; cyril.michel@noaa.gov

#################################################################
#### HOW TO PULL IN REAL-TIME FISH DETECTION DATA INTO R ########
#################################################################

# WEBSITE:----
#https://calfishtrack.github.io/real-time/index.html THEN GO TO ARCHIVE STUDY

####STEP 1:
## install and load the 'rerddap' library
#install.packages("rerddap", dependencies = T)
library(rerddap)
#####STEP 2:
## Find out details on the database
db <- info('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/")
## This will tell you columns and their data types in database
#### STEP 3: FIND THE STUDY ID NAME FOR YOUR PROJECT
db$variables
## This will tell you unique StudyID names
as.data.frame(tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", fields = c("Study_ID"), distinct = T))

# copy study ID then skip to line 34
cache_delete(dat) # run this code to clear the cache if pulling new data in 

## Download all data (will take a little while, large database).
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/")
## ALTERNATIVELY, download only the data you need, see following code snippets

## Download only data from 1 studyID, here for example, Juv_Green_Sturgeon_2018 study
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'Study_ID="Juv_Green_Sturgeon_2018"')

# USE THIS ONE----:
##### STEP 4: PULL DATA, USE STUDY ID NAME FOR YOUR PROJECT (looks like we already tried to do this with the dat_rice object)
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'Study_ID="SCARF_San_Joaquin_Spring_run_2020"')

#### STEP 5: WRITE TO CSV
write.csv(dat, "data_output/Data_from_erdapp2020.csv")
###TIP: do not commit/push any large data files (100s on lines), can cause issues. Just do tables and scripts

## Download only data from 1 receiver location
#examples
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'general_location="MiddleRiver"')

## Download only data from a specific time range (in UTC time)
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'time>=2019-01-01', 'time<=2019-01-10')

## Download data from a combination of conditions. For example, Study_ID="MillCk_SH_Wild_S2019" and general_location="ButteBr_RT"
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'Study_ID="MillCk_SH_Wild_S2019"', 'general_location="ButteBrRT"')

## Download only specific columns for a studyID (or a general location, time frame or other constraint)
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'general_location="MiddleRiver"', fields = c("TagCode","Study_ID"))

## Finally, download a summary of unique records. Say for example you want to know the unique TagCodes detected in the array from a studyID
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'Study_ID="DeerCk_SH_Wild_S2019"', fields = c("TagCode"), distinct = T)

## Or, number of unique fish detected at each receiver location for a studyID
dat <- tabledap('FEDcalFishTrack', url = "http://oceanview.pfeg.noaa.gov/erddap/", 'Study_ID="DeerCk_SH_Wild_S2019"', fields = c("general_location","TagCode"), distinct = T)

## PLEASE NOTE: IF A DATA REQUEST ABOVE RETURNS SIMPLY "Error: ", THIS LIKELY MEANS THE DATA REQUEST CAME UP WITH ZERO RETURNS
