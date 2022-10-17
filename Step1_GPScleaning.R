# Authors: Jesse Wolf 
# Purpose: Clean GPS data for downstream home range analysis
# Input: GPS data in .csv format
# output: Cleaned GPS data in .csv format
# Title: Step1_GPSCleaning

# Packages and environment preparation ----

libs <- c('tidyverse','data.table', 'ggplot2', 'dplyr', 'readr','chron', 'rgdal')
lapply(libs, require, character.only = TRUE)

# Setting object to UTM zone 9N
utm9N <-'+proj=utm +zone=9 ellps=WGS84'

# Reading in data
allgps <- read_csv('data.csv')

# Filtering data----
allgps_clean <- allgps %>%
  filter (Latitude!="NA") %>%
  filter(`Mort_status`=="normal")%>%
  filter(`Height` > 600, `Height` <2500)%>%
  filter(DOP > 10)

# Paste date and time to new variable 
datetime <- (paste0 (allgps_clean$LMT_Date, sep = " ", allgps_clean$LMT_Time))

# Bind DateTime to column cleaned GPS data frame
allgps_clean$datetime <- datetime

# Check structure of GPS data frame
str(allgps_clean)

# Making GPS data frame into a data table
allgps_clean_dt<-as.data.table(allgps_clean)


### Changing coordinates and setting boundaries 
# This will create an Easting and Northing column 
allgps_clean_dt[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(`Longitude`, `Latitude`), utm9N))]

# Plotting to see what data looks like
plot(allgps_clean_dt$EASTING, allgps_clean_dt$NORTHING)

# Generating coordinate boundaries if any clearly erroneous points exist from plot above
lowEast <- 600000; highEast <- 640000
lowNorth <- 6100000; highNorth <- 6500000

## Filter by coordinate boundaries IF NECESSARY
allgps_clean_dt <- allgps_clean_dt[(lowEast < EASTING & EASTING < highEast) &
               (lowNorth < NORTHING & EASTING < highNorth)]
# Plotting to see what data looks like
plot(allgps_clean_dt$EASTING, allgps_clean_dt$NORTHING)

# Set variable LMTdatetime as proper format to use further
allgps_clean_dt [,LMTdatetime := as.POSIXct(datetime)]

# Produces table with animal ID and number of fixes ----
fixes_per_ind <- group_by(allgps_clean_dt) %>%
  group_by(AnimalID) %>%
  summarise(number_of_fixes = n())

### Generating Step Length ---- 
#This next part will help you produce a simple step length and then a movement rate so 
#that you can then remove any biologically impossible movements (in this case 15 km/h movement rates) 

#Sort Data
allgps_clean_dt <- allgps_clean_dt[order(allgps_clean_dt$AnimalID, 
                                         allgps_clean_dt$LMTdatetime),]
# Set columns
time.col <- 'LMTdatetime'
coord.cols <- c('EASTING', 'NORTHING')

# Create lag and dif column names
lag.cols <- paste('lag', coord.cols, sep = '')
difference.cols <- c('difX', 'difY')

lag.time.col <- paste0('lag', time.col)
dif.time.col <- paste0('dif', time.col)

# Use shift to create lagged cols
allgps_clean_dt[order(get(time.col)), (lag.cols) := shift(.SD, 1, NA, 'lag'),
     by = .(CollarID),
     .SDcols = coord.cols]

# Find the difference squared between all points in each x,y separately
allgps_clean_dt[, (difference.cols) := .((get(coord.cols[1]) - get(lag.cols[1])) ^2,
                              (get(coord.cols[2]) - get(lag.cols[2])) ^2)]

# Square root the summed difference for a simple step length
allgps_clean_dt[, simpleStep := sqrt(rowSums(.SD)),
     .SDcols = difference.cols]

## Calculate change in time
allgps_clean_dt[order(get(time.col)), (lag.time.col) := shift(.SD, 1, NA, 'lag'), 
     by = .(CollarID),
     .SDcols = time.col]


# Calculating the difference in time in hours
allgps_clean_dt[, (dif.time.col) := as.numeric(get(time.col) - get(lag.time.col), units = 'hours')]

# Simple step length divided by time difference 
allgps_clean_dt[, moveRate := simpleStep / (get(dif.time.col))]

# Drop more than 15km/hr movements
allgps_clean_dt <- allgps_clean_dt%>%
  filter(moveRate<15000)