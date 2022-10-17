# Authors: Jesse Wolf 
# Purpose: Generating Brownian Bridge Movement Models for core and home range isopleths in mountain goats
# Input: Cleaned GPS data from Step 1 in .csv
# Output: Shapefiles (.shp) of Brownian Bridge Movement Model core and home ranges 
# Title: Step2_BBMM

# Packages and environment preparation

libs <- c('maptools', 'readr', 'sp', 'foreign', 'lattice', 'BBMM', 'rgdal', 'PBSmapping', 'stringr', 'dplyr')
lapply(libs, require, character.only = TRUE)

data<-read_csv("data.csv")

# Add a column with 2x the time between fix rates
data$maxlag <- data$difLMTdatetime *2*60

# Ensure ID is being treated as a factor
data$CatID <- as.factor(data$AnimalID)

# Get date and time into proper format for R
data$DT <- as.POSIXct(strptime(data$datetime, format="%Y-%m-%d  %H:%M:%S"))

# Confirm that DT column is in proper format
str(data)

#Sort Data
data <- data[order(data$CatID, data$DT),]

# Creating time differences for time lag
timediff <- diff(data$DT)

# Remove first entry without any difference 
data <- data[-1,] 

# Bind time lag column (in minutes) to data 
data$timelag <-as.numeric(abs(timediff))

# Subset data into an individual AnimalID
mg1<-subset(data, data$CatID == "1")
str(mg1)

# Removing first entry with no difference
mg1 <- mg1[-1,] 

# Generating Brownian Bridge Movement Model for individual
BBMM_mg1 = brownian.bridge(x=mg1$EASTING, y=mg1$NORTHING, time.lag=mg1$timelag, max.lag =mg1$maxlag, location.error=20, cell.size=25)
bbmm.summary(BBMM_mg1)

# Plot results for all density contours
contours = bbmm.contour(BBMM_mg1, levels=c(seq(50, 90, by=10), 95, 99), locations=mg1, plot=TRUE)

# Create data.frame indicating cells within the contour desired and associated probability
bbmm.contour = data.frame(x = BBMM_mg1$x, y = BBMM_mg1$y, probability = BBMM_mg1$probability)

# Pick a contour for export 
bbmm.50 = bbmm.contour[bbmm.contour$probability >= contours$Z[1],]
#bbmm.50$in.out <- 1 

# Removing probability column
bbmm.50 <-bbmm.50[,-3]

# Define the projection of the coordinates
proj4string <- CRS("+proj=utm +zone=9N +ellps=WGS84")

# Generate file for cells within specified contour.
m50 = SpatialPixelsDataFrame(points = bbmm.50[c("x", "y")], data=bbmm.50)

# Convert to SpatialPolygonsDataFrame and export as ESRI Shapefile
shp.50 <- as(m50, "SpatialPolygonsDataFrame")
map.ps50 <- SpatialPolygons2PolySet(shp.50)
diss.map.50 <- joinPolys(map.ps50, operation = 'UNION')
diss.map.50 <- as.PolySet(diss.map.50, projection = 'UTM', zone = '9')
diss.map.p50 <- PolySet2SpatialPolygons(diss.map.50, close_polys = TRUE)
data50 <- data.frame(PID = 1)
diss.map.p50 <- SpatialPolygonsDataFrame(diss.map.p50, data = data50)
writeOGR(diss.map.p50, dsn = ".", layer="contour50_mg1maxlag", driver = "ESRI Shapefile")
map.50 <- readOGR(dsn=".", layer="contour50_mg1maxlag")
plot(map.50)

