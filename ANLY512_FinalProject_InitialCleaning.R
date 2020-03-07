#import required libraries
library(lubridate)

# Initial Import & Downsize (DON'T RUN AGAIN) -----------------------------------------------
##read-in and check the quality of the full-sized Traffic_Violations Dataset import
#DF <- read.csv('Traffic_Violations.csv')

#head(DF)
#dim(DF)
#str(DF)

##convert 'Date.Of.Stop' to a Date object for easier manipulation
#DF$Date.Of.Stop <- as.Date(DF$Date.Of.Stop, format = '%m/%d/%Y')

##reduce the size of the dataset for easier computation (three-year period)
#TrafficDF <- DF[DF$Date.Of.Stop > as.Date('12/31/2016', format = '%m/%d/%Y'), ]
#TrafficDF <- TrafficDF[TrafficDF$Date.Of.Stop < as.Date('01/01/2020', format = '%m/%d/%Y'), ]

#save the subsetted dataset
#write.csv(TrafficDF, file = 'Traffic_Violations2.csv')



# Data Import & Preliminary Cleaning -------------------------------------------
##import reduced size dataset and check quality of import
TrafficDF <- read.csv('Traffic_Violations2.csv',  na.strings = '')
head(TrafficDF)
dim(TrafficDF)
str(TrafficDF)

#convert all columns to appopriate types
TrafficDF$X            <- NULL   #drop the index column
TrafficDF$SeqID        <- as.character(TrafficDF$SeqID)   #unqiue case number
TrafficDF$Date.Of.Stop <- as.Date(TrafficDF$Date.Of.Stop, format = '%m/%d/%Y')  #convert to Date Object
TrafficDF$Time.Of.Stop <- hms(TrafficDF$Time.Of.Stop)   #convert to Time Object
TrafficDF$Agency       <- NULL  #all records are 'MCP', so drop
TrafficDF$Description  <- as.character(TrafficDF$Description)   #relatively unique inputs for each
TrafficDF$Location     <- as.character(TrafficDF$Location)   #relatively unique inputs for each
TrafficDF$Search.Reason.For.Stop <- as.character(TrafficDF$Search.Reason.For.Stop)   #relatively unique inputs for each
TrafficDF$Year         <- as.integer(as.character(TrafficDF$Year))   #convert to INT for easier manipulation
TrafficDF$Make         <- as.character(TrafficDF$Make)   #too many to types to check against (from personal cars to farm tractor brands)
TrafficDF$Model        <- as.character(TrafficDF$Model)   #relatively unique inputs
TrafficDF$Charge       <- as.character(TrafficDF$Charge)   #relatively unique inputs
TrafficDF$Driver.City  <- as.character(TrafficDF$Driver.City)   #relatively unique inputs
TrafficDF$Geolocation  <- NULL  #already recorded in Lat/Long Columns

#confirm column type changes were appropriately registered
str(TrafficDF)



# Understand Dataset Cleanliness ------------------------------------------
#extract column names for easy recall later
featureNames <- colnames(TrafficDF)

#evaluate the number of missing values in the data set
missing <- sapply(TrafficDF, function(x) sum(is.na(x)))

##evaluate each column for values out of range
#SeqID:           Unique Identifier...only invalid if NA
#Date.Of.Stop:    Culled During Dataset Reduction
#Time.Of.Stop:    Cohersion Ensured Cleanliness...inappropriate values would be returned NA
#Description:     How to asses if appropriate...its a custom input field...
#Location:        How to asses if appropriate...its a custom input field...
#Latitude: Range(38.90 to 39.33)
LatWrong   <- length(which(TrafficDF$Latitude > 39.50)) + length(which(TrafficDF$Latitude < 38.90)) - length(which(TrafficDF$Latitude == 0))
LatMissing <- length(which(TrafficDF$Latitude==0))

#Longitude: Range (-76.88 to -77.52)
LonWrong   <- length(which(TrafficDF$Longitude < -77.52)) + length(which(TrafficDF$Longitude > -76.88)) - length(which(TrafficDF$Longitude == 0))
LonMissing <- length(which(TrafficDF$Longitude==0))

#Year: Range (invalid if before 1929 or after 2020)
yearWrong   <- length(which(TrafficDF$Year < 1929)) + length(which(TrafficDF$Year > 2020)) - length(which(TrafficDF$Year == 0))
yearMissing <- length(which(TrafficDF$Year == 0))

#check all features with categorical variables for appropriate values
for (i in featureNames){
  if (class(TrafficDF[,i]) == 'factor') {
    print(i)
    print(levels(TrafficDF[,i]))}
}

##investigate the following features more indepth: State, Driver.State, DL.State
#initialize a vector containing acceptable state abbreviations
stateList <- append(state.abb, c('AB', 'BC', 'MB', 'NB', 'NL', 'NS', 'NT', 'NU', 'ON', 'PE', 'QC', 'SK', 'YT', 'DC', 'AS', 'GU', 'PR', 'VI', 'XX'))

#State Name (US Territories, DC, & Canadian Provinces)
stateWrong <- length(TrafficDF$State) - length(which(TrafficDF$State %in% stateList))

#Driver.State (Include Canadian Provinces)
driverWrong <- length(TrafficDF$Driver.State) - length(which(TrafficDF$Driver.State %in% stateList))

#DL.State (Include Canadian Provinces)
DLWrong <- length(TrafficDF$DL.State) - length(which(TrafficDF$DL.State %in% stateList))

#sum total number of missing data
missing['Latitude']  <- missing['Latitude'] + LatMissing
missing['Longitude'] <- missing['Longitude'] + LonMissing

#print number of missing values in the dataset
print(missing)

#sum number of missing/incorrect values in the dataset
missing_wrong <- missing
missing_wrong['Latitude']     <- missing_wrong['Latitude'] + LatWrong
missing_wrong['Longitude']    <- missing_wrong['Longitude'] + LonWrong
missing_wrong['Year']         <- missing_wrong['Year'] + yearWrong
missing_wrong['State']        <- missing_wrong['State'] + stateWrong
missing_wrong['Driver.State'] <- missing_wrong['Driver.State'] + driverWrong
missing_wrong['DL.State']     <- missing_wrong['DL.State'] + DLWrong

#print the total number of missing/incorrect values in the dataset
print(missing_wrong)
sum(missing_wrong)

#return total DF cleanliness score (percent of available spaces appropriately filled)
sum(missing_wrong) / (length(TrafficDF$SeqID) * length(featureNames))


####NEXT STEPS####
#NOTE: CRTL+SHIFT+R inputs a chuck, which keeps the code clean
#drop rows with missing primary response variables ('Violation', 'Fatal', and 'Personal Injury')
#drop rows with low density errors (missing 'Model' and 'Driver.City' for example)
#identify and rectify values that are incorrectly missing versus those that originated from a Not Applicable (N/A type items) as was the case in many 'Search' Columns
#impute as many missing values as reasonable using mean, median, or regression based predictors as possible
#generate new features (light/weather data) and binned categories
#rerun data-cleanliness chunk
#conduct summary statistical analysis


