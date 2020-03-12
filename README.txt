README

Name: Patrick Aquino, Adam Imran, Thomas Malejko, Doug Post
NetID : pma50, ai410, tpm72, dyp6, 
Class: ANLY 512

#####  FILES  ##### 
README.txt
ANLY512_FinalProject_InitialCleaning.r		Script that breaks down the full dataset into a three year chunk and conducts preliminary cleaning and statistical analysis
ANLY512_ProjectProposal.docx			Project Proposal Document (Working)
Traffic_Violations.csv				Original/Complete Dataset (NOT IN REPO DUE TO SIZE)
Traffic_Violations2.csv				Subsetted Dataset	  (NOT IN REPO DUE TO SIZE)
MergedTrafficWeather				Subsetted Dataset w/ Weather Data (NOT IN REPO DUE TO SIZE)
Traffic_Violations_Clean.csv			Final Cleaned Dataset for Final Analysis

#####  'ANLY512_FinalProject_InitialCleaning.r' NOTES  ##### 
REQUIRED LIBRARIES:
lubridate					Required for proper parsing of DTG features

REQUIRED INPUTS: 
Traffic_Violations.csv
Traffic_Violations2.csv
MergedTrafficWeather				Modified form of Traffic_Violations2.csv that includes weather data

EXPECTED OUTPUTS:
Traffic_Violations2.csv
Traffic_Violations_Clean.csv	

GENERAL NOTES: 
** Do not run the "Initial Import & Downsize (DON'T RUN AGAIN)" Code Chunk...this is needed to subset the original dataframe into the three-year window required for the assignment. It takes about 90 sec to 2 minutes to load the original data frame into R. 
** The "Data Import & Preliminary Cleaning" Code Chunk imports the modified dataset (Traffic_Violations2.csv) and begins all subsequent cleaning/modifications against that dataset.
** Description of Variables: https://www.opendatanetwork.com/dataset/data.montgomerycountymd.gov/4mse-ku6q


CONTACT
If you have any questions or concerns about this program, feel free to contact me at 908-798-2262 or tpm72@georgetown.edu.