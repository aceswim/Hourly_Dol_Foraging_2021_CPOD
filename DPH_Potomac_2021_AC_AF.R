# Hourly Foraging Analysis for Potomac 2016-2019 --------------------------

# Download the .CP3 files for each Potomac deployment from Bailey 
# Lab Drive (-> Dolphin Watch -> Potomac_CPOD) to working directory

# Using CPOD.exe export ICI (all) and DPM (1 min period) results as text files

# Delete the extraneous information above the column headings and save

# Proceed to run the following code

# Load packages -----------------------------------------------------------

rm(list = ls()) # clear workspace

install.packages("data.table")
install.packages("dplyr")
install.packages("reshape2")

library(dplyr)
library(data.table)
library(reshape2)

# Load the data ----------------------------------------------

# Location of CPOD data (working directory)

CPODdata <- "F:/Marine Mammal Lab Work/CPOD_Foraging_2021/Foraging_2021_CPOD"

# files must be in the format of Site Date POD# file01 Dol DPM/ICI.txt
filenames <- list.files(path = CPODdata, pattern = 'Dol ICIs.txt')


# Create a list of all the file names by site ---------------------------------

# get the CPOD site names from above
cpods <- sapply(filenames, function(x) substr(x, 1, 3))
DeployMonthYear <- paste(substr(filenames[1],10,11), substr(filenames[1], 5, 8), sep = '')

# Files with occurrences
DPMfilenames <- list.files(path = CPODdata, pattern = 'Dol DPM.txt')

# Pull in ICI data and determine foraging events ----------------------

# pre-allocate as a list to put data frames in
D <- list()
propForage <- list(length(filenames))
# for (j in 1:length(filenames)){ #### This is not running in a loop, so manually changing j value below
j=1
# read in ICI data
f0 <- fread(input = paste(CPODdata,filenames[j], sep = '/'))
# sort out the ChunkEnd into DateTime
f0$ChunkEnd <- as.POSIXct(f0$ChunkEnd, format = '%d/%m/%Y %H:%M') #Check whether y(17) or Y(2017)

f0$lnICIus <- log(f0$ICIus)
f0$Foraging <- f0$lnICIus < 9.201 #set limit to only include foraging buzzes
f1 <- aggregate(lnICIus ~ ChunkEnd, FUN = mean, data = f0) # get the mean ICI per minute
f2 <- aggregate(Foraging ~ ChunkEnd, FUN = any, data = f0) # select only foraging minutes
f1 <- merge(f1, f2) # merge the two
f1 <- f1[order(f1$ChunkEnd), ] #just ordering in time


# Pull in DPM data --------------------------------------------------------

d0 <- fread(paste(CPODdata,DPMfilenames[j], sep = '/'))
# select only minutes where the C-POD is on
d0 <- d0[d0$MinsOn==1,]
# sort out the ChunkEnd into DateTime
d0$ChunkEnd <- as.POSIXct(d0$ChunkEnd, format = '%d/%m/%Y %H:%M')
#Remove the rows with "0" presence:
d0$DPM<-as.integer(d0$DPM)

# Create new data frame that merges d0 and f1 by ChunkEnd -----------------

df <- as.data.frame(merge(d0, f1, by = "ChunkEnd"))

# Aggregate by hours
df$Date <- format(df$ChunkEnd, format = "%Y/%m/%d")
df$Time <- format(df$ChunkEnd,"%H:%M")
df$Hour <- format(df$ChunkEnd,"%H")

# Group by date and hour
df_hour <- df %>%
  dplyr:: group_by(Date, Hour)%>%
  dplyr:: count(Foraging)

# Create new data frame with foraging hours only --------------------------

dfHourT <- as.data.frame(subset(df_hour, Foraging!="FALSE"))
## Note: n = number of foraging events in that hour


# Write CSV file for each result ------------------------------------------

## Once the file is written, go back up to line 48 and change j= to the next
## file until all have been analyzed

write.csv(dfHourT, "PotomacMay2019_PropHrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column

write.csv(dfHourT, "PotomacOct2019_PropHrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column