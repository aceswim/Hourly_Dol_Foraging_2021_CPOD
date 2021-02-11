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

CPODdata <- "F:/Marine Mammal Lab Work/CPOD_Foraging_2021/Hourly_Dol_Foraging_2021_CPOD/Hourly_Dol_Foraging_2021_CPOD"

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

# * Write CSV file for each result ----------------------------------------

## Note: n = number of detection events in that hour (foraging and non-foraging)
## df_hour versions

# write.csv(df_hour, "PotomacMay2016_Hrly_Dol_Dets.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(df_hour, "PotomacMay2017_Hrly_Dol_Dets.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(df_hour, "PotomacJun2017_Hrly_Dol_Dets.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(df_hour, "PotomacJun2018_Hrly_Dol_Dets.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(df_hour, "PotomacMay2019_Hrly_Dol_Dets.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(df_hour, "PotomacOct2019_Hrly_Dol_Dets.csv")
# Note: Manually input the file name from d0 into the first column


# Create new data frame with foraging hours only --------------------------

dfHourT <- as.data.frame(subset(df_hour, Foraging!="FALSE"))
## Note: n = number of foraging events in that hour

# * Write CSV file for each result ------------------------------------------

## Once the file is written, go back up to line 48 and change j= to the next
## file until all have been analyzed

# write.csv(dfHourT, "PotomacMay2019_PropHrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(dfHourT, "PotomacOct2019_PropHrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(dfHourT, "PotomacJun2018_PropHrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(dfHourT, "PotomacJun2017_HrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(dfHourT, "PotomacMay2017_HrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column

# write.csv(dfHourT, "PotomacMay2016_HrlyForaging.csv")
# Note: Manually input the file name from d0 into the first column


# Visualize Results -------------------------------------------------------

library(ggplot2)
library(tidyr)

# * All Dolphin Detections ------------------------------------------------
# Load in data
HourlyDets = read.csv("F:/Marine Mammal Lab Work/CPOD_Foraging_2021/Hourly_Dol_Foraging_2021_CPOD/Hourly_Dol_Foraging_2021_CPOD/Hourly_Foraging_Results/Master_All_Hourly_Foraging_NonForaging_Results.csv") 

# Create year, month, day columns

#format date column
HourlyDets$Date <- as.POSIXct(HourlyDets$Date, format = '%m/%d/%Y')

HourlyDets$Year <- format(HourlyDets$Date, format = "%Y")
HourlyDets$Month <- format(HourlyDets$Date, format = "%m")
HourlyDets$Day <- format(HourlyDets$Date, format = "%d")

#write.csv(HourlyDets, "HourlyDets.csv")

# Visualize 

# Load color blind friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# * * Plot Monthly and Hourly Foraging Event Totals for all years ---------

# * * * Month ---------------------------------------------------------------

# plot_month_all_dets <- ggplot(data = HourlyDets, mapping = aes(x = Month, y = Total_Events, fill = Foraging))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_fill_manual(values = cbp1)+
#   coord_cartesian(ylim = c(0,55))+
#   #scale_y_continuous(name = "Total Detection Events", limits = c(0,55))+
#   facet_wrap(~Year)+
#   ggtitle("Dolphin Detection Events in the Potomac by Month")
# plot_month_all_dets
# 
# a <- filter(HourlyDets, Month == "05" & Year == "2016")
# # 2016 05 False = 99, not 27 like plot indicates...what's going wrong?
# 
# ?geom_bar
# geom_bar and geom_col are related. Where geom_bar does calcs under 
# the hood via "stat" and geom_col just takes values you give it and plots.
# Default stat for geom_bar is "count". I'd changed it to "identity which calls 
# stat_identity() which tells it to do nothing and in essence,
# changes geom_bar to geom_col (Which is why it accepted the y argument without warning me
# that it wasn't actually going to use it) (geom_bar usually warns you that it only needs one of x or y)
# if you scroll down to the help docs it'll start talking about stat_count, which is what stat = "count" in geom_bar calls
# it says that it accepts x,y, group, and weight aesthetics those are the arguments that 
# itll accept in aes() at the top of the help doc, it says "geom_bar() makes the height of the bar proportional to the 
# number of cases in each group (or if the weight aesthetic is supplied, the sum of the weights)"
# so, since you want the sum of Total_Events to be shown, you need to write aes(weight = Total_Events) to tell stat_count
# -- called by stat = 'count' in geom_bar --
# to sum over that column

#Redo; want the sum of Total_events, so need stat = count and bars to be height of total_events thru weight
plot_month_all_dets <- ggplot(data = HourlyDets, mapping = aes(x = Month, weight = Total_Events, fill = Foraging))+
  geom_bar(stat = "count", position = "dodge")+
  scale_fill_manual(values = cbp1)+
  facet_wrap(~Year)+
  ggtitle("Dolphin Detection Events in the Potomac by Month")
plot_month_all_dets

# * * * Hour ----------------------------------------------------------------

# plot_hour_all_dets <- ggplot(data = HourlyDets, mapping = aes(x = Hour, y = Total_Events, fill = Foraging))+
#   geom_bar(stat = "identity", position = "dodge")+
#   scale_fill_manual(values = cbp1)+
#   scale_y_continuous(name = "Total Detection Events", breaks = seq(0,55,5))+
#   scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
#   facet_wrap(~Year)+
#   ggtitle("Dolphin Detection Events in the Potomac by Hour")
# plot_hour_all_dets

#Redo; want the sum of Total_events, so need stat = count and bars to be height of total_events thru weight
plot_hour_all_dets <- ggplot(data = HourlyDets, mapping = aes(x = Hour, weight = Total_Events, fill = Foraging))+
  geom_bar(stat = "count", position = "dodge")+
  scale_fill_manual(values = cbp1)+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  scale_y_continuous(name = "Total Detection Events", breaks = seq(0,500,100))+
  facet_wrap(~Year)+
  ggtitle("Dolphin Detection Events in the Potomac by Hour")
plot_hour_all_dets


# * *  Proportion of foraging events --------------------------------------

# * * * Month -------------------------------------------------------------

# * * * * 2019 ------------------------------------------------------------

#Get monthly totals for all events
pot2019_alldets <- filter(HourlyDets, Year == 2019) %>%
  group_by(Month) %>%
  summarize(Montly_totals_alldets = sum(Total_Events))

# Get monthly totals for only foraging events
pot2019_foragingdets <- filter(HourlyDets, Year == 2019) %>%
  filter(Foraging == TRUE)%>%
  group_by(Month) %>%
  summarize(Montly_foraging_totals = sum(Total_Events))

# Merge by Month
pot2019_propF_month <- as.data.frame(merge(pot2019_alldets, pot2019_foragingdets, by = "Month"))

pot2019_propF_month <- pot2019_propF_month %>%
  mutate(percent_forage = Montly_foraging_totals/ Montly_totals_alldets * 100)

plot_pot2019_alldets <- ggplot(pot2019_propF_month, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2019 Potomac Foraging Occurrence")
plot_pot2019_alldets


# * * * * 2018 ------------------------------------------------------------

#Get monthly totals for all events
pot2018_alldets <- filter(HourlyDets, Year == 2018) %>%
  group_by(Month) %>%
  summarize(Montly_totals_alldets = sum(Total_Events))

# Get monthly totals for only foraging events
pot2018_foragingdets <- filter(HourlyDets, Year == 2018) %>%
  filter(Foraging == TRUE)%>%
  group_by(Month) %>%
  summarize(Montly_foraging_totals = sum(Total_Events))

# Merge by Month
pot2018_propF_month <- as.data.frame(merge(pot2018_alldets, pot2018_foragingdets, by = "Month"))

pot2018_propF_month <- pot2018_propF_month %>%
  mutate(percent_forage = Montly_foraging_totals/ Montly_totals_alldets * 100)

plot_pot2018_alldets <- ggplot(pot2018_propF_month, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2018 Potomac Foraging Occurrence")
plot_pot2018_alldets

# * * * * 2017 ------------------------------------------------------------

#Get monthly totals for all events
pot2017_alldets <- filter(HourlyDets, Year == 2017) %>%
  group_by(Month) %>%
  summarize(Montly_totals_alldets = sum(Total_Events))

# Get monthly totals for only foraging events
pot2017_foragingdets <- filter(HourlyDets, Year == 2017) %>%
  filter(Foraging == TRUE)%>%
  group_by(Month) %>%
  summarize(Montly_foraging_totals = sum(Total_Events))

# Merge by Month
pot2017_propF_month <- as.data.frame(merge(pot2017_alldets, pot2017_foragingdets, by = "Month"))

pot2017_propF_month <- pot2017_propF_month %>%
  mutate(percent_forage = Montly_foraging_totals/ Montly_totals_alldets * 100)

plot_pot2017_alldets <- ggplot(pot2017_propF_month, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2017 Potomac Foraging Occurrence")
plot_pot2017_alldets

# * * * * 2016 ------------------------------------------------------------

#Get monthly totals for all events
pot2016_alldets <- filter(HourlyDets, Year == 2016) %>%
  group_by(Month) %>%
  summarize(Montly_totals_alldets = sum(Total_Events))

# Get monthly totals for only foraging events
pot2016_foragingdets <- filter(HourlyDets, Year == 2016) %>%
  filter(Foraging == TRUE)%>%
  group_by(Month) %>%
  summarize(Montly_foraging_totals = sum(Total_Events))

# Merge by Month
pot2016_propF_month <- as.data.frame(merge(pot2016_alldets, pot2016_foragingdets, by = "Month"))

pot2016_propF_month <- pot2016_propF_month %>%
  mutate(percent_forage = Montly_foraging_totals/ Montly_totals_alldets * 100)

plot_pot2016_alldets <- ggplot(pot2016_propF_month, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2016 Potomac Foraging Occurrence")
plot_pot2016_alldets

# * * * * Combine Plots ---------------------------------------------------
library(cowplot)

pot_month_all_years <- plot_grid(plot_pot2016_alldets, plot_pot2017_alldets, plot_pot2018_alldets, plot_pot2019_alldets, labels = "AUTO", label_size = 12)
pot_month_all_years

# Save as jpeg with 850 x 600 dimensions

# * * * Hour --------------------------------------------------------------

# * * * * 2019 ------------------------------------------------------------

#Get Hourly totals for all events
pot2019_alldets_hr <- filter(HourlyDets, Year == 2019) %>%
  group_by(Hour) %>%
  summarize(Hourly_totals_alldets = sum(Total_Events))

# Get Hourly totals for only foraging events
pot2019_foragingdets_hr <- filter(HourlyDets, Year == 2019) %>%
  filter(Foraging == TRUE)%>%
  group_by(Hour) %>%
  summarize(Hourly_foraging_totals = sum(Total_Events))

# Merge by Hour
pot2019_propF_Hour <- as.data.frame(merge(pot2019_alldets_hr, pot2019_foragingdets_hr, by = "Hour"))

pot2019_propF_Hour <- pot2019_propF_Hour %>%
  mutate(percent_forage = Hourly_foraging_totals/ Hourly_totals_alldets * 100)

plot_pot2019_alldets_hr <- ggplot(pot2019_propF_Hour, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2019 Potomac Foraging Occurrence")
plot_pot2019_alldets_hr

# * * * * 2018 ------------------------------------------------------------

#Get Hourly totals for all events
pot2018_alldets_hr <- filter(HourlyDets, Year == 2018) %>%
  group_by(Hour) %>%
  summarize(Hourly_totals_alldets = sum(Total_Events))

# Get Hourly totals for only foraging events
pot2018_foragingdets_hr <- filter(HourlyDets, Year == 2018) %>%
  filter(Foraging == TRUE)%>%
  group_by(Hour) %>%
  summarize(Hourly_foraging_totals = sum(Total_Events))

# Merge by Hour
pot2018_propF_Hour <- as.data.frame(merge(pot2018_alldets_hr, pot2018_foragingdets_hr, by = "Hour"))

pot2018_propF_Hour <- pot2018_propF_Hour %>%
  mutate(percent_forage = Hourly_foraging_totals/ Hourly_totals_alldets * 100)

plot_pot2018_alldets_hr <- ggplot(pot2018_propF_Hour, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2018 Potomac Foraging Occurrence")
plot_pot2018_alldets_hr

# * * * * 2017 ------------------------------------------------------------

#Get Hourly totals for all events
pot2017_alldets_hr <- filter(HourlyDets, Year == 2017) %>%
  group_by(Hour) %>%
  summarize(Hourly_totals_alldets = sum(Total_Events))

# Get Hourly totals for only foraging events
pot2017_foragingdets_hr <- filter(HourlyDets, Year == 2017) %>%
  filter(Foraging == TRUE)%>%
  group_by(Hour) %>%
  summarize(Hourly_foraging_totals = sum(Total_Events))

# Merge by Hour
pot2017_propF_Hour <- as.data.frame(merge(pot2017_alldets_hr, pot2017_foragingdets_hr, by = "Hour"))

pot2017_propF_Hour <- pot2017_propF_Hour %>%
  mutate(percent_forage = Hourly_foraging_totals/ Hourly_totals_alldets * 100)

plot_pot2017_alldets_hr <- ggplot(pot2017_propF_Hour, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2017 Potomac Foraging Occurrence")
plot_pot2017_alldets_hr

# * * * * 2016 ------------------------------------------------------------

#Get Hourly totals for all events
pot2016_alldets_hr <- filter(HourlyDets, Year == 2016) %>%
  group_by(Hour) %>%
  summarize(Hourly_totals_alldets = sum(Total_Events))

# Get Hourly totals for only foraging events
pot2016_foragingdets_hr <- filter(HourlyDets, Year == 2016) %>%
  filter(Foraging == TRUE)%>%
  group_by(Hour) %>%
  summarize(Hourly_foraging_totals = sum(Total_Events))

# Merge by Hour
pot2016_propF_Hour <- as.data.frame(merge(pot2016_alldets_hr, pot2016_foragingdets_hr, by = "Hour"))

pot2016_propF_Hour <- pot2016_propF_Hour %>%
  mutate(percent_forage = Hourly_foraging_totals/ Hourly_totals_alldets * 100)

plot_pot2016_alldets_hr <- ggplot(pot2016_propF_Hour, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2016 Potomac Foraging Occurrence")
plot_pot2016_alldets_hr

# * * * * Combine Plots ---------------------------------------------------
library(cowplot)

pot_hr_all_years <- plot_grid(plot_pot2016_alldets_hr, plot_pot2017_alldets_hr, plot_pot2018_alldets_hr, plot_pot2019_alldets_hr, labels = "AUTO", label_size = 12)
pot_hr_all_years

# * Foraging Events Only ----------------------------------------------------

# Load in data
HourlyF = read.csv("F:/Marine Mammal Lab Work/CPOD_Foraging_2021/Hourly_Dol_Foraging_2021_CPOD/Hourly_Dol_Foraging_2021_CPOD/Hourly_Foraging_Results/Master_All_Hourly_Foraging_Results_Potomac_CPOD_AC.csv") 

# Create year, month, day columns

#format date column
HourlyF$Date <- as.POSIXct(HourlyF$Date, format = '%m/%d/%Y')

HourlyF$Year <- format(HourlyF$Date, format = "%Y")
HourlyF$Month <- format(HourlyF$Date, format = "%m")
HourlyF$Day <- format(HourlyF$Date, format = "%d")

#Get general overview look
MonthPotomac<-as.data.frame(tapply(HourlyF$Total_Foraging_Events,list(HourlyF$Month,HourlyF$Year),sum))
HourPotomac<-as.data.frame(tapply(HourlyF$Total_Foraging_Events,list(HourlyF$Hour,HourlyF$Year),sum))

# Load color blind friendly palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Plot Monthly and Hourly Foraging Event Totals for all years

# Month
plot_month <- ggplot(data = HourlyF, mapping = aes(x = Month, y = Total_Foraging_Events, fill = Year))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = cbp1)+
  scale_y_continuous(name = "Total Foraging Events", limits = c(0,50))+
  ggtitle("Monthly Dolphin Foraging Events in Potomac")
plot_month

# Hour
plot_hour <- ggplot(data = HourlyF, mapping = aes(x = Hour, y = Total_Foraging_Events, fill = Year))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = cbp1)+
  scale_y_continuous(name = "Total Foraging Events", breaks = seq(0,55,5))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("Hourly Dolphin Foraging Events in Potomac")
plot_hour

# Combine month and hourly plots
library(cowplot)

pot_monthhr_all_years <- plot_grid(plot_month, plot_hour, labels = "AUTO", label_size = 12)
pot_monthhr_all_years

# Calculate proportions
# Need proportion of hours with events per month
# Need proportion of days with events per hour

# Look at percent foraging by month
# a_all <- HourlyF %>%
#   group_by(Month, Year) %>%
#   summarize(Montly_totals = sum(Total_Foraging_Events))%>%
#   mutate(percent_forage = Montly_totals/ sum(Montly_totals) * 100)
# 
# View(a_all)
# 
# plot_month_all <- ggplot(a_all, aes(x = Month, y = percent_forage, fill = Year)) +
#   geom_col()+
#   scale_fill_manual(values = cbp1)+
#   scale_y_continuous(limits = c(0,100))
# plot_month_all

# Not working how I want, so subset into each year

# Month
pot2016 <- filter(HourlyF, Year == 2016) %>%
  group_by(Month) %>%
  summarize(Montly_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Montly_totals/ sum(Montly_totals) * 100)

plot_pot2016 <- ggplot(pot2016, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2016 Potomac Foraging Occurrence")
plot_pot2016

pot2017 <- filter(HourlyF, Year == 2017) %>%
  group_by(Month) %>%
  summarize(Montly_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Montly_totals/ sum(Montly_totals) * 100)

plot_pot2017 <- ggplot(pot2017, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2017 Potomac Foraging Occurrence")
plot_pot2017

pot2018 <- filter(HourlyF, Year == 2018) %>%
  group_by(Month) %>%
  summarize(Montly_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Montly_totals/ sum(Montly_totals) * 100)

plot_pot2018 <- ggplot(pot2018, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2018 Potomac Foraging Occurrence")
plot_pot2018

pot2019 <- filter(HourlyF, Year == 2019) %>%
  group_by(Month) %>%
  summarize(Montly_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Montly_totals/ sum(Montly_totals) * 100)

plot_pot2019 <- ggplot(pot2019, aes(x = Month, y = percent_forage)) +
  geom_col()+
  theme_minimal()+
  scale_y_continuous(name = "Percent Total Foraging Events", limits = c(0,100))+
  ggtitle("2019 Potomac Foraging Occurrence")
plot_pot2019

# Plot all monthly plots in one figure

library(cowplot)

plot_pot_month_all <- plot_grid(plot_pot2016, plot_pot2017, plot_pot2018, plot_pot2019, labels = "AUTO", label_size = 12)
plot_pot_month_all

# Look at percent foraging by hour (Of the foraging events detected, what percent occurred in each hour)

pot2016hr <- filter(HourlyF, Year == 2016) %>%
  group_by(Hour) %>%
  summarize(Hourly_event_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Hourly_event_totals/ sum(Hourly_event_totals) * 100)

plot_pot2016hr <- ggplot(pot2016hr, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  scale_y_continuous(name = "Percent of Foraging Events", limits = c(0,15))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2016 Potomac Foraging Occurrence")
plot_pot2016hr

pot2017hr <- filter(HourlyF, Year == 2017) %>%
  group_by(Hour) %>%
  summarize(Hourly_event_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Hourly_event_totals/ sum(Hourly_event_totals) * 100)

plot_pot2017hr <- ggplot(pot2017hr, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  scale_y_continuous(name = "Percent of Foraging Events", limits = c(0,15))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2017 Potomac Foraging Occurrence")
plot_pot2017hr

pot2018hr <- filter(HourlyF, Year == 2018) %>%
  group_by(Hour) %>%
  summarize(Hourly_event_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Hourly_event_totals/ sum(Hourly_event_totals) * 100)

plot_pot2018hr <- ggplot(pot2018hr, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  scale_y_continuous(name = "Percent of Foraging Events", limits = c(0,15))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2018 Potomac Foraging Occurrence")
plot_pot2018hr

pot2019hr <- filter(HourlyF, Year == 2019) %>%
  group_by(Hour) %>%
  summarize(Hourly_event_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Hourly_event_totals/ sum(Hourly_event_totals) * 100)

plot_pot2019hr <- ggplot(pot2019hr, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  scale_y_continuous(name = "Percent of Foraging Events", limits = c(0,15))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))+
  ggtitle("2019 Potomac Foraging Occurrence")
plot_pot2019hr

# Plot all monthly plots in one figure

library(cowplot)

plot_pot_hr_all <- plot_grid(plot_pot2016hr, plot_pot2017hr, plot_pot2018hr, plot_pot2019hr, labels = "AUTO", label_size = 12)
plot_pot_hr_all
# Old attempts code -------------------------------------------------------

# Create actual subset dataframes
HourlyF_2017 <- filter(HourlyF, Year == 2017)
HourlyF_2018 <- filter(HourlyF, Year == 2018)
HourlyF_2019 <- filter(HourlyF, Year == 2019)

# Look at percent foraging by month
a <- HourlyF_2017 %>%
  group_by(Month) %>%
  summarize(Montly_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Montly_totals/ sum(Montly_totals) * 100)

View(a)

plota <- ggplot(a, aes(x = Month, y = percent_forage)) +
  geom_col()+
  scale_y_continuous(limits = c(0,100))
plota

# Look at percent foraging by hour (Of the foraging events detected, what percent occurred in each hour)
b <- HourlyF_2017 %>%
  group_by(Hour) %>%
  summarize(Hourly_event_totals = sum(Total_Foraging_Events))%>%
  mutate(percent_forage = Hourly_event_totals/ sum(Hourly_event_totals) * 100)

View(b)

plotb <- ggplot(b, aes(x = Hour, y = percent_forage)) +
  geom_col()+
  scale_y_continuous(name = "Percent of Foraging Events", limits = c(0,100))+
  scale_x_continuous(name = "Hour (EST)", breaks = seq(0,23,1))
plotb


