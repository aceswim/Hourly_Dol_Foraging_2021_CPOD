rm(list = ls()) # clear workspace

install.packages("data.table")
install.packages("ggplot2")
install.packages("reshape2")
library(data.table)
library(ggplot2)
library(reshape2)

#####------ File with clicks ------#####
# location of cpod data
CPODdata <- "F:/Marine Mammal Lab Work/CPOD_Foraging_2021/Foraging_2021_CPOD"
# files must be in the format of Site Date POD# file01 Dol DPM/ICI.txt
filenames <- list.files(path = CPODdata, pattern = 'Dol ICIs.txt')

# get the cpod sitenames from above
cpods <- sapply(filenames, function(x) substr(x, 1, 3))
DeployMonthYear <- paste(substr(filenames[1],10,11), substr(filenames[1], 5, 8), sep = '')

# File with occurrences
DPMfilenames <- list.files(path = CPODdata, pattern = 'Dol DPM.txt')

# pre-allocate as a list to put dataframes in
D <- list()
propForage <- list(length(filenames))
# for (j in 1:length(filenames)){ #### This is not running in a loop, so manually changing j value below
j=1
# read in ICI data
f0 <- fread(input = paste(CPODdata,filenames[j], sep = '/'))
# sort out the ChunkEnd into DateTime
f0$ChunkEnd <- as.POSIXct(f0$ChunkEnd, format = '%d/%m/%Y %H:%M') #Check whether y(17) or Y(2017)

f0$lnICIus <- log(f0$ICIus)
f0$Foraging <- f0$lnICIus < 9.201
f1 <- aggregate(lnICIus ~ ChunkEnd, FUN = mean, data = f0) # get the mean ICI per minute
f2 <- aggregate(Foraging ~ ChunkEnd, FUN = any, data = f0) # select only foraging minutes
f1 <- merge(f1, f2) # merge the two
f1 <- f1[order(f1$ChunkEnd), ] #just ordering in time
write.csv(f1, "Potomac2019_AllICI.csv") # Export to fix time
#Before running rest of code, manually reformat the chunk end in excel
#(format in time, then custom with mm/dd/yyyy HH:mm)

f1.1<-read.csv("Potomac2019_AllICI.csv", header = T) # Import back in
f1.1$ChunkEnd <- as.POSIXct(f1.1$ChunkEnd, format = '%m/%d/%Y %H:%M') # Check whether y(17) or Y(2017)
# rm(f0, f2) #remove the initially loaded file if need to save RAM

# read in DPM data
d0 <- fread(paste(CPODdata,DPMfilenames[j], sep = '/'))
# select only minutes where the C-POD is on
d0 <- d0[d0$MinsOn==1,]
# sort out the ChunkEnd into DateTime
d0$ChunkEnd <- as.POSIXct(d0$ChunkEnd, format = '%d/%m/%Y %H:%M')
#Remove the rows with "0" presence:
d0$DPM<-as.integer(d0$DPM)
d1 <- d0[d0$DPM > 0, ]
# rm(d0) #remove the initial file if need to save RAM
d1 <- d1[order(d1$ChunkEnd), ]
#Time between occurrences, minutes
d1$MinutesBetwOcc <- c(NA, diff(d1$ChunkEnd))
#Number the encounters:
ThreshEnc <- 37 #Threshold in minutes defining new encounter
d1$EncNumber <- NA
enc <- 1
d1$EncNumber[1] <- enc
for(i in 2:length(d1$EncNumber)){ 
  if(d1$MinutesBetwOcc[i] < ThreshEnc){
    d1$EncNumber[i] <- enc
  } else {
    enc <- enc + 1
    d1$EncNumber[i] <- enc
  }
}
#Check correctness:
if (isTRUE(max(d1$EncNumber) == sum(d1$MinutesBetwOcc[-1] >= ThreshEnc) + 1)){
  #####------ Merge two datasets ------#####
  D <- as.data.frame(merge(d1, f1.1, by = "ChunkEnd")) # check that D has the same nrow as d1 and f1
  colnames(D) <- c('DateTime','File','DPM','Nall','MinsOn','MinutesBetwOcc','EncNumber','?','lnICIus','Foraging')
  # find the proportion of minutes in each encounter foraging
  # first assign the start datetime
  # propForage[[j]] <- data.frame(c(aggregate(D$DateTime, by = list(D$EncNumber), 
  #                                                                   FUN = min)[,2], format = '%d/%m/%Y %H:%M:%s'),
  #                                              c(unique(D$EncNumber)), # now give it the encounter number
  #                                              c(aggregate(D$Foraging, by = list(D$EncNumber),
  #                                                                       FUN = sum)[,2]/aggregate(D$DPM,
  #                                                                                                by = list(D$EncNumber),
  #                                                                                                FUN = sum)[,2] * 100), # find the proportion of minutes foraging
  #                               c(aggregate(D$Foraging, by = list(D$EncNumber),
  #                                    FUN = sd)[,2] * 100), # standard deviation of number of minutes foraging
  #                          c(aggregate(D$DPM, by = list(D$EncNumber),
  #                                    FUN = sum)[,2]))
  # colnames(propForage) = c('DateTime','EncNumber','%ageForaging','%ageForagingSD',
  #                               'EncDuration(mins)')
  # # save the dataset as an RData file
  # save(D, propForage, file = paste('~/Desktop/CPOD_Data'
  #                      ,cpods[j], DeployMonthYear, '.RData', sep = '')) # create filename from the C-POD list
  # # save csv for future reference
  # # write.csv(D[[j]], file = paste('~/Desktop/CPOD_Data',
  #                                # cpods[j],'OctMay2018.csv', sep = ''), row.names = F)
  # } else {
  # print(paste('Check filename number', j, sep = ' '))
}

# lapply(1:length(cpods), function(x)
#   write.csv(propForage[[x]], file = paste('D:/Aran/UMCES/Dolphin/StormProject/Foraging/',
#                                 cpods[x],'PropForaging', DeployMonthYear, '.csv', sep = ''), row.names = F))

EncNumber <- as.data.frame(D$EncNumber)
DateTime <- as.data.frame(D$DateTime) #Change this to DateTIme or ChunkEnd, whichever is apropriate
Foraging <- as.data.frame(D$Foraging)
DPM <- as.data.frame(D$DPM)
D2 <- as.data.frame(c(DateTime,EncNumber,Foraging,DPM))
colnames(D2) <- c("DateTime", "EncNumber","Foraging","DPM")

DT <- data.frame(c(aggregate(D2$DateTime, by=list(D2$EncNumber), FUN=min, na.rm=T)))
ForSum <- data.frame(aggregate(D2$Foraging, by = list(D2$EncNumber), FUN = sum, na.rm=T))
DPMSum <- data.frame(aggregate(D2$DPM,by = list(D2$EncNumber),FUN = sum, na.rm=T))
PercFor <-  (ForSum/DPMSum)  # find the proportion of minutes foraging
SD <- data.frame(aggregate(D2$Foraging, by = list(D2$EncNumber), FUN = sd, na.rm=T) * 100) # standard deviation of number of minutes foraging

DateTime <-list(DT$x)
EncNumber <- list(DT$Group.1)
PercForaging <- list(PercFor$x)
PercForagingSd <- list(SD$x)
EncDuration <- list(DPMSum$x)
DFinal <- as.data.frame(c(DateTime, EncNumber, PercForaging, PercForagingSd, EncDuration))

colnames(DFinal) = c('DateTime','EncNumber','%ageForaging','%ageForagingSD',
                     'EncDuration(mins)')
write.csv(DFinal, "Potomac2019_PropForaging_2019.csv")
