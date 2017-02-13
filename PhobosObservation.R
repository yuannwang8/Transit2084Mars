## Parse and plot locations where the occultation of Earth by Phobos can be seen
## during the Earth-Moon Transit from Mars of 2084-11-10

## HORIZONS systems Input variables
# Ephemeris type: Observer
# Target body: Earth [Geocenter] [399]
# Observer location: sampled location on Mars [499] - note height is referenced from MOLA data
# Time span: 2084-11-10 Start and Stop hh:mm to be provided, 
#            recommends 2 minute intervals with Step Size at 120 equal intervals for 1 second-steps 
# Table Settings
# - Note: Variable selection: read-in frames should include only Quantities 1, 4, 12, 13, 16, 23, and 25. 
# - Note 2: Optional observer-table settings to include 
# -- angle format: decimal degrees
# -- CVS format: yes (very important unless you prefer fixed-with text-table imports)
# -- object page: yes (more context)
# Display/Output: plain text format (for viewing and saving)
# - Save the files in a folder called /Line/ in your working folder

# You may be wondering: why download so many variables if only 4 and 25 are used in the final analysis? 
# This is because the other variables are used to determine if the Sun, at a given time, is above the horizon, 
# if the Earth is transiting the Sun, and how far away the Earth's sub-solar point is away from the sub-observer point. 
# These are used to narrow down the desired surface location.  
# Remember, you want a Sun-Earth-Phobos-Observer line on the sunny side of Mars!

########
# R environment preparation

# set working folder to your working folder
setwd("your_working_folder")

# set system time to UTC and double check it. Just to make sure POSIXcx behaves
Sys.setenv(TZ="UTC")
# > Sys.timezone()

#########
# Prepare data

# a function to read the HORIZONS output, extract location and minimum ToI (Earth-Observer-Phobos) in one file
parse.file<-function(file.name){
  A <-readLines(file.name)
# Extract data table 
  As<-grep("\\$\\$SOE", A)+1
  Ae<-grep("\\$\\$EOE", A)-1
  keep<-A[c(As:Ae)]
#  read in and extract relevant columns
  dataT<-read.table(text=keep, sep=",", stringsAsFactors = FALSE)
  dataT<-dataT[,c(1,6,7,15)]
  colnames(dataT)<-c("Time","Azimuth","Elevation","TOI")
  dataT[,"Time"]<-as.POSIXct(strptime(dataT$Time,format="%Y-%b-%d %H:%M:%S"),tz="UTC")
# find and extract row with minimum ToI
  dataT<-dataT[which(dataT$TOI==-min(abs(dataT$TOI))),]
# extract and add location to the dataframe
  location<-A[44]
  location<-gsub("Center geodetic : ","",location)
  location<-gsub("\\{","",location)
  location<-gsub("\\}","",location)
  location<-gsub("\\(","",location)
  location<-gsub("\\)","",location)
  location<-gsub(" E-londeg,Latdeg,Altkm","",location)
  locationT<-read.table(text=location, sep=",", stringsAsFactors = FALSE)  
  colnames(locationT)<-c("Elondeg","Latdeg","Altkm")
  dataTL<-merge(dataT, locationT)
# add EOP file - references the in-system source file and provides prediction good-till date  
  EOPFile<-A[56]
  EOPFile<-gsub("EOP file        : ","",EOPFile)
  EOPFile<-gsub("^\\s+|\\s+$", "", EOPFile)
  EOPFileT<-read.table(text=EOPFile, sep=",", stringsAsFactors = FALSE)
  colnames(EOPFileT)<-c("EOPFile")
  dataTL<-merge(dataTL,EOPFileT)
# select the first row in the unlikely event that there are two entries with the same minimum TOI
  dataTL<-dataTL[1,]
}

# loop function over the files in /Line/
file.name <- list.files(c("Line/"))
landlist<-NULL # initiate an empty dataframe
# create and run loop - add a row with each new file read in
for (i in 1:length(paste0(c("Line/"),file.name))){
  landlist<-rbind(landlist, parse.file(paste0(c("Line/"),file.name[i])))}

# where necessary, save a copy of this output
saveRDS(landlist,"landlist.rds")

# select those with TOI below threshold of 0.02 degrees
landplot<-subset(landlist, abs(TOI)<0.02001)
# identify those locations that need to be requeried
landfix<-subset(landlist, abs(TOI)>0.02)

#########
# Plot data
library(ggplot2)

ggplot(landlist, aes(x=-Elondeg, y=Latdeg)) + 
  geom_point(color="gray") + 
  geom_point(data=landplot, aes(color=Altkm)) +
  geom_line(data=landplot, aes(color=Altkm))

