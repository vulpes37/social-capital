# clear environment
rm(list=ls())


# INSTALL ANY NEEDED PACKAGES----
#install.packages('reshape2')
library(reshape2)

# SET YOUR DIRECTORY ----
cd <- "~/social-capital"
cd_data <- paste(cd, "data", sep=.Platform$file.sep)
setwd(cd_data)
# **********************************************************************

# load data 

# get list of files names 
fileNames <- list.files(pattern = "\\.csv$")

# load first file 
start_i <- 1
dat <- read.csv( fileNames[start_i], stringsAsFactors = FALSE )

# year 
year <- as.integer( substr(fileNames[start_i], 10, 13) )
dat$file_year <- year

# delete mising
dat <- dat[ !(is.na(dat$FIPS)), ]
dat <- dat[dat$FIPS == 22071,  c("EIN", "FIPS", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP")]

for(i in 2:length(fileNames)){
  
  # index 
  print( paste("just loaded", substr(fileNames[i], 10, 13), sep = " ") )
  print( paste(i, paste( "out of", length(fileNames), sep = " "), sep = " ") )
  
  # load
  temp <- read.csv( fileNames[i], stringsAsFactors = FALSE )

  # year 
  year <- as.integer( substr(fileNames[i], 10, 13) )
  temp$file_year <- year
  
  if(year==2004){
    # delete mising
    temp <- temp[ !(is.na(temp$Fips)), ]
    temp <- temp[temp$FIPS == 22071,  c("EIN", "Fips", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP")]
  }else{
    # delete mising
    temp <- temp[ !(is.na(temp$FIPS)), ]
    temp <- temp[temp$FIPS == 22071,  c("EIN", "FIPS", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP")]
  }

  # bind with full set
  dat <- rbind(dat, temp)
}

write.csv("nccs_orleans.csv", row.names = FALSE)  
