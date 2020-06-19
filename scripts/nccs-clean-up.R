# clear environment
rm(list=ls())


# INSTALL ANY NEEDED PACKAGES----
#install.packages('reshape2')
library(readstata13)
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
dat <- dat[dat$FIPS == 22071,  c("EIN", "FIPS", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP", "ZIP5")]

for(i in 2:length(fileNames)){
  
  # index 
  print( paste("just loaded", substr(fileNames[i], 10, 13), sep = " ") )
  print( paste(i, paste( "out of", length(fileNames), sep = " "), sep = " ") )
  
  # load
  temp <- read.csv( fileNames[i], stringsAsFactors = FALSE )
  
  # year 
  year <- as.integer( substr(fileNames[start_i], 10, 13) )
  temp$file_year <- year
  
  # delete mising
  temp <- temp[ !(is.na(temp$FIPS)), ]
  temp <- temp[temp$FIPS == 22071,  c("EIN", "FIPS", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP", "ZIP5")]
  
  # bind with full set
  dat <- rbind(dat, temp)
}
  

compare_fwf_dta <- function() {
  
  dta <- read.dta13("la_births_deaths/LA_Births.dta")
  bf_widths <- read.csv("la_births_deaths/B9909_widths.csv")
  bf <- read.fwf("la_births_deaths/B9909", widths=bf_widths["width"])
  print( paste('LA_Births.dta shape:', nrow(dta), 'rows,', ncol(dta), 'cols.', sep = " ") )
  print( paste('B9909.txt shape:', nrow(bf), 'rows,', ncol(bf), 'cols.', sep = " ") )
  
} 

# compare dta vs. B9909 - warning: loading fwf takes around an hour.
# compare_fwf_dta()
# "LA_Births.dta shape: 725534 rows, 141 cols."
# "B9909.txt shape: 725534 rows, 158 cols."

