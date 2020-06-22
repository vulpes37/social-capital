# clear environment
rm(list=ls())


# INSTALL ANY NEEDED PACKAGES----
#install.packages('reshape2')
#install.packages('readstata13')
library(readstata13)
library(reshape2)

# SET YOUR DIRECTORY ----
cd <- "~/social-capital"
cd_data <- paste(cd, "data", sep=.Platform$file.sep)
setwd(cd_data)
# **********************************************************************

nccs_orleans <- function() {

  fileNames <- list.files(pattern = "\\.csv$")
  start_i <- 1
  dat <- read.csv( fileNames[start_i], stringsAsFactors = FALSE )
  year <- as.integer( substr(fileNames[start_i], 10, 13) )
  dat$file_year <- year
  dat <- dat[ !(is.na(dat$FIPS)), ]
  dat <- dat[dat$FIPS == 22071,  c("EIN", "FIPS", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP")]
  
  for(i in 2:length(fileNames)){
    

    print( paste("just loaded", substr(fileNames[i], 10, 13), sep = " ") )
    print( paste(i, paste( "out of", length(fileNames), sep = " "), sep = " ") )
    

    temp <- read.csv( fileNames[i], stringsAsFactors = FALSE )
    year <- as.integer( substr(fileNames[i], 10, 13) )
    temp$file_year <- year
    
    if(year==2004){
      temp <- temp[ !(is.na(temp$Fips)), ]
      temp <- temp[temp$FIPS == 22071,  c("EIN", "Fips", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP")]
    }else{
      temp <- temp[ !(is.na(temp$FIPS)), ]
      temp <- temp[temp$FIPS == 22071,  c("EIN", "FIPS", "STYEAR", "file_year", "NTEE1", "TOTREV", "ZIP")]
    }

    dat <- rbind(dat, temp)
  }
  
  write.csv("nccs_orleans.csv", row.names = FALSE)  

}

compare_fwf_dta <- function() {
  
  dta <- read.dta13("la_births_deaths/LA_Births.dta")
  bf_widths <- read.csv("la_births_deaths/B9909_widths.csv")
  bf <- read.fwf("la_births_deaths/B9909", widths=bf_widths["width"])
  print( paste('LA_Births.dta shape:', nrow(dta), 'rows,', ncol(dta), 'cols.', sep = " ") )
  print( paste('B9909.txt shape:', nrow(bf), 'rows,', ncol(bf), 'cols.', sep = " ") )
  
} 

clean_bea <- function() {
  
  dat <- read.csv("la_bea_raw.csv", stringsAsFactors = FALSE, check.names = F )
  dat <- dat[!is.na(dat$LineCode), ]
  dat <- melt(data=dat, id.vars = c("GeoFips", "GeoName", "LineCode", "Description"))
  names(dat) <- c("fips", "name", "code", "var", "year", "value")
  dat <- dat[ , names(dat)!="code"]
  dat <- dcast(data=dat, fips + name + year  ~ var, value.var = "value" )
  names(dat) <- c("fips", "name", "year", "total_wages", "total_pop", "total_inc", "total_emp")
  write.csv(dat, "la_bea_clean.csv", row.names = FALSE) 
  
}


# pull out orleans from nccs data 
# nccs_orleans()

# compare dta vs. B9909 - warning: loading fwf takes around an hour.
# compare_fwf_dta()
# "LA_Births.dta shape: 725534 rows, 141 cols."
# "B9909.txt shape: 725534 rows, 158 cols."

# clean la bea file
# clean_bea()
  