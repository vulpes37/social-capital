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

aggregate_la_births <- function() {
  
  dat <- read.csv("la_births_deaths/la_births.csv", stringsAsFactors = FALSE, check.names = F )
  
  # make date variable from integer Delivery Date (del_date)
  dat$del_date <- paste("00", c(dat$del_date), sep="")
  dat$del_date <- as.Date(substr(dat$del_date, start=nchar(dat$del_date)-5, stop=nchar(dat$del_date)),
                           format="%m%d%y")
  dat$del_year_month <- format(dat$del_date, format="%Y-%m")
  
  # counstruct fipscode from residence parish (res_par1)
  dat$fips <- paste("00", c(dat$res_par1), sep="")
  dat$fips <- paste("22", substr(dat$fips, start=nchar(dat$fips)-2, stop=nchar(dat$fips)), sep="")
  
  # aggregate by fips and month
  births_by_fips_month <- aggregate(dat$livebirth, by=list(dat$del_year_month, dat$fips), FUN=length)
  names(births_by_fips_month) <- c("year_month", "fips", "num_births")
  write.csv(births_by_fips_month, "la_births_deaths/la_births_fips_month.csv", row.names = FALSE)

}

# pull out orleans from nccs data 
# nccs_orleans()

# compare dta vs. B9909 - warning: loading fwf takes around an hour.
# compare_fwf_dta()
# "LA_Births.dta shape: 725534 rows, 141 cols."
# "B9909.txt shape: 725534 rows, 158 cols."

# clean la bea file
# clean_bea()

# aggregate la_births.csv by fips and month
# aggregate_la_births()
  