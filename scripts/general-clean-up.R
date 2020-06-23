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
# **********************************************************************

nccs_orleans <- function() {
  
  cd_nccs <- paste(cd_data, "nccs", "raw", sep=.Platform$file.sep)

  fileNames <- list.files(path=cd_nccs, pattern = "\\.csv$")
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
  save_clean <- paste(cd_data, "nccs","nccs_orleans.csv", sep=.Platform$file.sep)
  write.csv(save_clean, row.names = FALSE)  

}

compare_fwf_dta <- function() {
  cd_births <- paste(cd_data, "la_births_deaths", sep=.Platform$file.sep)

  dta <- read.dta13(paste(cd_births, "LA_Births.dta", sep=.Platform$file.sep))
  bf_widths <- read.csv(paste(cd_births, "B9909_widths.csv", sep=.Platform$file.sep))
  bf <- read.fwf(paste(cd_births, "B9909", sep=.Platform$file.sep), widths=bf_widths["width"])
  print( paste('LA_Births.dta shape:', nrow(dta), 'rows,', ncol(dta), 'cols.', sep = " ") )
  print( paste('B9909.txt shape:', nrow(bf), 'rows,', ncol(bf), 'cols.', sep = " ") )
  
} 

clean_bea <- function() {
  cd_bea <- paste(cd_data, "bea", sep=.Platform$file.sep)
  dat <- read.csv(paste(cd_bea, "la_bea_raw.csv", sep=.Platform$file.sep), stringsAsFactors = FALSE, check.names = F )
  dat <- dat[!is.na(dat$LineCode), ]
  dat <- melt(data=dat, id.vars = c("GeoFips", "GeoName", "LineCode", "Description"))
  names(dat) <- c("fips", "name", "code", "var", "year", "value")
  dat <- dat[ , names(dat)!="code"]
  dat <- dcast(data=dat, fips + name + year  ~ var, value.var = "value" )
  names(dat) <- c("fips", "name", "year", "total_wages", "total_pop", "total_inc", "total_emp")
  write.csv(dat, paste(cd_bea, "la_bea_clean.csv", sep=.Platform$file.sep), row.names = FALSE) 
  
}

aggregate_la_births <- function() {
  
  cd_births <- paste(cd_data, "la_births_deaths", sep=.Platform$file.sep)
  dat <- read.csv(paste(cd_births, "la_births.csv", sep=.Platform$file.sep), stringsAsFactors = FALSE, check.names = F )

  # residents of Louisiana
  dat <- dat[dat$m_state== 19, ] 
  
  # make date variable from integer Delivery Date (del_date)
  dat$del_date <- paste("00", c(dat$del_date), sep="")
  dat$del_date <- as.Date(substr(dat$del_date, start=nchar(dat$del_date)-5, stop=nchar(dat$del_date)),
                           format="%m%d%y")
  dat$del_year_month <- format(dat$del_date, format="%Y-%m")
  
  # id variable 
  dat$id <- dat$res_par1

  # aggregate by fips and month
  births_by_id_month <- aggregate(data=dat, livebirth ~ del_year_month + id, FUN="sum") 
  names(births_by_id_month) <- c("year_month", "id", "num_births")
  
  write.csv(births_by_id_month, paste(cd_births, "births_by_id_month.csv", sep=.Platform$file.sep), row.names = FALSE)
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
  