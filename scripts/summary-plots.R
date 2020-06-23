# clear environment
rm(list=ls())

# INSTALL ANY NEEDED PACKAGES----
#install.packages('reshape2')
#install.packages('ggplot2')
#install.packages('scales')
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
library(extrafont)

# SET YOUR DIRECTORY ----
cd <- "~/social-capital"
cd_data <- paste(cd, "data", sep=.Platform$file.sep)
# **********************************************************************

population_corr <- function(year) {
  # Takes either an integer for the year you want to plot, ex year=2004 
  # Or the string year="all" to plot all years 
  
  # load data
  dat <- read.csv(paste(cd_data , "la_births_deaths", "births_by_id_month.csv", sep=.Platform$file.sep), stringsAsFactors = F)                 
  pop <- read.csv(paste(cd_data , "bea", "la_bea_clean.csv", sep=.Platform$file.sep), stringsAsFactors = F)                 
  
  # crosswalk fips to id variable
  crosswalk <- unique(pop[ , c("fips", "name")])
  crosswalk <- crosswalk[crosswalk$fips!=22000, ]
  crosswalk$id <- 1:64
  write.csv(crosswalk, paste(cd_data , "la_births_deaths", "crosswalk.csv", sep=.Platform$file.sep), row.names = F)
  
  # merge with fipscode
  pop <- merge(pop, crosswalk, by=c("fips", "name"))
  
  # aggregate numbirths by year
  dat$year <- substr(dat$year_month,1,4)
  dat <- aggregate(data=dat, num_births ~ year + id, FUN = "sum")

  # merge with population data 
  dat <- merge(pop, dat, by=c("year", "id"))
  dat <- dat[order(dat$fips, dat$year), ]
  dat <- dat[ , c("id", "fips", "name", "year", "total_pop", "total_emp", "num_births")]
  
  
  # log variables 
  dat$pop_log <- log(dat$total_pop)
  dat$emp_log <- log(dat$total_emp)
  dat$births_log <- log(dat$num_births)

  if(year=="all"){
    temp <- dat
  }else{
    temp <- dat[dat$year==year, ]
  }
  
  ggplot(data=temp, aes(births_log, pop_log)) + geom_point(color="#154578") + 
    geom_smooth(method='lm', formula= y~x, color="#A6CFEB", fill="#162530", alpha=0.2) + 
    scale_y_continuous(labels=scales::comma) + 
    scale_x_continuous(labels=scales::comma) + 
    xlab("Annual Count of Live Births") + 
    ylab("Total Population (in thousands)") + 
    ggtitle("Correlation of Live Births and Populations Across Louisiana Parishes") +
    theme_minimal() + 
    theme(
      # text = element_text(family = "Montserrat"),
      # axis.text =  element_text(family = "Montserrat"),
      axis.line = element_line())
  
  x <- lm(data=temp, emp_log~births_log)
  print(summary(x))
  
  file_name <- paste0("pop_births_correlation_log_", year, ".png")
  ggsave(paste(cd,file_name, sep=.Platform$file.sep), width=10, height=7)
  
}


county_timeseries <- function(){
  
  # load data
  dat <- read.csv(paste(cd_data , "la_births_deaths", "births_by_id_month.csv", sep=.Platform$file.sep), stringsAsFactors = F)                 
  pop <- read.csv(paste(cd_data , "bea", "la_bea_clean.csv", sep=.Platform$file.sep), stringsAsFactors = F)                 
  
  # crosswalk fips to id variable
  crosswalk <- unique(pop[ , c("fips", "name")])
  crosswalk <- crosswalk[crosswalk$fips!=22000, ]
  crosswalk$id <- 1:64
  
  # merge with fipscode
  dat <- merge(dat, crosswalk, by=c("id"))

  # subset
  trt <- c("22051","22071","22075","22087", "22089", "22103")
  
  ts_plot <- function(trt){
    
    temp <- dat[dat$fips==trt, ]
    label <- dat[dat$fips==trt, "name"]
    temp$time <- 1:nrow(temp)
    
    ggplot(data = temp, aes(time, num_births)) +
      geom_vline(xintercept=temp[ temp$year_month == "2005-08", "time" ], color="#A6CFEB", size=0.8) +
      geom_line(color="#162530", size=0.6) + 
      xlab("Month") +
      ylab("Live Births") + 
      scale_y_continuous(labels=scales::comma) + 
      ggtitle(label) +
      theme_minimal() + 
      theme(
        text = element_text(family = "Montserrat"),
        axis.text =  element_text(family = "Montserrat"),
        axis.line = element_line())
  }
  
  
  png(paste(cd,"county_timeseries.png", sep=.Platform$file.sep), width = 12, height = 10, units = "in", res=300 )
  grid.arrange( ts_plot(trt[1]), ts_plot(trt[2]),
                ts_plot(trt[3]), ts_plot(trt[4]),
                ts_plot(trt[5]), ts_plot(trt[6]),
                ncol =2 ) 
  dev.off()
}  


