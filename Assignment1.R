install.packages("dplyr")

dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip", exdir = ".")
list.files("specdata")

files_list <- list.files("specdata", full.names=TRUE)
files_list

directory <- "specdata"
pollutant <- "sulfate"
pollutantmean <- function (directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame() 
  #creates an empty data frame
  for (i in id) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
    print(i)
  }
  mean(dat[, pollutant], na.rm=TRUE)      #identifies the median weight 
  #while stripping out the NAs
}

complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame() 
  #creates an empty data frame
  for (i in id) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
    print(i)
  }
  
  complete_cases <- complete.cases(dat)
  dat_com_cases <- dat[complete_cases,]
  gr_data <- group_by(dat_com_cases, ID)
  summarize(gr_data, mycount = n())
  
}

corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame() 
  #creates an empty data frame
  for (i in 1:332) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  
  complete_cases <- complete.cases(dat)
  dat_com_cases <- dat[complete_cases,]
  print(head(dat_com_cases))
  nobs <- sum(complete_cases)
    
  correlations <- numeric(0)
  for (myid in unique(dat_com_cases$ID)){
    if (nrow(dat_com_cases[dat_com_cases$ID == myid,]) >= threshold) {
      #print(myid)
      correlations <- c(correlations, cor(dat_com_cases[dat_com_cases$ID == myid,"sulfate"], dat_com_cases[dat_com_cases$ID == myid,"nitrate"]))
      #myid = myid + 1
      #print(i )
      #print(dat_com_cases[i, "sulfate"])
      #print(cor(dat_com_cases[dat_com_cases$ID == myid,"sulfate"], dat_com_cases[dat_com_cases$ID == myid,"nitrate"]))
    }
  }
  correlations

}