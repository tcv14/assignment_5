library(tidyverse)
library(stringr)

# preparing to read in data from website
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

# first group of data (1985 to 2006)
years.pt1 <- c(1985:2006)
urls.pt1 <- str_c(url1, years.pt1, url2, sep = "")
filenames <- str_c(years.pt1, sep = "")
N <- length(urls.pt1)

# use for loop to read in data
for (i in 1:N){
  suppressMessages(
    assign(filenames[i], read_table(urls.pt1[i], col_names = TRUE))
  )
  
  file <- get(filenames[i])
  
  colnames(file)[1] <- "YYYY"
  file[1] <- sapply(file[1],function(x) { # add 19 to beginning of years where there is no 19
    if (file[1,1]<100) {
      mutate(file[1]+1900)
    }
    else {
      file[1] 
    }
    })
  
  file$TIDE <- NA # add the column for TIDE and there are no values yet
  
  file$mm <- "00" # add the column for minutes (mm) and there are no values yet
  
  # WSDP and GST are read in as one column, so have to split them up
  file$`WSPD` <- str_split_fixed(file$`WSPD GST`," ",2)[,1]
  file$`GST` <- str_split_fixed(file$`WSPD GST`," ",2)[,2]
  file <- file %>% select(-`WSPD GST`)
  # rename newly created columns
  file <- file %>% dplyr::rename("WDIR"="WD","PRES"="BAR")
  
  # reorder columns to match website
  file <- file[c(1:4,16,5,17:18,6:15)]
  
  # filter down to one observation at noon time
  file <- file %>% filter(file$hh==12)
  
  # save data altogether in object called bouy.pt1 as data is read in
  if (i == 1) {
    bouy.pt1 <- file
  }
  else {
    bouy.pt1 <- rbind.data.frame(bouy.pt1, file)
  }
}

# second group of data (2007 to 2017, no data available for 2013)
years.pt2 <- c(2007:2012,2014:2017)
urls.pt2 <- str_c(url1, years.pt2, url2, sep = "")
filenames <- str_c(years.pt2, sep = "")
N <- length(urls.pt2)

# use for loop to read in data
for (i in 1:N){
  suppressMessages(
    assign(filenames[i], read_table(urls.pt2[i], col_names = TRUE))
  )
  
  file <- get(filenames[i])[-1,]
  
  colnames(file)[1] <- "YYYY"
  
  # WDIR, WSDP, and GST are read in as one column, so have to split them up
  values <- unlist(regmatches(file$`WDIR WSPD GST`,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",file$`WDIR WSPD GST`)))
  file$`WDIR` <- values[seq(1,length(values),by=3)]
  file$`WSPD` <- values[seq(2,length(values),by=3)]
  file$`GST` <- values[seq(3,length(values),by=3)]
  file <- file %>% select(-`WDIR WSPD GST`)
  
  # MWD and PRES also read in as one column, so have to split them
  if (colnames(file[9])=="MWD   PRES") {
    file$MWD <- str_split_fixed(file$`MWD   PRES`," ",2)[,1]
    file$PRES <- str_split_fixed(file$`MWD   PRES`," ",2)[,2]
    file <- file %>% select(-`MWD   PRES`)
  }
  
  # filter down to one observation at noon time or as close to noon time as possible
  file <- file %>% filter((file$hh=="12" & file$mm=="00")|(file$hh=="11" & file$mm=='50'))
  
  # reorder columns to match website
  file <- file[c(1:5,14:16,6:8,17:18,9:13)]
  
  # save data altogether in object called bouy.pt1 as data is read in
  if (i == 1) {
    bouy.pt2 <- file
  }
  else {
    bouy.pt2 <- rbind.data.frame(bouy.pt2, file)
  }
}

# row bind the two groups of data together
bouy <- rbind(bouy.pt1,bouy.pt2)

# tidying data (using NAs where there are unreasonable data values,
# selecting columns of interest, and uniting columns to form a date)
bouy.tidy <- bouy %>% 
  mutate(MWD=replace(MWD,MWD=="999",NA)) %>%
  mutate(ATMP=replace(ATMP,ATMP=="999.0",NA)) %>%
  mutate(ATMP=replace(ATMP,ATMP=="999",NA)) %>%
  mutate(ATMP=replace(ATMP,ATMP=="99",NA)) %>%
  mutate(ATMP=replace(ATMP,ATMP=="99.0",NA)) %>%
  mutate(WTMP=replace(WTMP,WTMP=="99.0",NA)) %>%
  mutate(WTMP=replace(WTMP,WTMP=="999.0",NA)) %>%
  mutate(WTMP=replace(WTMP,WTMP=="999",NA)) %>%
  mutate(DEWP=replace(DEWP,DEWP=="999.0",NA)) %>%
  mutate(DEWP=replace(DEWP,DEWP=="999",NA)) %>%
  mutate(VIS=replace(VIS,VIS=="99.0",NA)) %>%
  mutate(VIS=replace(VIS,VIS=="99",NA)) %>%
  mutate(TIDE=replace(TIDE,TIDE=="99.00",NA)) %>%
  mutate(PRES=replace(PRES,PRES=="9999.0",NA)) %>%
  mutate(PRES=replace(PRES,PRES=="9999",NA)) %>%
  mutate(WDIR=replace(WDIR,WDIR=="999",NA)) %>%
  mutate(WSPD=replace(WSPD,WSPD=="99.0",NA)) %>%
  mutate(GST=replace(GST,GST=="99.0",NA)) %>%
  mutate(WVHT=replace(WVHT,WVHT=="99.00",NA)) %>%
  mutate(WVHT=replace(WVHT,WVHT=="99",NA)) %>%
  mutate(DPD=replace(DPD,DPD=="99.00",NA)) %>%
  mutate(DPD=replace(DPD,DPD=="99",NA)) %>%
  mutate(APD=replace(APD,APD=="99.00",NA)) %>%
  mutate(APD=replace(APD,APD=="99",NA)) %>%
  select(1:3,14:15) %>%
  unite(Date,1:3,sep="-")

# convert columns into numeric values
bouy.tidy$ATMP <- as.numeric(bouy.tidy$ATMP)
bouy.tidy$WTMP <- as.numeric(bouy.tidy$WTMP)

# save workspace as RData file
save.image(file="data_workspace.RData")
# load workspace from RData file
load("data_workspace.RData")
