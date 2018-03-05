library(tidyverse)
library(stringr)

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years.pt1 <- c(1985:2006)
urls.pt1 <- str_c(url1, years.pt1, url2, sep = "")
filenames <- str_c(years.pt1, sep = "")
N <- length(urls.pt1)

for (i in 1:N){
  suppressMessages(
    assign(filenames[i], read_table(urls.pt1[i], col_names = TRUE))
  )
  
  file <- get(filenames[i])
  
  colnames(file)[1] <- "YYYY"
  file[1] <- sapply(file[1],function(x) {
    if (file[1,1]<100) {
      mutate(file[1]+1900)
    }
    else {
      file[1] 
    }
    })
  
  file$TIDE <- NA
  
  file$mm <- "00"
  
  file$`WSPD` <- str_split_fixed(file$`WSPD GST`," ",2)[,1]
  file$`GST` <- str_split_fixed(file$`WSPD GST`," ",2)[,2]
  file <- file %>% select(-`WSPD GST`)
  
  file <- file %>% dplyr::rename("WDIR"="WD","PRES"="BAR")
  
  file <- file[c(1:4,16,5,17:18,6:15)]
  
  file <- file %>% filter(file$hh==12)

  if (i == 1) {
    bouy.pt1 <- file
  }
  else {
    bouy.pt1 <- rbind.data.frame(bouy.pt1, file)
  }
}


years.pt2 <- c(2007:2012,2014:2017)
urls.pt2 <- str_c(url1, years.pt2, url2, sep = "")
filenames <- str_c(years.pt2, sep = "")
N <- length(urls.pt2)

for (i in 1:N){
  suppressMessages(
    assign(filenames[i], read_table(urls.pt2[i], col_names = TRUE))
  )
  
  file <- get(filenames[i])[-1,]
  
  colnames(file)[1] <- "YYYY"
  
  values <- unlist(regmatches(file$`WDIR WSPD GST`,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",file$`WDIR WSPD GST`)))
  file$`WDIR` <- values[seq(1,length(values),by=3)]
  file$`WSPD` <- values[seq(2,length(values),by=3)]
  file$`GST` <- values[seq(3,length(values),by=3)]
  file <- file %>% select(-`WDIR WSPD GST`)
  
  if (colnames(file[9])=="MWD   PRES") {
    file$MWD <- str_split_fixed(file$`MWD   PRES`," ",2)[,1]
    file$PRES <- str_split_fixed(file$`MWD   PRES`," ",2)[,2]
    file <- file %>% select(-`MWD   PRES`)
  }
  
  file12 <- file %>% filter(file$hh=="12" & file$mm=="00")
  file1150 <- file %>% filter(file$hh=="11" & file$mm=='50')
  
  file <- rbind(file12,file1150)
  
  file <- file[c(1:5,14:16,6:8,17:18,9:13)]
  
  if (i == 1) {
    bouy.pt2 <- file
  }
  else {
    bouy.pt2 <- rbind.data.frame(bouy.pt2, file)
  }
}

bouy <- rbind(bouy.pt1,bouy.pt2)

bouy.tidy <- bouy %>% 
  mutate(MWD=replace(MWD,MWD=="999",NA)) %>%
  mutate(ATMP=replace(ATMP,ATMP=="999.0",NA)) %>%
  mutate(ATMP=replace(ATMP,ATMP=="999",NA)) %>%
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

bouy.tidy$ATMP <- as.numeric(bouy.tidy$ATMP)
bouy.tidy$WTMP <- as.numeric(bouy.tidy$WTMP)
