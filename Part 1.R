# load required packages
library(plyr)
library(tidyverse)

# download and save data

data <- function() {
  year1 <- c(1985:2006)
  for (i in year1) {
    data <- read.table(file=paste("http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h",i,".txt.gz&dir=data/historical/stdmet/",sep=""),header=TRUE,fill=TRUE)
    saveRDS(data,file=paste("./Data/",i,".rds",sep=""))
  }
  year2 <- c(2007:2012,2014:2017) # skip year 2013 because there is no data for it
  for (i in year2) {
    data <- read.table(file=paste("http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h",i,".txt.gz&dir=data/historical/stdmet/",sep=""),header=FALSE,fill=TRUE)
    colnames(data) <- c('YY','MM','DD','hh','mm','WDIR','WSPD','GST','WVHT','DPD','APD','MWD','PRES','ATMP','WTMP','DEWP','VIS','TIDE')
    saveRDS(data,file=paste("./Data/",i,".rds",sep=""))
  }
}

# data() # uncomment this line if needed

# read data

ldf <- llply(list.files("./Data", pattern="*.rds", full.names=TRUE), readRDS)
names(ldf) <- c(1985:2012,2014:2017)

group1 <- ldf[1:15] # year 1985-1999
group2 <- ldf[16:20] # year 2000-2004
group3 <- ldf[21:32] # year 2005-2017 (excluding 2013)

# tidy data by group

group1 <- group1 %>% rbind.fill() %>% as.tibble %>% 
  select (YY,MM,DD,hh,ATMP,WTMP) %>% 
  replace_na(list(YY=99)) %>%
  filter(hh == 12) %>% 
  mutate(century = 19) %>% 
  unite(YYYY, century, YY, sep = "") %>% 
  mutate(hh = NULL) %>%  
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  unite(Date,YYYY,MM,DD,sep = "-")


group2 <- group2 %>% rbind.fill() %>% as.tibble %>% 
  select(YYYY,MM,DD,hh,ATMP,WTMP) %>% 
  filter(hh == 12) %>% 
  mutate(hh = NULL) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  unite(Date,YYYY,MM,DD,sep="-")


group3 <- group3 %>% rbind.fill() %>% as.tibble %>% 
  select(YYYY,YY,MM,DD,hh,mm,ATMP,WTMP) %>% 
  filter((hh == 11 & mm == 50) | (hh == 12 & mm ==0)) %>% 
  mutate(YYYY = ifelse(is.na(YY),YYYY,YY)) %>% 
  unite(Date,YYYY,MM,DD,sep="-") %>% 
  mutate(YY = NULL, hh = NULL, mm = NULL) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA))

tidy <- bind_rows(group1,group2,group3) # use this to plot
