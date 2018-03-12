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

# use the line below to read data from the website
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
  mutate(ATMP = replace(ATMP, ATMP == 99, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 99, NA)) %>% 
  unite(Date,YYYY,MM,DD,sep = "-")

group2 <- group2 %>% rbind.fill() %>% as.tibble %>% 
  select(YYYY,MM,DD,hh,ATMP,WTMP) %>% 
  filter(hh == 12) %>% 
  mutate(hh = NULL) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 99, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 99, NA)) %>% 
  unite(Date,YYYY,MM,DD,sep="-")


group3 <- group3 %>% rbind.fill() %>% as.tibble %>% 
  select(YYYY,YY,MM,DD,hh,mm,ATMP,WTMP) %>% 
  filter((hh == 11 & mm == 50) | (hh == 12 & mm ==0)) %>% 
  mutate(YYYY = ifelse(is.na(YY),YYYY,YY)) %>% 
  unite(Date,YYYY,MM,DD,sep="-") %>% 
  mutate(YY = NULL, hh = NULL, mm = NULL) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 99, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 99, NA))

tidy <- bind_rows(group1,group2,group3)

tidy$Date <- as.Date(tidy$Date)

rm(group1)
rm(group2)
rm(group3)
rm(ldf)

# if we were to use complete observations

ldf <- llply(list.files("./Data", pattern="*.rds", full.names=TRUE), readRDS)
names(ldf) <- c(1985:2012,2014:2017)

group1 <- ldf[1:15] # year 1985-1999
group2 <- ldf[16:20] # year 2000-2004
group3 <- ldf[21:32] # year 2005-2017 (excluding 2013)


group1 <- group1 %>% rbind.fill() %>% as.tibble %>% 
  select (YY,MM,DD,hh,ATMP,WTMP) %>% 
  replace_na(list(YY=99)) %>%
  mutate(century = 19) %>% 
  unite(YYYY, century, YY, sep = "") %>% 
  unite(Date,YYYY,MM,DD,sep = "-") %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 99, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 99, NA)) %>% 
  mutate(hh = NULL)


group2 <- group2 %>% rbind.fill() %>% as.tibble %>% 
  select(YYYY,MM,DD,hh,ATMP,WTMP) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 99, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 99, NA)) %>%
  mutate(hh = NULL) %>% 
  unite(Date,YYYY,MM,DD,sep="-")


group3 <- group3 %>% rbind.fill() %>% as.tibble %>% 
  select(YYYY,YY,MM,DD,hh,mm,ATMP,WTMP) %>% 
  mutate(YYYY = ifelse(is.na(YY),YYYY,YY)) %>% 
  unite(Date,YYYY,MM,DD,sep="-") %>% 
  mutate(YY = NULL, hh = NULL, mm = NULL) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 999, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 999, NA)) %>% 
  mutate(ATMP = replace(ATMP, ATMP == 99, NA)) %>% 
  mutate(WTMP = replace(WTMP, WTMP == 99, NA)) 

tidy.com <- bind_rows(group1,group2,group3) # contains complete observations

tidy.com$Date <- as.Date(tidy.com$Date)

rm(group1)
rm(group2)
rm(group3)
rm(ldf)

# save workspace as RData file
# save.image(file="./Data/part1_tidydata.RData")

# directly load this file
load("./Data/part1_tidydata.RData") 

# time series plot

plot.ATMP <- ggplot(tidy,aes(Date, ATMP)) + geom_line() +
  ylab('Air Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
  theme(axis.text.x=element_text(angle=65, hjust=1))

plot.WTMP <- ggplot(tidy,aes(Date, WTMP)) + geom_line() +
  ylab('Sea Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
  theme(axis.text.x=element_text(angle=65, hjust=1))

plot.mixed <- ggplot(tidy, aes(Date)) + 
  geom_line(aes(y = ATMP, color = "Air Temperature")) + 
  geom_line(aes(y = WTMP, color = "Sea Temperature")) +
  ylab('Temperature') + scale_x_date(date_breaks = '1 year',date_labels = '%b %y') +
  theme(axis.text.x=element_text(angle=65, hjust=1))

corr <- cor(tidy$ATMP,tidy$WTMP,use = 'complete.obs')


# for shiny dashboard

tidy.shiny <- tidy %>%
  separate(Date, into=c("Year","Month","Day"),sep='-') %>%
  mutate(Date=str_c(Year,Month,Day,sep="-"))
tidy.shiny$Date <- as.Date(tidy.shiny$Date)

# Others: tests

# has the mean temperature changed for past 30 years?
# use one way ANOVA test to see if means have changed for air and sea temperatures (no restrictions on equal variance)

tidy$Year <- as.factor(format(as.Date(tidy$Date),"%Y"))

oneway.test(ATMP ~ Year, data = tidy) # significant change in air temperature
oneway.test(WTMP ~ Year, data = tidy) # significant change in sea temperature

# issue with sampling method

tidy.com$Year <- as.factor(format(as.Date(tidy.com$Date), "%Y"))

oneway.test(ATMP ~ Year, data = tidy.com) # significant change in air temperature
oneway.test(WTMP ~ Year, data = tidy.com) # significant change in sea temperature

