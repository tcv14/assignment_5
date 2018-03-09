# load required packages
library(tidyverse)
library(readxl)

veg <- read_xlsx("./Data/veg1.xlsx")
a <- apply(veg, 2, n_distinct)
b <- names(a[a>1])
veg.1 <- veg %>% 
  select(b) %>%
  dplyr::rename(Geo = `Geo Level`, State = `State ANSI`,
                Data = `Data Item`, Category = `Domain Category`) %>%
  separate(Category, into = c("Label", "Type"), sep=",")

# tidy restricted use chemicals
veg.chem <- veg.1 %>%
  filter(Label=="RESTRICTED USE CHEMICAL") %>%
  separate(Data, into=c("A","Class Desc"),sep=" - ") %>%
  separate(`Class Desc`, into=c("Class Desc","Production Practice","Unit Desc"),sep=",") %>%
  separate(`Production Practice`,into=c("Production Practice","Utilization Practice","Statistic Category"),sep=" / ") %>%
  select(-A) %>%
  separate(Domain,into=c("Domain","B"),sep=", ") %>%
  dplyr::rename(Type=`B`,Chemical=`Type`) %>%
  select(-Label)

ruchemicals <- veg.chem %>%
  select(Domain,Chemical) %>% 
  unique()
