# load required packages
library(tidyverse)
library(readxl)

# read in data
veg <- read_xlsx("./Data/veg1.xlsx")

# tidy veg data
a <- apply(veg, 2, n_distinct)
b <- names(a[a>1])
veg.tidy <- veg %>% 
  select(b) %>%
  dplyr::rename(Geography = `Geo Level`, State = `State ANSI`,
                Data = `Data Item`, Category = `Domain Category`) %>%
  separate(Category, into = c("Label", "Type"), sep=",") %>%
  separate(Data, into=c("A","Class Desc"),sep=" - ") %>%
  separate(`Class Desc`, into=c("Class Desc","Production Practice","Unit Desc"),sep=",") %>%
  separate(`Production Practice`,into=c("Production Practice","Utilization Practice","Statistic Category"),sep=" / ") %>%
  separate(Domain,into=c("Domain","B"),sep=", ") %>%
  dplyr::rename(Type=`B`,Chemical=`Type`) %>%
  separate(Chemical, into=c("C","Active Ingrediant or Action Taken"),sep=": ") %>%
  separate("Active Ingrediant or Action Taken", into=c("D","Active Ingrediant or Action Taken","E"),sep=c(1,-2)) %>%
  separate("Active Ingrediant or Action Taken", into=c("Active Ingrediant or Action Taken","EPA Pesticide Chemical Code"),sep="=") %>%
  select(-A,-Label,-C,-D,-E)

# get restricted use chemicals
veg.chem <- veg.tidy %>%
  filter(Domain=="RESTRICTED USE CHEMICAL") %>%
  select(Domain:`EPA Pesticide Chemical Code`) %>% 
  unique() %>%
  arrange(Type)

# table for toxicity
toxicity <- tibble(
  `Toxicity Measurements` = c("20-150 mg/kg", "5620-8350 mg/kg", "10 mg/kg",
                              "380 - 651 mg/kg", "54-70 mg/kg", "5000 mg/kg",
                              "2000 mg/kg", "869 - 1271 mg/kg")
)

# information taken from www.fao.org, https://pubchem.ncbi.nlm.nih.gov, and pmep.cce.cornell.edu

# join veg.chem and toxicity tables together
