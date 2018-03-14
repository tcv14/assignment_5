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
  dplyr::rename(Area = `Geo Level`, State = `State ANSI`,
                Data = `Data Item`, Category = `Domain Category`) %>%
  separate(Category, into = c("Label", "Type"), sep=",") %>%
  separate(Data, into=c("A","Class Desc"),sep=" - ") %>%
  separate(`Class Desc`, into=c("Class Desc","Production Practice","Unit Desc"),sep=",") %>%
  separate(`Production Practice`,into=c("Production Practice","Utilization Practice","Statistic Category"),sep=" / ") %>%
  separate(Domain,into=c("Domain","B"),sep=", ") %>%
  dplyr::rename(Type=`B`,Chemical=`Type`) %>%
  separate(Chemical, into=c("C","Active Ingrediant or Action Taken"),sep=": ") %>%
  separate(`Active Ingrediant or Action Taken`, into=c("D","Active Ingrediant or Action Taken","E"),sep=c(1,-2)) %>%
  separate(`Active Ingrediant or Action Taken`, into=c("Active Ingrediant or Action Taken","EPA Pesticide Chemical Code"),sep="=") %>%
  select(-A,-Label,-C,-D,-E)

# get restricted use chemicals
veg.chem <- veg.tidy %>%
  filter(Domain=="RESTRICTED USE CHEMICAL") %>%
  select(Commodity, Domain:`EPA Pesticide Chemical Code`) %>% 
  unique() %>%
  arrange(Type) %>%
  dplyr::rename(`Active Ingrediant`=`Active Ingrediant or Action Taken`)

# table for toxicity
toxicity <- tibble(
  `Toxicity Measurements (oral, for an experimental rat)` = c("20-150 mg/kg", "5620-8350 mg/kg", "10 mg/kg",
                              "380 - 651 mg/kg", "54-70 mg/kg", "5000 mg/kg",
                              "2000 mg/kg", "869 - 1271 mg/kg", "3129 mg/kg",
                              "458 mg/kg", "450 mg/kg", "144 mg/kg",
                              "12-48 mg/kg", "50-281 mg/kg", "50 mg/kg",
                              "430-4000 mg/kg", "1563 mg/kg", "86 mg/kg",
                              "2.75-720 mg/kg", "60-387 mg/kg", "1.9-12.5 mg/kg",
                              "66.7-70.6 mg/kg", "150-2000 mg/kg", ">4640 mg/kg",
                              "55 mg/kg", "16-21 mg/kg", "39.1-398 mg/kg", "115-165 mg/kg"
                              )
)

# information taken from www.fao.org, https://pubchem.ncbi.nlm.nih.gov, pmep.cce.cornell.edu, 
# https://www.bartlett.com, and https://sitem.herts.ac.uk/

# join veg.chem and toxicity tables together
veg.chem <- veg.chem %>% bind_cols(toxicity)
