library(readr)
library(tidyr)
suppressPackageStartupMessages(library(dplyr))

#Load merged data
x <- read_csv("outputs/merged_all.csv")
print("Loaded CSV")

#Init Exp of data
# summary stats
library(skimr)
skim(x)

# dim
cat("Col number: \n")
nrow(x)
cat("Row number: \n")
ncol(x)

# dupes
cat("Dupes: \n")
x %>% filter(duplicated(.)) %>% nrow()


x %>% 
  filter(parameter_id == "IMPC_ABR_001_001")

#n/as overall 
x_clean %>% summarise(across(everything(), ~sum(is.na(.))))


