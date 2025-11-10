library(readr)
library(tidyr)
library(stringr) 
suppressPackageStartupMessages(library(dplyr))

#fcns
#this fcn will only remove whitespaces from character columns
trim_whitespace <- function(df) {
  df %>% mutate( #remove from start and end of string
    across(
      where(
        is.character
        ),
      function(x) stringr::str_trim(x, side = "both")
      )
    ) %>% 
    mutate( 
#make sure inner whitespaces are only single, not double
      across(
        where(is.character), 
        stringr::str_squish
        )
      )
}


std_na <- function(df) { 
#standardize n/a
  df %>%
    mutate(across(where(is.character), ~ {
      x <- stringr::str_trim(.x)       # trim
      x_lower <- tolower(x)            # normalize for comparison
      
      bad_vals <- c("", "na", "null")  # all invalid tokens
      x[x_lower %in% bad_vals] <- NA   # convert to real NA
      
      x
    }))
}

coerce_bool <- function(df, cols) { 
#to force columns to be true boolean
  df %>%
    mutate(across(all_of(cols), ~ case_when(
      tolower(as.character(.x)) %in% c("true","t","1","yes","y")  ~ TRUE,
      tolower(as.character(.x)) %in% c("false","f","0","no","n") ~ FALSE,
      TRUE ~ NA
    )))
}

#Load merged data
x <- read_csv("outputs/merged_all.csv")
print("Loaded CSV")

#And the rest of the files
params <- read.csv("/scratch/grp/msc_appbio/DCDM/Group1/metadata/IMPC_parameter_description.txt")
procedure <- read.csv("/scratch/grp/msc_appbio/DCDM/Group1/metadata/IMPC_procedure.txt")

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

#n/as overall 
#x_clean %>% summarise(across(everything(), ~sum(is.na(.))))

#clean and std misc files:

params_c <- params %>%
  trim_whitespace() %>%
  std_na() %>%
  distinct(impcParameterOrigId, parameterId, .keep_all = TRUE)

procedure_c <- procedure %>%
  trim_whitespace() %>%
  std_na() %>% coerce_bool("isMandatory") %>%
  distinct(name,description,isMandatory,impcParameterOrigId, .keep_all = TRUE)

#rename columns (makes querying easier later...)

params_std <- params_c %>%
  rename(
    parameter_id = parameterId,
    parameter_name = name,
    parameter_description = description,
    impc_parameter_orig_id = impcParameterOrigId
  )

procedure_std <- procedure_c %>%
  rename(
    procedure_name = name,
    procedure_description = description,
    is_mandatory = isMandatory,
    impc_parameter_orig_id = impcParameterOrigId
  )

