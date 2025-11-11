install.packages("data.table")
library("data.table")
# install packages

setwd('~/Desktop/Group1')
sop=read.table("IMPC_SOP.csv",sep =",",header=T)
head(sop)
allFields = sop[, 1]
head(allFields)
allMins = sop[, 3]
allMaxs = sop[, 4]
#find the range of data by sop
analysis_id = allFields[1]
analysis_id_Min = allMins[1]
analysis_id_Max = allMaxs[1]
gene_accession_id = allFields[2]
gene_accession_id_Min = allMins[2]
gene_accession_id_Max = allMaxs[2]
gene_symbol = allFields[3]
gene_symbol_Min = allMins[3]
gene_symbol_Max = allMaxs[3]
mouse_strain = allFields[4]
mouse_strain_Min = allMins[4]
mouse_strain_Max = allMaxs[4]
mouse_life_stage = allFields[5]
mouse_life_stage_Min = allMins[5]
mouse_life_stage_Max = allMaxs[5]
parameter_id = allFields[6]
parameter_id_Min = allMins[6]
parameter_id_Max = allMaxs[6]
parameter_name = allFields[7]
parameter_name_Min = allMins[7]
parameter_name_Max = allMaxs[7]
pvalue = allFields[8]
pvalue_Min = allMins[8]
pvalue_Max = allMaxs[8]
#defind the range of data

files <- list.files( "data",full.names =TRUE)

head(files)
counter = 0
for (i in files){
  counter = counter+1
  
  theFile <- read.csv(i) # load the data file
  theFileDF <- as.data.frame(theFile)
  
  print(i)
  if (counter == 10) # we do not want to load the whole dataset, this will break out of the loop.
  {
    break
  }
  
  headBandFile <- read.csv(i) # load file
  theField = theFileDF[, 1]
  allValues = theFileDF[, 2]
  file_P_V = theField[5]
  P_V = suppressWarnings(as.numeric(allValues[5]))
  
  if (is.na(P_V)) {
    print(paste(file_P_V, "skipped"))
    next               # skip this iteration to avoid NA
  }
  
  
  
  if ( P_V > pvalue_Min & pvalue_Max > P_V){
    print(paste(file_P_V, "passed QC"))
  }else{
    print(paste(file_P_V, "failed QC"))
  }
}

#run a loop to see if the range is working

read.csv("merged_all.csv")

df <- fread("merged_all.csv")
df$pvalue <- as.numeric(gsub(",", ".", trimws(df$pvalue)))
str(df$pvalve)
class(df$pvalue)
summary(df$pvalue)
head(df$pvalue,20)
#datat clean in merged file
df_pass <- df[!is.na(pvalue) & pvalue > pvalue_Min & pvalue < pvalue_Max]                              
df_fail <- df[!(row.names(df) %in% row.names(df_pass))]

fwrite(df_pass, "QC_Pvaluepass_data.csv")
fwrite(df_fail, "QC_Pvaluefail_data.csv")
# get the pass and fail data
nrow(df_pass) + nrow(df_fail) == nrow(df)  # to see if all data is tested 

library(dplyr)

read.csv("QC_Pvaluepass_data.csv")
df <- fread("QC_Pvaluepass_data.csv") # for large data mount

df_clean <- df %>%
  mutate(
    gene_accession_id = ifelse(nchar(gene_accession_id) >= 9 & nchar(gene_accession_id) <= 11,gene_accession_id, NA),
    gene_symbol = ifelse(nchar(gene_symbol) >= 1 & nchar(gene_symbol) <= 13, gene_symbol,NA),
    mouse_strain = ifelse(nchar(mouse_strain) >= 3 & nchar(mouse_strain) <= 5, mouse_strain,NA),
    mouse_life_stage = ifelse(nchar(mouse_life_stage) >= 4 & nchar(mouse_life_stage) <= 17, mouse_life_stage,NA),
    parameter_id = ifelse(nchar(parameter_id) >= 15 & nchar(parameter_id) <= 20, parameter_id,NA),
    parameter_name = ifelse(nchar(parameter_name) >= 2 & nchar(parameter_name) <= 74, parameter_name,NA),
    analysis_id = ifelse(nchar(analysis_id) == 15, analysis_id,NA)
  )
# test the rest to clean the data
fwrite(df_clean, "QC_data.csv")
#get the final cleaned data
