read.csv("merged_all.csv")
install.packages("data.table")
library("data.table")
df <- fread("merged_all.csv")
df$pvalue <- as.numeric(gsub(",", ".", trimws(df$pvalue)))
str(df$pvalve)
class(df$pvalue)
summary(df$pvalue)
head(df$pvalue,20)


df_pass <- df[!is.na(pvalue) & pvalue > pvalue_Min & pvalue < pvalue_Max]                              
df_fail <- df[!(row.names(df) %in% row.names(df_pass))]

fwrite(df_pass, "QC_Pvaluepass_data.csv")
fwrite(df_fail, "QC_Pvaluefail_data.csv")

nrow(df_pass) + nrow(df_fail) == nrow(df)