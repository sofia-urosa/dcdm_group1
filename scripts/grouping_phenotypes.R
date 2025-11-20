suppressPackageStartupMessages({
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(magrittr)
})
args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (grepl("=", arg)) {
      tmp <- strsplit(arg, "=", fixed = TRUE)[[1]]
      k <- sub("^--", "", tmp[1])
      out[[k]] <- tmp[2]
    }
  }
  out
}

opts <- parse_args(args)

if (is.null(opts$input_dir) ) {
  stop("Missing required arguments: --input_dir and/or --output_dir", call. = FALSE)
}

DATA_DIR <- opts$input_dir

merge_data = read_csv(file.path(DATA_DIR, "outputs/clean_data.csv"))

# 62316 rows & 10 columns
#glimpse(merge_data)
#head(merge_data)
#class(merge_data)
#dim(merge_data)
#colnames(merge_data)
#view(merge_data)

# Lists all the distinct parameters within the csv
unique_param = unique(merge_data$parameter_name)
#length(unique(merge_data$parameter_name))
#head(unique((merge_data$parameter_name)))
#head(unique(merge_data$parameter_name), 149)
#view(unique_param)

# Frequency of each parameter

param_freq = merge_data %>%
  count(parameter_name, sort = TRUE)

#print(param_freq, n = 149)

#view(param_freq)

# Remove any duplicates
sum(duplicated(merge_data))

# Finding common words within parameter names

common_word = merge_data %>%
  mutate(parameter_name = tolower(parameter_name))%>%
  separate_rows(parameter_name, sep = " ")%>%
  count(parameter_name, sort = TRUE)
#view(common_word)




merge_data <- merge_data %>%
  mutate(parameter_group = case_when(
    
    # Metabolism group
    (str_detect(parameter_name, regex(
        "glucose|cholesterol|hdl-cholesterol|creatinine|alanine|urea|insulin|fructosamine|
         magnesium|albumin|protein|calcium|iron|fatty|phospholipids|phosphatase|fattyacids|
         triglycerides|phosphorus", ignore_case = TRUE)) |
        str_starts(parameter_id, "IMPC_IPG") |
        str_starts(parameter_id, "IMPC_INS")
    ) ~ "Metabolism",
    
    # Weight group
    (str_detect(parameter_name, regex(
        "Bone|weight|mass|lean|lean/body|fat|\\bfat\\|fat/body|composition",
        ignore_case = TRUE)) |
        str_starts(parameter_id, "IMPC_DXA") |
        str_starts(parameter_id, "IMPC_HWT") |
        str_starts(parameter_id, "IMPC_OWT")
    ) ~ "Weight",
    
    # Images group
    (str_detect(parameter_name, regex(
        "morphology|retina|retinal|eye|optic|lens|vitreous|
         vessels|hyaloid|pattern|shape|thickness|color|texture|appearance",
        ignore_case = TRUE)) |
        str_starts(parameter_id, "CCP_XRY") |
        str_starts(parameter_id, "JAX_XRY") |
        str_starts(parameter_id, "TCP_XRY") |
        str_starts(parameter_id, "IMPC_EYE")
    ) ~ "Images",
    
    # Brain group
    (str_detect(parameter_name, regex(
        "brain|reflex|response|sleep|balance|prepulse|hemorrhage|ABR",
        ignore_case = TRUE)) |
        str_starts(parameter_id, "IMPC_ABR")
    ) ~ "Brain",
    
    # Haematology group
    (str_detect(parameter_name, regex(
        "vessels|vessel|platelets|hematological|blood|leukocytes|white blood|bilirubin|
         eosinophil|basophil|neutrophil|platelet|lymphocytes|CD8+|haemoglobin|hemoglobin",
        ignore_case = TRUE)) |
        str_starts(parameter_id, "IMPC_HEM") |
        str_starts(parameter_id, "IMPC_CBC")
    ) ~ "Haematology",
    
    # Motor function group
    (str_detect(parameter_name, regex(
        "grip|strength|locomotor|motor|movement|gait",
        ignore_case = TRUE)) |
        str_starts(parameter_id, "IMPC_GRS") |
        str_starts(parameter_id, "HMGULA_GRS")
    ) ~ "Motor function",
    
    TRUE ~ "Other"
  ))



#colnames(merge_data)
#table(merge_data$parameter_group)
#view(merge_data)


# Remember there is a lower case parameter ID which needs to be capitalized
# 451 cells in the parameter_id column were lowercase 

merge_data$parameter_id = toupper(merge_data$parameter_id)
#view(merge_data$parameter_id)

# Gene accession ID needs to be capitalized 

merge_data$gene_accession_id = toupper(merge_data$gene_accession_id)
#view(merge_data$gene_accession_id)
#table(merge_data$gene_accession_id)

# Check if any value in "gene_accession_id" & "parameter_id" contains any lowercase letters

any_lowercase_present = any(grepl("[a-z]", merge_data$gene_accession_id)) |
  any(grepl("[a-z]", merge_data$parameter_id))

if (any_lowercase_present) {
  print("At least one lowercase letter is present in gene_accession_id or parameter_id or Mouse_life_stage")
} else {
  print("No lowercase letters are present in either column")
}


#view(merge_data)

#tail(merge_data)
#head(merge_data)


# Detecting unique mouse strains 

unique_mouse_strain = unique(merge_data$mouse_strain)
#view(unique_mouse_strain)

# Detecting unique gene symbols

unique_gene_symbol = unique(merge_data$gene_symbol)
#view(unique_gene_symbol)

# Listing unique parameter_id
unique_parameter_id = unique(merge_data$parameter_id)
#length(unique_parameter_id)
unique_parameter_id_df = data.frame(parameter_id = unique_parameter_id)
#view(unique_parameter_id_df)


#parameter_group

parameter_group <- merge_data %>%
  distinct(parameter_group) %>%
  rename(group_name = parameter_group)

write.csv(parameter_group, file.path(DATA_DIR, "/outputs/parameter_group.csv"), row.names = FALSE)
print("Created parameter_group.csv")

#parameter_group_map
parameter_group_map <- merge_data %>% 
  distinct(parameter_id, parameter_group) %>% 
  rename(group_name = parameter_group)

write.csv(parameter_group_map, file.path(DATA_DIR, "/outputs/parameter_group_map.csv"), row.names = FALSE)
print("Created parameter_group_map.csv")
