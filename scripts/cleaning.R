#Short variable prefix guide
#rn_ = renamed columns
#c_ = went through initial cleaning
#sop_compliant = has been checked against SOP requirements 

#packages needed...
#install.packages("stringdist")

library(dplyr)    # filter, mutate, distinct, joins
library(stringr)  # str_trim
library(tidyr)
library(stringdist)

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

if (is.null(opts$input_dir) || is.null(opts$output_dir) ) {
  stop("Missing required arguments: --input_dir and/or --output_dir", call. = FALSE)
}

DATA_DIR <- opts$input_dir
OUTPUT_DIR <- opts$output_dir

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

#logging

logfile <- file.path(OUTPUT_DIR, "/logs/cleaning_log.txt")
log_con <- file(logfile, open = "at")

log <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", timestamp, "] ", msg, "\n")
  cat(line)
  writeLines(line, con = log_con)
}

log("Starting cleaning script")
log(paste("Input directory:", DATA_DIR))
log(paste("Output directory:", OUTPUT_DIR))

#Load data

log("Loading metadata files...")
params <- read.csv(file.path(DATA_DIR, "/metadata/IMPC_parameter_description.txt"))
procedure <- read.csv(file.path(DATA_DIR, "/metadata/IMPC_procedure.txt"))
disease_info <- read.csv(
  file.path(DATA_DIR, "/metadata/Disease_information.txt"),
  sep = "\t", check.names = FALSE, stringsAsFactors = FALSE
)

log("Loading merged data...")
data <- read.csv(file.path(OUTPUT_DIR, "/merged_all.csv"))
sop <- read.csv(file.path(DATA_DIR, "/metadata/IMPC_SOP.csv"))

valid_mouse_strains <- c("C57BL", "B6J", "C3H", "129SV")

#fcn

trim_whitespace <- function(df) {
  df %>% mutate(
    across(where(is.character), function(x) stringr::str_trim(x, side = "both"))
  ) %>% 
    mutate(across(where(is.character), stringr::str_squish))
}

std_na <- function(df) { 
  df %>%
    mutate(across(where(is.character), ~ {
      x <- stringr::str_trim(.x)
      x_lower <- tolower(x)
      bad_vals <- c("", "na", "null")
      x[x_lower %in% bad_vals] <- NA
      x
    }))
}

coerce_bool <- function(df, cols) { 
  df %>%
    mutate(across(all_of(cols), ~ case_when(
      tolower(as.character(.x)) %in% c("true","t","1","yes","y")  ~ TRUE,
      tolower(as.character(.x)) %in% c("false","f","0","no","n") ~ FALSE,
      TRUE ~ NA
    )))
}

check_chr <- function(df, col, good, max_dist = 2) {
  
  x <- df[[col]]
  x_upper <- toupper(trimws(x))
  good_upper <- toupper(trimws(good))
  
  idx <- amatch(x_upper, good_upper, maxDist = max_dist)
  
  bad_raw <- x[is.na(idx)]
  corrected <- x[!is.na(idx) & !(x_upper %in% good_upper)]
  
  fixed <- ifelse(x_upper %in% good_upper, x, good[idx])
  df[[col]] <- fixed
  
  qc <- data.frame(
    column = col,
    original_value = c(bad_raw, corrected),
    status = c(
      rep("no_match, therefore NA", length(bad_raw)),
      rep("corrected", length(corrected))
    ),
    stringsAsFactors = FALSE
  )
  
  list(data = df, qc = qc)
}

validate_sop <- function(df_list,sop){
  if (is.data.frame(df_list)) df_list <- list(df = df_list)
  
  cleaned_list <- list()
  qc <- list()
  
  for (df_name in names(df_list)) {
    
    df <- df_list[[df_name]]
    all_bad_rows <- integer(0)
    
    missing_col <- setdiff(sop$dataField, names(df))
    
    if (length(missing_col) > 0){
      qc[[length(qc) + 1]] <- data.frame(
        origin = df_name,
        field = missing_col,
        issue = "Column missing from data frame",
        expected = "Column expected by SOP",
        value = "No action needed",
        stringsAsFactors = FALSE
      )
    }
    
    common_fields <- intersect(sop$dataField, names(df))
    if (length(common_fields) == 0) {
      cleaned_list[[df_name]] <- df
      next
    }
    
    sop_subset <- sop[sop$dataField %in% common_fields,]
    
    for (i in 1:nrow(sop_subset)){
      
      field <- sop_subset$dataField[i]
      dtype <- sop_subset$dataType[i]
      minV  <- sop_subset$minValue[i]
      maxV  <- sop_subset$maxValue[i]
      col <- df[[field]]
      
      valid_type <- 
        (dtype == "String"  && is.character(col)) ||
        (dtype == "Float"   && is.numeric(col))   ||
        (dtype == "Integer" && is.integer(col))
      
      if (!valid_type) {
        qc[[length(qc) + 1]] <- data.frame(
          origin = df_name,
          field = field,
          issue = "Invalid data type",
          expected = dtype,
          value = paste("Actual:", class(col)[1]),
          stringsAsFactors = FALSE
        )
        next
      }
      
      if (dtype == "String"){
        len <- nchar(col)
        bad_idx <- which(len < minV | len > maxV)
        
        if (length(bad_idx) > 0) {
          all_bad_rows <- c(all_bad_rows, bad_idx)
          qc[[length(qc) + 1]] <- data.frame(
            origin = df_name,
            field = field,
            issue = "String length out of range",
            value = col[bad_idx],
            expected = paste(minV, "to", maxV),
            stringsAsFactors = FALSE
          )
        }
      }
      
      if (dtype == "Float") {
        bad_idx <- which(col < minV | col > maxV) 
        
        if (length(bad_idx) > 0) {
          all_bad_rows <- c(all_bad_rows, bad_idx)
          qc[[length(qc) + 1]] <- data.frame(
            origin = df_name,
            field = field,
            issue = "Numeric value out of range",
            value = col[bad_idx],
            expected = paste(minV, "to", maxV),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    bad_unique <- unique(all_bad_rows)
    
    if (length(bad_unique) == 0) cleaned_df <- df
    else cleaned_df <- df[-bad_unique, , drop = FALSE]
    
    cleaned_list[[df_name]] <- cleaned_df
  }
  
  if (length(qc) == 0) {
    return(list(data = cleaned_list, qc = data.frame()))
  } else {
    return(list(data = cleaned_list, qc = do.call(rbind,qc)))
  }
}

#clean orthogonal files

log("Cleaning orthogonal files...")

rn_params <- params %>%
  rename(
    parameter_id = parameterId,
    parameter_name = name,
    parameter_description = description,
    impc_parameter_orig_id = impcParameterOrigId
  )

rn_procedure <- procedure %>%
  rename(
    procedure_name = name,
    procedure_description = description,
    is_mandatory = isMandatory,
    impc_parameter_orig_id = impcParameterOrigId
  )

rn_disease_info <- disease_info %>%
  rename(
    do_disease_id      = `DO Disease ID`,
    do_disease_name    = `DO Disease Name`,
    omim_id            = `OMIM IDs`,
    gene_accession_id  = `Mouse MGI ID`
  )

c_params <- rn_params %>% trim_whitespace() %>% std_na() %>%
  distinct(impc_parameter_orig_id, parameter_id, .keep_all = TRUE)

c_procedure <- rn_procedure %>% trim_whitespace() %>% std_na() %>%
  coerce_bool("is_mandatory") %>%
  distinct(procedure_name,procedure_description,is_mandatory,impc_parameter_orig_id, .keep_all = TRUE)

c_disease_info <- rn_disease_info %>%
  trim_whitespace() %>%
  separate_rows(omim_id, sep = "\\|") %>% 
  std_na() %>%
  distinct(do_disease_id,do_disease_name,omim_id,gene_accession_id, .keep_all = TRUE)

log("Running SOP validation for metadata...")
orth_sop_compliant = validate_sop(
  list(params = c_params, procedure = c_procedure, disease_info = c_disease_info),
  sop
)

sop_compliant_params = orth_sop_compliant$data$params
sop_compliant_procedure = orth_sop_compliant$data$procedure
sop_compliant_disease_info = orth_sop_compliant$data$disease_info
qc_orth_sop_compliant = orth_sop_compliant$qc

#clean data

log("Cleaning main data...")

c_data <- data %>%
  trim_whitespace() %>%
  std_na() %>%
  distinct() %>% 
  mutate(
    gene_accession_id = toupper(gene_accession_id),
    parameter_id = toupper(parameter_id),
    mouse_life_stage = str_to_sentence(str_to_lower(mouse_life_stage)),
    gene_symbol = str_to_sentence(str_to_lower(gene_symbol))
  )

qc_corrected_mouse_strain <- NULL
qc_sop_compliant <- NULL

log("Checkin SOP compliance...")

sop_compliant_data <- c_data %>%
  check_chr("mouse_strain", valid_mouse_strains, max_dist = 2) %>%
  { 
    qc_corrected_mouse_strain <<- .$qc
    df <- .$data
    result <- validate_sop(list(clean_data = df), sop)
    qc_sop_compliant <<- result$qc
    result$data$clean_data
  }

#export

log("Saving cleaned datasets...")

write.csv(sop_compliant_data, file.path(OUTPUT_DIR, "clean_data.csv"), row.names = FALSE)
write.csv(c_disease_info, file.path(OUTPUT_DIR, "clean_disease_info.csv"), row.names = FALSE)
write.csv(c_params, file.path(OUTPUT_DIR, "clean_params.csv"), row.names = FALSE)
write.csv(c_procedure, file.path(OUTPUT_DIR, "clean_procedure.csv"), row.names = FALSE)

#qc csv
log("Saving QC tables...")

if (!is.null(qc_corrected_mouse_strain))
  write.csv(qc_corrected_mouse_strain, file.path(OUTPUT_DIR, "qc_mouse_strain.csv"), row.names = FALSE)

if (!is.null(qc_sop_compliant))
  write.csv(qc_sop_compliant, file.path(OUTPUT_DIR, "qc_sop_data.csv"), row.names = FALSE)

if (nrow(qc_orth_sop_compliant) > 0)
  write.csv(qc_orth_sop_compliant, file.path(OUTPUT_DIR, "qc_sop_orthogonal.csv"), row.names = FALSE)

log("Done!")

close(log_con)
