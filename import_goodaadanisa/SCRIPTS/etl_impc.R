#!/usr/bin/env Rscript

############################################################
# ETL script: clean_data.csv -> MySQL impc_db
#
# Responsibilities:
# - Read canonical clean_data.csv
# - Validate required columns
# - Enforce p-value rule: 0 < p <= 1, else set to NULL and flag
# - Deduplicate rows per:
#     (gene_accession_id, parameter_id, mouse_strain,
#      mouse_life_stage, analysis_id)
#   keeping the row with the lowest valid p-value
# - Load dimensions, then facts, then archive
# - Append one row to ingest_log with counts + SHA256 checksum
############################################################

suppressPackageStartupMessages({
  library(optparse)
  library(DBI)
  library(RMariaDB)  # use RMySQL instead if your setup prefers it
  library(dplyr)
  library(readr)
  library(digest)
})

option_list <- list(
  make_option(c("--csv"),  type = "character", help = "Path to clean_data.csv (required)"),
  make_option(c("--db"),   type = "character", default = "impc_db", help = "Database name [default %default]"),
  make_option(c("--host"), type = "character", default = "127.0.0.1", help = "MySQL host [default %default]"),
  make_option(c("--user"), type = "character", default = "root", help = "MySQL user [default %default]"),
  make_option(c("--pass"), type = "character", default = "", help = "MySQL password [default empty]"),
  make_option(c("--port"), type = "integer",   default = 3306, help = "MySQL port [default %default]"),
  # Optional metadata (you can wire these later if you want)
  make_option(c("--params"),    type = "character", default = NULL, help = "Optional clean_params.csv"),
  make_option(c("--procedures"),type = "character", default = NULL, help = "Optional clean_procedure.csv"),
  make_option(c("--disease"),   type = "character", default = NULL, help = "Optional clean_disease_info.csv"),
  make_option(c("--groups"),    type = "character", default = NULL, help = "Optional parameter_group_map.csv")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$csv)) {
  stop("You must provide --csv pointing to clean_data.csv", call. = FALSE)
}

############################################################
# Connect to MySQL
############################################################

con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname   = opt$db,
  host     = opt$host,
  user     = opt$user,
  password = opt$pass,
  port     = opt$port
)

on.exit({
  try(dbDisconnect(con), silent = TRUE)
}, add = TRUE)

message("Connected to database: ", opt$db)

############################################################
# Read and validate main CSV
############################################################

message("Reading input CSV: ", opt$csv)
main_df <- read_csv(opt$csv, show_col_types = FALSE)

required_cols <- c(
  "gene_accession_id",
  "gene_symbol",
  "mouse_strain",
  "mouse_life_stage",
  "parameter_id",
  "parameter_name",
  "pvalue",
  "analysis_id",
  "filename"
)

missing <- setdiff(required_cols, names(main_df))
if (length(missing) > 0) {
  stop("Input CSV is missing required columns: ",
       paste(missing, collapse = ", "))
}

rows_in <- nrow(main_df)
message("Rows in input: ", rows_in)

############################################################
# P-value cleaning and flags
############################################################

main_df <- main_df %>%
  mutate(
    pvalue      = as.numeric(pvalue),
    invalid_p   = is.na(pvalue) | pvalue <= 0 | pvalue > 1,
    p_value_clean = ifelse(invalid_p, NA_real_, pvalue),
    cleaning_flag = ifelse(invalid_p, "pvalue_out_of_bounds", NA_character_),
    row_id      = row_number()  # for stable dedup tracking
  )

invalid_p_count <- sum(main_df$invalid_p, na.rm = TRUE)
message("Rows with invalid p-values: ", invalid_p_count)

############################################################
# Deduplication
#   Key: (gene_accession_id, parameter_id, mouse_strain,
#         mouse_life_stage, analysis_id)
#   Rule: keep the row with lowest valid p-value; if all invalid,
#         keep one invalid row (flagged), archive the rest.
############################################################

dedup_keys <- c(
  "gene_accession_id",
  "parameter_id",
  "mouse_strain",
  "mouse_life_stage",
  "analysis_id"
)

# Order by key, then valid p first (invalid_p = FALSE), then lowest p_value_clean
main_df_ordered <- main_df %>%
  arrange(
    across(all_of(dedup_keys)),
    invalid_p,              # FALSE (valid) before TRUE (invalid)
    p_value_clean           # smallest p first among valids
  )

kept <- main_df_ordered %>%
  group_by(across(all_of(dedup_keys))) %>%
  slice(1L) %>%
  ungroup()

archive <- main_df %>%
  filter(!(row_id %in% kept$row_id)) %>%
  mutate(
    cleaning_flag = ifelse(
      is.na(cleaning_flag),
      "duplicate_row",
      cleaning_flag
    )
  )

rows_loaded   <- nrow(kept)
rows_archived <- nrow(archive)

message("Rows kept for measurement: ", rows_loaded)
message("Rows archived (duplicates/extra): ", rows_archived)

############################################################
# Build dimension data frames (from kept rows only)
############################################################

# One row per gene_accession_id.
# If a gene appears multiple times with different symbols, we just take the first symbol.
dim_gene <- kept %>%
  group_by(gene_accession_id) %>%
  summarise(
    gene_symbol = dplyr::first(gene_symbol),
    .groups = "drop"
  )

# One row per parameter_id.
# If the same parameter_id appears with slightly different names, we keep the first.
dim_parameter <- kept %>%
  group_by(parameter_id) %>%
  summarise(
    parameter_name = dplyr::first(parameter_name),
    .groups = "drop"
  )

# Each strain / life stage appears only once.
dim_strain <- kept %>%
  distinct(mouse_strain)

# Normalise missing life stages to 'unknown' so they are never NULL in the DB.
dim_life_stage <- kept %>%
  mutate(
    mouse_life_stage = ifelse(
      is.na(mouse_life_stage) | mouse_life_stage == "",
      "unknown",
      mouse_life_stage
    )
  ) %>%
  distinct(mouse_life_stage)

# Each analysis_id / filename pair appears only once.
dim_analysis <- kept %>%
  distinct(analysis_id, filename)


############################################################
# Truncate existing fact/dim tables (full refresh)
############################################################

message("Truncating dimension and fact tables for a clean load...")

dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")

tables_to_truncate <- c(
  "measurement",
  "measurement_archive",
  "gene_disease",
  "parameter_group_map",
  "parameter_group",
  "gene",
  "parameter",
  "strain",
  "life_stage",
  "analysis"
)

existing <- dbListTables(con)

for (tbl in tables_to_truncate) {
  if (tolower(tbl) %in% tolower(existing)) {
    dbExecute(con, paste0("TRUNCATE TABLE `", tbl, "`"))
  }
}

dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1")

############################################################
# Load dimensions
############################################################

message("Loading dimension tables...")

# Load gene directly (we have already grouped by gene_accession_id)
dbWriteTable(con, "gene", dim_gene, append = TRUE, row.names = FALSE)

# Load parameter via a temporary table + INSERT IGNORE
# This protects us from any duplicate parameter_id values.
dbWriteTable(con, "parameter_temp", dim_parameter, overwrite = TRUE, row.names = FALSE)
dbExecute(con, "
  INSERT IGNORE INTO parameter (parameter_id, parameter_name)
  SELECT parameter_id, parameter_name
  FROM parameter_temp;
")
dbExecute(con, "DROP TABLE parameter_temp;")

# Load remaining small dimensions directly
dbWriteTable(con, "strain",     dim_strain,     append = TRUE, row.names = FALSE)
dbWriteTable(con, "life_stage", dim_life_stage, append = TRUE, row.names = FALSE)
dbWriteTable(con, "analysis",   dim_analysis,   append = TRUE, row.names = FALSE)

############################################################
# (Optional) Procedures, diseases, parameter groups
# These are left as optional extensions; you can wire them up
# to your metadata CSVs later by adjusting column names.
############################################################

if (!is.null(opt$procedures) && file.exists(opt$procedures)) {
  message("NOTE: procedures file provided (", opt$procedures, ").",
          " You can add code here to load into procedure_dim.")
}

if (!is.null(opt$params) && file.exists(opt$params)) {
  message("NOTE: params file provided (", opt$params, ").",
          " You can add code here to enrich the parameter table.")
}

if (!is.null(opt$disease) && file.exists(opt$disease)) {
  message("NOTE: disease file provided (", opt$disease, ").",
          " You can add code here to load disease and gene_disease.")
}

if (!is.null(opt$groups) && file.exists(opt$groups)) {
  message("NOTE: groups file provided (", opt$groups, ").",
          " You can add code here to load parameter_group and parameter_group_map.")
}

############################################################
# Load fact tables
############################################################

message("Loading measurement fact table...")

measurement_insert <- kept %>%
  mutate(
    mouse_life_stage = ifelse(
      is.na(mouse_life_stage) | mouse_life_stage == "",
      "unknown",
      mouse_life_stage
    )
  ) %>%
  transmute(
    gene_accession_id,
    parameter_id,
    mouse_strain,
    mouse_life_stage,
    analysis_id,
    p_value       = p_value_clean,
    source_file   = filename,
    cleaning_flag = cleaning_flag
  )


dbWriteTable(con, "measurement", measurement_insert,
             append = TRUE, row.names = FALSE)

message("Loading measurement_archive...")

measurement_archive_insert <- archive %>%
  mutate(
    mouse_life_stage = ifelse(
      is.na(mouse_life_stage) | mouse_life_stage == "",
      "unknown",
      mouse_life_stage
    )
  ) %>%
  transmute(
    gene_accession_id,
    parameter_id,
    mouse_strain,
    mouse_life_stage,
    analysis_id,
    p_value       = p_value_clean,
    source_file   = filename,
    cleaning_flag = cleaning_flag
  )


if (nrow(measurement_archive_insert) > 0) {
  dbWriteTable(con, "measurement_archive", measurement_archive_insert,
               append = TRUE, row.names = FALSE)
}

############################################################
# Ingest log
############################################################

sha <- digest(file = opt$csv, algo = "sha256")

log_sql <- "
  INSERT INTO ingest_log
    (run_at, filename, sha256, rows_in, rows_loaded, rows_archived, invalid_p)
  VALUES
    (NOW(), ?, ?, ?, ?, ?, ?)
"

dbExecute(
  con,
  log_sql,
  params = list(
    basename(opt$csv),
    sha,
    rows_in,
    rows_loaded,
    rows_archived,
    invalid_p_count
  )
)

message("ETL complete. Ingest log row written.")
