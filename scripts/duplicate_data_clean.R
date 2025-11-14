# ==========================================
# Remove fully duplicated rows based on:
# 1. pvalue must be identical
# 2. all other columns must also be identical， except the analysis_id and filename
# ==========================================

library(dplyr)

# --- read the CSV file ---
df <- read.csv("clean_data.csv", stringsAsFactors = FALSE)
# Columns used for second-level duplicate checking
other_cols <- c("gene_symbol","gene_accession_id","mouse_strain","mouse_life_stage", "parameter_id", "parameter_name")   # <-- modify as needed
# Check columns exist
missing <- setdiff(c("pvalue", other_cols), names(df))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse=", "))
# ---- Two-step duplicate removal ----
df_clean <- df %>%
  group_by(pvalue) %>%                              # Step 1: only compare rows with same pvalue
  distinct(across(all_of(other_cols)),              # Step 2: inside same pvalue, remove duplicates on selected columns
           .keep_all = TRUE) %>%
  ungroup()

# Diagnostics
cat("Original rows:", nrow(df), "\n")
cat("Rows after cleaning:", nrow(df_clean), "\n")
cat("Rows removed:", nrow(df) - nrow(df_clean), "\n")

# Save output
write.csv(df_clean, "clean_data_duplicate.csv", row.names = FALSE)
cat("Saved to cleaned_by_pvalue_and_columns.csv\n")


