# ===========================================
# upload_raw_csv.R
# Minimal ETL script: Upload a raw CSV into MySQL
# ===========================================

library(DBI)
library(RMariaDB)

# ---- 1. Set configuration ----

# Path to the raw CSV file you want to upload
csv_file <- "clean_data.csv"

# MySQL table name where the data should be inserted
table_name <- "experiment_value"

# Connect to MySQL database
# Adjust the credentials according to your environment
con <- dbConnect(
  RMariaDB::MariaDB(),
  user = "root",
  password = "yourpassword",
  host = "localhost",
  dbname = "impc_database",
  port = 3306
)

# ---- 2. Load CSV into R ----
# 'stringsAsFactors = FALSE' prevents automatic conversion to factors
cat("Reading CSV file...\n")
df <- read.csv(csv_file, header = TRUE, stringsAsFactors = FALSE)

# ---- 3. Upload dataframe to MySQL ----
# 'append = TRUE' means new rows are added without deleting the table
# Set overwrite = TRUE if you want to replace the entire table
cat("Uploading data to MySQL table:", table_name, "...\n")
dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)

cat("Upload complete! CSV successfully imported.\n")

# ---- 4. Close connection ----
dbDisconnect(con)
