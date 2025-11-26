#!/bin/bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <input_dir> <output_dir>"
    exit 1
fi

INPUT_DIR="$1"
OUTPUT_DIR="$2"


# make sure renv is installed + restore env
# this assumes user is running the code on the project folder. It will fail if renv folder is not found
echo ">> Checking renv installation..."
Rscript - <<'EOF'
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos="https://cran.r-project.org")
}
EOF

echo ">> Restoring renv environment..."
Rscript -e "renv::restore(prompt = FALSE)"


# run scripts

echo ">> Running cleaning.R..."
Rscript ./scripts/cleaning.R \
  --input_dir="${INPUT_DIR}" \
  --output_dir="${OUTPUT_DIR}"

echo ">> Running grouping_phenotypes.R..."
Rscript ./scripts/grouping_phenotypes.R \
  --input_dir="${INPUT_DIR}"

echo ">> Data cleaning and parameter grouping done!"
