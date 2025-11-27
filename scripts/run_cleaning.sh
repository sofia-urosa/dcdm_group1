#!/bin/bash
set -euo pipefail

INPUT_DIR=""
OUTPUT_DIR=""

#old parse was causing issues... keeping R script the same
#this should match R
for arg in "$@"; do
  case "$arg" in
    --input_dir=*)
      INPUT_DIR="${arg#*=}"
      ;;
    --output_dir=*)
      OUTPUT_DIR="${arg#*=}"
      ;;
  esac
done

if [ -z "$INPUT_DIR" ] || [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 --input_dir=PATH --output_dir=PATH"
    exit 1
fi

#make sure im running this from prj folder
cd "$PRJ_DIR"

# make sure renv is installed + restore env
# this assumes user is running the code on the project folder. It will fail if renv folder is not found
echo ">> Checking renv installation..."
if ! Rscript -e "quit(status = !requireNamespace('renv', quietly = TRUE))"; then
  #quit, do not reinstall
  echo ">> Error: renv is not installed in the project folder."
  echo ">> Please install renv before running this script."
  exit 1

fi

echo ">> Restoring renv environment..."
Rscript -e "setwd('$PRJ_DIR'); renv::restore(prompt = FALSE)"
Rscript -e "setwd('$PRJ_DIR'); renv::update()"


# run scripts

echo ">> Running cleaning.R..."
Rscript ./scripts/cleaning.R \
  --input_dir="${INPUT_DIR}" \
  --output_dir="${OUTPUT_DIR}"

echo ">> Running grouping_phenotypes.R..."
Rscript ./scripts/grouping_phenotypes.R \
  --input_dir="${INPUT_DIR}"

echo ">> Done!"
