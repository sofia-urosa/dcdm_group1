#!/bin/bash

set -euo pipefail
PRJ_DIR=${PRJ_DIR:? "Error: PRJ_DIR not set"}

# find al .csv files in ./originals/data:

# Project root: set BEFORE submitting

PRJ_DIR=${PRJ_DIR:? "Error: PRJ_DIR not set"}

cd "$PRJ_DIR/originals/data"

find . -type f -name "*.csv" \
  | split -n r/64 -d - "$PRJ_DIR/lists/list."
# split them into 64, name them list.XX