#!/bin/bash

set -euo pipefail
cd "$PRJ_DIR/outputs/csv_shards"

# pick header from shard_00
head -n 1 shard_0.csv > "$PRJ_DIR/outputs/merged_all.csv"

# skip header and append
for f in shard_*.csv; do
    tail -n +2 "$f" >> "$PRJ_DIR/outputs/merged_all.csv"
done
