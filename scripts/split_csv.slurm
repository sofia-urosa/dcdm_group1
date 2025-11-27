#!/bin/bash
#SBATCH --partition=cpu
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --array=0-63
#SBATCH --job-name=csv_merge
#SBATCH --output=%x_%A_%a.out
#SBATCH --error=%x_%A_%a.err

# Accessing thousands of individual files is slow because 
# the Ceph file system must communicate with the Metadata Server (MDS) daemon
# on each I/O operation. 
# When using TAR you perform only ONE set of metadata operations: Open, Stat, Close. 
# Metadata for the individual files inside a TAR is handled seq. by the local TAR
# utility = skip remote MDS operations!

# In essence, the system is better at handling a few BIG files (few large network 
# transfers) compared to thousands of small ones (reading N files requires at least 
# N network requests for the data, plus N metadata requests to the MDS)

# also, we move the operations into /tmp, which uses xfs. xfs is temporary storage area 
# on the comp node that is designed for high-speed, short-term data processing

# this makes the process highly scalable!

# Stop if errors are found:
set -euo pipefail

# Project root: set BEFORE submitting
# (export PRJ_DIR=/scratch/.../Group1)

PRJ_DIR=${PRJ_DIR:? "Error: PRJ_DIR not set"}

INPUT_DIR="$PRJ_DIR/originals/data"
LIST_DIR="$PRJ_DIR/lists"
OUT_DIR="$PRJ_DIR/outputs"
LOG_DIR="$PRJ_DIR/outputs/logs"
SHARD_DIR="$PRJ_DIR/outputs/csv_shards"
HEADER="$PRJ_DIR/metadata/keys.txt"

# make sure directories for logs and shards exist, othwerise create
mkdir -p "$LOG_DIR" "$SHARD_DIR"

LISTFILE=$(printf "list.%02d" $SLURM_ARRAY_TASK_ID)
LISTPATH="$LIST_DIR/$LISTFILE"

#list.%02d

# Sanity checks

echo "Using list: $LISTPATH"
echo "SLURM_TMPDIR: $SLURM_TMPDIR"

echo "LISTPATH: $LISTPATH"
echo "INPUT_DIR: $INPUT_DIR"
echo "HEADER: $HEADER"
[ -d "$INPUT_DIR" ] || { echo "INPUT_DIR missing"; exit 1; }
[ -f "$LISTPATH" ]  || { echo "LISTPATH missing"; exit 1; }
[ -s "$LISTPATH" ]  || { echo "LISTPATH empty"; exit 1; }
[ -f "$HEADER" ]    || { echo "HEADER missing"; exit 1; }

head -n 5 "$LISTPATH"


TAR="$SLURM_TMPDIR/shard_${SLURM_ARRAY_TASK_ID}.tar"
OUT="$SLURM_TMPDIR/shard_${SLURM_ARRAY_TASK_ID}.csv"

# tar all CSVs in list.XX

# Create a file (-cf) named by the path in $TAR. Before adding files, 
# change directory (-C) to $INPUT_DIR. The list of files will be read 
# from the file (-T) in $LISTPATH.

tar -cf "$TAR" -C "$INPUT_DIR" -T "$LISTPATH"

HEADER_CSV=$(paste -sd, "$HEADER")

echo "$HEADER_CSV" > "$OUT"

COUNT=0
STEP=500

# for every csv shard: untar > pass to awk > awk makes them wide
tar -tf "$TAR" | while read -r F; do
    COUNT=$((COUNT + 1))

    if (( COUNT % STEP == 0 )); then
        echo "[$SLURM_ARRAY_TASK_ID] Processed $COUNT files..."
    fi
    
#Extract file $F, pipe into awk. fname="$F" ' means they are comma separated
    tar -xf "$TAR" -O "$F" \
      | awk -F, -v header="$HEADER" -v fname="$F" '
            BEGIN {
            #case insensitive
                IGNORECASE=1 
                n=0
            #read keys (header col)
                while ((getline k < header) > 0) {
                    keys[++n] = k
                }
                close(header)
            }
            {
            # NF = Number of Fields
            # if NF = 1 then key is missing value (or vice versa), so awk will fail
            # its just a protective measure
                if (NF >= 2)
                
            #$1 = key, $2 = value. Make an array where value is assigned to key (lowercase)
                    m[tolower($1)] = $2
            }
            END {
            #Loop through required keys, printing value from 'm'
                for (i=1; i<n; i++) {
                    printf "%s,", m[keys[i]]
                }
                printf "%s\n", m[keys[n]]
            }
            #into our out file:
        ' >> "$OUT"

done

echo "[$SLURM_ARRAY_TASK_ID] Finished. Total files: $COUNT"

mv "$OUT" "$SHARD_DIR/"

mv $PRJ_DIR/*err $PRJ_DIR/*out "$LOG_DIR"
