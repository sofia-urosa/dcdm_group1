#!/bin/bash
#SBATCH --partition=cpu
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --array=0-63
#SBATCH --job-name=csv_merge
#SBATCH --output=/scratch/grp/msc_appbio/DCDM/Group1/outputs/csv_merge_%A_%a.out
#SBATCH --error=/scratch/grp/msc_appbio/DCDM/Group1/outputs/csv_merge_%A_%a.err

set -euo pipefail

INPUT_DIR=/scratch/grp/msc_appbio/DCDM/Group1/originals/data
LIST_DIR=/scratch/grp/msc_appbio/DCDM/Group1/lists
OUT_DIR=/scratch/grp/msc_appbio/DCDM/Group1/outputs
HEADER=/scratch/grp/msc_appbio/DCDM/Group1/metadata/keys.txt

mkdir -p "$OUT_DIR"

LISTFILE=$(printf "list.%02d" $SLURM_ARRAY_TASK_ID)
LISTPATH="$LIST_DIR/$LISTFILE"

#list.%02d

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

tar -cf "$TAR" -C "$INPUT_DIR" -T "$LISTPATH"

HEADER_CSV=$(paste -sd, "$HEADER")
OUT="$SLURM_TMPDIR/shard_${SLURM_ARRAY_TASK_ID}.csv"

echo "$HEADER_CSV" > "$OUT"

COUNT=0
STEP=500

tar -tf "$TAR" | while read -r F; do
    COUNT=$((COUNT + 1))

    if (( COUNT % STEP == 0 )); then
        echo "[$SLURM_ARRAY_TASK_ID] Processed $COUNT files..."
    fi

    tar -xf "$TAR" -O "$F" \
      | awk -F, -v header="$HEADER" -v fname="$F" '
            BEGIN {
                IGNORECASE=1 
                n=0
                while ((getline k < header) > 0) {
                    keys[++n] = k
                }
                close(header)
            }
            {
                if (NF >= 2)
                    m[tolower($1)] = $2
            }
            END {
                for (i=1; i<n; i++) {
                    printf "%s,", m[keys[i]]
                }
                printf "%s\n", fname
            }
        ' >> "$OUT"

done

echo "[$SLURM_ARRAY_TASK_ID] Finished. Total files: $COUNT"

mv "$OUT" "$OUT_DIR/"
