cd /scratch/grp/msc_appbio/DCDM/Group1/outputs

# pick header from shard_00
head -n 1 shard_0.csv > merged_all.csv

# skip header and append
for f in shard_*.csv; do
    tail -n +2 "$f" >> merged_all.csv
done
