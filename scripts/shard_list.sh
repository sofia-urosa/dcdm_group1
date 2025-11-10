cd /scratch/grp/msc_appbio/DCDM/Group1/originals/data

find . -type f -name "*.csv" \
  | split -n r/64 -d - /scratch/grp/msc_appbio/DCDM/Group1/lists/list.
