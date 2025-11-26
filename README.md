# IMPC Data Cleaning and Data Management 
## 7BBG1003 Group 1

This repository includes all code used for the data-cleaning pipeline, database design and RShiny dashboard architecture requested by collaborators of IMPC. 

## Installation

Use the package manager [renv]([https://pip.pypa.io/en/stable/](https://rstudio.github.io/renv/)) to install the necessary R dependencies.

```R
install.packages("renv")
renv::init()
```

## Usage

Assuming `$PRJ_DIR` is the project's root directory and that the folder structure matches the layout described in this repository:

---

### Merging CSVs

Generate the shard lists:

```bash
$PRJ_DIR/scripts/shard_list.sh
```

Submit the SLURM array job:

```bash
sbatch $PRJ_DIR/scripts/split_csv.slurm
```
split_csv.slurm submits 64 jobs to SLURM and assumes your raw CSVs are located in:

```bash
$PRJ_DIR/originals/data
```

You _must_ wait for all 64 jobs to finish (usually a few seconds) before continuing.

Merge the generated shards:

```bash
$PRJ_DIR/scripts/merge_shards.sh
```

What each script does

`shard_list.sh` 	: Splits the list of CSVs in originals/data into 64 separate lists.

`split_csv.slurm` 	: For each list, it:

```
1.	tars the CSVs,					
2.	processes them using awk to convert key-value format into a wide table,
3.	writes out 64 CSV shards.
```

`merge_shards.sh` 	: Appends all 64 shards into a single file:

```
$PRJ_DIR/outputs/merged_all.csv
```

### Cleaning

Run:

```
$PRJ_DIR/scripts/run_cleaning.sh --input_dir=$PRJ_DIR --output_dir=$PRJ_DIR/outputs
```

This will run `cleaning.R` and `grouping_phenotypes.R`, which:

 1. Loads the metadata (parameter descriptions, procedure definitons, Disease Ontology mappings, and SOP constraints)

 2. Cleans and standardizes duplicates, missing values, and coerces booleans.

 3. Validates against the SOP:
    Each filed is checked against the IMPC SOP rules for range, type, and acceptable string-length
    Any invalid rows are removed and logged
 
 4. Applies fuzzy correction of catergorical fields

 5. Generates cleaned metadata tables
    
```
    clean_data.csv
    clean_disease_info.csv
    clean_params.csv
    clean_procedure.csv
```
 
 7. Exports cleaned dataset

```
 clean_data.csv
```
 
 9. Generates QC reports

 10. Creates the phenotype groupings.
    

## Folder structure

The repository follows this structure:

```bash
metadata/              		# Original metadata files (parameters, procedures, diseases, SOP)
raw/                   		# Raw IMPC files (not included in the repository)
outputs/               		# All cleaned and generated outputs
outputs/csv_shards/    		# CSV shards created during merge process
outputs/logs/          		# QC logs and SLURM logs
scripts/               		# Data cleaning, grouping, merging, and dashboard scripts
sql/                   		# SQL schema, views, dump files, and example queries
sql/examples/          		# Query examples for collaborators
```

The pipeline will not run correctly unless these directories exist in this layout.
	
	
























