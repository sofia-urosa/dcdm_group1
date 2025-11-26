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

```R
--
```

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

## How to execute the pipeline 

The full data-cleaning workflow is carried out through the **cleaning.R** in the scripts/ directory which takes two arguments:

  • --input_dir: The directory containg the raw files and metadata
  • --ouput_dir: The directory where cleaned data, logs and QC reports will be written

What the script does

The **cleaning.R** script performs the following:

 1. Loads the metadata:
    • Parameter descriptions, procedure definitons, Disease Ontology mappings, and SOP constraints

 2. Cleaning and standardisatgion of the dataset:
    • Removal of duplicates
    • Standardised missing values such as "na", "N/A", and "NULL"
    • Case normalisation
    • Coercion of boolean values

 3. SOP validation:
    Each filed is checked against the IMPC SOP rules for range, type, and acceptable string-length
    Any invalid rows are removed and logged
 
 4. Fuzzy correction of catergorical fields

 5. Generated cleaned metadata tables
    • clean_data.csv
    • clean_disease_info.csv
    • clean_params.csv
    • clean_procedure.csv
 
 6. Exporting cleaned dataset
    clean_data.csv  is exported to outputs/
 
 7. Generating QC reports
    QC logs for SOP violations and corrected strains are exported to:
    • qc_sop_data.csv
    • qc_mouse_strain.csv
    • qc_sop_orthogonal.csv

## Outputs files
The following files will appear under outputs/:
    
	• clean_data.csv
	• clean_params.csv
	• clean_procedure.csv
	• clean_disease_info.csv
	• parameter_procedure.csv
	• dim_procedure.csv
	• disease.csv
	• gene_disease.csv
	• qc_mouse_strain.csv
	• qc_sop_data.csv
	• qc_sop_orthogonal.csv
	• logs/cleaning_log.txt
	
	
























