# IMPC Data Cleaning and Data Management 
## 7BBG1003 Group 1

This repository includes all code used for the data-cleaning pipeline, database design and RShiny dashboard architecture requested by collaborators of IMPC. 

## Installation

Use the package manager [rencv]([https://pip.pypa.io/en/stable/](https://rstudio.github.io/renv/)) to install the necessary R dependencies.

```R
install.packages("renv")
renv::init()
```

## Usage

```R
--
```

## Folder structure

This project assumes the following folder structure:

  • originals/ contains the original data. originals/data contains the key-value CSVs.  
	•	metadata/ contains all SOP files and metadata tables required by the pipeline.
	•	outputs/ is where all logs and generated outputs will be written.

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


    
    
    



