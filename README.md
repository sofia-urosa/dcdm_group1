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

