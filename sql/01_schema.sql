CREATE DATABASE IF NOT EXISTS impc;
USE impc;

DROP VIEW IF EXISTS pvalue_log10;
DROP VIEW IF EXISTS pvalue_collapsed;

-- Text-book 3NF says splitting gene, strain and life_stage into
-- their own tables is “more correct”, but for our dataset it added
-- complexity without giving us anything useful back.
-- These fields have tiny cardinality and don’t change over time,
-- so normalising them only creates more joins and slows queries.

-- I. Parameters:
-- one row per distinct IMPC parameter

CREATE TABLE IF NOT EXISTS `parameters` (
  parameter_id   	          VARCHAR(64) PRIMARY KEY,
  parameter_name 	          VARCHAR(255) NOT NULL,
  parameter_description 	 	TEXT,
  impc_orig_id   	          INT
) ENGINE=InnoDB;

-- II. Procedures
CREATE TABLE IF NOT EXISTS `dim_procedure` (
  `procedure_id`		          VARCHAR(64) PRIMARY KEY,
  `procedure_name`		        VARCHAR(255),
  `procedure_description`			TEXT,
  is_mandatory                BOOLEAN,
) ENGINE=InnoDB;

-- III. Parameter <-> procedure (many-to-many)
CREATE TABLE parameter_procedure(
	parameter_id 	VARCHAR(64),
	procedure_id 	VARCHAR(64),
PRIMARY KEY (parameter_id, procedure_id),
FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id),
FOREIGN KEY (procedure_id) REFERENCES dim_procedure(procedure_id)
) ENGINE=InnoDB;

-- IV. Diseases
CREATE TABLE IF NOT EXISTS `disease` (
  do_disease_id     VARCHAR(32) PRIMARY KEY,
  do_disease_name	VARCHAR(255),
  omim_id			VARCHAR(32)
) ENGINE=InnoDB;

-- V. Gene <-> disease map (many-to-many)
CREATE TABLE IF NOT EXISTS `gene_disease` (
  `gene_accession_id` VARCHAR(32) NOT NULL,
  `do_disease_id`     VARCHAR(32) NOT NULL,
  PRIMARY KEY (gene_accession_id, do_disease_id),
   FOREIGN KEY (do_disease_id) REFERENCES disease(do_disease_id)
) ENGINE=InnoDB;

-- VI. Parameter Groups
CREATE TABLE IF NOT EXISTS `parameter_group` (
  group_name        VARCHAR(64) PRIMARY KEY
) ENGINE=InnoDB;

-- VII. Parameter <-> group (many-to-many)
CREATE TABLE IF NOT EXISTS `parameter_group_map` (
  `parameter_id`	VARCHAR(64),
  `group_name`      VARCHAR(64),
  PRIMARY KEY (`parameter_id`, `group_name`),
  FOREIGN KEY (`parameter_id`) REFERENCES `parameters`(`parameter_id`),
  FOREIGN KEY (`group_name`) REFERENCES `parameter_group`(`group_name`)
) ENGINE=InnoDB;

-- VIII. Fact table:
-- stores phenotyping results
CREATE TABLE IF NOT EXISTS `analysis` (
  analysis_id		 VARCHAR(64) PRIMARY KEY,
  gene_accession_id  VARCHAR(32),
  gene_symbol        VARCHAR(32),
  mouse_strain       VARCHAR(16),
  mouse_life_stage   VARCHAR(32),
  parameter_id       VARCHAR(64),
  pvalue             DECIMAL(10,7),
  FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id)
) ENGINE=InnoDB;

-- IX. View of min pvalue per gene,strain,stage,parameter. 
-- Helper for RShiny Figures
CREATE VIEW pvalue_collapsed AS 
SELECT 
gene_accession_id, 
gene_symbol, 
mouse_strain, 
mouse_life_stage, 
parameter_id, 
min(pvalue) AS lowest_pvalue
FROM analysis 
GROUP BY 1,2,3,4,5;

-- X. And -Log10(pvalue) to aid further with viz
CREATE VIEW pvalue_log10 AS 
SELECT 
  gene_accession_id, 
  gene_symbol, 
  mouse_strain, 
  mouse_life_stage, 
  parameter_id,
  CASE WHEN lowest_pvalue > 0 AND lowest_pvalue <= 1 
       THEN -LOG10(lowest_pvalue) 
       ELSE NULL 
  END AS neglog10p
FROM pvalue_collapsed;



