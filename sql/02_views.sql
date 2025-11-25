CREATE DATABASE IF NOT EXISTS database1;
USE database1;

DROP VIEW IF EXISTS pvalue_log10;
DROP VIEW IF EXISTS pvalue_collapsed;

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