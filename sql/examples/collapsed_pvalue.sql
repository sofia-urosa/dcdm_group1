-- most significant phenotype in list using rank()

USE database1;

SELECT x.*, p.parameter_name FROM (
SELECT
    gene_symbol,
    parameter_id,
    lowest_pvalue,
    ROW_NUMBER() OVER (PARTITION BY gene_symbol ORDER BY lowest_pvalue ASC) AS rn
FROM pvalue_collapsed
WHERE gene_symbol in ('Parp11','Plcb2','Prpf31','Pabpc4l') 
) x
LEFT JOIN parameters p on x.parameter_id = p.parameter_id
WHERE rn = 1;