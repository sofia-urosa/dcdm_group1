-- counts number of phenotypes below threshold and lists disease terms

USE database1;

SELECT
    a.gene_symbol,
    COUNT(CASE WHEN a.pvalue < 0.05 THEN 1 END) AS n_significant,
    GROUP_CONCAT(DISTINCT d.do_disease_name SEPARATOR ', ') AS diseases
FROM analysis a
LEFT JOIN gene_disease gd ON a.gene_accession_id = gd.gene_accession_id
LEFT JOIN disease d ON gd.do_disease_id = d.do_disease_id
WHERE a.gene_symbol IN ('Parp11','Plcb2','Prpf31','Pabpc4l')
GROUP BY 1
ORDER BY 2 DESC;