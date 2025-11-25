-- significant phenotypes of genes of interest

USE database1;

SELECT 
    a.gene_symbol,
    p.parameter_name,
    a.pvalue
FROM analysis a
JOIN parameters p ON a.parameter_id = p.parameter_id
WHERE a.gene_symbol in ('Parp11','Plcb2','Prpf31','Pabpc4l') AND a.pvalue < 0.05
GROUP BY 1,2,3
ORDER BY a.pvalue
;