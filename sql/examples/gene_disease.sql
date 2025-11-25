-- maps gene symbols with disease ontology terms (DO)

USE database1;

SELECT 
    a.gene_symbol,
    a.gene_accession_id,
    d.do_disease_id,
    d.do_disease_name,
    d.omim_id
FROM analysis a
JOIN gene_disease gd ON a.gene_accession_id = gd.gene_accession_id
JOIN disease d ON gd.do_disease_id = d.do_disease_id
WHERE a.gene_symbol in ('Parp11','Plcb2','Prpf31','Pabpc4l') 
GROUP BY 1,2,3,4,5
ORDER BY 4;