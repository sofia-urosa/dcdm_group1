-- Mapping parameters to procedure and group

USE database1;


SELECT DISTINCT
    a.gene_symbol,
    p.parameter_name,
    dp.procedure_name,
    pgm.group_name
FROM analysis a 
LEFT JOIN parameters p ON a.parameter_id = p.parameter_id
LEFT JOIN parameter_procedure pp ON a.parameter_id = pp.parameter_id
LEFT JOIN dim_procedure dp ON pp.procedure_id = dp.procedure_id
LEFT JOIN parameter_group_map pgm ON a.parameter_id = pgm.parameter_id
WHERE a.gene_symbol in ('Parp11','Plcb2','Prpf31','Pabpc4l') 
;