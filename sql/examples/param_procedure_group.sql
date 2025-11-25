-- Mapping parameters to procedure and group

USE database1;

SELECT
    p.parameter_id,
    p.parameter_name,
    dp.procedure_name,
    pgm.group_name
FROM parameters p
LEFT JOIN parameter_procedure pp USING (parameter_id)
LEFT JOIN dim_procedure dp USING (procedure_id)
LEFT JOIN parameter_group_map pgm USING (parameter_id)
ORDER BY pgm.group_name, dp.procedure_name;
