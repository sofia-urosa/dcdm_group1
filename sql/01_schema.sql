# Create and select the IMPC database.
# Running this file sets up the schema.
CREATE DATABASE IF NOT EXISTS impc_db;
USE impc_db;

# Core dimension tables for normalized lookup values.
CREATE TABLE IF NOT EXISTS `gene` (
  `gene_accession_id` VARCHAR(64) PRIMARY KEY,
  `gene_symbol`       VARCHAR(128)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `strain` (
  `mouse_strain` VARCHAR(128) PRIMARY KEY
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `life_stage` (
  `mouse_life_stage` VARCHAR(64) PRIMARY KEY
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `analysis` (
  `analysis_id` VARCHAR(64) PRIMARY KEY,
  `filename`    VARCHAR(255)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `parameter` (
  `parameter_id`   VARCHAR(64) PRIMARY KEY,
  `parameter_name` VARCHAR(255) NOT NULL
) ENGINE=InnoDB;

# Optional metadata dimensions for procedures and diseases.
CREATE TABLE IF NOT EXISTS `procedure_dim` (
  `procedure_id` VARCHAR(64) PRIMARY KEY,
  `code`         VARCHAR(64),
  `name`         VARCHAR(255),
  `description`  TEXT
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `disease` (
  `disease_id` VARCHAR(64) PRIMARY KEY,
  `doid`       VARCHAR(64),
  `name`       VARCHAR(255)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `gene_disease` (
  `gene_accession_id` VARCHAR(64) NOT NULL,
  `disease_id`        VARCHAR(64) NOT NULL,
  `evidence`          VARCHAR(255),
  PRIMARY KEY (`gene_accession_id`, `disease_id`),
  FOREIGN KEY (`gene_accession_id`) REFERENCES `gene`(`gene_accession_id`),
  FOREIGN KEY (`disease_id`)        REFERENCES `disease`(`disease_id`)
) ENGINE=InnoDB;

# Parameter grouping tables for reporting and visualisation.
CREATE TABLE IF NOT EXISTS `parameter_group` (
  `group_id`    INT AUTO_INCREMENT PRIMARY KEY,
  `group_name`  VARCHAR(64) UNIQUE NOT NULL,
  `description` TEXT
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS `parameter_group_map` (
  `parameter_id` VARCHAR(64) NOT NULL,
  `group_id`     INT NOT NULL,
  PRIMARY KEY (`parameter_id`, `group_id`),
  FOREIGN KEY (`parameter_id`) REFERENCES `parameter`(`parameter_id`),
  FOREIGN KEY (`group_id`)     REFERENCES `parameter_group`(`group_id`)
) ENGINE=InnoDB;

# Main fact table storing one row per measurement with foreign key links.
CREATE TABLE IF NOT EXISTS `measurement` (
  `measurement_id`    BIGINT AUTO_INCREMENT PRIMARY KEY,
  `gene_accession_id` VARCHAR(64) NOT NULL,
  `parameter_id`      VARCHAR(64) NOT NULL,
  `mouse_strain`      VARCHAR(128) NOT NULL,
  `mouse_life_stage`  VARCHAR(64)  NOT NULL,
  `analysis_id`       VARCHAR(64)  NOT NULL,
  `p_value`           DECIMAL(8,7) NULL,
  `source_file`       VARCHAR(255),
  `cleaning_flag`     VARCHAR(64),
  CONSTRAINT `fk_m_gene`   FOREIGN KEY (`gene_accession_id`) REFERENCES `gene`(`gene_accession_id`),
  CONSTRAINT `fk_m_param`  FOREIGN KEY (`parameter_id`)      REFERENCES `parameter`(`parameter_id`),
  CONSTRAINT `fk_m_strain` FOREIGN KEY (`mouse_strain`)      REFERENCES `strain`(`mouse_strain`),
  CONSTRAINT `fk_m_stage`  FOREIGN KEY (`mouse_life_stage`)  REFERENCES `life_stage`(`mouse_life_stage`),
  CONSTRAINT `fk_m_an`     FOREIGN KEY (`analysis_id`)       REFERENCES `analysis`(`analysis_id`),
  INDEX `idx_m_gene` (`gene_accession_id`),
  INDEX `idx_m_param` (`parameter_id`),
  INDEX `idx_m_strain` (`mouse_strain`),
  INDEX `idx_m_stage` (`mouse_life_stage`),
  INDEX `idx_m_an` (`analysis_id`),
  INDEX `idx_m_gene_param` (`gene_accession_id`, `parameter_id`),
  # Unique key to enforce one row per gene, parameter, strain, life stage, and analysis.
  UNIQUE KEY `uk_m_dedup` (
    `gene_accession_id`,
    `parameter_id`,
    `mouse_strain`,
    `mouse_life_stage`,
    `analysis_id`
  )
) ENGINE=InnoDB;

# Archive table for discarded or duplicate measurements.
CREATE TABLE IF NOT EXISTS `measurement_archive` LIKE `measurement`;

# View with convenience column for -log10(p) on valid p-values.
CREATE OR REPLACE VIEW `measurement_v` AS
SELECT m.*,
  CASE WHEN m.p_value > 0 AND m.p_value <= 1 THEN -LOG10(m.p_value) END AS neglog10_p
FROM `measurement` m;

# ETL run log with basic metadata and row counts.
CREATE TABLE IF NOT EXISTS `ingest_log` (
  `ingest_id`     BIGINT AUTO_INCREMENT PRIMARY KEY,
  `run_at`        DATETIME NOT NULL,
  `filename`      VARCHAR(255),
  `sha256`        CHAR(64),
  `rows_in`       INT,
  `rows_loaded`   INT,
  `rows_archived` INT,
  `invalid_p`     INT
) ENGINE=InnoDB;
