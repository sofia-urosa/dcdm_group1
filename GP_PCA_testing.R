install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("ggfortify")
install.packages("ggplot2")

# --- 1. Load Libraries ---
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggfortify)
library(ggplot2)

print("Libraries loaded.")

# --- 2. Load Data ---
# Define the file path
file_path <- "/users/ryadl/Downloads/clean_data.csv"

# Check if file exists
if (!file.exists(file_path)) {
  stop("Error: clean_data.csv not found in the working directory.")
}

print("Loading data...")
all_data <- readr::read_csv(file_path, show_col_types = FALSE)

# --- 3. Pre-process Data for PCA ---
# We need a wide-format matrix: [genes] x [parameters]
# The values in the matrix will be the p-values.

print("Pivoting data to wide format (genes x parameters)...")
pca_data_wide <- all_data %>%
  # Select only the columns needed for the matrix
  select(gene_symbol, parameter_name, pvalue) %>%
  
  # De-duplicate: If a gene/parameter combo is listed multiple times,
  # keep the one with the *lowest* p-value.
  group_by(gene_symbol, parameter_name) %>%
  summarise(pvalue = min(pvalue, na.rm = TRUE), .groups = 'drop') %>%
  
  # Pivot to wide format
  # Each gene becomes a row
  # Each parameter_name becomes a column
  pivot_wider(
    names_from = parameter_name,
    values_from = pvalue,
    # If a gene was not tested for a parameter, fill with 1.0
    # (assuming 1.0 is the "non-significant" default)
    values_fill = 1.0
  )

# Remove any rows (genes) that have NA values for any reason
pca_data_wide <- na.omit(pca_data_wide)

# The 'prcomp' function needs a numeric matrix.
# We'll set the gene_symbol as row names, then remove the column.
pca_matrix <- pca_data_wide %>%
  tibble::column_to_rownames("gene_symbol")

print(paste("Data prepared. Matrix dimensions:", 
            nrow(pca_matrix), "rows (genes) x", 
            ncol(pca_matrix), "cols (parameters)"))

# --- 4. Run PCA ---
# We MUST set center = TRUE and scale. = TRUE.
# Scaling is critical so that parameters with different p-value ranges
# are weighted equally.
print("Running PCA (with centering and scaling)...")
pca_results <- prcomp(pca_matrix, center = TRUE, scale. = TRUE)

print("PCA complete.")

# --- 5. Visualize Results ---

# --- Plot 1: Scree Plot (Variance Explained) ---
# This shows how much information is captured by each new component.
print("Generating Scree Plot...")
scree_plot <- autoplot(pca_results, plot.type = "scree") +
  ggtitle("PCA Scree Plot - Variance Explained by Component")

ggsave("pca_scree_plot.png", plot = scree_plot, width = 10, height = 6)
print("Saved pca_scree_plot.png")


# --- Plot 2: Biplot (Parameter Similarity) ---
# This is the key plot for your question.
# We are plotting the *variables* (parameters) in the PCA space.
# We set shape = FALSE because plotting 17,000+ genes is unreadable.
print("Generating PCA Biplot (Parameter Loadings)...")

pca_biplot <- autoplot(
  pca_results,
  loadings = TRUE,          # Show the variable arrows (parameters)
  loadings.colour = 'blue', # Make arrows blue
  loadings.label = TRUE,    # Add labels to the arrows
  loadings.label.repel = TRUE, # Use ggrepel to avoid overlap
  loadings.label.size = 3,
  loadings.label.vjust = 1.2,
  shape = FALSE             # Do NOT plot individual samples (genes)
) +
  ggtitle("PCA Biplot of Parameters") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme_minimal()

# Save the biplot. You may need to adjust width/height for readability.
ggsave("pca_parameter_biplot.png", plot = pca_biplot, width = 14, height = 14)

print("Saved pca_parameter_biplot.png")

# --- 6. Inspect PC1 Loadings ---

print("--- Inspecting PC1 ---")

# 1. Extract the loadings (called 'rotation' in prcomp)
pc_loadings <- pca_results$rotation

# 2. Get the loadings for PC1 only
pc1_loadings <- pc_loadings[, "PC1"]

# 3. Sort by the *absolute value* to see which are most influential
# (We use absolute value because a large negative loading is just as
# important as a large positive one)
pc1_loadings_sorted <- sort(abs(pc1_loadings), decreasing = TRUE)

# 4. Print the top 10 most influential parameters for PC1
print("Top 10 most influential parameters for PC1:")
print(head(pc1_loadings_sorted, 10))

# --- (Optional) See positive vs. negative contributions ---

# Get the original loading values (not absolute) for the top 10
top_10_names <- names(head(pc1_loadings_sorted, 10))
top_10_actual_loadings <- pc1_loadings[top_10_names]

print("Actual loading values for top 10 (shows direction):")
print(sort(top_10_actual_loadings, decreasing = TRUE))

print("--- Script Finished ---")