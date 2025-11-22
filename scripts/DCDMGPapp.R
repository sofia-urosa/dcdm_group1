#To Do List
# - (1) DONE Add the ability to select a particular knockout mouse and visualize the statistical scores of all phenotypes tested.
# - (2) DONE Add different point shapes or colors for phenotype significantly impacted.
# - (3) DONE Visualise the statistical scores of all knockout mice for a selected phenotype.
# - (4) DONE Visualise clusters of genes with similar phenotype scores (PCA)
# - (5) DONE Create separate tabs for each visualisation (Tab 1: Gene vs Phenotypes, Tab 2: Phenotype vs Genes, Tab 3: Gene Clusters)


library(shiny)
library(ggplot2)   # Used to make plots     
library (readr)
library (dplyr)    # Data manipulation
library(tidyr)     # Pivot_wider
library(DT)        # Interactive tables
library(heatmaply) # Interactive heatmap
library(bslib)     # Themes
library (viridis)  # Colour scheme for heatmap

# ========================== GLOBAL DATA SETUP =============================== #

# Define the file path as a string and checks it is there.
file_path <- "/Users/sof/Documents/Msc/DCDM/dcdm_group1/outputs/clean_data.csv"
if (!file.exists(file_path)) {
  stop("Error: File not found at the specified path.")
}

# If the check passes, read the file
dat <- readr::read_csv(file_path)

#Creates sorted, unique lists for the SelectInput drop down menus in the UI
gene_symbol_names <- sort(unique(dat$gene_symbol))
parameter_name_names <- sort(unique(dat$parameter_name))

#Calculates the universal maximum -log p-value, used to cap infinite values in the heatmap.
max_log_pvalue <- dat %>%
  mutate(negp = -log10(pvalue)) %>%
  filter(is.finite(negp)) %>%
  pull(negp) %>%
  max(na.rm = TRUE)


# ====================== USER INTERFACE ======================

ui <- fluidPage(
  titlePanel(h1("IMPC Gene-Phenotype Explorer", align = "center")),
  theme = bs_theme(preset = "zephyr"),  # Tried minty but too green, zephyr looks cleaner 
  tabsetPanel(
    
    #========= Tab 1: Search by Gene ========= #
    tabPanel("Search by Gene",
                       
      sidebarLayout( #Input on the left and plot outputted on the right.
                         
        sidebarPanel(
          selectInput("gene_selection",
            "Select a gene for your knockout mouse!",
            choices = gene_symbol_names,
            selected = gene_symbol_names[1])
          ),
        
        mainPanel(
          tabsetPanel( #Nested tabs for Plot and table output
            tabPanel("Plot", plotOutput("tab1_plot")), 
              tabPanel("Table", dataTableOutput("tab1_table"))
            )
          )
        )
      ),
    
    #========= Tab 2: Search by Phenotype ========= #
    tabPanel("Search by Phenotype",
      sidebarLayout(
               
        sidebarPanel(
          selectInput("phenotype_selection",
            "Select a phenotype!",
            choices = parameter_name_names,
            selected = parameter_name_names[1])
            ),
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", plotOutput("tab2_plot")),
            tabPanel("Table", dataTableOutput("tab2_table"))
            )
          )
        )
      ),
    
    
    # ========== Tab 3: Heatmap =============== #
    tabPanel("Cluster Heatmap",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Clustering Controls"),
                 sliderInput("n_clusters",
                             "Number of clusters:",
                             min = 2,
                             max = 10,
                             value = 4,
                             step = 1),
                 hr(),
                 h4("Matrix Size Controls"),
                 sliderInput("n_genes",
                             "Number of genes to display:",
                             min = 10,
                             max = 100,
                             value = 50,  # 50 is good balance
                             step = 5),
                 sliderInput("n_phenotypes",
                             "Number of phenotypes to display:",
                             min = 10,
                             max = 100,
                             value = 50,
                             step = 5),
                 hr(),
                 helpText("Adjust sliders to control cluster number and matrix size.",
                          "Fewer genes/phenotypes = faster rendering.")
               ),
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   tabPanel("Heatmap", plotlyOutput("Heatmap", height = 800, width = "100%")),
                   tabPanel("Table", dataTableOutput("heatmap_table")),
                   tabPanel("PCA Plot", plotOutput("pca_plot"))
                 )
               )
             )
    )))
# ====================== SERVER LOGIC ======================

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ========= Tab 1 Data Prep =========== #

  tab1_data <- reactive({
    tab1 <- dat %>%
      filter(gene_symbol == input$gene_selection) %>% #Filter to the user selected gene
      mutate(neg_log_pvalue = -log10(pvalue), .after = pvalue) %>% # Log transformation for visual scaling
      #Creates a new column called Significance, where 1.301 = -log(0.05)
      mutate(Significance = if_else(neg_log_pvalue > 1.301, "Significant", "Not Significant"), .after = neg_log_pvalue) %>%
      
      #De-duplicates parameters with multiple test results, retaining the lowest p-value.
      group_by(parameter_name) %>%
      slice_min(pvalue, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(desc(neg_log_pvalue)) #Sort from highest (most significant) to lowest p-value.
    
    #Counts the number of significant rows for dynamic slicing.
    num_significant <- nrow(filter(tab1, Significance == "Significant")) 
    #Shows all significant rows + the closest 3 to significance.
    n_to_show = num_significant + 3
    
    slice(tab1, 1:n_to_show) # Returns the final clean and sliced dataset.
      })
  
  # ======== Tab 1 Data Visualisation ========= #
  
      output$tab1_plot <- renderPlot({
        tab1 <- tab1_data()
        top_n <- nrow(tab1) # Allows us to dynamically change the title based on the number of rows.
        
        # reorder() sorts bars by neg_log_pvalue (significance) instead of alphabetically, ensuring the most significant are at the top.
        ggplot(tab1, aes(x = neg_log_pvalue, y = reorder(parameter_name, neg_log_pvalue), fill = Significance)) + 
          geom_col() +
          # Tried chartreuse1 but too bright - chartreuse3 better for presentations
          scale_fill_manual(values = c("Significant" = "chartreuse3", "Not Significant" = "grey70")) +
          guides(fill = guide_legend(reverse = TRUE)) + # Legend is by default alphabetical order, this ensures significant is at the top.
          geom_vline(xintercept = 1.301, linetype = "longdash") + # Draws the significance line.
          ggtitle(paste(top_n, "Most Significant Phenotypes for", input$gene_selection, "Knockout")) +
          labs(x = "Negative log10 p-value", y = "Parameters", subtitle = "Dotted line = p>0.05") +
          theme_bw() +
          theme(plot.subtitle = element_text(size = 10, face = "italic"))
    })
      
      output$tab1_table <- renderDataTable({
        tab1 <- tab1_data()
        tab1
        })
  
  # ========= Tab 2 Data Prep ==========#
      
  tab2_data <- reactive({
    tab2 <- dat %>%
      filter(parameter_name == input$phenotype_selection) %>%
      mutate(neg_log_pvalue = -log10(pvalue), .after = pvalue) %>%
      mutate(Significance = if_else(neg_log_pvalue > 1.301, "Significant", "Not Significant"), .after = neg_log_pvalue) %>%
      group_by(gene_symbol) %>%
      slice_min(pvalue, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(desc(neg_log_pvalue))
    
    num_significant <- nrow(filter(tab2, Significance == "Significant")) 
    n_to_show = num_significant + 3
    
    slice(tab2, 1:n_to_show)
  })
  
  # ======= Tab 2 Data Visualisation ====== #
    
  output$tab2_plot <- renderPlot({
    tab2 <- tab2_data()
    top_n <- nrow(tab2)
    
    ggplot(tab2, aes(x = neg_log_pvalue, y = reorder(gene_symbol, neg_log_pvalue), fill = Significance)) +
      geom_col() +
      scale_fill_manual(values = c("Significant" = "chartreuse3", "Not Significant" = "grey70")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_vline(xintercept = 1.301, linetype = "longdash") +
      ggtitle(paste(top_n, "Most Significant Genotypes for", input$phenotype_selection)) +
      labs(x = "Negative log10 p-value", y = "Genes", subtitle = "Dotted line = p>0.05") +
      theme_bw() +
      theme(plot.subtitle = element_text(size = 10, face = "italic"))
  })
  
  output$tab2_table <- renderDataTable({
    tab2 <- tab2_data()
    tab2
  })

  # ========== Tab 3 Data Prep ============ #
  heatmap_react <- reactive({
    # Create the full matrix first
    full_matrix <- dat %>%
      mutate(neg_log_pvalue = -log10(pvalue), .after = pvalue) %>%
      
      #Replace infinite values with the maximum finite value, as calculated during global data prep.
      mutate(neg_log_pvalue = ifelse(is.infinite(neg_log_pvalue), max_log_pvalue, neg_log_pvalue)) %>%
      mutate(Significance = if_else(neg_log_pvalue > 1.301, "Significant", "Not Significant"), .after = neg_log_pvalue) %>%
      group_by(gene_symbol, parameter_name) %>%
      slice_min(pvalue, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(gene_symbol, parameter_name, neg_log_pvalue) %>% # Keeps only these 3 columns.
      
      #Reshapes the long data into a wide Gene x Parameter matrix
      pivot_wider(names_from = parameter_name, values_from = neg_log_pvalue, values_fill = 0) 
    
    # Calculate variance to select most informative genes and phenotypes
    mat <- full_matrix %>%
      tibble::column_to_rownames("gene_symbol") %>%
      as.matrix()
    
    # Select top N genes with highest variance across phenotypes
    # High variance = more interesting for clustering
    gene_variance <- apply(mat, 1, var)
    top_genes <- names(sort(gene_variance, decreasing = TRUE)[1:min(input$n_genes, nrow(mat))])
    
    # Select top N phenotypes with highest variance across genes
    pheno_variance <- apply(mat, 2, var)
    top_phenotypes <- names(sort(pheno_variance, decreasing = TRUE)[1:min(input$n_phenotypes, ncol(mat))])
    
    # Filter to selected genes and phenotypes
    full_matrix %>%
      filter(gene_symbol %in% top_genes) %>%
      select(gene_symbol, all_of(top_phenotypes))
  })
  
  # ======== Tab 3 Data Visualisation ======== #
  
  matrix_table <- reactive({
    heatmap_react() %>%
      tibble::column_to_rownames("gene_symbol") %>% # Converts gene_symbol column into the row names.
      as.matrix()
  })
  
  output$Heatmap <- renderPlotly({
    mat <- matrix_table()
    #Prevent k from exceeding number of genes - was getting errors before
    k_clusters <- min(input$n_clusters, nrow(mat) - 1)
    
    heatmaply(mat, 
              colors = viridis(256), 
              dendrogram = "both",
              method = "ward.D2",
              k_row = k_clusters,
              k_col = k_clusters,
              main = paste("Gene-Phenotype Clustering (k =", k_clusters, ")"),
              xlab = "Phenotype Parameters",
              ylab = "Gene Knockouts",
              fontsize_row = 8, 
              fontsize_col = 8)
  })
  
  output$heatmap_table <- renderDataTable(heatmap_react()) # Produces a table for verification.
  
  # ======== K-MEANS ============ #
  kmean_results <- reactive ({
    mat <- matrix_table()
    k_clusters <- min(input$n_clusters, nrow(mat) - 1)
    set.seed(123)  # For reproducibility
    kmeans(mat, centers = k_clusters, nstart = 25)
  })
  
  pca_coordinates <- reactive({
    prcomp(matrix_table(), center = TRUE, scale. = TRUE)
  })
  
  pca_plot_data <- reactive ({
    pca_res <- pca_coordinates()
    km <- kmean_results()
    data.frame(
      cluster = as.factor(km$cluster),
      PC1 = pca_res$x[, 1],
      PC2 = pca_res$x[, 2]
    )
  })
  
  output$pca_plot <- renderPlot({
    pca_res <- pca_coordinates()
    var_explained <- summary(pca_res)$importance[2, 1:2] * 100  # Get % variance
    plot_data <- pca_plot_data()
    
    ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_viridis_d() +  # Matches heatmap colors
      labs(title = paste("PCA Plot: Gene Clusters (k =", input$n_clusters, ")"),
           subtitle = paste0("PC1 explains ", round(var_explained[1], 1), 
                             "% variance, PC2 explains ", round(var_explained[2], 1), "% variance"),
           x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
           y = paste0("PC2 (", round(var_explained[2], 1), "%)")) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic", color = "gray40"))
  })
  
  }


  


# Run the application 
shinyApp(ui = ui, server = server)



