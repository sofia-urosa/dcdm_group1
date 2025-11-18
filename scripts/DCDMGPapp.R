#To Do List
# - (1) DONE Add the ability to select a particular knockout mouse and visualize the statistical scores of all phenotypes tested.
# - (2) DONE Add different point shapes or colors for phenotypes significantly impacted.
# - (3) DONE Visualise the statistical scores of all knockout mice for a selected phenotype.
# - (4) Visualise clusters of genes with similar phenotype scores (PCA)
# - (5) DONE Create separate tabs for each visualation (Tab 1: Gene vs Phenotypes, Tab 2: Phenotype vs Genes, Tab 3: Gene Clusters)


library(shiny)
library(ggplot2)      
library (readr)
library (dplyr)
library(tidyr)
library(DT)
library(heatmaply)
library(viridis)
library(shinythemes)
library(bslib)

# ==========================================================================

# Define the file path as a string
file_path <- "/Users/ryadl/Desktop/App_Bioinf/DCDM_GP/data/UpToDate_Data/DCDM_Files/clean/clean_data.csv"

# Check if the file exists AT THAT PATH
if (!file.exists(file_path)) {
  stop("Error: File not found at the specified path.")
}

# If the check passes, *then* read the file
dat <- readr::read_csv(file_path)

# You can add a print statement to confirm
print("File loaded successfully.")

#Creates a vector containing the gene_symbol names, removes the duplicates and sorts in alphabetical order.
gene_symbol_names <- sort(unique(dat$gene_symbol))

#Creates a vector containing the parameter_name names, removes the duplicates and sorts in alphabetical order.
parameter_name_names <- sort(unique(dat$parameter_name))

#Calculates the maximum -log p-value, needed for the heatmap.
max_log_pvalue <- dat %>%
  mutate(negp = -log10(pvalue)) %>%
  filter(is.finite(negp)) %>%
  pull(negp) %>%
  max(na.rm = TRUE)


# ====================== USER INTERFACE ======================

ui <- fluidPage(
  titlePanel("Group 1: IMPC Gene-Phenotype Explorer"), 
  theme = bs_theme(preset = "zephyr"), 
  tabsetPanel(
    
    #========= Tab 1: Search by Gene ========= #
    tabPanel("Search by Gene",
                       
      sidebarLayout(
                         
        sidebarPanel(
          selectInput("gene_selection",
            "Select a gene for your knockout mouse!",
            choices = gene_symbol_names,
            selected = gene_symbol_names[1])
          ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", plotOutput("phenotype_plot")), # Output plot display
              tabPanel("Table", dataTableOutput("prows_table"))
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
            tabPanel("Plot", plotOutput("gene_plot"))
            )
          )
        )
      ),
    
    
    # ========== Tab 3: Heatmap =============== #
    tabPanel("Cluster Heatmap",
           tabsetPanel(
             tabPanel("Heatmap", plotlyOutput("Heatmap", height = 1500, width = 1500)),
             tabPanel("Table", dataTableOutput("heatmap_table"))
          )
        )
       )
      )
  

# ====================== SERVER LOGIC ======================

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ========= Tab 1 Data Prep =========== #
  prows_data <- reactive({
    prows <- dat %>%
      filter(gene_symbol == input$gene_selection) %>%
      mutate(neg_log_pvalue = -log10(pvalue), .after = pvalue) %>%
      mutate(Significance = if_else(neg_log_pvalue > 1.301, "Significant", "Not Significant"), .after = neg_log_pvalue) %>%
      group_by(parameter_name) %>%
      slice_min(pvalue, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(desc(neg_log_pvalue))
    
    num_significant <- nrow(filter(prows, Significance == "Significant")) 
    n_to_show = num_significant + 3
    
    slice(prows, 1:n_to_show)
      })
  # ======== Tab 1 Data Visualisation =========#
  
      output$phenotype_plot <- renderPlot({
        prows <- prows_data()
        top_n <- nrow(prows) # Allows us to dynamically change the title based on the number of rows.
        
        ggplot(prows, aes(x = neg_log_pvalue, y = reorder(parameter_name, neg_log_pvalue), fill = Significance)) +
          geom_col() +
          scale_fill_manual(values = c("Significant" = "chartreuse1", "Not Significant" = "grey")) +
          guides(fill = guide_legend(reverse = TRUE)) +
          geom_vline(xintercept = 1.301, linetype = "longdash") +
          ggtitle(paste(top_n, "Most Significant Phenotypes for", input$gene_selection, "Knockout")) +
          labs(x = "Parameters", y = "Negative log10 p-value", subtitle = "Dotted line = p>0.05") +
          theme_bw() +
          theme(plot.subtitle = element_text(size = 10, face = "italic"))
    })
      
      output$prows_table <- renderDataTable({
        prows <- prows_data()
        prows
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
    
  output$gene_plot <- renderPlot({
    tab2 <- tab2_data()
    top_n <- nrow(tab2) # Allows us to dynamically change the title based on the number of rows.
    
    ggplot(tab2, aes(x = neg_log_pvalue, y = reorder(gene_symbol, neg_log_pvalue), fill = Significance)) +
      geom_col() +
      scale_fill_manual(values = c("Significant" = "chartreuse1", "Not Significant" = "grey")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_vline(xintercept = 1.301, linetype = "longdash") +
      ggtitle(paste(top_n, "Most Significant Genotypes for", input$phenotype_selection)) +
      labs(x = "Negative log10 p-value", y = "Genes", subtitle = "Dotted line = p>0.05") +
      theme_bw() +
      theme(plot.subtitle = element_text(size = 10, face = "italic"))
  })

  # ========== Tab 3 Data Prep ============ #
  heatmap_react <- reactive({
    heatmap_data <- dat %>%
      mutate(neg_log_pvalue = -log10(pvalue), .after = pvalue) %>%
      mutate(Significance = if_else(neg_log_pvalue > 1.301, "Significant", "Not Significant"), .after = neg_log_pvalue) %>%
      group_by(gene_symbol, parameter_name) %>%
      slice_min(pvalue, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(neg_log_pvalue = ifelse(is.infinite(neg_log_pvalue), max_log_pvalue, neg_log_pvalue)) %>%
      select(gene_symbol, parameter_name, neg_log_pvalue) %>%
      pivot_wider(names_from = parameter_name, values_from = neg_log_pvalue, values_fill = 0)
  })
  
  output$Heatmap <- renderPlotly({
    heatmap_react() %>%
    tibble::column_to_rownames("gene_symbol") %>%
    as.matrix() %>%
    heatmaply_cor(colors = viridis)
  })
  
  output$heatmap_table <- renderDataTable(heatmap_react())
  
  }
  

# Run the application 
shinyApp(ui = ui, server = server)
