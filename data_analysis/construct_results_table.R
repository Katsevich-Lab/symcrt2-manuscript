# code to generate the data analysis summary table
library(kableExtra)
library(dplyr)
library(knitr)

split_pcm_name <- "split_pcm_da_manu"
gcm_name <- "gcm_da_manu"
# hrt_names <- c("hrt_da_manu_03", "hrt_da_manu_035", "hrt_da_manu_04")
hrt_names <- c("hrt_da_manu_035")
# oat_pcm_names <- c("oat_pcm_da_manu_03", "oat_pcm_da_manu_035", "oat_pcm_da_manu_04")
oat_pcm_names <- c("oat_pcm_da_manu_035")
alpha = 0.1

# read in all of the results
split_pcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                                    split_pcm_name, "/", split_pcm_name, "_results.rds"))
gcm_results <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                              gcm_name, "/", gcm_name, "_results.rds"))
hrt_results <- list()
oat_pcm_results <- list()
i <- 1
for (name in hrt_names) {
  hrt_results[[hrt_names[i]]] <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                                                name, "/", name, "_results.rds"))[[1]]
  i <- i + 1
}
i <- 1
for (name in oat_pcm_names) {
  oat_pcm_results[[oat_pcm_names[i]]] <- readRDS(paste0(.get_config_path("LOCAL_SYMCRT2_DATA_DIR"),"/private/results/",  
                                                        name, "/", name, "_results.rds"))[[1]]
  i <- i + 1
}

# multiple testing correction function factory
correction_alpha_function <- function(method = "BH", alpha = 0.1) {
  correction_alpha <- function(result) {
    p_values <- result$p_values  # Example function that doubles the input
    p_values_adjusted <- p.adjust(p_values, method = method)
    nonnulls <- which(p_values_adjusted <= alpha)
    return(nonnulls)
  }
  return(correction_alpha)
}

# define function to convert p-values into rejection set using Bonferroni.
bonferroni_alpha <- correction_alpha_function(method = "bonferroni", alpha = alpha)

# rejection sets based on bonferroni
split_pcm_bonferroni <- sapply(split_pcm_results, bonferroni_alpha)
gcm_bonferroni <- sapply(gcm_results, bonferroni_alpha)
hrt_bonferroni <- sapply(hrt_results, bonferroni_alpha)
oat_pcm_bonferroni <- sapply(oat_pcm_results, bonferroni_alpha)

# find the actual genes
gene_id <- readRDS("data_analysis/gene_id.rds")

# Methods
method_names <- c("Tower PCM", "PCM", "HRT", "Tower GCM")
method_times <- c(split_pcm_results$split_pcm_035$time, 
                  oat_pcm_results$oat_pcm_da_manu_035$time, 
                  hrt_results$hrt_da_manu_035$time, 
                  gcm_results$drvs_gcm_5$time)
method_times <- as.numeric(method_times,  units = "mins")
genes_discovered <- c(paste(gene_id[1,split_pcm_bonferroni$split_pcm_035], collapse = ", "),
                      paste(gene_id[1,oat_pcm_bonferroni$oat_pcm_da_manu_035], collapse = ", "),
                      paste(gene_id[1,hrt_bonferroni], collapse = ", "),
                      paste(as.character(gene_id[1,gcm_bonferroni]), collapse = ", "))

results <- data.frame(Method = method_names, Time = method_times, 
                      Rejections = genes_discovered) |> 
  mutate(across(where(is.numeric), round, 2))
colnames(results)[2] <- "Time (mins)" 
colnames(results)[3] <- "Discovered genes" 

results_table <- kable(results, format = "latex", 
                       caption = "A summary of the computation time and number of discoveries for each of the methods with family-wise error rate control at level \\(\\alpha = 0.1\\) in the breast cancer dataset.", 
                       align = 'l', 
                       label = "real_da_results") %>%
  kable_styling(latex_options = "HOLD_position", position = "center") %>%
  add_header_above()


# Specify the directory where you want to save the file
path_to_table <- "manuscript/tables"

# Check if the directory exists
if (!dir.exists(path_to_table)) {
  # If the directory does not exist, create it
  dir.create(path_to_table, recursive = TRUE)
}

# Specify the file name and path
file_path <- file.path(path_to_table, "table.tex")

writeLines(results_table, file_path)




