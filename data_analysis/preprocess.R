# preprocess the data as in Liu et. al. (2022): 
# https://github.com/moleibobliu/Distillation-CRT/blob/master/real%20example/real_data_analysis.R
library(data.table)
library(dplyr)
# list of genes 

# gene_lst <- read.csv('~/data/external/LIU22/ncomms11479-s4.csv', head = T)
gene_lst <- read.csv(paste0(.get_config_path("LOCAL_EXTERNAL_DATA_DIR"), 
                            "/LIU22/ncomms11479-s4.csv"), head = T)
gene_lst <- as.character(gene_lst$X.Supplementary.Dataset.3...Matrix.for.mutations.across.all.genes.and.samples...NA.no.coding.mutation...For.inframe.indels.and.missense.SNVs..the.distinction.between.recurrent.and.non.recurrent.events.is.made.)

# CNA and RNA
# cna_data <- read.csv('~/data/external/LIU22/data_CNA.csv', head = T)
cna_data <- read.csv(paste0(.get_config_path("LOCAL_EXTERNAL_DATA_DIR"), 
                            "/LIU22/data_CNA.csv"), head = T)
# RNA_data <- fread('~/data/external/LIU22/data_expression.txt')
RNA_data <- fread(paste0(.get_config_path("LOCAL_EXTERNAL_DATA_DIR"), 
                         "/LIU22/data_expression.txt"))
gene_lst <- intersect(intersect(RNA_data$Hugo_Symbol, gene_lst),  cna_data[,1])

# We use the intersection of the gene_lst and the measured genes in cna_data and RNA data as the covariates

gene_indx_cna <- which(as.character(cna_data[,1]) %in% gene_lst)
gene_indx_rna <- which(as.character(RNA_data$Hugo_Symbol) %in% gene_lst)

cna_data <- cna_data[gene_indx_cna, ]
cna_data <- t(cna_data)
RNA_data <- RNA_data[gene_indx_rna,]
RNA_data <- t(RNA_data)
RNA_data <- RNA_data[,match(cna_data[1,], RNA_data[1,])]

for (j in 2:length(rownames(cna_data))){
  string <- strsplit(rownames(cna_data)[j], split = '[.]')
  rownames(cna_data)[j] <- paste(string[[1]][1], '-', string[[1]][2], sep = '')
}

# Clinical outcome
# clinical_data <- read.table('~/data/external/LIU22/data_clinical_sample.csv', head = T,
#                            sep = ',')
clinical_data <- read.table(paste0(.get_config_path("LOCAL_EXTERNAL_DATA_DIR"), 
                                   "/LIU22/data_clinical_sample.csv"), head = T,
                            sep = ',')

#### Merge the data by patient ID #####

pat_set <- intersect(intersect(as.character(clinical_data$PATIENT_ID),
                               rownames(RNA_data)), rownames(cna_data))

X_rna <- c()
X_cna <- c()
ER_status <- c()
Y_clinical <- c()

for (i in 1:length(pat_set)) {
  pat_id <- pat_set[i]
  indx_rna <- which(rownames(RNA_data) == pat_id)
  indx_cna <- which(rownames(cna_data) == pat_id)
  indx_clinic <- which(as.character(clinical_data$PATIENT_ID) == pat_id)
  ER_status <- c(ER_status, clinical_data$ER_STATUS[indx_clinic])
  
  ## choosing the outcome
  Y_clinical <- rbind(Y_clinical, 
                      c(clinical_data$TUMOR_SIZE[indx_clinic], clinical_data$GRADE[indx_clinic],
                        clinical_data$TUMOR_STAGE[indx_clinic]))
  
  X_cna <- rbind(X_cna, as.vector(as.numeric(cna_data[indx_cna,])))
  X_rna <- rbind(X_rna, as.vector(as.numeric(RNA_data[indx_rna,])))
}


X_rna_pos <- X_rna
X_cna_pos <- X_cna


##### Adjust the RNA data using the CNA data #####

piecewise_linear <- function(X_rna, X_cna){
  p <- length(X_rna[1,])
  R_sq_lst <- c()
  for (j in 1:p) {
    R_tot <- var(X_rna[,j])
    Y1 <- mean(X_rna[which(X_cna[,j] <= -1), j])
    Y2 <- mean(X_rna[which(X_cna[,j] == 0), j])
    Y3 <- mean(X_rna[which(X_cna[,j] >= 1), j])
    X_rna[which(X_cna[,j] <= -1), j] <- X_rna[which(X_cna[,j] <= -1), j] - Y1
    X_rna[which(X_cna[,j] == 0), j] <- X_rna[which(X_cna[,j] == 0), j] - Y2
    X_rna[which(X_cna[,j] >= 1), j] <- X_rna[which(X_cna[,j] >= 1), j] - Y3
    R_res <- var(X_rna[,j])
    R_sq_lst <- c(R_sq_lst, (R_tot - R_res) / R_tot)
  }
  return(list(R = R_sq_lst, X_rna = X_rna))
}

adj_result <- piecewise_linear(X_rna_pos, X_cna_pos)
X_rna_new <- adj_result$X_rna

# The processed data:
data_use <- as.data.frame(cbind(X_rna_new, ER_status, Y_clinical))

N <- length(data_use[,1])
p <- length(data_use[1,])
data_use <- data_use[,c(1:165,167)]
data_use <- data_use[which(complete.cases(data_use)),]




########### Extract the final data set ###########
# changed gene_use to gene_lst
ER_lst <- data_use$ER_status
X <- as.matrix(data_use[,1:length(gene_lst)])
Y <- data_use[,length(data_use[1,])]

# We only use patients with postive ER status
# changed == 2 to == "Positive"
ER_pos_set <- which(ER_lst == "Positive")
X_pos_dummy <- X[ER_pos_set,]
# added X_pos <- as.numeric(X_pos)
X_pos <- matrix(as.numeric(X_pos_dummy), nrow = nrow(X_pos_dummy), ncol = ncol(X_pos_dummy))
# X_pos <- normalize(X_pos)
# normalize the X variables
X_pos <- apply(X_pos, 2, function(x) (x - mean(x)) / sd(x))


p <- length(X_pos[1,])

# Merge two categories in Y since one of them is of very small size:

Y_pos <- Y[ER_pos_set]
Y_binary <- ifelse(Y_pos == 3, 1, 0)


# We finally use X_pos and Y_binary
data <- list(X = X_pos, Y = Y_binary)
saveRDS(data, file = "data_analysis/data.rds")

# Also save the identities of the genes
saveRDS(RNA_data[1:2,], file = "data_analysis/gene_id.rds")

