# -------------------------
# Estimate Fossil Weights Using Highest R2
# -------------------------
# -------------------------
# Read in Fossil Data
data <- read.csv(paste0(data_dir, "/Paisley-Neotoma-MASTER-2026-05-08.csv"))

# --------------------------
# Data Cleaning
# Only keep specimens that have at least one measurement
cols=c("Diastema.length", # subset out the measurements only
         "Alveolar.length",
         "M1M2.Length",
         "M1.length",
         "M1.Width",
         "M2.Length",
         "M3.Length")

rows_all_na <- apply(is.na(data[ , cols]), 1, function(r) all(r))
df_rows_all_na <- data[-rows_all_na, ]

data2 <- data[-which(rows_all_na),]

# Remove Neotoma cin.fusc and Neotoma sp.
data2 <- data2[-which(data2$Taxon=="Neotoma cin.fusc"|data2$Taxon=="Neotoma sp."),]

# Read in Body Size Models
wt.mod.max <- read.csv(paste0(output_dir, "/3.1-Models-Maxilla-Weight.csv"))
wt.mod.mand <- read.csv(paste0(output_dir, "/3.1-Models-Mandible-Weight.csv"))
wt.mod <- rbind(wt.mod.max,wt.mod.mand)

l.mod.max <- read.csv(paste0(output_dir, "/3.2-Models-Maxilla-Length.csv"))
l.mod.mand <- read.csv(paste0(output_dir, "/3.2-Models-Mandible-Length.csv"))
l.mod <- rbind(l.mod.max,l.mod.mand)

# -------------------------
# Standardize species names
wt.mod$Species[which(wt.mod$Species=="Cinerea")] <- c("Neotoma cinerea")
wt.mod$Species[which(wt.mod$Species=="Lepida")] <- c("Neotoma lepida")
wt.mod$Species[which(wt.mod$Species=="Fuscipes")] <- c("Neotoma fuscipes")

l.mod$Species[which(l.mod$Species=="Cinerea")] <- c("Neotoma cinerea")
l.mod$Species[which(l.mod$Species=="Lepida")] <- c("Neotoma lepida")
l.mod$Species[which(l.mod$Species=="Fuscipes")] <- c("Neotoma fuscipes")

# --------------------------
# Loop through specimens and estimate size using the nls model with the highest R2
predicted.out <- dim(0)
for (n in 1:nrow(data2)) {
  temp <- data2[n,] # Pull out the specimen to predict
  
  #pare down the models by species and bone
  mod.w <- wt.mod[which(wt.mod$Species==temp$Taxon & wt.mod$Bone==temp$Bone),]
  mod.l <- l.mod[which(l.mod$Species==temp$Taxon & l.mod$Bone==temp$Bone),]
  
  # subset out the measurements only from the specimen
  m <- subset(temp,select=c("Diastema.length", # subset out the measurements only
                            "Alveolar.length",
                            "M1M2.Length",
                            "M1.length",
                            "M1.Width",
                            "M2.Length",
                            "M3.Length"))
  # Keep only columns with measurements
  mm <- m[,!colSums(is.na(m)),drop=FALSE]
  
  # -------- Weight prediction
  # Select the models to compare based on the measurement
  mmm <- mod.w[match(colnames(mm),wt.mod$Measurement),] 
  best.m <- mmm[which(mmm$nls.r2==max(mmm$nls.r2)),] # Select the best model
  
  # Select the measurement value to use with the best model
  x <- as.numeric(mm[,which(colnames(mm)==best.m$Measurement)]) 
  
  # Make the prediction following:  a * x^b
  predicted.weight <- best.m$nls.a*x^best.m$nls.b
  
  # Collate output
  temp.out <- cbind(temp,predicted.weight,best.m$Measurement,best.m$nls.r2,best.m$nls.pe)
  colnames(temp.out)[c(30:33)] <- c("Weight","wt.nls.used","wt.nls.R2","wt.nls.PE")
  
  # -------- Length prediction
  # Select the models to compare based on the measurement
  mmm <- mod.l[match(colnames(mm),l.mod$Measurement),] 
  best.m <- mmm[which(mmm$nls.r2==max(mmm$nls.r2)),] # Select the best model
  
  # Select the measurement value to use with the best model
  x <- as.numeric(mm[,which(colnames(mm)==best.m$Measurement)]) 
  
  # Make the prediction following:  a * x^b
  predicted.length <- best.m$nls.a*x^best.m$nls.b
  
  # Collate output
  temp.out <- cbind(temp.out,predicted.length,best.m$Measurement,best.m$nls.r2,best.m$nls.pe)
  colnames(temp.out)[c(34:37)] <- c("Length","l.nls.used","l.nls.R2","l.nls.PE")
  
predicted.out <- rbind(predicted.out,temp.out)
}

# --------------------
# Output Data
write.csv(predicted.out,paste0(output_dir, "/4.0-PaisleyFossils-PredictedSize.csv"),row.names=F)

