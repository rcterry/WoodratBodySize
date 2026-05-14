# -------------------------
# ANOVA of means over time
# -------------------------

# --------------------------
# Read in Data
data <- read.csv(paste0(output_dir, "/4.0-PaisleyFossils-PredictedSize.csv"))
data <- subset(data,select=c("RCTID","Taxon","Crown.Height","Bchron.Age","Age.Bin","Weight","wt.nls.used","Length","l.nls.used"))

########################################
# Weight and Length Ranges
########################################
wt.rng <- aggregate(data$Weight,by=list(data$Taxon),range)
l.rng <- aggregate(data$Length,by=list(data$Taxon),range)

#########################################
# Run ANOVAs by Time Bin for each species - Length
#########################################
species <- c("Neotoma cinerea","Neotoma lepida") # Can't do fuscipes b/c only Holocene
  
for (s in 1:length(species)) {
    temp <- data[which(data$Taxon==species[s]),]
    a <- aov(Length ~ Age.Bin, data = temp)
    aa <- summary(a)
    tukey <- TukeyHSD(a)
    
    sink(paste0(output_dir, "/", paste("4.2-OverTime-",species[s],"-ANOVA-Length.txt",sep="")))
    print(a)
    print(aa)
    print(tukey)
    sink()
  }
  
#########################################
# Run ANOVAs by Time Bin for each species - Weight
#########################################
species <- c("Neotoma cinerea","Neotoma lepida")
  
  for (s in 1:length(species)) {
    temp <- data[which(data$Taxon==species[s]),]
    a <- aov(Weight ~ Age.Bin, data = temp)
    aa <- summary(a)
    tukey <- TukeyHSD(a)
    
    sink(paste0(output_dir, "/", paste("4.2-OverTime-",species[s],"-ANOVA-Weight.txt",sep="")))
    print(a)
    print(aa)
    print(tukey)
    sink()
  }
  
##################################################
# Run Cross-Species ANOVAs for Holocene and Modern
##################################################
time.bin <- c("Holocene","Modern")

for (t in 1:length(time.bin)) {
  temp <- data[which(data$Age.Bin==time.bin[t]),]
  a <- aov(Weight ~ Taxon, data = temp)
  aa <- summary(a)
  tukey <- TukeyHSD(a)
  
  sink(paste0(output_dir, "/", paste("4.2-OverTime-",time.bin[t],"-ANOVA-Weight.txt",sep="")))
  print(a)
  print(aa)
  print(tukey)
  sink()
}

for (t in 1:length(time.bin)) {
  temp <- data[which(data$Age.Bin==time.bin[t]),]
  a <- aov(Length ~ Taxon, data = temp)
  aa <- summary(a)
  tukey <- TukeyHSD(a)
  
  sink(paste0(output_dir, "/", paste("4.2-OverTime-",time.bin[t],"-ANOVA-Length.txt",sep="")))
  print(a)
  print(aa)
  print(tukey)
  sink()
}

