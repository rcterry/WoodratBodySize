# -----------------------------------------------------------------
# Creates Boxplots by Species for Linear Morphometric Measurements
# Runs ANOVA on field weight and total length
# -----------------------------------------------------------------

# --- Read in Dataset
data <- read.csv(paste0(data_dir, "/Museum-Neotoma-MAXILLA.csv"))

# --- Define color palette ---
cb_palette <- c(
  "Cinerea"  = "#882255",  # red/purple
  "Fuscipes" = "#117733",  # green
  "Lepida"   = "#332288"   # blue
)

# --- Helper function to plot + run ANOVA/Tukey ---
plot_and_anova <- function(trait, x_label, title_trait) {
  # Define custom labels in italics using expression()
  species_labels <- list(
    Cinerea  = expression(italic("N.") ~ italic("cinerea")),
    Lepida   = expression(italic("N.") ~ italic("lepida")),
    Fuscipes = expression(italic("N.") ~ italic("fuscipes"))
  )
  
  boxplot(
    formula = as.formula(paste(trait, "~ SPECIES")),
    data = data,
    horizontal = TRUE,
    # main = bquote(italic(Neotoma) ~ " sp. Maxilla" ~ .(title_trait)),
    main = "",
    xlab = x_label,
    # ylab = bquote(italic(Neotoma) ~ " " ~ Species),
    ylab = "",
    cex.main = 1.2,
    cex.lab = 1.1,
    cex.axis = 1.2,
    col = cb_palette[levels(factor(data$SPECIES))],
    names = sapply(levels(factor(data$SPECIES)), function(sp) species_labels[[sp]])
  )
  
  model <- aov(as.formula(paste(trait, "~ SPECIES")), data = data)
  print(summary(model))
  print(TukeyHSD(model))
}

run_anova <- function(trait) {
  model <- aov(as.formula(paste(trait, "~ SPECIES")), data = data)
  print(summary(model))
  print(TukeyHSD(model))
}

pdf(paste0(output_dir, "/2.3-Interspecies_Boxplots_MaxillaryMeasurements.pdf"),height = 12, width = 8)
par(mfrow=c(4,2))

# --- Run for each trait ---
plot_and_anova("Alveolar.length", "Alveolar Length (mm)", "Alveolar Length")
plot_and_anova("Diastema.length", "Diastema Length (mm)", "Diastema Length")
plot_and_anova("M1.length", "M1 Length (mm)", "M1 Length")
plot_and_anova("M2.Length", "M2 Length (mm)", "M2 Length")
plot_and_anova("M3.Length", "M3 Length (mm)", "M3 Length")
plot_and_anova("Premax.to.frontal", "Premax to Frontal (mm)", "Premax to frontal")
plot_and_anova("M1.Width", "M1 Width (mm)", "M1 Width")
plot_and_anova("M1M2.Length", "M1M2 Length (mm)", "M1M2 Length")

dev.off()

sink(paste0(output_dir, "/2.3-Interspecies_Boxplots_MaxillaryMeasurements_ANOVA.txt"))
cat("ALVEOLAR LENGTH\n")
run_anova("Alveolar.length")
cat("\nDIASTEMA LENGTH\n")
run_anova("Diastema.length")
cat("\nM1 LENGTH\n")
run_anova("M1.length")
cat("\nM2 LENGTH\n")
run_anova("M2.Length")
cat("\nM3 LENGTH\n")
run_anova("M3.Length")
cat("\nPREMAX TO FRONTAL\n")
run_anova("Premax.to.frontal")
cat("\nM1 WIDTH\n")
run_anova("M1.Width")
cat("\nM1M2 LENGTH\n")
run_anova("M1M2.Length")
sink()

######################################
## REPEAT FOR MANDIBULAR MEASUREMENTS
######################################
data <- read.csv(paste0(data_dir, "/Museum-Neotoma-MANDIBLE.csv"))

cb_palette <- c(
  "Cinerea"  = "#882255",  # red/purple
  "Fuscipes" = "#117733",  # green
  "Lepida"   = "#332288"   # blue
)

# --- Helper function to plot + run ANOVA/Tukey ---
plot_and_anova <- function(trait, x_label, title_trait) {
  # Define custom labels in italics using expression()
  species_labels <- list(
    Cinerea  = expression(italic("N.") ~ italic("cinerea")),
    Lepida   = expression(italic("N.") ~ italic("lepida")),
    Fuscipes = expression(italic("N.") ~ italic("fuscipes"))
  )
  
  boxplot(
    formula = as.formula(paste(trait, "~ SPECIES")),
    data = data,
    horizontal = TRUE,
    # main = bquote(italic(Neotoma) ~ " sp. Mandible" ~ .(title_trait)),
    main = "",
    xlab = x_label,
    # ylab = bquote(italic(Neotoma) ~ " " ~ Species),
    ylab = "",
    cex.main = 1.2,
    cex.lab = 1.1,
    cex.axis = 1.2,
    col = cb_palette[levels(factor(data$SPECIES))],
    names = sapply(levels(factor(data$SPECIES)), function(sp) species_labels[[sp]])
  )
  
  model <- aov(as.formula(paste(trait, "~ SPECIES")), data = data)
  print(summary(model))
  print(TukeyHSD(model))
}

run_anova <- function(trait) {
  model <- aov(as.formula(paste(trait, "~ SPECIES")), data = data)
  print(summary(model))
  print(TukeyHSD(model))
}

pdf(paste0(output_dir, "/2.3-Interspecies_Boxplots_MandibularMeasurements.pdf"),height = 12, width = 8)
par(mfrow=c(4,2))

# --- Run for each trait ---
plot_and_anova("Alveolar.length", "Alveolar Length (mm)", "Alveolar Length")
plot_and_anova("Diastema.length", "Diastema Length (mm)", "Diastema Length")
plot_and_anova("M1.length", "m1 Length (mm)", "m1 Length")
plot_and_anova("M2.Length", "m2 Length (mm)", "m2 Length")
plot_and_anova("M3.Length", "m3 Length (mm)", "m3 Length")
plot_and_anova("M1.Width", "m1 Width (mm)", "m1 Width")
plot_and_anova("M1M2.Length", "m1m2 Length (mm)", "m1m2 Length")

dev.off()

sink(paste0(output_dir, "/2.3-Interspecies_Boxplots_MandibularMeasurements_ANOVA.txt"))
cat("ALVEOLAR LENGTH\n")
run_anova("Alveolar.length")
cat("\nDIASTEMA LENGTH\n")
run_anova("Diastema.length")
cat("\nM1 LENGTH\n")
run_anova("M1.length")
cat("\nM2 LENGTH\n")
run_anova("M2.Length")
cat("\nM3 LENGTH\n")
run_anova("M3.Length")
cat("\nM1 WIDTH\n")
run_anova("M1.Width")
cat("\nM1M2 LENGTH\n")
run_anova("M1M2.Length")
sink()

################################################
## Interspecies Comparisons - Weight and Length
################################################
pdf(paste0(output_dir, "/2.3-Interspecies_Boxplots_FieldWeight_Length.pdf"),height = 4, width = 8)

w <- aggregate(data$Weight,by=list(data$SPECIES),mean,na.rm=TRUE)
colnames(w) <- c("Species","Weight")
wsd <- aggregate(data$Weight,by=list(data$SPECIES),sd,na.rm=TRUE)
colnames(wsd) <- c("Species","Weight SD")

l <- aggregate(data$Total.length..TL.,by=list(data$SPECIES),mean,na.rm=TRUE)
colnames(l) <- c("Species","Length")
lsd <- aggregate(data$Total.length..TL.,by=list(data$SPECIES),sd,na.rm=TRUE)
colnames(lsd) <- c("Species","Length SD")

bodysize <- merge(w,wsd)
bodysize <- merge(bodysize,l)
bodysize <- merge(bodysize,lsd)

par(mfrow=c(1,2))

plot_and_anova("Weight","Field Weight (g)","Weight")
plot_and_anova("Total.length..TL.","Total Length (mm)","Length")

dev.off()

sink(paste0(output_dir, "/2.3-Interspecies_Boxplots_FieldWeight_Length_ANOVA.txt"))
cat("FIELD WEIGHT\n")
run_anova("Weight")
cat("\nTOTAL LENGTH\n")
run_anova("Total.length..TL.")
cat("\nMEANS\n")
print(bodysize)
sink()

