# -------------------------~~~~~~~~~~~~~~~~~~~~~~~~~
# Weighted Regression Over Time
# -------------------------~~~~~~~~~~~~~~~~~~~~~~~~~



#--------------------------
# Read in Data
data <- read.csv(paste0(output_dir, "/4.0-PaisleyFossils-PredictedSize.csv"))
data <- subset(data,select=c("RCTID","Taxon","Crown.Height","Bchron.Age","Age.Bin",
                             "Weight","wt.nls.used","wt.nls.R2","wt.nls.PE",
                             "Length","l.nls.used","l.nls.R2","l.nls.PE"))

Cinerea <- data[which(data$Taxon=="Neotoma cinerea"),]
Lepida <- data[which(data$Taxon=="Neotoma lepida"),]
Fuscipes <- data[which(data$Taxon=="Neotoma fuscipes"),]

# --- Colorblind-friendly palette (Okabe-Ito) ---
cb_palette <- c(
  cin  = "#882255",   # red/purple
  fusc = "#117733",   # green
  lep  = "#332288"    # blue
)

##################
# WEIGHT
##################
# --- Plot setup ---
xlim_range <- range(Cinerea$Bchron.Age, Fuscipes$Bchron.Age, Lepida$Bchron.Age, na.rm = TRUE)

# --- Weighted linear models (weighted by wt.nls.R2) ---
lm_cin.r  <- lm(Weight ~ Bchron.Age, data = Cinerea,  weights = Cinerea$wt.nls.R2)
lm_fusc.r <- lm(Weight ~ Bchron.Age, data = Fuscipes, weights = Fuscipes$wt.nls.R2)
lm_lep.r  <- lm(Weight ~ Bchron.Age, data = Lepida,   weights = Lepida$wt.nls.R2)

# --- Weighted linear models (weighted by wt.nls.PE) ---
lm_cin.pe  <- lm(Weight ~ Bchron.Age, data = Cinerea,  weights = Cinerea$wt.nls.PE)
lm_fusc.pe <- lm(Weight ~ Bchron.Age, data = Fuscipes, weights = Fuscipes$wt.nls.PE)
lm_lep.pe  <- lm(Weight ~ Bchron.Age, data = Lepida,   weights = Lepida$wt.nls.PE)

# --- Prediction grids (na.rm ensures finite limits) ---
age_seq.cin  <- data.frame(Bchron.Age = seq(min(Cinerea$Bchron.Age,  na.rm = TRUE),
                                            max(Cinerea$Bchron.Age,  na.rm = TRUE),
                                            length.out = 100))
age_seq.fusc <- data.frame(Bchron.Age = seq(min(Fuscipes$Bchron.Age, na.rm = TRUE),
                                            max(Fuscipes$Bchron.Age, na.rm = TRUE),
                                            length.out = 100))
age_seq.lep  <- data.frame(Bchron.Age = seq(min(Lepida$Bchron.Age,   na.rm = TRUE),
                                            max(Lepida$Bchron.Age,   na.rm = TRUE),
                                            length.out = 100))

# --- Predictions with confidence intervals ---
preds.cin  <- predict(lm_cin.r,  age_seq.cin,  se.fit = TRUE)
preds.fusc <- predict(lm_fusc.r, age_seq.fusc, se.fit = TRUE)
preds.lep  <- predict(lm_lep.r,  age_seq.lep,  se.fit = TRUE)

preds.cin$lower  <- preds.cin$fit  - 1.96 * preds.cin$se.fit
preds.cin$upper  <- preds.cin$fit  + 1.96 * preds.cin$se.fit
preds.fusc$lower <- preds.fusc$fit - 1.96 * preds.fusc$se.fit
preds.fusc$upper <- preds.fusc$fit + 1.96 * preds.fusc$se.fit
preds.lep$lower  <- preds.lep$fit  - 1.96 * preds.lep$se.fit
preds.lep$upper  <- preds.lep$fit  + 1.96 * preds.lep$se.fit

# --- PLOT ORDER: Lepida (bottom) → Cinerea (middle) → Fuscipes (top) ---

ylim = c(25, 500) # Double check its consistent with the boxplots (Code 4.1)

pdf(paste0(output_dir, "/5-Regression-OverTimeGraph-Weight.pdf"),width=6.5,height=5)
# Base plot (Lepida first)
plot(Weight ~ Bchron.Age, data = Lepida,
     pch = 22, col = cb_palette["lep"], bg = cb_palette["lep"], cex = 1.2,
     xlab = "Calibrated Years BP", 
     ylab = "Weight (g)",
     xlim = rev(xlim_range),
     ylim = ylim)

# --- Confidence polygons ---
polygon(c(age_seq.lep$Bchron.Age,  rev(age_seq.lep$Bchron.Age)),
        c(preds.lep$lower,  rev(preds.lep$upper)),
        border = NA, col = alpha(cb_palette["lep"], 0.3))

polygon(c(age_seq.cin$Bchron.Age,  rev(age_seq.cin$Bchron.Age)),
        c(preds.cin$lower,  rev(preds.cin$upper)),
        border = NA, col = alpha(cb_palette["cin"], 0.3))

polygon(c(age_seq.fusc$Bchron.Age, rev(age_seq.fusc$Bchron.Age)),
        c(preds.fusc$lower, rev(preds.fusc$upper)),
        border = NA, col = alpha(cb_palette["fusc"], 0.3))

# --- Regression lines ---
lines(age_seq.lep$Bchron.Age,  preds.lep$fit,  lwd = 1.5, col = cb_palette["lep"])
lines(age_seq.cin$Bchron.Age,  preds.cin$fit,  lwd = 1.5, col = cb_palette["cin"])
lines(age_seq.fusc$Bchron.Age, preds.fusc$fit, lwd = 1.5, col = cb_palette["fusc"])

# --- Points ---
points(Weight ~ Bchron.Age, data = Cinerea,
       pch = 21, col = cb_palette["cin"], bg = cb_palette["cin"], cex = 1.2)

points(Weight ~ Bchron.Age, data = Fuscipes,
       pch = 24, col = cb_palette["fusc"], bg = cb_palette["fusc"], cex = 1.2)

# --- Legend (match top-to-bottom visual order) ---
legend("topright",
       legend = c(expression(italic("N. fuscipes")),
                  expression(italic("N. cinerea")),
                  expression(italic("N. lepida"))),
       pch = c(24, 21, 22),
       pt.bg = cb_palette[c("fusc", "cin", "lep")],
       col = cb_palette[c("fusc", "cin", "lep")],
       cex = 1)
dev.off()

#--------------------------------
# Export regression stats

sink(paste0(output_dir, "/5-OverTime-RegressionStats-Weight.txt"))
cat("Weighted by R2\n")
cat("N. cinerea\n")
print(summary(lm_cin.r))
cat("\n")
cat("N. fuscipes\n")
print(summary(lm_fusc.r))
cat("\n")
cat("N. lepida\n")
print(summary(lm_lep.r))

cat("\n----------------------------\n")
cat("Weighted by PE\n")
cat("N. cinerea\n")
print(summary(lm_cin.pe))
cat("\n")
cat("N. fuscipes\n")
print(summary(lm_fusc.pe))
cat("\n")
cat("N. lepida\n")
print(summary(lm_lep.pe))
sink()


##########
# LENGTH 
##########
# --- Plot setup ---
xlim_range <- range(Cinerea$Bchron.Age, Fuscipes$Bchron.Age, Lepida$Bchron.Age, na.rm = TRUE)

# --- Weighted linear models (weighted by l.nls.R2) ---
lm_cin.r  <- lm(Length ~ Bchron.Age, data = Cinerea,  weights = Cinerea$l.nls.R2)
lm_fusc.r <- lm(Length ~ Bchron.Age, data = Fuscipes, weights = Fuscipes$l.nls.R2)
lm_lep.r  <- lm(Length ~ Bchron.Age, data = Lepida,   weights = Lepida$l.nls.R2)

# --- Weighted linear models (weighted by l.nls.PE) ---
lm_cin.pe  <- lm(Length ~ Bchron.Age, data = Cinerea,  weights = Cinerea$l.nls.PE)
lm_fusc.pe <- lm(Length ~ Bchron.Age, data = Fuscipes, weights = Fuscipes$l.nls.PE)
lm_lep.pe  <- lm(Length ~ Bchron.Age, data = Lepida,   weights = Lepida$l.nls.PE)

# --- Prediction grids (na.rm ensures finite limits) ---
age_seq.cin  <- data.frame(Bchron.Age = seq(min(Cinerea$Bchron.Age,  na.rm = TRUE),
                                            max(Cinerea$Bchron.Age,  na.rm = TRUE),
                                            length.out = 100))
age_seq.fusc <- data.frame(Bchron.Age = seq(min(Fuscipes$Bchron.Age, na.rm = TRUE),
                                            max(Fuscipes$Bchron.Age, na.rm = TRUE),
                                            length.out = 100))
age_seq.lep  <- data.frame(Bchron.Age = seq(min(Lepida$Bchron.Age,   na.rm = TRUE),
                                            max(Lepida$Bchron.Age,   na.rm = TRUE),
                                            length.out = 100))

# --- Predictions with confidence intervals ---
preds.cin  <- predict(lm_cin.r,  age_seq.cin,  se.fit = TRUE)
preds.fusc <- predict(lm_fusc.r, age_seq.fusc, se.fit = TRUE)
preds.lep  <- predict(lm_lep.r,  age_seq.lep,  se.fit = TRUE)

preds.cin$lower  <- preds.cin$fit  - 1.96 * preds.cin$se.fit
preds.cin$upper  <- preds.cin$fit  + 1.96 * preds.cin$se.fit
preds.fusc$lower <- preds.fusc$fit - 1.96 * preds.fusc$se.fit
preds.fusc$upper <- preds.fusc$fit + 1.96 * preds.fusc$se.fit
preds.lep$lower  <- preds.lep$fit  - 1.96 * preds.lep$se.fit
preds.lep$upper  <- preds.lep$fit  + 1.96 * preds.lep$se.fit

# --- PLOT ORDER: Lepida (bottom) → Cinerea (middle) → Fuscipes (top) ---

ylim = c(200, 550) # Double check its consistent with the boxplots (Code 4.1)

pdf(paste0(output_dir, "/5-Regression-OverTimeGraph-Length.pdf"),width=6.5,height=5)
# Base plot (Lepida first)
plot(Length ~ Bchron.Age, data = Lepida,
     pch = 22, col = cb_palette["lep"], bg = cb_palette["lep"], cex = 1.2,
     xlab = "Calibrated Years BP", 
     ylab = "Length (mm)",
     xlim = rev(xlim_range),
     ylim = ylim)

# --- Confidence polygons ---
polygon(c(age_seq.lep$Bchron.Age,  rev(age_seq.lep$Bchron.Age)),
        c(preds.lep$lower,  rev(preds.lep$upper)),
        border = NA, col = alpha(cb_palette["lep"], 0.3))

polygon(c(age_seq.cin$Bchron.Age,  rev(age_seq.cin$Bchron.Age)),
        c(preds.cin$lower,  rev(preds.cin$upper)),
        border = NA, col = alpha(cb_palette["cin"], 0.3))

polygon(c(age_seq.fusc$Bchron.Age, rev(age_seq.fusc$Bchron.Age)),
        c(preds.fusc$lower, rev(preds.fusc$upper)),
        border = NA, col = alpha(cb_palette["fusc"], 0.3))

# --- Regression lines (same order) ---
lines(age_seq.lep$Bchron.Age,  preds.lep$fit,  lwd = 1.5, col = cb_palette["lep"])
lines(age_seq.cin$Bchron.Age,  preds.cin$fit,  lwd = 1.5, col = cb_palette["cin"])
lines(age_seq.fusc$Bchron.Age, preds.fusc$fit, lwd = 1.5, col = cb_palette["fusc"])

# Points
points(Length ~ Bchron.Age, data = Cinerea,
       pch = 21, col = cb_palette["cin"], bg = cb_palette["cin"], cex = 1.2)

# Fuscipes on top
points(Length ~ Bchron.Age, data = Fuscipes,
       pch = 24, col = cb_palette["fusc"], bg = cb_palette["fusc"], cex = 1.2)


# --- Legend (match top-to-bottom visual order) ---
legend("topleft",
       legend = c(expression(italic("N. fuscipes")),
                  expression(italic("N. cinerea")),
                  expression(italic("N. lepida"))),
       pch = c(24, 21, 22),
       pt.bg = cb_palette[c("fusc", "cin", "lep")],
       col = cb_palette[c("fusc", "cin", "lep")],
       cex = 1)
dev.off()

#--------------------------------
# Export regression stats

sink(paste0(output_dir, "/5-OverTime-RegressionStats-Length.txt"))
cat("Weighted by R2\n")
cat("N. cinerea\n")
print(summary(lm_cin.r))
cat("\n")
cat("N. fuscipes\n")
print(summary(lm_fusc.r))
cat("\n")
cat("N. lepida\n")
print(summary(lm_lep.r))

cat("\n----------------------------\n")
cat("Weighted by PE\n")
cat("N. cinerea\n")
print(summary(lm_cin.pe))
cat("\n")
cat("N. fuscipes\n")
print(summary(lm_fusc.pe))
cat("\n")
cat("N. lepida\n")
print(summary(lm_lep.pe))
sink()

############################
# Sampling Ticker
############################
#--------------------------
# Read in Data
cave_levels <- read.csv(paste0(data_dir, "/Paisley-AllLevels.csv"))
cave_levels$Y <- rep(1,nrow(cave_levels))
cave_levels$Rats[which(is.na(cave_levels$Rats))] <- 0
cave_levels <- cave_levels[-which(cave_levels$Bchron.Age>max(xlim_range)+50),]


pdf(paste0(output_dir, "/5-Sampleing_Ticker.pdf"),width=6.5,height=2)
# Base plot of sampling levels
plot(cave_levels$Bchron.Age,cave_levels$Y,
     pch = 21,
     col="grey",
     cex=0.75,
     xlab = "Calibrated Years BP", 
     ylab = "",
     xlim = rev(xlim_range)
     )

# levels with woodrats
points(cave_levels$Bchron.Age,cave_levels$Rats, pch = 19, col="grey",cex=0.75)
dev.off()




