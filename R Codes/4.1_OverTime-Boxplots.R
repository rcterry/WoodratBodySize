# -------------------------
# Boxplots by Time Period for Paisley Samples
# Boxplots with Colorblind-Friendly Colors (abbreviated labels)
# -------------------------

# --------------------------
# Read in Data
data <- read.csv(paste0(output_dir, "/4.0-PaisleyFossils-PredictedSize.csv"))
data <- subset(data,select=c("RCTID","Taxon","Crown.Height","Bchron.Age","Age.Bin","Weight","wt.nls.used","Length","l.nls.used"))

pleist <- data[which(data$Age.Bin=="Pleistocene"),]
holo <- data[which(data$Age.Bin=="Holocene"),]
mod <- data[which(data$Age.Bin=="Modern"),]

species_colors <- c(
  "Neotoma cinerea" = "#882255",
  "Neotoma fuscipes" = "#117733",
  "Neotoma lepida"  = "#332288"
)

par(mfrow = c(1, 3),
    mar = c(5, 4, 4, 1),
    oma = c(0, 0, 0, 0))

# --- helper function for custom labels ---
make_labels <- function(levels_vec) {
  sapply(levels_vec, function(x) {
    sp <- strsplit(x, " ")[[1]][2]  # take species part
    parse(text = paste0("italic('N.'~", sp, ")"))
  })
}

# -----------------------------------------
# WEIGHT Boxplots over time

pdf(paste0(output_dir, "/4.1-OverTime-BoxPlots-Weight.pdf"),height=5,width=6.5)
par(mfrow = c(1,3))
# --- Pleistocene ---
lev1 <- levels(factor(pleist$Taxon))
labels1 <- make_labels(lev1)
ylim = c(25, 500)

boxplot(Weight ~ Taxon, data = pleist,
        main = "Pleistocene",
        ylim = ylim,
        cex.main = 1.5,
        cex.lab = 1.4,
        cex.axis = 1.2,
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n",
        col = species_colors[lev1])
axis(1, at = seq_along(lev1), labels = labels1, cex.axis = 1.1)
axis(2,cex.axis = 1.1)
# points(1,means$Pl.mn[1],cex=1.5,col="red",pch=19)
# arrows(1,means$Pl.mn[1]+means$Pl.sd[1],1,means$Pl.mn[1]-means$Pl.sd[1],length=0,col="red",lwd=1.5)

# --- Holocene ---
lev2 <- levels(factor(holo$Taxon))
labels2 <- make_labels(lev2)

boxplot(Weight ~ Taxon, data = holo,
        main = "Holocene",
        ylim = ylim,
        yaxt = "n", xlab = "", ylab = "",
        cex.main = 1.5,
        cex.lab = 1.4,
        cex.axis = 1.2,
        xaxt = "n",
        col = species_colors[lev2])
axis(1, at = seq_along(lev2), labels = labels2, cex.axis = 1.1)
# points(1,means$Ho.mn[1],cex=1.5,col="red",pch=19)
# arrows(1,means$Ho.mn[1]+means$Ho.sd[1],1,means$Ho.mn[1]-means$Ho.sd[1],length=0,col="red",lwd=1.5)

# --- Modern ---
lev3 <- levels(factor(mod$Taxon))
labels3 <- make_labels(lev3)

boxplot(Weight ~ Taxon, data = mod,
        main = "Modern",
        ylim = ylim,
        yaxt = "n", xlab = "", ylab = "",
        cex.main = 1.5,
        cex.lab = 1.4,
        cex.axis = 1.2,
        xaxt = "n",
        col = species_colors[lev3])
axis(1, at = seq_along(lev3), labels = labels3, cex.axis = 1.1)
# points(1,means$Mo.mn[1],cex=1.5,col="red",pch=19)
# arrows(1,means$Mo.mn[1]+means$Mo.sd[1],1,means$Mo.mn[1]-means$Mo.sd[1],length=0,col="red",lwd=1.5)

dev.off()

# -----------------------------------------
# LENGTH Boxplots over time

pdf(paste0(output_dir, "/4.1-OverTime-BoxPlots-Length.pdf"),height=5,width=6.5)
par(mfrow = c(1,3))
# --- Pleistocene ---
lev1 <- levels(factor(pleist$Taxon))
labels1 <- make_labels(lev1)
ylim = c(200, 550)

boxplot(Length ~ Taxon, data = pleist,
        main = "Pleistocene",
        ylim = ylim,
        cex.main = 1.5,
        cex.lab = 1.4,
        cex.axis = 1.2,
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n",
        col = species_colors[lev1])
axis(1, at = seq_along(lev1), labels = labels1, cex.axis = 1.1)
axis(2,cex.axis = 1.1)
# points(1,means$Pl.mn[1],cex=1.5,col="red",pch=19)
# arrows(1,means$Pl.mn[1]+means$Pl.sd[1],1,means$Pl.mn[1]-means$Pl.sd[1],length=0,col="red",lwd=1.5)

# --- Holocene ---
lev2 <- levels(factor(holo$Taxon))
labels2 <- make_labels(lev2)

boxplot(Length ~ Taxon, data = holo,
        main = "Holocene",
        ylim = ylim,
        yaxt = "n", xlab = "", ylab = "",
        cex.main = 1.5,
        cex.lab = 1.4,
        cex.axis = 1.2,
        xaxt = "n",
        col = species_colors[lev2])
axis(1, at = seq_along(lev2), labels = labels2, cex.axis = 1.1)
# points(1,means$Ho.mn[1],cex=1.5,col="red",pch=19)
# arrows(1,means$Ho.mn[1]+means$Ho.sd[1],1,means$Ho.mn[1]-means$Ho.sd[1],length=0,col="red",lwd=1.5)

# --- Modern ---
lev3 <- levels(factor(mod$Taxon))
labels3 <- make_labels(lev3)

boxplot(Length ~ Taxon, data = mod,
        main = "Modern",
        ylim = ylim,
        yaxt = "n", xlab = "", ylab = "",
        cex.main = 1.5,
        cex.lab = 1.4,
        cex.axis = 1.2,
        xaxt = "n",
        col = species_colors[lev3])
axis(1, at = seq_along(lev3), labels = labels3, cex.axis = 1.1)
# points(1,means$Mo.mn[1],cex=1.5,col="red",pch=19)
# arrows(1,means$Mo.mn[1]+means$Mo.sd[1],1,means$Mo.mn[1]-means$Mo.sd[1],length=0,col="red",lwd=1.5)

dev.off()

