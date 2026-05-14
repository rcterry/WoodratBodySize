# ------------------------------------------
# PCA on Neotoma Mandibles (MAMMALS sheet)
# Using 8 predictors to compare SPECIES
# ------------------------------------------


# 1. Load the dataset
# data_raw <- `**Interspecies.Mandibles.xlsx...MAMMALS`
data_raw <- read.csv(paste0(data_dir, "/Museum-Neotoma-MANDIBLE.csv"))

# 2. Select predictors + species
predictors <- c("Alveolar.length", "Diastema.length", "M1.length",
                "M2.Length", "M3.Length", 
                "M1.Width", "M1M2.Length")

# Keep only predictors + SPECIES column
data_subset <- data_raw[, c("SPECIES", predictors)]

# Make sure predictors are numeric
for (col in predictors) {
  data_subset[[col]] <- as.numeric(data_subset[[col]])
}

# 3. Drop rows with missing values
data_subset <- na.omit(data_subset)

# 4. Split predictors and response
X <- data_subset[, predictors]
species <- data_subset$SPECIES

# Remove any zero-variance columns
X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0]

# 5. Scale and run PCA
X_scaled <- scale(X)
pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# 6. Inspect results
print(summary(pca_result))       # variance explained
print(pca_result$rotation)       # loadings (variable contributions)

# 7. Scree plot
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
ggplot(data.frame(PC = 1:length(var_explained), Var = var_explained),
       aes(x = PC, y = Var)) +
  geom_line() +
  geom_point(size = 2) +
  ylab("Proportion of Variance Explained") +
  xlab("Principal Component") +
  theme_minimal()


# --- Consistent colorblind-friendly palette (same as GAM plot) ---
cb_palette <- c(
  Cinerea = "#882255",   # red
  Fuscipes = "#117733",  # green
  Lepida  = "#332288"    # blue
)

# 8. PCA scatterplot by species (2D)
scores <- as.data.frame(pca_result$x)
scores$SPECIES <- species

# Convert species labels to italic expressions
species_labels <- c(
  Cinerea  = expression(italic("N. cinerea")),
  Fuscipes = expression(italic("N. fuscipes")),
  Lepida   = expression(italic("N. lepida"))
)

# --- Solid shapes (no fill vs. border issue) ---
shape_map <- c(
  Cinerea = 16,   # solid circle
  Fuscipes = 17,  # solid triangle
  Lepida  = 15    # solid square
)

# --- Draw Plot and Export to PDF


p <- ggplot(scores, aes(x = PC1, y = PC2,
                   color = SPECIES,
                   shape = SPECIES)) +
  geom_point(size = 3, alpha = 0.8) +
  
  # 95% confidence ellipses (outline only, but not in legend)
  stat_ellipse(aes(group = SPECIES, color = SPECIES),
               type = "norm", level = 0.95, linewidth = 1,
               show.legend = FALSE) +   # <--- hides ellipses from legend
  
  scale_color_manual(values = cb_palette, labels = species_labels) +
  scale_shape_manual(values = shape_map, labels = species_labels) +
  
  theme_minimal() +
  labs(
    x = paste0("PC1 (", round(var_explained[1]*100, 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2]*100, 1), "%)")
  ) +
  
  # Adjust font sizes
  theme(
    legend.title = element_text(size = 14),  # Legend title font size
    legend.text = element_text(size = 12),   # Legend labels font size
    axis.title = element_text(size = 14),    # Axis labels font size
    axis.text = element_text(size = 12),     # Axis ticks/marks font size
    strip.text = element_text(size = 14)     # Facet strip text font size (if you use faceting)
  ) +
  theme(panel.border = element_rect(colour = "black",
                                    fill   = NA,
                                    linewidth   = 1.5)) +
  theme(legend.position = c(0.1,0.9))

ggsave(paste0(output_dir, "/2.2-PCA-Mandible.pdf"),height=6,width=7)


########### 3D Plots - Not used for MS ############

# # 9. PCA scatterplot (3D)
# scores$color <- cb_palette[as.character(scores$SPECIES)]
# scores$shape <- shape_map[as.character(scores$SPECIES)]
# 
# # static 3D scatterplot (with fill hack)
# s3d <- scatterplot3d(scores$PC1, scores$PC2, scores$PC3,
#                      type = "n",  # draw axes only
#                      xlab = paste0("PC1 (", round(var_explained[1]*100, 1), "%)"),
#                      ylab = paste0("PC2 (", round(var_explained[2]*100, 1), "%)"),
#                      zlab = paste0("PC3 (", round(var_explained[3]*100, 1), "%)"))
# 
# s3d$points3d(scores$PC1, scores$PC2, scores$PC3,
#              pch = scores$shape,
#              col = "black",           # outline
#              bg = scores$color,       # fill
#              cex = 1.2)
# 
# # Italicized legend
# legend("topright",
#        legend = c(expression(italic("Cinerea")),
#                   expression(italic("Fuscipes")),
#                   expression(italic("Lepida"))),
#        pch = shape_map,
#        pt.bg = cb_palette,
#        col = "black")

