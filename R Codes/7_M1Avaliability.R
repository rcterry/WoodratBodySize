# -------------------------
# M1 Availability + Upper/Lower Composition
# Percentages = % of TOTAL dataset
# -------------------------

# -------------------------
# Input file
# -------------------------
data <- read.csv( paste0( data_dir,
  "/Paisley-Neotoma-MASTER-2026-05-08.csv"
))

# -------------------------
# Total dataset size
# -------------------------
total_dataset <- nrow(data)

# -------------------------
# Measurement columns
# -------------------------
measure_cols <- c(
  "Diastema.length",
  "Alveolar.length",
  "M1.length",
  "M1.Width",
  "M2.Length",
  "M2.Width",
  "M3.Length",
  "M3.Width",
  "M1M2.Length",
  "Crown.Height"
)

# =====================================================
# MEASUREMENT MATRIX
# =====================================================

meas_matrix <- !is.na(data[, measure_cols])

# -------------------------
# M1 variables
# -------------------------
has_M1_length <- meas_matrix[, "M1.length"]
has_M1_width  <- meas_matrix[, "M1.Width"]

# -------------------------
# Other measurements
# EXCLUDING Crown.Height
# -------------------------
other_cols <- setdiff(
  measure_cols,
  c("M1.length", "M1.Width", "Crown.Height")
)

has_other <- rowSums(
  meas_matrix[, other_cols]
) > 0

# -------------------------
# Any M1
# -------------------------
has_any_M1 <- has_M1_length | has_M1_width

# -------------------------
# ONLY M1
# -------------------------
only_M1 <- has_any_M1 & !has_other

# -------------------------
# No M1
# -------------------------
no_M1 <- !has_any_M1

# =====================================================
# BONE TYPES
# =====================================================

is_lower <- tolower(trimws(data$Bone)) == "mandible"
is_upper <- tolower(trimws(data$Bone)) == "maxilla"

# =====================================================
# FIGURE 1
# M1 ONLY vs NO M1
# =====================================================

m1_only_lower <- sum(only_M1 & is_lower)
m1_only_upper <- sum(only_M1 & is_upper)

no_m1_lower <- sum(no_M1 & is_lower)
no_m1_upper <- sum(no_M1 & is_upper)

plot_matrix <- rbind(
  Lower = c(m1_only_lower, no_m1_lower),
  Upper = c(m1_only_upper, no_m1_upper)
)

colnames(plot_matrix) <- c("Only M1", "No M1")

totals <- colSums(plot_matrix)

bar_perc <- round(totals / total_dataset * 100, 1)

lower_perc <- round(plot_matrix["Lower", ] / total_dataset * 100, 1)
upper_perc <- round(plot_matrix["Upper", ] / total_dataset * 100, 1)

bar_labels <- paste0(
  colnames(plot_matrix),
  "\n(n=", totals, " | ", bar_perc, "%)"
)

y_max <- max(totals)
step <- ceiling(y_max / 10); if(step == 0) step <- 1
y_top <- ceiling(y_max / step) * step + step
y_ticks <- seq(0, y_top, by = step)

pdf(paste0(output_dir, "/7_M1_ONLY_vs_NO_M1_UpperLower.pdf"),
    width = 5, height = 6)

par(mar = c(10, 5, 5, 2))

bp <- barplot(
  plot_matrix,
  beside = FALSE,
  names.arg = bar_labels,
  col = c("grey70", "grey30"),
  border = "black",
  ylim = c(0, y_top),
  las = 1,
  ylab = "Number of Specimens",
  main = "M1 Availability by Bone Type",
  yaxt = "n",
  cex.names = 1
)

axis(2, at = y_ticks, las = 1)

legend(
  "topleft",
  inset = c(0, -0.03),
  legend = c("Lower (Mandible)", "Upper (Maxilla)"),
  fill = c("grey70", "grey30"),
  bty = "n"
)

text(bp, plot_matrix["Lower", ] / 2,
     labels = paste0(lower_perc, "%"), cex = 1)

text(bp, plot_matrix["Lower", ] + plot_matrix["Upper", ] / 2,
     labels = paste0(upper_perc, "%"), col = "white", cex = 1)

dev.off()

# =====================================================
# FIGURE 2
# M1 LENGTH vs WIDTH vs BOTH
# =====================================================

m1_data <- data[only_M1, ]

m1_length <- !is.na(m1_data$M1.length)
m1_width  <- !is.na(m1_data$M1.Width)

m1_upper <- tolower(trimws(m1_data$Bone)) == "maxilla"
m1_lower <- tolower(trimws(m1_data$Bone)) == "mandible"

length_only <- m1_length & !m1_width
width_only  <- m1_width & !m1_length
both_vals   <- m1_length & m1_width

upper_counts <- c(
  sum(length_only & m1_upper),
  sum(width_only & m1_upper),
  sum(both_vals & m1_upper)
)

lower_counts <- c(
  sum(length_only & m1_lower),
  sum(width_only & m1_lower),
  sum(both_vals & m1_lower)
)

plot2_matrix <- rbind(upper_counts, lower_counts)
rownames(plot2_matrix) <- c("Upper", "Lower")
colnames(plot2_matrix) <- c("M1L Only", "M1W Only", "Both")

upper_total <- sum(upper_counts)
lower_total <- sum(lower_counts)

upper_bar_perc <- round(upper_total / total_dataset * 100, 1)
lower_bar_perc <- round(lower_total / total_dataset * 100, 1)

upper_percent <- round(upper_counts / total_dataset * 100, 1)
lower_percent <- round(lower_counts / total_dataset * 100, 1)

y_max2 <- max(rowSums(plot2_matrix))
step2 <- ceiling(y_max2 / 10); if(step2 == 0) step2 <- 1
y_top2 <- ceiling(y_max2 / step2) * step2 + step2
y_ticks2 <- seq(0, y_top2, by = step2)

pdf(paste0(output_dir, "/7_M1_Upper_vs_Lower_Measurements.pdf"),
    width = 5, height = 6)

par(mar = c(10, 5, 5, 2))

bp2 <- barplot(
  t(plot2_matrix),
  beside = FALSE,
  col = c("grey85", "grey55", "grey25"),
  border = "black",
  names.arg = c(
    paste0("Upper\n(n=", upper_total, " | ", upper_bar_perc, "%)"),
    paste0("Lower\n(n=", lower_total, " | ", lower_bar_perc, "%)")
  ),
  ylim = c(0, y_top2),
  las = 1,
  ylab = "Number of Specimens",
  main = "M1 Measurement Types",
  yaxt = "n",
  cex.names = 1
)

axis(2, at = y_ticks2, las = 1)

legend(
  "topleft",
  inset = c(0, -0.03),
  legend = c("M1L Only", "M1W Only", "Both"),
  fill = c("grey85", "grey55", "grey25"),
  bty = "n"
)

segment_matrix <- t(plot2_matrix)
cum_heights <- apply(segment_matrix, 2, cumsum)

percent_matrix <- rbind(upper_percent, lower_percent)

for(i in 1:ncol(segment_matrix)) {
  
  bottoms <- c(0, cum_heights[-nrow(cum_heights), i])
  mids <- bottoms + segment_matrix[, i] / 2
  
  text(
    x = bp2[i],
    y = mids,
    labels = paste0(percent_matrix[i, ], "%"),
    cex = 0.9
  )
}

dev.off()

