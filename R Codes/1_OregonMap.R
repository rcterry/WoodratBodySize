
# --- Set up plotting parameters ---
par(mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0), mfrow = c(1, 1))

# --- Load your data ---

maxillae <- read.csv(paste0(data_dir, "/Museum-Neotoma-MAXILLA.csv"))

Cinerea <- maxillae[which(maxillae$SPECIES=="Cinerea"),]
Fuscipes <- maxillae[which(maxillae$SPECIES=="Fuscipes"),]
Lepida <- maxillae[which(maxillae$SPECIES=="Lepida"),]

Paisley <- read.csv(paste0(data_dir, "/Paisley Metadata.xlsx - MAMMALS.csv"))


# --- Define colors ---
cb_palette <- c(
  Cinerea = "#882255",
  Fuscipes = "#117733",
  Lepida  = "#332288"
)

# Transparent fills
red_transparent   <- rgb(136/255, 34/255, 85/255, alpha = 0.1)
green_transparent <- rgb(17/255, 119/255, 51/255, alpha = 0.1)
blue_transparent  <- rgb(51/255, 34/255, 136/255, alpha = 0.1)

# --- Main Map (Nevada / Oregon region) ---

pdf(paste0(output_dir, "/1-OR_Map.pdf"),height=7,width=5)
map("county", region = c("oregon", "nevada,washoe", "nevada,humboldt"), 
    fill = FALSE, col = "gray40", lwd = 1)

with(Lepida,   points(LONG, LAT, pch = 22, col = cb_palette['Lepida'],  bg = blue_transparent,  cex = 1.5))
with(Fuscipes, points(LONG, LAT, pch = 24, col = cb_palette['Fuscipes'], bg = green_transparent, cex = 1.5))
with(Cinerea,  points(LONG, LAT, pch = 21, col = cb_palette['Cinerea'],  bg = red_transparent,   cex = 1.5))
with(Paisley,  points(LONG, LAT, pch = 1, col = 'black', cex = 3, lwd = 2.5))

# --- Legend ---
south_lat <- min(c(Cinerea$LAT, Fuscipes$LAT, Lepida$LAT, Paisley$LAT), na.rm = TRUE)
legend_x <- -120
legend_y <- south_lat - 0.3

legend(legend_x, legend_y,
       legend = c(expression(italic("N. cinerea")),
                  expression(italic("N. fuscipes")),
                  expression(italic("N. lepida"))),
       col   = c(cb_palette['Cinerea'], cb_palette['Fuscipes'], cb_palette['Lepida']),
       pch   = c(21, 24, 22),
       pt.bg = c(cb_palette['Cinerea'], cb_palette['Fuscipes'], cb_palette['Lepida']),
       pt.cex = 1.5, cex = 1.2, bty = "o",
       bg="white")
dev.off()


# --- Inset Map (North America + red study box) ---

# par(fig = c(0.7, 0.95, 0.7, 0.95), new = TRUE, mar = c(0, 0, 0, 0))

# par(fig = c(0.7, 0.95, 0.7, 0.95), mar = c(0, 0, 0, 0))


pdf(paste0(output_dir, "/1-NorthAmerica_Map.pdf"),height=6,width=5)
par(mar = c(0, 0, 0, 0), xpd=TRUE)
map("world", 
    xlim = c(-170, -50), ylim = c(15, 75),
    fill = TRUE, col = "gray90", bg = "white")

map("usa", add = TRUE, fill = TRUE, col = "gray80", border = "black")

map("state", region = "oregon", add = TRUE, fill = TRUE, col = "grey50", border = "grey20")
dev.off()

# Fix red box to outline Nevada–Oregon region
# Longitude and latitude roughly bounding your main map
# rect(-125, 38, -115, 45, border = "red", lwd = 2)

# Add labels
# text(-100, 100, "North America", cex = 0.7, font = 2)
# text(-150, 35, "USA", cex = 0.7, font = 2)

# Restore full plotting region
# par(fig = c(0, 1, 0, 1), new = FALSE)

