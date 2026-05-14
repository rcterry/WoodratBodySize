# =============================================================================
# SETUP.R - Project Configuration and Package Loading
# =============================================================================
# This script configures all project paths, loads required packages, and sets
# up the environment for the Neotoma body size analysis.
# =============================================================================

# =============================================================================
# 1. DEFINE PATHS
# =============================================================================
# Define key project directories (relative paths)
data_dir <- "Data"
output_dir <- "R Output"
code_dir <- "R Codes"

# =============================================================================
# 2. LOAD REQUIRED PACKAGES
# =============================================================================
# List of required packages with their uses
packages_required <- c(
  "maps",               # Geographic mapping (script 1)
  "ggplot2",            # Data visualization (scripts 2.1, 2.2)
  "scatterplot3d",      # 3D scatter plots (scripts 2.1, 2.2)
  "AICcmodavg",         # Model comparison and AIC utilities (scripts 3.1, 3.2)
  "mgcv",               # Generalized additive models / smoothing (script 5)
  "scales"              # Color and scale transformations (script 5)
)

# Function to load packages with error handling
load_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg, quiet = TRUE)
    require(pkg, character.only = TRUE, quietly = TRUE)
  }
  invisible(NULL)
}

# Load all packages
for (pkg in packages_required) {
  load_package(pkg)
}

# Verify all packages loaded
loaded_packages <- (.packages())
missing <- setdiff(packages_required, loaded_packages)
if (length(missing) > 0) {
  warning("The following packages could not be loaded: ", paste(missing, collapse = ", "))
}

# =============================================================================
# 3. SET PLOTTING PARAMETERS
# =============================================================================
# Default graphical parameters for consistent output across all scripts
par(
  mar = c(5, 4, 4, 2) + 0.1,  # Margins: bottom, left, top, right
  oma = c(0, 0, 0, 0),         # Outer margins
  cex = 1,                      # Character expansion
  cex.lab = 1,                  # Axis label expansion
  cex.axis = 0.9                # Axis tick expansion
)

# =============================================================================
# 4. PROJECT SUMMARY
# =============================================================================
cat("\n")
cat("================================================================================\n")
cat("  Neotoma Body Size Estimation Analysis - Project Setup Complete\n")
cat("================================================================================\n")
cat("Data Directory:", data_dir, "\n")
cat("Output Directory:", output_dir, "\n")
cat("Code Directory:", code_dir, "\n")
cat("Packages Loaded:", length(loaded_packages), "\n")
cat("================================================================================\n")
cat("\n")

# Clean up temporary variables
rm(packages_required, load_package, loaded_packages, missing)
