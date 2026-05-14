# =============================================================================
# RunMe.R - Master Analysis Script
# =============================================================================
# This is the main script that orchestrates all analyses
# =============================================================================


# Load all setup configurations (paths, packages, parameters)
source("Setup.R")

# =============================================================================
# STEP 1: GEOGRAPHIC CONTEXT
# =============================================================================
# Creates geographic distribution maps showing where museum specimens were
# collected for the three focal Neotoma species across Oregon and adjacent states.
# Also provides North American context for the study region.

cat("\n>>> STEP 1: Creating geographic distribution maps...\n")
source(file.path(code_dir, "1_OregonMap.R"))
# Outputs:
#   - 1-OR_Map.pdf: Primary study area (Oregon/Nevada region with specimen locations)
#   - 1-NorthAmerica_Map.pdf: Broader geographic context

# =============================================================================
# STEP 2A: EXPLORATORY ANALYSIS - MAXILLARY MEASUREMENTS
# =============================================================================
# Performs Principal Component Analysis on maxillary craniomandibular measurements
# to visualize morphological differences among the three Neotoma species in 
# multivariate space. This addresses whether species are distinct based on 
# skeletal features likely to be preserved in fragmentary fossils.

cat("\n>>> STEP 2A: PCA analysis - Maxillary measurements...\n")
source(file.path(code_dir, "2.1_PCA_Max.R"))
# Output:
#   - 2.1-PCA-Maxilla.pdf: Ordination plot showing species clustering

# =============================================================================
# STEP 2B: EXPLORATORY ANALYSIS - MANDIBULAR MEASUREMENTS
# =============================================================================
# Performs Principal Component Analysis on mandibular measurements to parallel
# the maxillary analysis and compare morphological patterns between different
# skeletal elements.

cat("\n>>> STEP 2B: PCA analysis - Mandibular measurements...\n")
source(file.path(code_dir, "2.2_PCA_Mand.R"))
# Output:
#   - 2.2-PCA-Mandible.pdf: Ordination plot for mandibular features

# =============================================================================
# STEP 2C: INTERSPECIES MORPHOMETRIC COMPARISONS
# =============================================================================
# Creates boxplots for all measured craniomandibular features and conducts
# one-way ANOVA with Tukey post-hoc comparisons to identify which measurements
# differ significantly among species. Results guide which measurements to use
# in body size estimation models.

cat("\n>>> STEP 2C: Interspecies morphometric comparisons and ANOVA...\n")
source(file.path(code_dir, "2.3_Interspecies boxplots-revised.R"))
# Outputs:
#   - 2.3-Interspecies_Boxplots_FieldWeight_Length.pdf
#   - 2.3-Interspecies_Boxplots_FieldWeight_Length_ANOVA.txt
#   - 2.3-Interspecies_Boxplots_MandibularMeasurements.pdf
#   - 2.3-Interspecies_Boxplots_MandibularMeasurements_ANOVA.txt
#   - 2.3-Interspecies_Boxplots_MaxillaryMeasurements.pdf
#   - 2.3-Interspecies_Boxplots_MaxillaryMeasurements_ANOVA.txt

# =============================================================================
# STEP 2D: SEXUAL DIMORPHISM ASSESSMENT
# =============================================================================
# Tests for sex-based differences in body size (field weight and total length)
# within each species using t-tests. Determines whether males and females
# should be pooled or analyzed separately in subsequent model fitting.

cat("\n>>> STEP 2D: Sexual dimorphism analysis...\n")
source(file.path(code_dir, "2.4_SexDiffs.R"))
# Outputs:
#   - 2.4-SexDiffs-Boxplots-Length.pdf
#   - 2.4-SexDiffs-Boxplots-Weight.pdf

# =============================================================================
# STEP 3A: NONLINEAR MODEL FITTING - BODY WEIGHT
# =============================================================================
# Fits allometric (power-law) models using nonlinear least squares regression
# to predict field-collected body weight from craniomandibular measurements.
# Tests all available measurements to identify the best predictors (highest R²,
# lowest percent prediction error). Models use the form: weight ~ a * measurement^b

cat("\n>>> STEP 3A: Nonlinear model fitting for body weight...\n")
source(file.path(code_dir, "3.1_Model_Fitting-Weight-revised.R"))
# Outputs:
#   - 3.1-Mandible-NLS_vs_LS-Model_Estimates-Weight.txt
#   - 3.1-Mandible-NLS_vs_LS-Model-R2-Weight.pdf
#   - 3.1-Mandible-NLS_vs_LS-Model-PE-Weight.pdf
#   - 3.1-Maxilla-NLS_vs_LS-Model_Estimates-Weight.txt
#   - 3.1-Maxilla-NLS_vs_LS-Model-R2-Weight.pdf
#   - 3.1-Maxilla-NLS_vs_LS-Model-PE-Weight.pdf
#   - 3.1-Models-Mandible-Weight.csv
#   - 3.1-Models-Maxilla-Weight.csv

# =============================================================================
# STEP 3B: NONLINEAR MODEL FITTING - TOTAL LENGTH
# =============================================================================
# Fits allometric models to predict total length from craniomandibular
# measurements, following the same procedure as Step 3A. Total length is a
# standard measurement of mammal body size collected by field biologists.

cat("\n>>> STEP 3B: Nonlinear model fitting for total length...\n")
source(file.path(code_dir, "3.2_Model_Fitting-Length-revised.R"))
# Outputs:
#   - 3.2-Mandible-NLS_vs_LS-Model_Estimates-Length.txt
#   - 3.2-Mandible-NLS_vs_LS-Model-R2-Length.pdf
#   - 3.2-Mandible-NLS_vs_LS-Model-PE-Length.pdf
#   - 3.2-Maxilla-NLS_vs_LS-Model_Estimates-Length.txt
#   - 3.2-Maxilla-NLS_vs_LS-Model-R2-Length.pdf
#   - 3.2-Maxilla-NLS_vs_LS-Model-PE-Length.pdf
#   - 3.2-Models-Mandible-Length.csv
#   - 3.2-Models-Maxilla-Length.csv

# =============================================================================
# STEP 4A: ESTIMATE FOSSIL BODY SIZES
# =============================================================================
# Applies the best-fit models from Steps 3A and 3B to fossil Neotoma specimens
# from the Paisley Caves. For each fossil specimen, uses the model with the
# highest R² value from the museum specimen training data to predict body weight
# and total length. Accommodates fragmentary remains by using whichever
# measurements are available.

cat("\n>>> STEP 4A: Estimating body sizes for Paisley Caves fossils...\n")
source(file.path(code_dir, "4.0_EstimateFossils-revised.R"))
# Output:
#   - 4.0-PaisleyFossils-PredictedSize.csv: Fossil specimen body size estimates
#     with stratigraphic positions and dating

# =============================================================================
# STEP 4B: VISUALIZE TEMPORAL TRENDS IN FOSSIL BODY SIZE
# =============================================================================
# Creates boxplots of reconstructed body weight and total length for each
# Neotoma species grouped by time period (Pleistocene, Holocene, Modern).
# Visualizes whether body size has changed over time for each species.

cat("\n>>> STEP 4B: Boxplots of fossil body sizes by time period...\n")
source(file.path(code_dir, "4.1_OverTime-Boxplots.R"))
# Outputs:
#   - 4.1-OverTime-Boxplots-Weight.pdf
#   - 4.1-OverTime-Boxplots-Length.pdf

# =============================================================================
# STEP 4C: STATISTICAL ANALYSIS OF TEMPORAL TRENDS
# =============================================================================
# Tests whether mean body size differs significantly among time periods
# (Pleistocene, Holocene, Modern) for each species using one-way ANOVA.
# Conducts Tukey post-hoc comparisons to identify which time periods differ.

cat("\n>>> STEP 4C: ANOVA of body size across time periods...\n")
source(file.path(code_dir, "4.2_OverTime-Anova.R"))
# Outputs:
#   - 4.2-OverTime-Modern-ANOVA-Weight.txt
#   - 4.2-OverTime-Modern-ANOVA-Length.txt
#   - 4.2-OverTime-Holocene-ANOVA-Weight.txt
#   - 4.2-OverTime-Holocene-ANOVA-Length.txt

# =============================================================================
# STEP 4D: CROWN HEIGHT AND ONTOGENETIC AGE ANALYSIS
# =============================================================================
# Investigates whether enamel crown height (proxy for ontogenetic age) correlates
# with estimated body size for N. cinerea. Tests whether any temporal patterns
# in body size could be artifacts of age structure in the fossil sample rather
# than true size evolution.

cat("\n>>> STEP 4D: Crown height vs. body size analysis...\n")
source(file.path(code_dir, "4.3_OverTime-CrownHeightCorrelation-revised.R"))
# Outputs:
#   - 4.3-CrownHeightCorrelation-Weight.pdf
#   - 4.3-CrownHeightCorrelation-Length.pdf

# =============================================================================
# STEP 5: WEIGHTED REGRESSION OF BODY SIZE OVER TIME
# =============================================================================
# Performs weighted linear regression of body size against time for each species.
# Weights are based on the R² value of the model used to estimate each specimen's
# body size (higher confidence estimates receive higher weights). Tests whether
# there is a significant linear trend in body size over ~15,000 years.

cat("\n>>> STEP 5: Weighted regression analysis of size change over time...\n")
source(file.path(code_dir, "5_OverTime-WeightedRegression-revised.R"))
# Outputs:
#   - 5-OverTime-WeightedRegression-Weight.pdf
#   - 5-OverTime-WeightedRegression-Length.pdf

# =============================================================================
# STEP 6: BODY SIZE VARIATION ANALYSIS (RESAMPLING)
# =============================================================================
# Evaluates whether the variability (standard deviation) in body size changed
# over time for N. cinerea (the only species spanning all three time periods).
# Uses resampling to account for unequal sample sizes across time bins.
# Asks whether the range of body sizes became more constrained over time.

cat("\n>>> STEP 6: Resampling analysis of body size variation...\n")
source(file.path(code_dir, "6_OverTime-SD-Resampling.R"))
# Outputs:
#   - 6-OverTime-SD-Resampling.pdf: Resampled SD distributions by species/period

# =============================================================================
# STEP 7: M1 AVAILABILITY AND COMPOSITION SUMMARY
# =============================================================================
# Summarizes M1 measurement availability across fossils and visualizes upper/lower
# composition and M1 measurement types (M1 length only, M1 width only, both).

cat("\n>>> STEP 7: M1 availability and upper/lower composition...\n")
source(file.path(code_dir, "7_M1Avaliability.R"))
# Outputs:
#   - M1_ONLY_vs_NO_M1_UpperLower.png
#   - M1_Upper_vs_Lower_Measurements.png

# =============================================================================
# ANALYSIS COMPLETE
# =============================================================================
cat("\n")
cat("================================================================================\n")
cat("  ALL ANALYSES COMPLETED SUCCESSFULLY\n")
cat("================================================================================\n")
cat("================================================================================\n")
cat("\n")
