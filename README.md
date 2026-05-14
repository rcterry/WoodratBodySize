# Taphonomically-informed Body Size Estimation of Woodrat (*Neotoma*) Fossils from the Paisley Caves, Oregon

For mammals, body size is a key indicator of life history traits and ecology, and shapes responses to environmental change. For paleontological specimens, fragmentary skeletal remains often lack the first molar which is traditionally used for body size reconstruction. We measured craniomandibular features likely to persist on fragmentary remains for three Oregon Great Basin woodrat species and fit nonlinear models to predict weight and length using these features.  We then applied these models to the woodrats from the Paisley Caves, OR, to track body size changes over the last 17,000 years. This code repository contains all data and R scripts used in our analysis.

---

## Repository Structure

### Key Folders

- **`Data/`** — Museum specimen measurements and fossil specimen data
  - Contains measurement data for modern *Neotoma* specimens from the Oregon State University Fish and Wildlife Collection and the Terry Lab collection
  - Contains fossil specimen measurements from the Paisley Caves along with radiocarbon age estimates
  
- **`R Codes/`** — Analysis scripts numbered 1–7 and executed in order by `RunMe.R`.

  
- **`R Output/`** — Output directory for all figures, tables, and statistical results
  - PDFs for all visualizations
  - TXT files for statistical tests
  - CSV files for model parameters and predictions

### Master Scripts

- **`Setup.R`** — Project configuration script (loads packages, defines paths, sets defaults)
- **`RunMe.R`** — Master script that sources all numbered R scripts in sequence

---

## Requirements & Dependencies

- **R version:** 3.6.0 or later (4.0+ recommended)

The following packages are automatically installed by `Setup.R` if not already present:

| Package | Version | Purpose |
|---------|---------|---------|
| `maps` | — | Geographic mapping and distributions |
| `ggplot2` | ≥ 3.0.0 | Data visualization and plotting |
| `scatterplot3d` | — | 3D visualization for PCA results |
| `AICcmodavg` | — | Model comparison utilities |
| `mgcv` | — | Smoothing and regression modeling |
| `scales` | — | Color scales and transformations |

---

## Contact Information

**Joaquin S. Rico**  
Email: joaquinrico4444@gmail.com  

**Rebecca C. Terry**  
Email: terryr@oregonstate.edu  
Department of Integrative Biology  
Oregon State University  
Corvallis, OR 97331

---

## Funding

This research was supported by:
- NSF DEB grant #2228632 (to R.C. Terry)
- Oregon State University College of Science SURE Award (to J.S. Rico)


---

## Specimen Information

Fossil specimens are housed at the Museum of Natural and Cultural History, University of Oregon. Modern specimens are housed in the Oregon State University Fish and Wildlife Collection and the Terry Lab at OSU.

---

## Citation

If you use this code or data, please cite:

Rico, J.S. and Terry, R.C. (in review). Taphonomically-informed body size estimation of woodrat (*Neotoma*) fossils from the Paisley Caves, Oregon.

---

*Last Updated: May 13, 2026*
