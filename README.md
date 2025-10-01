# PRSscz_brainhealth

Codes for the analysis of polygenic risk for schizophrenia and brain health

## Scripts Description

This repository contains the core R scripts used for the analyses.

*   `README.md`
    *   This file, providing an overview of the project.

*   `pca.R`
    *   **Purpose**: Performs Principal Component Analysis (PCA) on eight different metrics derived from a specific white matter tract. The goal is to reduce the dimensionality of these correlated imaging metrics. The first two principal components (PC1 and PC2), which capture the largest amount of variance in the data, are extracted.

*   `glm.R`
    *   **Purpose**: Implements the main association analysis. It runs General Linear Models (GLMs) to test the primary effect of the schizophrenia Polygenic Risk Score (PRSscz) on various brain health phenotypes.

*   `prs*sex.R`
    *   **Purpose**: Conducts interaction analysis by testing the interaction effect between PRSscz and sex on the outcome phenotypes using GLMs.

*   `prs*IMD.R`
    *   **Purpose**: Conducts interaction analysis by testing the interaction effect between PRSscz and the Index of Multiple Deprivation (IMD) on the outcome phenotypes using GLMs.

*   `cma.R`
    *   **Purpose**: Performs Causal Mediation Analysis (CMA) to investigate whether a brain white matter phenotype mediates the relationship between the PRSscz and another cognitive or mental health outcome.
