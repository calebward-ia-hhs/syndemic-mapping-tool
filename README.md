# Syndemic Mapping Tool

An interactive Shiny application for visualizing syndemic patterns of infectious diseases and social determinants of health (SDOH) using bivariate choropleth mapping.

## Live Application

The tool is publicly available here:

https://syndemic-tools-iowa.shinyapps.io/syndemic-mapping-tool/

## Overview

This repository contains the source code for a reproducible workflow that integrates:

- HIV, viral hepatitis, and STI surveillance data
- CDC PLACES social determinants of health indicators
- Social Vulnerability Index (SVI)
- County-level bivariate choropleth mapping in R

The tool supports syndemic-informed public health planning by visualizing overlapping disease burden and social vulnerability.

## Data Availability

The surveillance and disease data used in the Iowa implementation of this tool cannot be publicly shared due to data use agreements and confidentiality restrictions.

However, the analytical workflow is fully reproducible. Jurisdictions can prepare their own datasets using locally available surveillance systems (e.g., HIV, STI, and viral hepatitis surveillance data), CDC PLACES indicators, and the Social Vulnerability Index (SVI).

Users are encouraged to adapt this codebase by replacing the example or placeholder datasets with jurisdiction-specific data while maintaining the same variable structure and processing workflow.

## Intended Use

This repository is intended as a reusable template for state and local health departments to develop syndemic mapping dashboards tailored to their own surveillance systems and populations.

## Citation

If you use this tool, please cite:

Ward CL, Wurtzel J, Campbell K, et al. A Reproducible Bivariate Mapping Workflow for Visualizing Syndemics. (2026).

## Running the application locally

```r
shiny::runApp()
