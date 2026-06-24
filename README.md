# Syndemic Mapping Tool: A Reproducible Bivariate Mapping Workflow for Visualizing Syndemics

An interactive Shiny application for visualizing syndemic patterns of infectious diseases and social determinants of health (SDOH) using bivariate choropleth mapping.

---

## Live Application

The deployed application is available here:

https://syndemic-tools-iowa.shinyapps.io/syndemic-mapping-tool/

---

## Overview

This tool was developed by the Iowa Department of Health and Human Services to support syndemic-informed public health planning.

It integrates:
- HIV, viral hepatitis B and C, and sexually transmitted infection (STI) surveillance data
- CDC PLACES social determinants of health (SDOH) indicators
- CDC Social Vulnerability Index (SVI)
- County-level bivariate choropleth mapping

The application allows users to explore overlapping patterns of disease burden and social vulnerability to identify priority areas for prevention, testing, outreach, and care coordination.

---

## Data Availability

The surveillance and disease data used in the Iowa implementation of this tool cannot be publicly shared due to data use agreements and confidentiality restrictions.

However, the workflow is fully reproducible.

Jurisdictions interested in using this tool are encouraged to prepare their own datasets using locally available surveillance systems, such as:
- HIV surveillance systems (e.g., eHARS)
- State STI and viral hepatitis surveillance systems
- CDC PLACES indicators
- CDC Social Vulnerability Index (SVI)

Users can replace the example or placeholder datasets with jurisdiction-specific data while maintaining the same variable structure and processing workflow.

---

## Intended Use

This repository is intended as a **reusable template** for state and local health departments to develop their own syndemic mapping dashboards.

Users are encouraged to adapt:
- Disease indicators
- SDOH variables
- Geographic boundaries
- Service layer overlays (e.g., clinics, outreach sites)

---

## Features

- Interactive county-level bivariate choropleth maps
- Selection of multiple infectious disease indicators
- Integration of SDOH and SVI measures
- Toggleable service layers (testing sites, outreach areas, clinics)
- County ranking and comparison tools
- Accessible color palette for bivariate visualization

---

## Running the Application Locally

To run the app locally:

```r
shiny::runApp()
