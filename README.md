# Measuring Financial Inclusion Across Indian States

## A Two-Stage PCA Approach

This repository contains the research materials associated with the dissertation **"Measuring Financial Inclusion Across Indian States: A Two-Stage PCA Approach."**

The study develops a multidimensional Financial Inclusion Index (FII) for **28 Indian states over 2016–17 to 2024–25** using a hierarchical Two-Stage Principal Component Analysis (PCA). The national-level PCA framework is validated against the Reserve Bank of India's Financial Inclusion Index (RBI FI-Index), and an alternative state-level index is constructed using Sarma's distance-based methodology to assess the robustness of the results.

---

## Research Objectives

The study aims to:

1. Construct a Financial Inclusion Index using a hierarchical Two-Stage PCA framework, validate the national-level index against the RBI Financial Inclusion Index, and extend the framework to 28 Indian states.
2. Construct state-level Financial Inclusion Indices using Sarma's distance-based methodology and compare the results with the PCA-based index.
3. Examine the spatial and temporal evolution of financial inclusion across Indian states, including interstate disparities, regional patterns, convergence and changes in relative performance.

---

## Data

The study uses two datasets.

### National Dataset

- **Coverage:** India
- **Period:** 2004–05 to 2024–25
- **Frequency:** Annual
- **Purpose:** Estimation of the Two-Stage PCA framework and validation against the RBI FI-Index.

### State Panel Dataset

- **Coverage:** 28 Indian states
- **Period:** 2016–17 to 2024–25
- **Structure:** Balanced state-year panel
- **Purpose:** Construction and analysis of comparable state-level Financial Inclusion Indices.

The nationally estimated PCA loadings are applied to the state panel to ensure that financial inclusion is measured using a common framework across states and years.

---

## Dimensions and Indicators

Financial inclusion is measured across three dimensions: **Penetration, Availability and Usage**.

| Dimension | Indicators |
|---|---|
| **Penetration** | Deposit Accounts per lakh pop, Credit Accounts per lakh pop |
| **Availability** | Scheduled Commercial Bank Offices per lakh pop, Rural Bank Offices per lakh pop, Bank Employees/Deposit Accounts, Insurance Offices per lakh pop, Internet Subscribers per lakh pop |
| **Usage** | Deposits per Capita, Credit Outstanding per Capita, Insurance Density |

The indicators were compiled from officially published secondary sources including:

- Reserve Bank of India (RBI)
- EPWRF
- Insurance Regulatory and Development Authority of India (IRDAI)
- Department of Telecommunications
- UIDAI

The RBI's national Financial Inclusion Index uses a much broader set of indicators. However, comparable state-level data are not available for all of these indicators. Therefore, this study uses **10 indicators that could be compiled consistently across the 28 states and over the full study period**, while maintaining coverage of the three core dimensions of financial inclusion.

---

## Methodology

### 1. Hierarchical Two-Stage PCA

The Two-Stage PCA is the **primary methodology** of the study.

#### Stage 1: Dimension-specific PCA

PCA is conducted separately for:

- Penetration
- Availability
- Usage

This produces three dimension-specific scores while preserving the multidimensional structure of financial inclusion.

#### Stage 2: Composite PCA

The three dimension scores are then combined using a second PCA to obtain the composite Financial Inclusion Index.

The PCA framework is first estimated using the national dataset. The resulting nationally derived loadings are subsequently applied to the state panel to construct comparable state-level indices.

This approach allows the state-level indices to be based on a common set of weights rather than estimating separate weights for each state or year.

---

### 2. Sarma Distance-Based Methodology

Sarma's methodology is used as an **alternative benchmark for robustness assessment**.

The same ten indicators are first normalised and grouped into the three dimensions of Penetration, Availability and Usage. Equal weights are used within each dimension to construct the dimension indices.

The three dimension indices are then combined using Sarma's distance-based framework, which considers both:

- the distance from the worst possible financial inclusion situation; and
- the distance from the ideal situation.

The resulting Sarma-based index ranges from 0 to 1, with higher values indicating greater financial inclusion.

Using the same underlying indicator framework allows the PCA and Sarma indices to be compared directly.

---

## Empirical Analysis

The study examines financial inclusion across several dimensions:

- State-level financial inclusion rankings
- Changes in financial inclusion over time
- Spatial distribution of financial inclusion
- Regional patterns and disparities
- Spatial autocorrelation and local spatial clusters
- Comparison of the Two-Stage PCA and Sarma indices
- Contribution of Penetration, Availability and Usage
- Financial Inclusion Development Typology (FIDT)
- Driver Strength
- Interstate convergence
- Markov transition probabilities and interstate mobility

---

## Key Findings

The main findings of the study include:

- Financial inclusion increased across **all 28 states** between 2016–17 and 2024–25.
- **Goa consistently occupied the highest position**, while Bihar remained at the bottom of the state distribution.
- Substantial interstate disparities persisted despite broad-based improvements.
- Financial inclusion displayed a clear geographical pattern, with higher levels concentrated particularly in western and southern India.
- Neighbouring states tended to exhibit similar levels of financial inclusion, indicating significant positive spatial autocorrelation.
- The Two-Stage PCA and Sarma indices showed **strong agreement across all 252 state-year observations**.
- The Pearson correlation between the two indices was **0.919**, while the Spearman rank correlation was **0.979**.
- **83.3%** of state-year observations differed by no more than three ranking positions between the two methods.
- Financial inclusion shifted nationally from stronger **Availability** towards stronger **Usage** over the study period.
- States followed different developmental pathways depending on the relative contribution of Penetration, Availability and Usage.
- Despite overall improvements, interstate relative positions remained persistent, with limited mobility across financial inclusion quartiles.

---

## Repository Structure

```text
Financial-Inclusion-India/
│
├── code/
│   ├── final_SARMA_METHOD.R
│   └── final_TWO_STAGE_PCA.R
│
├── .gitignore
├── FINAL_DATASHEET.xlsx
├── Financial_inclusion_RBI.pptx
├── Measuring Financial Inclusion Acr...pdf
└── README.md
