# ============================================================
# FINANCIAL INCLUSION ACROSS INDIAN STATES
# Hierarchical Two-Stage Principal Component Analysis
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(corrplot)
library(ggcorrplot)
library(reshape2)


# ============================================================
# 2. DATA IMPORT
# ============================================================

# Dataset should be stored in the "data" folder
# of the GitHub repository.

data_file <- "C:/Users/KIIT/Downloads/FINAL_DATASHEET.xlsx"

india_data <- read_excel(
  data_file,
  sheet = "INDIA_DATA"
)


# ============================================================
# 3. VARIABLE SELECTION
# ============================================================

india_pc2 <- india_data[, c(
  "Deposits per capita (₹)",
  "Credit Outstanding per capita (₹)",
  "Insurance Density (₹)",
  "SCB Offices per lakh pop",
  "Deposit Accounts per lakh pop",
  "SCB Offices (Rural) per lakh pop",
  "Employees per lakh deposit account",
  "insurance offices per lakh pop",
  "internet subscribers per lakh population",
  "credit acc per lakh pop"
)]

colnames(india_pc2) <- c(
  "DEP_PC",
  "CR_OUT_PC",
  "INS_DEN",
  "SCB_OFF",
  "DEP_ACC",
  "SCB_OFF_R",
  "EMP",
  "INS_OFF",
  "INT_SUB",
  "CR_ACC"
)


# ============================================================
# 4. DESCRIPTIVE STATISTICS
# ============================================================

desc_stats <- describe(
  india_data[, -1]
)

desc_stats


# ============================================================
# 5. EXPLORATORY DATA ANALYSIS
# ============================================================

# ------------------------------------------------------------
# 5.1 Pairwise scatterplots
# ------------------------------------------------------------

pairs(
  india_data[, -1],
  main = "Scatterplot Matrix"
)


# ------------------------------------------------------------
# 5.2 Boxplots
# ------------------------------------------------------------

boxplot(
  india_data[, -1],
  las = 2,
  cex.axis = 0.8,
  main = "Boxplots of Financial Inclusion Variables"
)


# ------------------------------------------------------------
# 5.3 Standardized boxplots
# ------------------------------------------------------------

india_scaled <- as.data.frame(
  scale(india_data[, -1])
)

boxplot(
  india_scaled,
  las = 2,
  cex.axis = 0.8,
  main = "Standardized Boxplots"
)


# ------------------------------------------------------------
# 5.4 Time-series trends
# ------------------------------------------------------------

trend_data <- melt(
  india_data,
  id.vars = "FY"
)

ggplot(
  trend_data,
  aes(
    x = FY,
    y = value,
    group = variable
  )
) +
  geom_line() +
  facet_wrap(
    ~variable,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ============================================================
# 6. CORRELATION ANALYSIS
# ============================================================

cor_matrix <- cor(india_pc2)

cor_matrix


# ------------------------------------------------------------
# 6.1 Correlation heatmap
# ------------------------------------------------------------

ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c(
    "red",
    "white",
    "blue"
  ),
  title = "Correlation Matrix of Financial Inclusion Indicators"
)


# ============================================================
# 7. PCA SUITABILITY TESTS
# ============================================================

# ------------------------------------------------------------
# 7.1 Determinant of correlation matrix
# ------------------------------------------------------------

det(cor_matrix)


# ------------------------------------------------------------
# 7.2 Kaiser-Meyer-Olkin test
# ------------------------------------------------------------

KMO(india_pc2)


# ------------------------------------------------------------
# 7.3 Bartlett's test of sphericity
# ------------------------------------------------------------

cortest.bartlett(
  cor_matrix,
  n = nrow(india_pc2)
)


# ============================================================
# 8. HIERARCHICAL TWO-STAGE PCA
# ============================================================


# ============================================================
# 8.1 STAGE 1: DEFINE DIMENSIONS
# ============================================================

# Penetration
penetration <- india_pc2[, c(
  "DEP_ACC",
  "CR_ACC"
)]

# Availability
availability <- india_pc2[, c(
  "SCB_OFF",
  "SCB_OFF_R",
  "EMP",
  "INS_OFF",
  "INT_SUB"
)]

# Usage
usage <- india_pc2[, c(
  "DEP_PC",
  "CR_OUT_PC",
  "INS_DEN"
)]


# ============================================================
# 8.2 STAGE 1 PCA
# ============================================================

pca_pen <- prcomp(
  penetration,
  center = TRUE,
  scale. = TRUE
)

pca_avail <- prcomp(
  availability,
  center = TRUE,
  scale. = TRUE
)

pca_usage <- prcomp(
  usage,
  center = TRUE,
  scale. = TRUE
)


# ------------------------------------------------------------
# PCA summaries
# ------------------------------------------------------------

summary(pca_pen)
summary(pca_avail)
summary(pca_usage)


# ------------------------------------------------------------
# PCA loadings
# ------------------------------------------------------------

pca_pen$rotation
pca_avail$rotation
pca_usage$rotation


# ------------------------------------------------------------
# Stage 1 dimension scores
# ------------------------------------------------------------

stage1_scores <- data.frame(
  Penetration = pca_pen$x[, 1],
  Availability = pca_avail$x[, 1],
  Usage = pca_usage$x[, 1]
)

head(stage1_scores)


# ------------------------------------------------------------
# Correlation among Stage 1 dimension scores
# ------------------------------------------------------------

cor(stage1_scores)

corrplot(
  cor(stage1_scores),
  method = "color",
  type = "upper",
  addCoef.col = "black"
)


# ============================================================
# 8.3 STAGE 2 PCA
# ============================================================

pca_stage2 <- prcomp(
  stage1_scores,
  center = TRUE,
  scale. = TRUE
)

summary(pca_stage2)

pca_stage2$rotation


# ------------------------------------------------------------
# Stage 2 eigenvalues
# ------------------------------------------------------------

stage2_eigen <- pca_stage2$sdev^2

stage2_eigen


# ------------------------------------------------------------
# Stage 2 scree plot
# ------------------------------------------------------------

plot(
  stage2_eigen,
  type = "b",
  pch = 19,
  xlab = "Principal Component",
  ylab = "Eigenvalue",
  main = "Stage 2 Scree Plot"
)

abline(
  h = 1,
  lty = 2
)


# ============================================================
# 9. NATIONAL FINANCIAL INCLUSION INDEX
# ============================================================

# Raw Stage 2 PCA index
india_index <- data.frame(
  FY = india_data$FY,
  Raw_Index = pca_stage2$x[, 1]
)


# ------------------------------------------------------------
# 9.1 Translation transformation
# ------------------------------------------------------------

min_value <- min(
  india_index$Raw_Index,
  na.rm = TRUE
)

india_index$Shifted_Index <-
  india_index$Raw_Index +
  abs(min_value) +
  1


# ------------------------------------------------------------
# 9.2 Rebase to RBI FI-Index
# ------------------------------------------------------------

# Base year: 2016-17
# RBI FI-Index value: 43.4

base_value <- india_index$Shifted_Index[
  india_index$FY == "2016-17"
]

india_index$FII_Rebased <-
  (
    india_index$Shifted_Index /
      base_value
  ) * 43.4

india_index


# ============================================================
# 10. VALIDATION AGAINST RBI FI-INDEX
# ============================================================

rbi_fii <- data.frame(
  FY = c(
    "2016-17",
    "2017-18",
    "2018-19",
    "2019-20",
    "2020-21",
    "2021-22",
    "2022-23",
    "2023-24",
    "2024-25"
  ),
  RBI_FII = c(
    43.4,
    46.0,
    49.9,
    53.1,
    53.9,
    56.4,
    60.1,
    64.2,
    67.0
  )
)


# ------------------------------------------------------------
# 10.1 Comparison dataset
# ------------------------------------------------------------

compare <- india_index %>%
  filter(
    FY %in% rbi_fii$FY
  ) %>%
  left_join(
    rbi_fii,
    by = "FY"
  )

compare


# ------------------------------------------------------------
# 10.2 Correlation analysis
# ------------------------------------------------------------

pearson_r <- cor(
  compare$FII_Rebased,
  compare$RBI_FII,
  method = "pearson"
)

spearman_r <- cor(
  compare$FII_Rebased,
  compare$RBI_FII,
  method = "spearman"
)

pearson_r
spearman_r


# ------------------------------------------------------------
# 10.3 PCA index vs RBI FI-Index
# ------------------------------------------------------------

ggplot(
  compare,
  aes(
    x = FY,
    group = 1
  )
) +
  
  geom_line(
    aes(
      y = RBI_FII,
      colour = "RBI FI-Index"
    ),
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(
      y = RBI_FII,
      colour = "RBI FI-Index"
    ),
    size = 3
  ) +
  
  geom_line(
    aes(
      y = FII_Rebased,
      colour = "PCA-Based Index"
    ),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(
      y = FII_Rebased,
      colour = "PCA-Based Index"
    ),
    size = 3
  ) +
  
  scale_colour_manual(
    values = c(
      "RBI FI-Index" = "#1F4E79",
      "PCA-Based Index" = "#C00000"
    )
  ) +
  
  labs(
    title = "PCA-Based Financial Inclusion Index and RBI FI-Index",
    subtitle = "Base Year: 2016-17 = 43.4",
    x = "Financial Year",
    y = "Index Value",
    colour = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )


# ============================================================
# 11. SENSITIVITY ANALYSIS
# Leave-One-Variable-Out PCA
# ============================================================

variables <- colnames(india_pc2)

sensitivity_results <- data.frame(
  Removed_Variable = character(),
  Pearson_Correlation = numeric(),
  Spearman_Correlation = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(variables)) {
  
  # Remove one variable
  temp_data <- india_pc2[, -i]
  
  # Re-estimate PCA
  temp_pca <- prcomp(
    temp_data,
    center = TRUE,
    scale. = TRUE
  )
  
  # Construct alternative index
  temp_index <- temp_pca$x[, 1]
  
  # Translation
  temp_index <-
    temp_index +
    abs(min(temp_index)) +
    1
  
  # Rebase
  temp_base <- temp_index[
    india_data$FY == "2016-17"
  ]
  
  temp_index <-
    (temp_index / temp_base) *
    43.4
  
  # Compare with original index
  pearson <- cor(
    india_index$FII_Rebased,
    temp_index,
    method = "pearson"
  )
  
  spearman <- cor(
    india_index$FII_Rebased,
    temp_index,
    method = "spearman"
  )
  
  sensitivity_results <- rbind(
    sensitivity_results,
    data.frame(
      Removed_Variable = variables[i],
      Pearson_Correlation = pearson,
      Spearman_Correlation = spearman
    )
  )
}

sensitivity_results


# ------------------------------------------------------------
# 11.1 Sensitivity plot
# ------------------------------------------------------------

ggplot(
  sensitivity_results,
  aes(
    x = reorder(
      Removed_Variable,
      Pearson_Correlation
    ),
    y = Pearson_Correlation
  )
) +
  
  geom_col(
    fill = "steelblue"
  ) +
  
  coord_flip() +
  
  coord_cartesian(
    ylim = c(0.99, 1)
  ) +
  
  labs(
    title = "Sensitivity Analysis",
    subtitle = "Leave-One-Variable-Out PCA",
    x = "Variable Removed",
    y = "Correlation with Original Index"
  ) +
  
  theme_minimal()


# ------------------------------------------------------------
# 11.2 Final PCA loadings
# ------------------------------------------------------------

pca_pen$rotation[, 1]
pca_avail$rotation[, 1]
pca_usage$rotation[, 1]
pca_stage2$rotation[, 1]


# ============================================================
# 12. STATE-LEVEL FINANCIAL INCLUSION INDEX
# ============================================================

panel <- read_excel(
  data_file,
  sheet = "STATE_DATA"
)

str(panel)


# ------------------------------------------------------------
# 12.1 Select state-level indicators
# ------------------------------------------------------------

state_pc <- panel[, c(
  "DEP_PC",
  "CR_OUT_PC",
  "INS_DEN",
  "SCB_OFF",
  "DEP_ACC",
  "SCB_OFF_R",
  "EMP",
  "INS_OFF",
  "INT_SUB",
  "CR_ACC"
)]


# ------------------------------------------------------------
# 12.2 Standardize state-level panel
# ------------------------------------------------------------

panel_mean <- colMeans(
  state_pc,
  na.rm = TRUE
)

panel_sd <- apply(
  state_pc,
  2,
  sd,
  na.rm = TRUE
)

panel_z <- panel

vars <- colnames(state_pc)

for (v in vars) {
  
  panel_z[[v]] <-
    (
      panel[[v]] -
        panel_mean[v]
    ) /
    panel_sd[v]
}


# ============================================================
# 13. STATE-LEVEL STAGE 1 SCORES
# ============================================================

# ------------------------------------------------------------
# 13.1 Penetration
# ------------------------------------------------------------

pen_load <- pca_pen$rotation[, 1]

panel_z$Penetration <-
  as.matrix(
    panel_z[, c(
      "DEP_ACC",
      "CR_ACC"
    )]
  ) %*%
  pen_load


# ------------------------------------------------------------
# 13.2 Availability
# ------------------------------------------------------------

avail_load <- pca_avail$rotation[, 1]

panel_z$Availability <-
  as.matrix(
    panel_z[, c(
      "SCB_OFF",
      "SCB_OFF_R",
      "EMP",
      "INS_OFF",
      "INT_SUB"
    )]
  ) %*%
  avail_load


# ------------------------------------------------------------
# 13.3 Usage
# ------------------------------------------------------------

usage_load <- pca_usage$rotation[, 1]

panel_z$Usage <-
  as.matrix(
    panel_z[, c(
      "DEP_PC",
      "CR_OUT_PC",
      "INS_DEN"
    )]
  ) %*%
  usage_load


head(panel_z)


# ============================================================
# 14. STATE-LEVEL STAGE 2 SCORE
# ============================================================

stage2_load <- pca_stage2$rotation[, 1]

stage2_load


# ------------------------------------------------------------
# 14.1 Final raw state-level index
# ------------------------------------------------------------

panel_z$Raw_Index <-
  as.matrix(
    panel_z[, c(
      "Penetration",
      "Availability",
      "Usage"
    )]
  ) %*%
  stage2_load


# ------------------------------------------------------------
# 14.2 Translation transformation
# ------------------------------------------------------------

min_value <- min(
  panel_z$Raw_Index,
  na.rm = TRUE
)

panel_z$State_Index <-
  panel_z$Raw_Index +
  abs(min_value) +
  1


panel_z$State_Index


# ------------------------------------------------------------
# 14.3 Export state-level index
# ------------------------------------------------------------

write.csv(
  panel_z,
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/state_2stage_PCA.csv",
  row.names = FALSE
)


# ============================================================
# 15. NATIONAL DIMENSIONAL ANALYSIS
# ============================================================

india_dimension <- data.frame(
  FY = india_data$FY,
  Penetration = stage1_scores$Penetration,
  Availability = stage1_scores$Availability,
  Usage = stage1_scores$Usage
)

india_dimension_long <-
  india_dimension %>%
  pivot_longer(
    cols = c(
      Penetration,
      Availability,
      Usage
    ),
    names_to = "Dimension",
    values_to = "Score"
  )


# ------------------------------------------------------------
# 15.1 Dimension trends
# ------------------------------------------------------------

ggplot(
  india_dimension_long,
  aes(
    x = FY,
    y = Score,
    colour = Dimension,
    group = Dimension
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2.6
  ) +
  
  scale_colour_manual(
    values = c(
      Availability = "#9FAFCA",
      Penetration = "#6D80A5",
      Usage = "#1F3358"
    )
  ) +
  
  labs(
    title = "Performance Across the Three Dimensions of Financial Inclusion",
    x = "Financial Year",
    y = "Dimension Score"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )


# ------------------------------------------------------------
# 15.2 Compare 2016-17 and 2024-25
# ------------------------------------------------------------

comparison <- india_dimension %>%
  filter(
    FY %in% c(
      "2016-17",
      "2024-25"
    )
  )

comparison


growth <- data.frame(
  Dimension = c(
    "Penetration",
    "Availability",
    "Usage"
  ),
  `2016-17` = as.numeric(
    comparison[1, 2:4]
  ),
  `2024-25` = as.numeric(
    comparison[2, 2:4]
  ),
  check.names = FALSE
)

growth$Change <-
  growth$`2024-25` -
  growth$`2016-17`

growth

# ============================================================
# 16. REGIONAL ANALYSIS
# ============================================================


# ============================================================
# 16.1 REGION CLASSIFICATION
# ============================================================

panel_z$Region <- case_when(
  
  panel_z$State %in% c(
    "Bihar",
    "Jharkhand",
    "Odisha",
    "West Bengal",
    "Sikkim"
  ) ~ "Eastern",
  
  panel_z$State %in% c(
    "Chhattisgarh",
    "Madhya Pradesh",
    "Uttar Pradesh",
    "Uttarakhand"
  ) ~ "Central",
  
  panel_z$State %in% c(
    "Goa",
    "Gujarat",
    "Maharashtra"
  ) ~ "Western",
  
  panel_z$State %in% c(
    "Andhra Pradesh",
    "Karnataka",
    "Kerala",
    "Tamil Nadu",
    "Telangana"
  ) ~ "Southern",
  
  panel_z$State %in% c(
    "Haryana",
    "Himachal Pradesh",
    "Punjab",
    "Rajasthan"
  ) ~ "Northern",
  
  panel_z$State %in% c(
    "Arunachal Pradesh",
    "Assam",
    "Manipur",
    "Meghalaya",
    "Mizoram",
    "Nagaland",
    "Tripura"
  ) ~ "North-Eastern",
  
  TRUE ~ NA_character_
)

table(panel_z$Region)


# ============================================================
# 16.2 REGIONAL SUMMARY
# ============================================================

region_summary <-
  panel_z %>%
  group_by(Year, Region) %>%
  summarise(
    Mean_FII = mean(State_Index),
    Median_FII = median(State_Index),
    SD_FII = sd(State_Index),
    Min_FII = min(State_Index),
    Max_FII = max(State_Index),
    States = n(),
    .groups = "drop"
  )

region_summary


# ------------------------------------------------------------
# Regional growth between 2016-17 and 2024-25
# ------------------------------------------------------------

regional_growth <-
  region_summary %>%
  filter(
    Year %in% c(
      "2016-17",
      "2024-25"
    )
  ) %>%
  select(
    Year,
    Region,
    Mean_FII
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = Mean_FII
  ) %>%
  mutate(
    Growth = `2024-25` - `2016-17`
  ) %>%
  arrange(
    desc(Growth)
  )

regional_growth


# ============================================================
# 16.3 REGIONAL TRENDS
# ============================================================

ggplot(
  region_summary,
  aes(
    x = Year,
    y = Mean_FII,
    colour = Region,
    group = Region
  )
) +
  
  geom_line(
    linewidth = 1.3
  ) +
  
  geom_point(
    size = 3
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Regional Trends in Financial Inclusion",
    subtitle = "Two-Stage PCA-Based Index",
    x = "Financial Year",
    y = "Average Financial Inclusion Index",
    colour = "Region"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ============================================================
# 16.4 REGIONAL HEATMAP
# ============================================================

ggplot(
  region_summary,
  aes(
    x = Year,
    y = Region,
    fill = Mean_FII
  )
) +
  
  geom_tile(
    colour = "white"
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "#08306B"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Regional Financial Inclusion Heatmap",
    x = "Financial Year",
    y = NULL,
    fill = "Mean\nFII"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ============================================================
# 16.5 REGIONAL RANKING — 2024-25
# ============================================================

latest_region <-
  region_summary %>%
  filter(
    Year == "2024-25"
  ) %>%
  arrange(
    desc(Mean_FII)
  )

latest_region


ggplot(
  latest_region,
  aes(
    x = reorder(
      Region,
      Mean_FII
    ),
    y = Mean_FII,
    fill = Region
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  coord_flip() +
  
  geom_text(
    aes(
      label = round(
        Mean_FII,
        2
      )
    ),
    hjust = -0.15,
    fontface = "bold"
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  labs(
    title = "Regional Financial Inclusion Rankings (2024–25)",
    x = NULL,
    y = "Average Financial Inclusion Index"
  ) +
  
  theme(
    legend.position = "none"
  )


# ============================================================
# 17. REGIONAL DIMENSIONAL ANALYSIS
# ============================================================

# Average Penetration, Availability and Usage scores
# across regions and years.

region_dimension <-
  panel_z %>%
  group_by(
    Year,
    Region
  ) %>%
  summarise(
    Penetration = mean(
      Penetration
    ),
    Availability = mean(
      Availability
    ),
    Usage = mean(
      Usage
    ),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17.1 Dimension scores — 2024-25
# ------------------------------------------------------------

region_dimension_latest <-
  region_dimension %>%
  filter(
    Year == "2024-25"
  ) %>%
  pivot_longer(
    cols = c(
      Penetration,
      Availability,
      Usage
    ),
    names_to = "Dimension",
    values_to = "Score"
  )

ggplot(
  region_dimension_latest,
  aes(
    x = Region,
    y = Score,
    fill = Dimension
  )
) +
  
  geom_col(
    position = "dodge"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Regional Performance by Financial Inclusion Dimension (2024–25)",
    x = NULL,
    y = "Average Dimension Score"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ------------------------------------------------------------
# 17.2 Dimension trends by region
# ------------------------------------------------------------

region_dimension_long <-
  region_dimension %>%
  pivot_longer(
    cols = c(
      Penetration,
      Availability,
      Usage
    ),
    names_to = "Dimension",
    values_to = "Score"
  )


# Final regional dimension trend plot

region_palette <- c(
  "Western" = "#CC79A7",
  "Southern" = "#0072B2",
  "Northern" = "#009E73",
  "Central" = "#D55E00",
  "Eastern" = "#F0E442",
  "North-Eastern" = "#000000"
)

ggplot(
  region_dimension_long,
  aes(
    x = Year,
    y = Score,
    colour = Region,
    group = Region
  )
) +
  
  geom_line(
    linewidth = 1.1
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  facet_wrap(
    ~Dimension,
    scales = "free_y"
  ) +
  
  scale_colour_manual(
    values = region_palette
  ) +
  
  labs(
    title = "Regional Trends in Financial Inclusion Dimensions",
    x = "Financial Year",
    y = "Average PCA Dimension Score",
    colour = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold",
      size = 13
    ),
    legend.position = "right",
    legend.text = element_text(
      size = 11
    )
  )


# ============================================================
# 18. STATE-WISE FII TRENDS WITHIN REGIONS
# ============================================================

okabe_ito <- c(
  "#0072B2",
  "#D55E00",
  "#009E73",
  "#CC79A7",
  "#E69F00",
  "#56B4E9",
  "#000000",
  "#F0E442"
)


plot_region <- function(
    region_name
) {
  
  region_data <-
    panel_z %>%
    filter(
      Region == region_name
    )
  
  ggplot(
    region_data,
    aes(
      x = Year,
      y = State_Index,
      colour = State,
      group = State
    )
  ) +
    
    geom_line(
      linewidth = 1.2
    ) +
    
    geom_point(
      size = 2.6
    ) +
    
    scale_colour_manual(
      values = okabe_ito
    ) +
    
    labs(
      title = paste(
        region_name,
        "Region"
      ),
      x = "Financial Year",
      y = "Financial Inclusion Index",
      colour = "State"
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "right",
      legend.title = element_text(
        face = "bold"
      ),
      legend.text = element_text(
        size = 11
      )
    )
}


# Generate regional state-level plots

plot_region("Western")
plot_region("Southern")
plot_region("Northern")
plot_region("Central")
plot_region("Eastern")
plot_region("North-Eastern")


# ============================================================
# 19. STATISTICAL TESTS OF REGIONAL DIFFERENCES
# ============================================================

# Restrict analysis to 2024-25.

latest_data <-
  panel_z %>%
  filter(
    Year == "2024-25"
  )


# ------------------------------------------------------------
# 19.1 One-way ANOVA
# ------------------------------------------------------------

anova_model <-
  aov(
    State_Index ~ Region,
    data = latest_data
  )

summary(anova_model)


# ------------------------------------------------------------
# 19.2 Kruskal-Wallis test
# ------------------------------------------------------------

kruskal_result <-
  kruskal.test(
    State_Index ~ Region,
    data = latest_data
  )

kruskal_result


# ============================================================
# 20. WITHIN-REGION HETEROGENEITY
# ============================================================

region_dispersion <-
  panel_z %>%
  group_by(
    Year,
    Region
  ) %>%
  summarise(
    Mean = mean(
      State_Index
    ),
    SD = sd(
      State_Index
    ),
    CV = SD / Mean,
    Range = max(
      State_Index
    ) - min(
      State_Index
    ),
    .groups = "drop"
  )

region_dispersion


# ------------------------------------------------------------
# 20.1 Standard deviation
# ------------------------------------------------------------

ggplot(
  region_dispersion,
  aes(
    x = Year,
    y = SD,
    colour = Region,
    group = Region
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Within-Region Dispersion of Financial Inclusion",
    x = "Financial Year",
    y = "Standard Deviation",
    colour = "Region"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ------------------------------------------------------------
# 20.2 Coefficient of variation
# ------------------------------------------------------------

ggplot(
  region_dispersion,
  aes(
    x = Year,
    y = CV,
    colour = Region,
    group = Region
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Within-Region Relative Inequality in Financial Inclusion",
    x = "Financial Year",
    y = "Coefficient of Variation",
    colour = "Region"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ------------------------------------------------------------
# 20.3 Regional dispersion — 2024-25
# ------------------------------------------------------------

latest_dispersion <-
  region_dispersion %>%
  filter(
    Year == "2024-25"
  ) %>%
  arrange(
    desc(CV)
  )

latest_dispersion

# ============================================================
# 21. STATE RANKINGS
# ============================================================


# ============================================================
# 21.1 YEAR-WISE STATE RANKINGS
# ============================================================

panel_z <- panel_z %>%
  group_by(Year) %>%
  mutate(
    Rank_PCA_State = dense_rank(
      desc(State_Index)
    )
  ) %>%
  ungroup()


# ------------------------------------------------------------
# Full state-year ranking table
# ------------------------------------------------------------

state_rankings <- panel_z %>%
  select(
    State,
    Year,
    State_Index,
    Rank_PCA_State
  ) %>%
  arrange(
    Year,
    Rank_PCA_State
  )

print(
  state_rankings,
  n = Inf
)


# ============================================================
# 21.2 WIDE STATE INDEX TABLE
# ============================================================

index_wide <- panel_z %>%
  select(
    State,
    Year,
    State_Index
  ) %>%
  pivot_wider(
    names_from = State,
    values_from = State_Index
  )

print(index_wide)


# ============================================================
# 21.3 WIDE RAW INDEX TABLE
# ============================================================

raw_index_wide <- panel_z %>%
  select(
    State,
    Year,
    Raw_Index
  ) %>%
  pivot_wider(
    names_from = State,
    values_from = Raw_Index
  )

print(raw_index_wide)


# ============================================================
# 21.4 WIDE RANK TABLE
# ============================================================

rank_wide <- panel_z %>%
  select(
    State,
    Year,
    Rank_PCA_State
  ) %>%
  pivot_wider(
    names_from = State,
    values_from = Rank_PCA_State
  )

print(rank_wide)


# ------------------------------------------------------------
# Export ranking outputs
# ------------------------------------------------------------

write.csv(
  index_wide,
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/state_index_wide_2stage_PCA.csv",
  row.names = FALSE
)

write.csv(
  raw_index_wide,
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/state_raw_index_wide_2stage_PCA.csv",
  row.names = FALSE
)

write.csv(
  rank_wide,
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/state_rank_wide_2stage_PCA.csv",
  row.names = FALSE
)


# ============================================================
# 22. STATE RANKINGS — 2024-25
# ============================================================

latest_rank <- panel_z %>%
  filter(
    Year == "2024-25"
  ) %>%
  arrange(
    desc(State_Index)
  ) %>%
  mutate(
    Rank = row_number()
  )

latest_rank %>%
  select(
    Rank,
    State,
    State_Index
  )


# ============================================================
# 22.1 TOP 5 STATES — 2024-25
# ============================================================

top5_states <- latest_rank %>%
  slice_head(
    n = 5
  )

ggplot(
  top5_states,
  aes(
    x = reorder(
      State,
      State_Index
    ),
    y = State_Index
  )
) +
  
  geom_col(
    fill = "forestgreen"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Top 5 States by Financial Inclusion Index (2024–25)",
    x = NULL,
    y = "Financial Inclusion Index"
  ) +
  
  theme_minimal(
    base_size = 12
  )


# ============================================================
# 22.2 BOTTOM 5 STATES — 2024-25
# ============================================================

bottom5_states <- latest_rank %>%
  slice_tail(
    n = 5
  )

ggplot(
  bottom5_states,
  aes(
    x = reorder(
      State,
      -State_Index
    ),
    y = State_Index
  )
) +
  
  geom_col(
    fill = "firebrick"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Bottom 5 States by Financial Inclusion Index (2024–25)",
    x = NULL,
    y = "Financial Inclusion Index"
  ) +
  
  theme_minimal(
    base_size = 12
  )


# ============================================================
# 22.3 STATE-WISE FII TRENDS
# ============================================================

ggplot(
  panel_z,
  aes(
    x = Year,
    y = State_Index,
    group = State,
    colour = State
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "none"
  ) +
  
  labs(
    title = "State-wise Financial Inclusion Index (2016–17 to 2024–25)",
    x = "Financial Year",
    y = "Financial Inclusion Index"
  )


# ============================================================
# 22.4 STATE-WISE INDEX HEATMAP — 2024-25
# ============================================================

latest_rank <- latest_rank %>%
  mutate(
    StateRank = paste0(
      Rank,
      ". ",
      State
    )
  )

ggplot(
  latest_rank,
  aes(
    x = 1,
    y = reorder(
      StateRank,
      State_Index
    ),
    fill = State_Index
  )
) +
  
  geom_tile(
    colour = "grey95",
    linewidth = 0.4
  ) +
  
  scale_fill_viridis_c(
    option = "C"
  ) +
  
  scale_x_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  labs(
    title = "State-wise Financial Inclusion Index (2024–25)",
    x = NULL,
    y = "State",
    fill = "Index"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 18
    )
  )


# ============================================================
# 23. TOP 5 AND BOTTOM 5 STATES — COMBINED
# ============================================================

top_bottom_5 <- bind_rows(
  
  latest_rank %>%
    slice_head(n = 5) %>%
    mutate(
      Group = "Top 5"
    ),
  
  latest_rank %>%
    slice_tail(n = 5) %>%
    mutate(
      Group = "Bottom 5"
    )
  
)


ggplot(
  top_bottom_5,
  aes(
    x = State_Index,
    y = reorder(
      State,
      State_Index
    ),
    fill = Group
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = round(
        State_Index,
        2
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  
  facet_grid(
    Group ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  scale_fill_manual(
    values = c(
      "Top 5" = "forestgreen",
      "Bottom 5" = "firebrick"
    )
  ) +
  
  labs(
    title = "Top 5 and Bottom 5 States by Financial Inclusion Index (2024–25)",
    x = "Financial Inclusion Index",
    y = NULL,
    fill = NULL
  ) +
  
  theme_minimal(
    base_size = 10
  ) +
  
  theme(
    legend.position = "none",
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    plot.title = element_text(
      face = "bold"
    )
  )


# ============================================================
# 24. RANK MOBILITY
# ============================================================

# ------------------------------------------------------------
# Rankings in 2016-17
# ------------------------------------------------------------

rank2017 <- panel_z %>%
  filter(
    Year == "2016-17"
  ) %>%
  arrange(
    desc(State_Index)
  ) %>%
  mutate(
    Rank2017 = row_number()
  ) %>%
  select(
    State,
    Rank2017,
    Index2017 = State_Index
  )


# ------------------------------------------------------------
# Rankings in 2024-25
# ------------------------------------------------------------

rank2025 <- panel_z %>%
  filter(
    Year == "2024-25"
  ) %>%
  arrange(
    desc(State_Index)
  ) %>%
  mutate(
    Rank2025 = row_number()
  ) %>%
  select(
    State,
    Rank2025,
    Index2025 = State_Index
  )


# ------------------------------------------------------------
# Merge rankings
# ------------------------------------------------------------

rank_mobility <- left_join(
  rank2017,
  rank2025,
  by = "State"
) %>%
  mutate(
    Rank_Change =
      Rank2017 - Rank2025
  )

rank_mobility


# ============================================================
# 24.1 LARGEST RANK IMPROVEMENTS
# ============================================================

biggest_improvements <- rank_mobility %>%
  arrange(
    desc(Rank_Change)
  ) %>%
  slice_head(
    n = 10
  )

biggest_improvements


# ============================================================
# 24.2 LARGEST RANK DECLINES
# ============================================================

biggest_declines <- rank_mobility %>%
  arrange(
    Rank_Change
  ) %>%
  slice_head(
    n = 10
  )

biggest_declines


# ============================================================
# 25. RANK PERSISTENCE
# ============================================================

# ------------------------------------------------------------
# Spearman rank correlation
# ------------------------------------------------------------

spearman_rank_test <- cor.test(
  rank_mobility$Rank2017,
  rank_mobility$Rank2025,
  method = "spearman"
)

spearman_rank_test


# ------------------------------------------------------------
# Wilcoxon signed-rank test
# ------------------------------------------------------------

wilcoxon_rank_test <- wilcox.test(
  rank_mobility$Rank2017,
  rank_mobility$Rank2025,
  paired = TRUE,
  exact = FALSE
)

wilcoxon_rank_test


# ============================================================
# 26. RANK MOBILITY BAR CHART
# ============================================================

ggplot(
  rank_mobility,
  aes(
    x = reorder(
      State,
      Rank_Change
    ),
    y = Rank_Change,
    fill = Rank_Change > 0
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick"
    ),
    guide = "none"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  
  labs(
    title = "Change in State Rankings (2016–17 to 2024–25)",
    x = NULL,
    y = "Change in Rank"
  ) +
  
  theme_minimal(
    base_size = 14
  )


# ============================================================
# 27. RANK MOBILITY — DUMBBELL PLOT
# ============================================================

ggplot(
  rank_mobility
) +
  
  geom_segment(
    aes(
      y = reorder(
        State,
        Rank2017
      ),
      yend = reorder(
        State,
        Rank2017
      ),
      x = Rank2017,
      xend = Rank2025
    ),
    colour = "grey70",
    linewidth = 0.8
  ) +
  
  geom_point(
    aes(
      x = Rank2017,
      y = reorder(
        State,
        Rank2017
      )
    ),
    colour = "steelblue",
    size = 3
  ) +
  
  geom_point(
    aes(
      x = Rank2025,
      y = reorder(
        State,
        Rank2017
      )
    ),
    colour = "firebrick",
    size = 3
  ) +
  
  scale_x_reverse(
    breaks = seq(
      1,
      28,
      by = 1
    )
  ) +
  
  labs(
    title = "State Rank Mobility (2016–17 vs 2024–25)",
    subtitle = "Blue: 2016–17 | Red: 2024–25",
    x = "Rank",
    y = "State"
  ) +
  
  theme_minimal(
    base_size = 12
  )


# ============================================================
# 28. TOP 5 IMPROVERS AND DECLINERS
# ============================================================

rank_change <- rank_mobility %>%
  mutate(
    Rank_Change =
      Rank2017 - Rank2025
  )


# ------------------------------------------------------------
# Select top five improvers
# ------------------------------------------------------------

top5_improved <- rank_change %>%
  arrange(
    desc(Rank_Change)
  ) %>%
  slice_head(
    n = 5
  ) %>%
  mutate(
    Group = "Most Improved"
  )


# ------------------------------------------------------------
# Select largest five declines
# ------------------------------------------------------------

bottom5_declined <- rank_change %>%
  arrange(
    Rank_Change
  ) %>%
  slice_head(
    n = 5
  ) %>%
  mutate(
    Group = "Largest Declines"
  )


plot_data <- bind_rows(
  top5_improved,
  bottom5_declined
) %>%
  arrange(
    desc(Group),
    desc(Rank_Change)
  ) %>%
  mutate(
    State = factor(
      State,
      levels = rev(State)
    )
  )


# ------------------------------------------------------------
# Final mobility plot
# ------------------------------------------------------------

ggplot(
  plot_data
) +
  
  geom_segment(
    aes(
      y = State,
      yend = State,
      x = Rank2017,
      xend = Rank2025
    ),
    colour = "grey70",
    linewidth = 1
  ) +
  
  geom_point(
    aes(
      x = Rank2017,
      y = State
    ),
    colour = "#4C78A8",
    size = 4
  ) +
  
  geom_point(
    aes(
      x = Rank2025,
      y = State
    ),
    colour = "#B22222",
    size = 4
  ) +
  
  geom_text(
    aes(
      x = ifelse(
        Rank_Change > 0,
        Rank2025 - 0.8,
        Rank2025 + 0.8
      ),
      y = State,
      label = ifelse(
        Rank_Change > 0,
        paste0(
          "+",
          Rank_Change
        ),
        Rank_Change
      )
    ),
    fontface = "bold",
    colour = "black",
    size = 4
  ) +
  
  scale_x_reverse(
    breaks = seq(
      1,
      28,
      by = 2
    ),
    limits = c(
      28.5,
      0.5
    )
  ) +
  
  facet_grid(
    Group ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  labs(
    title = "Largest Changes in State Financial Inclusion Rankings",
    subtitle = paste(
      "Top Five Improvements and Largest Five Declines",
      "(2016–17 to 2024–25)\n",
      "Blue = 2016–17   |   Red = 2024–25"
    ),
    x = "Rank",
    y = NULL
  ) +
  
  theme_classic(
    base_size = 13
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      colour = "#183A6B"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 11,
      colour = "#333333"
    ),
    axis.text.x = element_text(
      size = 11,
      colour = "#333333"
    ),
    panel.grid.major.x = element_line(
      colour = "#E6E6E6"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      20,
      20,
      20,
      20
    )
  )

# ============================================================
# 29. DIMENSIONAL RANK MOBILITY
# ============================================================


# ------------------------------------------------------------
# Function to calculate rank mobility for a dimension
# ------------------------------------------------------------

calculate_rank_mobility <- function(data, dimension) {
  
  rank_start <- data %>%
    filter(
      Year == "2016-17"
    ) %>%
    arrange(
      desc(.data[[dimension]])
    ) %>%
    mutate(
      Rank2017 = row_number()
    ) %>%
    select(
      State,
      Rank2017
    )
  
  
  rank_end <- data %>%
    filter(
      Year == "2024-25"
    ) %>%
    arrange(
      desc(.data[[dimension]])
    ) %>%
    mutate(
      Rank2025 = row_number()
    ) %>%
    select(
      State,
      Rank2025
    )
  
  
  rank_start %>%
    left_join(
      rank_end,
      by = "State"
    ) %>%
    mutate(
      Rank_Change =
        Rank2017 - Rank2025
    )
}


# ============================================================
# 29.1 PENETRATION RANK MOBILITY
# ============================================================

pen_rank <- calculate_rank_mobility(
  panel_z,
  "Penetration"
)

pen_rank


# Largest improvements in penetration ranking

pen_rank %>%
  arrange(
    desc(Rank_Change)
  ) %>%
  slice_head(
    n = 10
  )


# Penetration rank mobility plot

ggplot(
  pen_rank,
  aes(
    x = reorder(
      State,
      Rank_Change
    ),
    y = Rank_Change,
    fill = Rank_Change > 0
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick"
    ),
    guide = "none"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Penetration Rank Mobility",
    x = NULL,
    y = "Change in Rank"
  )


# ============================================================
# 29.2 AVAILABILITY RANK MOBILITY
# ============================================================

avail_rank <- calculate_rank_mobility(
  panel_z,
  "Availability"
)

avail_rank


# Availability rank mobility plot

ggplot(
  avail_rank,
  aes(
    x = reorder(
      State,
      Rank_Change
    ),
    y = Rank_Change,
    fill = Rank_Change > 0
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick"
    ),
    guide = "none"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Availability Rank Mobility",
    x = NULL,
    y = "Change in Rank"
  )


# ============================================================
# 29.3 USAGE RANK MOBILITY
# ============================================================

usage_rank <- calculate_rank_mobility(
  panel_z,
  "Usage"
)

usage_rank


# Usage rank mobility plot

ggplot(
  usage_rank,
  aes(
    x = reorder(
      State,
      Rank_Change
    ),
    y = Rank_Change,
    fill = Rank_Change > 0
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick"
    ),
    guide = "none"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  labs(
    title = "Usage Rank Mobility",
    x = NULL,
    y = "Change in Rank"
  )

# ============================================================
# 30. DIMENSIONAL GROWTH AND DRIVER ANALYSIS
#     2016-17 to 2024-25
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ============================================================
# 30.1 GROWTH OF THE THREE DIMENSIONS
# ============================================================

driver_table <- panel_z %>%
  
  # Keep only the beginning and ending years
  filter(
    Year %in% c(
      "2016-17",
      "2024-25"
    )
  ) %>%
  
  mutate(
    Year = case_when(
      Year == "2016-17" ~ "Y2017",
      Year == "2024-25" ~ "Y2025"
    )
  ) %>%
  
  select(
    State,
    Year,
    Penetration,
    Availability,
    Usage,
    State_Index
  ) %>%
  
  pivot_wider(
    names_from = Year,
    values_from = c(
      Penetration,
      Availability,
      Usage,
      State_Index
    )
  ) %>%
  
  mutate(
    
    # --------------------------------------------------------
    # Absolute growth
    # --------------------------------------------------------
    
    Penetration_Growth =
      Penetration_Y2025 -
      Penetration_Y2017,
    
    Availability_Growth =
      Availability_Y2025 -
      Availability_Y2017,
    
    Usage_Growth =
      Usage_Y2025 -
      Usage_Y2017,
    
    FII_Growth =
      State_Index_Y2025 -
      State_Index_Y2017,
    
    
    # --------------------------------------------------------
    # Percentage growth
    # --------------------------------------------------------
    
    Penetration_Growth_Percent =
      100 *
      (
        Penetration_Y2025 -
          Penetration_Y2017
      ) /
      abs(Penetration_Y2017),
    
    Availability_Growth_Percent =
      100 *
      (
        Availability_Y2025 -
          Availability_Y2017
      ) /
      abs(Availability_Y2017),
    
    Usage_Growth_Percent =
      100 *
      (
        Usage_Y2025 -
          Usage_Y2017
      ) /
      abs(Usage_Y2017),
    
    FII_Growth_Percent =
      100 *
      (
        State_Index_Y2025 -
          State_Index_Y2017
      ) /
      State_Index_Y2017
  )


# ------------------------------------------------------------
# Check raw growth table
# ------------------------------------------------------------

driver_table %>%
  select(
    State,
    Penetration_Growth,
    Availability_Growth,
    Usage_Growth,
    FII_Growth
  ) %>%
  arrange(
    desc(FII_Growth)
  )


# ------------------------------------------------------------
# FII growth summary
# ------------------------------------------------------------

driver_table %>%
  select(
    State,
    State_Index_Y2017,
    State_Index_Y2025,
    FII_Growth,
    FII_Growth_Percent
  ) %>%
  arrange(
    desc(FII_Growth)
  )


# ============================================================
# 30.2 FII GROWTH PLOT
# ============================================================

ggplot(
  driver_table,
  aes(
    x = reorder(
      State,
      FII_Growth
    ),
    y = FII_Growth
  )
) +
  
  geom_col(
    fill = "steelblue"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Change in Financial Inclusion Index",
    x = NULL,
    y = "Absolute Growth in FII"
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    )
  )


# ============================================================
# 30.3 STAGE 2 CONTRIBUTION DECOMPOSITION
# ============================================================

# Extract Stage 2 PCA loadings

w_pen <- unname(
  stage2_load["Penetration"]
)

w_avail <- unname(
  stage2_load["Availability"]
)

w_usage <- unname(
  stage2_load["Usage"]
)


# ------------------------------------------------------------
# Calculate weighted dimensional contributions
# ------------------------------------------------------------

driver_table <- driver_table %>%
  
  mutate(
    
    Pen_Contribution =
      w_pen *
      Penetration_Growth,
    
    Avail_Contribution =
      w_avail *
      Availability_Growth,
    
    Usage_Contribution =
      w_usage *
      Usage_Growth
  )


# ============================================================
# 30.4 TOTAL CONTRIBUTION
# ============================================================

driver_table <- driver_table %>%
  
  mutate(
    
    Total_Contribution =
      Pen_Contribution +
      Avail_Contribution +
      Usage_Contribution
  )


# ============================================================
# 30.5 VERIFY THE DECOMPOSITION
# ============================================================

decomposition_check <- driver_table %>%
  
  mutate(
    Difference =
      Total_Contribution -
      FII_Growth
  ) %>%
  
  select(
    State,
    FII_Growth,
    Total_Contribution,
    Difference
  )

decomposition_check


# ============================================================
# 30.6 PERCENTAGE CONTRIBUTION
# ============================================================

driver_table <- driver_table %>%
  
  mutate(
    
    Pen_Percent =
      100 *
      Pen_Contribution /
      Total_Contribution,
    
    Avail_Percent =
      100 *
      Avail_Contribution /
      Total_Contribution,
    
    Usage_Percent =
      100 *
      Usage_Contribution /
      Total_Contribution
  )


# ============================================================
# 30.7 MAIN DRIVER
# ============================================================

driver_table <- driver_table %>%
  
  rowwise() %>%
  
  mutate(
    
    Main_Driver = c(
      "Penetration",
      "Availability",
      "Usage"
    )[
      which.max(
        c(
          Pen_Percent,
          Avail_Percent,
          Usage_Percent
        )
      )
    ]
  ) %>%
  
  ungroup()


# ============================================================
# 30.8 FINAL DRIVER TABLE
# ============================================================

driver_results <- driver_table %>%
  
  select(
    State,
    FII_Growth,
    Pen_Percent,
    Avail_Percent,
    Usage_Percent,
    Main_Driver
  ) %>%
  
  arrange(
    desc(FII_Growth)
  )


# ------------------------------------------------------------
# View complete results
# ------------------------------------------------------------

driver_results


# ============================================================
# 30.9 TOP 10 STATES BY FII GROWTH
# ============================================================

top10_improvers <- driver_results %>%
  
  slice_head(
    n = 10
  )

top10_improvers


# ============================================================
# 30.10 LOWEST 10 STATES BY FII GROWTH
# ============================================================

lowest10_growth <- driver_results %>%
  
  arrange(
    FII_Growth
  ) %>%
  
  slice_head(
    n = 10
  )

lowest10_growth

# =========================================================
# 31. CONTRIBUTION OF FINANCIAL INCLUSION DIMENSIONS
#     TO STATE-LEVEL IMPROVEMENT
#     2016-17 to 2024-25
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ---------------------------------------------------------
# 1. PREPARE DATA
# ---------------------------------------------------------

plot_data <- driver_results %>%
  
  mutate(
    FII_Growth = as.numeric(FII_Growth),
    Pen_Percent = as.numeric(Pen_Percent),
    Avail_Percent = as.numeric(Avail_Percent),
    Usage_Percent = as.numeric(Usage_Percent)
  )


# ---------------------------------------------------------
# 2. ORDER STATES BY FII GROWTH
#    Highest FII growth first
# ---------------------------------------------------------

state_order <- plot_data %>%
  
  arrange(
    desc(FII_Growth)
  ) %>%
  
  pull(State)


# ---------------------------------------------------------
# 3. CREATE CLEAN DOMINANT DRIVER LABEL
# ---------------------------------------------------------

plot_data <- plot_data %>%
  
  mutate(
    
    Dominant_Driver = case_when(
      
      Main_Driver %in% c(
        "Penetration",
        "Penetration-led"
      ) ~ "Penetration",
      
      Main_Driver %in% c(
        "Availability",
        "Availability-led"
      ) ~ "Availability",
      
      Main_Driver %in% c(
        "Usage",
        "Usage-led"
      ) ~ "Usage",
      
      TRUE ~ NA_character_
    )
  )


# ---------------------------------------------------------
# 4. CREATE STACKED-BAR DATA
# ---------------------------------------------------------

driver_plot <- plot_data %>%
  
  select(
    State,
    Dominant_Driver,
    Avail_Percent,
    Pen_Percent,
    Usage_Percent
  ) %>%
  
  pivot_longer(
    
    cols = c(
      Avail_Percent,
      Pen_Percent,
      Usage_Percent
    ),
    
    names_to = "Dimension",
    
    values_to = "Contribution"
  ) %>%
  
  mutate(
    
    State = factor(
      State,
      levels = rev(state_order)
    ),
    
    Dimension = factor(
      
      Dimension,
      
      levels = c(
        "Avail_Percent",
        "Pen_Percent",
        "Usage_Percent"
      ),
      
      labels = c(
        "Availability",
        "Penetration",
        "Usage"
      )
    )
  )


# ---------------------------------------------------------
# 5. LABEL DATA
# ---------------------------------------------------------

label_df <- plot_data %>%
  
  select(
    State,
    Dominant_Driver
  ) %>%
  
  mutate(
    State = factor(
      State,
      levels = rev(state_order)
    )
  )


# ---------------------------------------------------------
# 6. COLOUR PALETTE
# ---------------------------------------------------------

rbi_palette <- c(
  
  "Availability" = "#243B64",
  
  "Penetration" = "#7088B3",
  
  "Usage" = "#C8D2E4"
  
)


driver_palette <- c(
  
  "Availability" = "#243B64",
  
  "Penetration" = "#7088B3",
  
  "Usage" = "#8FA6CC"
  
)


# ---------------------------------------------------------
# 7. FINAL FIGURE
# ---------------------------------------------------------

figure_6_11 <- ggplot(
  
  driver_plot,
  
  aes(
    x = State,
    y = Contribution,
    fill = Dimension
  )
  
) +
  
  geom_col(
    width = 0.82,
    colour = "white",
    linewidth = 0.25
  ) +
  
  coord_flip(
    clip = "off"
  ) +
  
  
  # -------------------------------------------------------
# Dominant Driver labels
# -------------------------------------------------------

geom_text(
  
  data = label_df,
  
  aes(
    x = State,
    y = 103,
    label = Dominant_Driver,
    colour = Dominant_Driver
  ),
  
  inherit.aes = FALSE,
  
  hjust = 0,
  
  size = 3.6,
  
  fontface = "bold",
  
  show.legend = FALSE
) +
  
  
  # -------------------------------------------------------
# Dominant Driver heading
# -------------------------------------------------------

annotate(
  
  "text",
  
  x = length(state_order) + 1.6,
  
  y = 103,
  
  label = "Dominant Driver",
  
  fontface = "bold",
  
  size = 4,
  
  hjust = 0
) +
  
  
  # -------------------------------------------------------
# Bar colours
# -------------------------------------------------------

scale_fill_manual(
  
  values = rbi_palette,
  
  breaks = c(
    "Availability",
    "Penetration",
    "Usage"
  )
) +
  
  
  # -------------------------------------------------------
# Driver label colours
# -------------------------------------------------------

scale_colour_manual(
  
  values = driver_palette
) +
  
  
  # -------------------------------------------------------
# Contribution axis
# -------------------------------------------------------

scale_y_continuous(
  
  limits = c(
    0,
    115
  ),
  
  breaks = seq(
    0,
    100,
    20
  ),
  
  expand = c(
    0,
    0
  )
) +
  
  
  # -------------------------------------------------------
# Titles
# -------------------------------------------------------

labs(
  
  title =
    "Contribution of Financial Inclusion Dimensions to State-level Improvement\n(2016–17 to 2024–25)",
  
  x = NULL,
  
  y = "Contribution (%)",
  
  fill = "Contribution\nDimension"
) +
  
  
  # -------------------------------------------------------
# Theme
# -------------------------------------------------------

theme_classic(
  base_size = 13
) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 13,
      colour = "#183A6B",
      margin = margin(
        b = 12
      )
    ),
    
    axis.title.x = element_text(
      face = "bold",
      size = 13
    ),
    
    axis.title.y = element_blank(),
    
    axis.text.x = element_text(
      size = 11,
      colour = "#333333"
    ),
    
    axis.text.y = element_text(
      size = 11,
      colour = "#333333"
    ),
    
    panel.grid.major.x = element_line(
      colour = "#E6E6E6",
      linewidth = 0.45
    ),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 12
    ),
    
    legend.text = element_text(
      size = 11
    ),
    
    plot.margin = margin(
      20,
      130,
      20,
      20
    )
  )


# ---------------------------------------------------------
# 8. DISPLAY
# ---------------------------------------------------------

figure_6_11


# ---------------------------------------------------------
# 9. SAVE
# ---------------------------------------------------------

ggsave(
  
  filename = "Figure_6_11_Drivers_of_Improvement.png",
  
  plot = figure_6_11,
  
  width = 12,
  
  height = 7,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
)

#==================================================
# 32. INDIA CHOROPLETH MAP (2024-25)
#==================================================

library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(stringr)
library(grid)

#--------------------------------------------------
# Read India shapefile
#--------------------------------------------------

india_map <-
  st_read(
    "C:/Users/KIIT/Downloads/maps-master/maps-master/Survey-of-India-Index-Maps/StateBoundary/StateBoundary.shp",
    quiet = TRUE
  )

#--------------------------------------------------
# Latest FII values
#--------------------------------------------------

latest_rank <-
  panel_z %>%
  filter(Year == "2024-25")

#--------------------------------------------------
# Standardize state names
#--------------------------------------------------

india_map$state  <- toupper(india_map$state)
latest_rank$State <- toupper(latest_rank$State)

#--------------------------------------------------
# Join map with data
#--------------------------------------------------

india_2025 <-
  india_map %>%
  left_join(
    latest_rank,
    by = c("state" = "State")
  )

india_2025$state <- str_to_title(india_2025$state)

#--------------------------------------------------
# State abbreviations
#--------------------------------------------------

abbr <- data.frame(
  
  State = c(
    "Andhra Pradesh","Arunachal Pradesh","Assam","Bihar",
    "Chhattisgarh","Goa","Gujarat","Haryana",
    "Himachal Pradesh","Jharkhand","Karnataka","Kerala",
    "Madhya Pradesh","Maharashtra","Manipur","Meghalaya",
    "Mizoram","Nagaland","Odisha","Punjab",
    "Rajasthan","Sikkim","Tamil Nadu","Telangana",
    "Tripura","Uttar Pradesh","Uttarakhand","West Bengal"
  ),
  
  Abbr = c(
    "AP","AR","AS","BR","CG","GA","GJ","HR",
    "HP","JH","KA","KL","MP","MH","MN","ML",
    "MZ","NL","OD","PB","RJ","SK","TN","TS",
    "TR","UP","UK","WB"
  )
  
)

india_2025 <-
  india_2025 %>%
  left_join(
    abbr,
    by = c("state" = "State")
  )

#--------------------------------------------------
# Keep 28 states + Jammu & Kashmir
#--------------------------------------------------

india_2025 <-
  india_2025 %>%
  filter(
    !state %in% c(
      "Andaman & Nicobar",
      "Chandigarh",
      "Dadra & Nagar Haveli And Daman & Diu",
      "Delhi",
      "Ladakh",
      "Lakshadweep",
      "Puducherry"
    )
  )

#--------------------------------------------------
# Label coordinates
#--------------------------------------------------

label_points <- st_centroid(india_2025)

coords <-
  cbind(
    label_points,
    st_coordinates(label_points)
  )

coords <-
  coords %>%
  mutate(
    
    X = case_when(
      Abbr=="AR" ~ X+0.9,
      Abbr=="NL" ~ X+0.8,
      Abbr=="MN" ~ X+0.7,
      Abbr=="MZ" ~ X+0.8,
      Abbr=="TR" ~ X+0.7,
      Abbr=="ML" ~ X-0.5,
      Abbr=="SK" ~ X+0.4,
      TRUE ~ X
    ),
    
    Y = case_when(
      Abbr=="AR" ~ Y+0.6,
      Abbr=="NL" ~ Y+0.4,
      Abbr=="MN" ~ Y-0.2,
      Abbr=="MZ" ~ Y-0.5,
      Abbr=="TR" ~ Y-0.8,
      Abbr=="ML" ~ Y+0.2,
      Abbr=="SK" ~ Y+0.4,
      TRUE ~ Y
    )
    
  )


#--------------------------------------------------
# Choropleth
#--------------------------------------------------

ggplot(india_2025) +
  
  geom_sf(
    aes(fill = State_Index),
    colour = "grey95",
    linewidth = 0.35
  ) +
  
  geom_text(
    data = coords,
    aes(
      X,
      Y,
      label = Abbr
    ),
    colour = "white",
    fontface = "bold",
    size = 2.2
  ) +
  
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    limits = c(1, 9),
    oob = scales::squish,
    breaks = c(2,4,6,8),
    name = "Financial Inclusion Indicator"
  ) +
  
  guides(
    fill = guide_colourbar(
      barheight = unit(4, "cm"),   # Reduce height
      barwidth  = unit(0.6, "cm")  # Reduce width
    )
  ) +
  
  labs(
    title = "Financial Inclusion across Indian States (2024-25)",
    subtitle = "Two-Stage PCA-Based Financial Inclusion Indicator",
    caption = "Source: Author's calculations"
  ) +
  
  theme_void(base_size = 14) +
  
  theme(
    plot.title = element_text(face="bold", size=14, hjust=.5),
    plot.subtitle = element_text(size=10, hjust=.5),
    plot.caption = element_text(size=9, hjust=1),
    legend.title = element_text(face="bold", size=12),
    legend.text = element_text(size=11),
    legend.key.height = unit(2.5,"cm")
  )



#--------------------------------------------------
# Choropleth
#--------------------------------------------------

ggplot(india_2025) +
  
  geom_sf(
    aes(fill = State_Index),
    colour = "grey95",
    linewidth = 0.35
  ) +
  
  geom_text(
    data = coords,
    aes(
      X,
      Y,
      label = Abbr
    ),
    colour = "white",
    fontface = "bold",
    size = 2.2
  ) +
  
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    limits = c(1, 9),
    oob = scales::squish,
    breaks = c(1, 9),
    labels = c("Low", "High"),
    name = "Financial Inclusion\nIndicator"
  ) +
  
  guides(
    fill = guide_colourbar(
      barheight = unit(4, "cm"),
      barwidth  = unit(0.6, "cm"),
      ticks = FALSE,          # Removes tick marks
      frame.colour = "black"
    )
  ) +
  
  labs(
    title = "Financial Inclusion across Indian States (2024-25)",
    subtitle = "Two-Stage PCA-Based Financial Inclusion Indicator",
    caption = "Source: Author's calculations"
  ) +
  
  theme_void(base_size = 14) +
  
  theme(
    plot.title = element_text(face="bold", size=14, hjust=.5),
    plot.subtitle = element_text(size=10, hjust=.5),
    plot.caption = element_text(size=9, hjust=1),
    legend.title = element_text(face="bold", size=12),
    legend.text = element_text(size=11),
    legend.key.height = unit(2.5,"cm")
  )
ggsave(
  "India_FII_Choropleth_2024_25.png",
  width = 10,
  height = 7,
  dpi = 600,
  bg = "white"
)

# ============================================================
# 33. FINANCIAL INCLUSION DEVELOPMENT TYPOLOGY (FIDT)
# ============================================================


# ============================================================
# 33.1 PREPARE 2024-25 FII DATA
# ============================================================

fii_2025 <- panel_z %>%
  
  filter(
    Year == "2024-25"
  ) %>%
  
  transmute(
    State,
    State_Index = as.numeric(State_Index)
  )


# Merge FII values with driver analysis

typology <- driver_results %>%
  
  left_join(
    fii_2025,
    by = "State"
  )


# ============================================================
# 34. CLASSIFY STATES BY FII LEVEL
# ============================================================

# Quartile thresholds

fii_quartiles <- quantile(
  typology$State_Index,
  probs = c(
    0.25,
    0.75
  ),
  na.rm = TRUE
)


typology <- typology %>%
  
  mutate(
    
    FII_Level = case_when(
      
      State_Index < fii_quartiles[1] ~
        "Low",
      
      State_Index >= fii_quartiles[2] ~
        "High",
      
      TRUE ~
        "Medium"
    )
  )


# ============================================================
# 35. IDENTIFY DOMINANT DRIVER
# ============================================================

# Ensure contribution variables are numeric

typology <- typology %>%
  
  mutate(
    
    Pen_Percent =
      as.numeric(Pen_Percent),
    
    Avail_Percent =
      as.numeric(Avail_Percent),
    
    Usage_Percent =
      as.numeric(Usage_Percent)
  )


# Identify dimension with the largest contribution

typology <- typology %>%
  
  rowwise() %>%
  
  mutate(
    
    Driver = c(
      
      "Penetration",
      "Availability",
      "Usage"
      
    )[
      
      which.max(
        c(
          Pen_Percent,
          Avail_Percent,
          Usage_Percent
        )
      )
    ]
  ) %>%
  
  ungroup()


# ============================================================
# 36. CALCULATE DRIVER DOMINANCE
# ============================================================

typology <- typology %>%
  
  rowwise() %>%
  
  mutate(
    
    Largest =
      max(
        Pen_Percent,
        Avail_Percent,
        Usage_Percent
      ),
    
    Second =
      sort(
        c(
          Pen_Percent,
          Avail_Percent,
          Usage_Percent
        ),
        decreasing = TRUE
      )[2],
    
    Dominance_Gap =
      Largest - Second
  ) %>%
  
  ungroup()


# ============================================================
# 37. CLASSIFY DRIVER STRENGTH
# ============================================================

gap_quartiles <- quantile(
  
  typology$Dominance_Gap,
  
  probs = c(
    0.25,
    0.75
  ),
  
  na.rm = TRUE
)


typology <- typology %>%
  
  mutate(
    
    Driver_Strength = case_when(
      
      Dominance_Gap < gap_quartiles[1] ~
        "Weak",
      
      Dominance_Gap < gap_quartiles[2] ~
        "Moderate",
      
      TRUE ~
        "Strong"
    )
  )


# ============================================================
# 38. CREATE FII POLICY GROUPS
# ============================================================

typology <- typology %>%
  
  mutate(
    
    FII_Group = case_when(
      
      FII_Level == "High" ~
        "Financial Inclusion Leaders",
      
      FII_Level == "Medium" ~
        "Emerging Financial Inclusion States",
      
      TRUE ~
        "Financial Inclusion Priority States"
    )
  )


# ============================================================
# 39. CREATE FINAL TYPOLOGY LABEL
# ============================================================

typology <- typology %>%
  
  mutate(
    
    Final_Typology = paste(
      Driver_Strength,
      Driver
    )
  )


# ============================================================
# 40. FINAL TYPOLOGY TABLE
# ============================================================

typology_final <- typology %>%
  
  arrange(
    desc(State_Index)
  ) %>%
  
  select(
    
    State,
    State_Index,
    FII_Group,
    Driver,
    Driver_Strength,
    
    Pen_Percent,
    Avail_Percent,
    Usage_Percent,
    
    Dominance_Gap,
    Final_Typology
  )


typology_final


# ============================================================
# 41. POLICY MATRIX
# ============================================================

policy_matrix <- typology %>%
  
  group_by(
    FII_Group,
    Driver
  ) %>%
  
  summarise(
    
    Number_of_States =
      n(),
    
    States =
      paste(
        State,
        collapse = ", "
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    factor(
      FII_Group,
      levels = c(
        "Financial Inclusion Leaders",
        "Emerging Financial Inclusion States",
        "Financial Inclusion Priority States"
      )
    ),
    Driver
  )


policy_matrix


# ============================================================
# 42. SUMMARY STATISTICS BY DOMINANT DRIVER
# ============================================================

driver_summary <- typology %>%
  
  group_by(
    Driver
  ) %>%
  
  summarise(
    
    Mean_FII =
      mean(
        State_Index,
        na.rm = TRUE
      ),
    
    Mean_Pen =
      mean(
        Pen_Percent,
        na.rm = TRUE
      ),
    
    Mean_Avail =
      mean(
        Avail_Percent,
        na.rm = TRUE
      ),
    
    Mean_Usage =
      mean(
        Usage_Percent,
        na.rm = TRUE
      ),
    
    Mean_Dominance_Gap =
      mean(
        Dominance_Gap,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


driver_summary


# ============================================================
# 43. DIMENSIONAL CONTRIBUTION HEATMAP
# ============================================================

heatmap_data <- typology %>%
  
  arrange(
    desc(State_Index)
  ) %>%
  
  select(
    State,
    Pen_Percent,
    Avail_Percent,
    Usage_Percent
  ) %>%
  
  pivot_longer(
    
    cols = c(
      Pen_Percent,
      Avail_Percent,
      Usage_Percent
    ),
    
    names_to = "Dimension",
    
    values_to = "Contribution"
  )


# Order states by FII level

heatmap_data$State <- factor(
  
  heatmap_data$State,
  
  levels = rev(
    unique(
      heatmap_data$State
    )
  )
)


# Clean dimension labels

heatmap_data$Dimension <- factor(
  
  heatmap_data$Dimension,
  
  levels = c(
    "Pen_Percent",
    "Avail_Percent",
    "Usage_Percent"
  ),
  
  labels = c(
    "Penetration",
    "Availability",
    "Usage"
  )
)


# ------------------------------------------------------------
# Final heatmap
# ------------------------------------------------------------

ggplot(
  
  heatmap_data,
  
  aes(
    x = Dimension,
    y = State,
    fill = Contribution
  )
) +
  
  geom_tile(
    colour = "white",
    linewidth = 0.4
  ) +
  
  geom_text(
    
    aes(
      label = round(
        Contribution,
        1
      ),
      colour = Contribution <= 40
    ),
    
    size = 3
  ) +
  
  scale_colour_manual(
    
    values = c(
      "TRUE" = "white",
      "FALSE" = "black"
    ),
    
    guide = "none"
  ) +
  
  scale_fill_viridis_c(
    option = "viridis"
  ) +
  
  labs(
    
    title =
      "Dimension-wise Contribution (%) to Growth in the Financial Inclusion Index",
    
    x = NULL,
    
    y = NULL,
    
    fill = "Contribution (%)"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )


# ============================================================
# 44. POLICY INTERPRETATION
# ============================================================

typology <- typology %>%
  
  mutate(
    
    Policy_Implication = case_when(
      
      # ------------------------------------------------------
      # Penetration-driven states
      # ------------------------------------------------------
      
      Driver == "Penetration" &
        Driver_Strength == "Strong" ~
        "Expand financial usage and improve service accessibility to complement strong account penetration.",
      
      Driver == "Penetration" &
        Driver_Strength == "Moderate" ~
        "Strengthen usage while maintaining continued expansion in financial access.",
      
      Driver == "Penetration" &
        Driver_Strength == "Weak" ~
        "Maintain balanced improvements across all three dimensions to sustain inclusive progress.",
      
      
      # ------------------------------------------------------
      # Availability-driven states
      # ------------------------------------------------------
      
      Driver == "Availability" &
        Driver_Strength == "Strong" ~
        "Shift policy emphasis from expanding infrastructure towards increasing account ownership and active usage.",
      
      Driver == "Availability" &
        Driver_Strength == "Moderate" ~
        "Complement infrastructure expansion with greater financial outreach and service utilisation.",
      
      Driver == "Availability" &
        Driver_Strength == "Weak" ~
        "Continue balanced improvements across infrastructure, penetration and usage.",
      
      
      # ------------------------------------------------------
      # Usage-driven states
      # ------------------------------------------------------
      
      Driver == "Usage" &
        Driver_Strength == "Strong" ~
        "Consolidate digital financial ecosystems while strengthening physical outreach and financial access.",
      
      Driver == "Usage" &
        Driver_Strength == "Moderate" ~
        "Promote wider financial access alongside continued growth in financial usage.",
      
      Driver == "Usage" &
        Driver_Strength == "Weak" ~
        "Preserve balanced multidimensional progress while sustaining digital adoption."
    )
  )


# ============================================================
# 45. EXPANDED POLICY MATRIX
# ============================================================

policy_matrix <- typology %>%
  
  group_by(
    FII_Group,
    Driver,
    Driver_Strength,
    Policy_Implication
  ) %>%
  
  summarise(
    
    Number_of_States =
      n(),
    
    States =
      paste(
        State,
        collapse = ", "
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    
    factor(
      FII_Group,
      levels = c(
        "Financial Inclusion Leaders",
        "Emerging Financial Inclusion States",
        "Financial Inclusion Priority States"
      )
    ),
    
    Driver,
    Driver_Strength
  )


policy_matrix


# ============================================================
# 46. DRIVER STRENGTH DISTRIBUTION
# ============================================================

driver_strength_summary <- typology %>%
  
  count(
    Driver,
    Driver_Strength
  ) %>%
  
  pivot_wider(
    
    names_from = Driver_Strength,
    
    values_from = n,
    
    values_fill = 0
  )


driver_strength_summary
# ============================================================
# 47. EXPORT FIDT RESULTS
# ============================================================

write.csv(
  
  typology_final,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/financial_inclusion_development_typology.csv",
  
  row.names = FALSE
)


write.csv(
  
  policy_matrix,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/financial_inclusion_policy_matrix.csv",
  
  row.names = FALSE
)


write.csv(
  
  driver_summary,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/financial_inclusion_driver_summary.csv",
  
  row.names = FALSE
)


write.csv(
  
  driver_strength_summary,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/driver_strength_summary.csv",
  
  row.names = FALSE
)

#==========================================================
# 47. SPATIAL AUTOCORRELATION ANALYSIS
# GLOBAL MORAN'S I
#==========================================================

library(sf)
library(dplyr)
library(spdep)
library(purrr)
library(tibble)
library(ggplot2)

#==========================================================
# 1. PREPARE SPATIAL DATA (2024-25)
#==========================================================

india_2025$State_Index <-
  as.numeric(india_2025$State_Index)

india_moran <-
  
  india_2025 %>%
  
  filter(
    
    !is.na(State_Index)
    
  )

#==========================================================
# 2. CREATE QUEEN CONTIGUITY NEIGHBOUR STRUCTURE
#==========================================================

nb <-
  
  poly2nb(
    
    india_moran,
    
    queen = TRUE
    
  )

# Summary of neighbour structure

summary(nb)

# Number of neighbours for each state

neighbour_table <-
  
  tibble(
    
    State = india_moran$state,
    
    Number_of_Neighbours = card(nb)
    
  ) %>%
  
  arrange(Number_of_Neighbours)

neighbour_table

#==========================================================
# 3. CREATE SPATIAL WEIGHTS MATRIX
#==========================================================

listw <-
  
  nb2listw(
    
    nb,
    
    style = "W",
    
    zero.policy = TRUE
    
  )

#==========================================================
# 4. GLOBAL MORAN'S I (2024-25)
#==========================================================

global_moran <-
  
  moran.test(
    
    india_moran$State_Index,
    
    listw,
    
    zero.policy = TRUE
    
  )

global_moran

#==========================================================
# 5. STORE GLOBAL MORAN'S I RESULTS
#==========================================================

moran_results <-
  
  tibble(
    
    Moran_I     = unname(global_moran$estimate[1]),
    
    Expected_I  = unname(global_moran$estimate[2]),
    
    Variance    = unname(global_moran$estimate[3]),
    
    Z_value     = unname(global_moran$statistic),
    
    P_value     = global_moran$p.value
    
  )

moran_results

#==========================================================
# 6. FUNCTION TO COMPUTE GLOBAL MORAN'S I
#    FOR ANY FINANCIAL YEAR
#==========================================================

compute_moran <- function(year_selected){
  
  temp_map <-
    
    india_moran %>%
    
    mutate(
      
      state = toupper(state)
      
    ) %>%
    
    select(
      
      state,
      
      geometry
      
    ) %>%
    
    left_join(
      
      panel_z %>%
        
        filter(
          
          Year == year_selected
          
        ) %>%
        
        transmute(
          
          State = toupper(State),
          
          State_Index = as.numeric(State_Index)
          
        ),
      
      by = c(
        
        "state" = "State"
        
      )
      
    )
  
  mt <-
    
    moran.test(
      
      temp_map$State_Index,
      
      listw,
      
      zero.policy = TRUE
      
    )
  
  tibble(
    
    Year = year_selected,
    
    Moran_I = unname(mt$estimate[1]),
    
    Expected_I = unname(mt$estimate[2]),
    
    Z_value = unname(mt$statistic),
    
    P_value = mt$p.value
    
  )
  
}

#==========================================================
# 7. GLOBAL MORAN'S I FOR ALL YEARS
#==========================================================

years <-
  
  unique(panel_z$Year)

moran_time <-
  
  map_dfr(
    
    years,
    
    compute_moran
    
  )

moran_time

moran_time <-
  
  moran_time %>%
  
  mutate(
    
    Significance = case_when(
      
      P_value < 0.001 ~ "***",
      
      P_value < 0.01  ~ "**",
      
      P_value < 0.05  ~ "*",
      
      TRUE            ~ "NS"
      
    )
    
  )

moran_time
#==========================================================
# 8. EVOLUTION OF GLOBAL MORAN'S I
#==========================================================

ggplot(
  
  moran_time,
  
  aes(
    
    x = Year,
    
    y = Moran_I,
    
    group = 1
    
  )
  
) +
  
  geom_line(
    
    linewidth = 1.2,
    
    colour = "#08306B"
    
  ) +
  
  geom_point(
    
    size = 3,
    
    colour = "#08306B"
    
  ) +
  
  geom_text(
    
    aes(
      
      label = round(Moran_I, 3)
      
    ),
    
    vjust = -0.8,
    
    size = 3.8
    
  ) +
  
  geom_hline(
    
    yintercept = unique(moran_time$Expected_I),
    
    linetype = "dashed",
    
    colour = "red"
    
  ) +
  
  theme_minimal(
    
    base_size = 12
    
  ) +
  
  labs(
    
    title = "Evolution of Global Moran's I of the Financial Inclusion Indicator",
    
    subtitle = "Indian States (2016–17 to 2024–25)",
    
    x = "Financial Year",
    
    y = "Global Moran's I"
    
  ) +
  
  theme(
    
    axis.text.x =
      
      element_text(
        
        angle = 45,
        
        hjust = 1
        
      )
    
  )

#--
library(ggplot2)

ggplot(
  moran_time,
  aes(
    x = Year,
    y = Moran_I,
    group = 1
  )
) +
  
  # Line
  geom_line(
    linewidth = 1.3,
    colour = "#08306B"
  ) +
  
  # Points
  geom_point(
    size = 3.5,
    colour = "#08306B"
  ) +
  
  # Value labels
  geom_text(
    aes(label = sprintf("%.3f", Moran_I)),
    vjust = -0.8,
    size = 4,
    fontface = "bold"
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 0.45),
    breaks = seq(0, 0.4, by = 0.1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  
  labs(
    title = "Evolution of Global Moran's I of the Financial Inclusion Indicator",
    subtitle = "Indian States (2016–17 to 2024–25)",
    x = "Financial Year",
    y = "Global Moran's I"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.minor = element_blank()
  )

#######===========================================================

library(spdep)

#==========================================================
# 48. PERMUTATION LOCAL MORAN'S I
#==========================================================

library(spdep)

set.seed(123)

local_perm <-
  
  localmoran_perm(
    
    india_moran$State_Index,
    
    listw,
    
    nsim = 999,
    
    zero.policy = TRUE
    
  )

local_perm
colnames(local_perm)

#==========================================================
# BUILD LISA RESULTS TABLE
#==========================================================

lisa_results <-
  
  india_moran %>%
  
  mutate(
    
    Ii = local_perm[, "Ii"],
    
    Z_Ii = local_perm[, "Z.Ii"],
    
    P_value = local_perm[, "Pr(z != E(Ii)) Sim"],
    
    Spatial_Lag = lag.listw(
      listw,
      State_Index,
      zero.policy = TRUE
    )
    
  )

#----------------------------------------------------------
# Standardize values
#----------------------------------------------------------

lisa_results <-
  
  lisa_results %>%
  
  mutate(
    
    FII_z = as.numeric(scale(State_Index)),
    
    Lag_z = as.numeric(scale(Spatial_Lag))
    
  )

#----------------------------------------------------------
# Create LISA Cluster
#----------------------------------------------------------

lisa_results <-
  
  lisa_results %>%
  
  mutate(
    
    LISA_Cluster = case_when(
      
      P_value > 0.05 ~ "Not Significant",
      
      FII_z > 0 &
        Lag_z > 0 ~ "High-High",
      
      FII_z < 0 &
        Lag_z < 0 ~ "Low-Low",
      
      FII_z > 0 &
        Lag_z < 0 ~ "High-Low",
      
      TRUE ~ "Low-High"
      
    )
    
  )

lisa_results %>%
  
  st_drop_geometry() %>%
  
  select(
    
    state,
    
    Ii,
    
    Z_Ii,
    
    P_value,
    
    Spatial_Lag,
    
    FII_z,
    
    Lag_z,
    
    LISA_Cluster
    
  )

# ============================================================
# 49. SIGMA CONVERGENCE
# ============================================================


# ============================================================
# 49.1 CALCULATE ANNUAL DISPERSION
# ============================================================

sigma_results <- panel_z %>%
  
  group_by(
    Year
  ) %>%
  
  summarise(
    
    Mean_FII =
      mean(
        State_Index,
        na.rm = TRUE
      ),
    
    SD_FII =
      sd(
        State_Index,
        na.rm = TRUE
      ),
    
    Variance =
      var(
        State_Index,
        na.rm = TRUE
      ),
    
    CV =
      SD_FII / Mean_FII,
    
    Minimum =
      min(
        State_Index,
        na.rm = TRUE
      ),
    
    Maximum =
      max(
        State_Index,
        na.rm = TRUE
      ),
    
    Range =
      Maximum - Minimum,
    
    .groups = "drop"
  )


sigma_results


# ------------------------------------------------------------
# Rounded summary table
# ------------------------------------------------------------

sigma_table <- sigma_results %>%
  
  mutate(
    across(
      where(is.numeric),
      ~ round(
        .,
        3
      )
    )
  )


sigma_table


# ============================================================
# 50. SIGMA CONVERGENCE — STANDARD DEVIATION
# ============================================================

ggplot(
  
  sigma_results,
  
  aes(
    x = Year,
    y = SD_FII,
    group = 1
  )
) +
  
  geom_line(
    linewidth = 1.3,
    colour = "#8B0000"
  ) +
  
  geom_point(
    size = 3,
    colour = "#8B0000"
  ) +
  
  geom_text(
    aes(
      label = round(
        SD_FII,
        2
      )
    ),
    vjust = -0.4,
    size = 3.8,
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    limits = c(
      1.6,
      2.60
    ),
    breaks = seq(
      1.6,
      2.6,
      by = 0.2
    )
  ) +
  
  labs(
    
    title =
      "Sigma Convergence in Financial Inclusion Across Indian States",
    
    subtitle =
      "Measured using the Standard Deviation of the Financial Inclusion Index",
    
    x =
      "Financial Year",
    
    y =
      "Standard Deviation"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ============================================================
# 51. RELATIVE DISPERSION — COEFFICIENT OF VARIATION
# ============================================================

ggplot(
  
  sigma_results,
  
  aes(
    x = Year,
    y = CV,
    group = 1
  )
) +
  
  geom_line(
    linewidth = 1.3,
    colour = "#1B7837"
  ) +
  
  geom_point(
    size = 3,
    colour = "#1B7837"
  ) +
  
  geom_text(
    aes(
      label = round(
        CV,
        2
      )
    ),
    vjust = -0.8,
    size = 3.8
  ) +
  
  scale_y_continuous(
    limits = c(
      0.45,
      0.55
    ),
    breaks = seq(
      0.45,
      0.55,
      by = 0.025
    ),
    expand = expansion(
      mult = c(
        0.02,
        0.02
      )
    )
  ) +
  
  labs(
    
    title =
      "Coefficient of Variation of the Financial Inclusion Indicator",
    
    subtitle =
      "Interstate Dispersion Relative to the Mean",
    
    x =
      "Financial Year",
    
    y =
      "Coefficient of Variation"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# ============================================================
# 52. SIGMA ROBUSTNESS CHECK — EXCLUDING GOA
# ============================================================

sigma_no_goa <- panel_z %>%
  
  filter(
    State != "Goa"
  ) %>%
  
  group_by(
    Year
  ) %>%
  
  summarise(
    
    Mean =
      mean(
        State_Index,
        na.rm = TRUE
      ),
    
    SD =
      sd(
        State_Index,
        na.rm = TRUE
      ),
    
    CV =
      SD / Mean,
    
    .groups = "drop"
  )


sigma_no_goa


# ============================================================
# 53. MARKOV QUARTILE CLASSIFICATION
# ============================================================

# Divide states into four financial inclusion quartiles
# separately for each year.

markov_data <- panel_z %>%
  
  group_by(
    Year
  ) %>%
  
  mutate(
    
    Quartile =
      paste0(
        "Q",
        ntile(
          State_Index,
          4
        )
      )
  ) %>%
  
  ungroup()


# ============================================================
# 54. MARKOV TRANSITIONS
# ============================================================

transition_data <- markov_data %>%
  
  arrange(
    State,
    Year
  ) %>%
  
  group_by(
    State
  ) %>%
  
  mutate(
    
    Next_Quartile =
      lead(
        Quartile
      )
  ) %>%
  
  ungroup() %>%
  
  filter(
    !is.na(
      Next_Quartile
    )
  )


# ------------------------------------------------------------
# Count transitions
# ------------------------------------------------------------

transition_counts <- table(
  
  transition_data$Quartile,
  
  transition_data$Next_Quartile
)


transition_counts


# ============================================================
# 55. MARKOV TRANSITION PROBABILITY MATRIX
# ============================================================

transition_matrix <- prop.table(
  
  transition_counts,
  
  margin = 1
)


round(
  transition_matrix,
  3
)


# Convert to data frame for reporting

transition_table <- as.data.frame.matrix(
  
  round(
    transition_matrix,
    3
  )
)


transition_table


# ============================================================
# 56. SHORROCKS MOBILITY INDEX
# ============================================================

n_quartiles <- nrow(
  transition_matrix
)


mobility_index <- (
  
  n_quartiles -
    sum(
      diag(
        transition_matrix
      )
    )
  
) / (
  
  n_quartiles - 1
  
)


mobility_index


# ============================================================
# 57. MARKOV TRANSITION HEATMAP
# ============================================================

transition_long <- reshape2::melt(
  transition_matrix
)


ggplot(
  
  transition_long,
  
  aes(
    x = Var2,
    y = Var1,
    fill = value
  )
) +
  
  # ----------------------------------------------------------
# Transition probabilities
# ----------------------------------------------------------

geom_tile(
  colour = "grey80",
  linewidth = 0.6
) +
  
  # ----------------------------------------------------------
# Emphasise diagonal persistence
# ----------------------------------------------------------

geom_tile(
  
  data =
    subset(
      transition_long,
      Var1 == Var2
    ),
  
  fill = NA,
  
  colour = "grey20",
  
  linewidth = 0.7
) +
  
  # ----------------------------------------------------------
# Probability labels
# ----------------------------------------------------------

geom_text(
  
  data =
    subset(
      transition_long,
      value <= 0.55
    ),
  
  aes(
    label = sprintf(
      "%.2f",
      value
    )
  ),
  
  colour = "black",
  
  size = 5.3
) +
  
  geom_text(
    
    data =
      subset(
        transition_long,
        value > 0.55
      ),
    
    aes(
      label = sprintf(
        "%.2f",
        value
      )
    ),
    
    colour = "white",
    
    fontface = "bold",
    
    size = 5.3
  ) +
  
  # ----------------------------------------------------------
# Colour scale
# ----------------------------------------------------------

scale_fill_gradient(
  
  low = "#E8EEF7",
  
  high = "#08306B",
  
  limits = c(
    0,
    1
  ),
  
  breaks = c(
    0,
    0.25,
    0.50,
    0.75,
    1.00
  ),
  
  labels = sprintf(
    "%.2f",
    c(
      0,
      0.25,
      0.50,
      0.75,
      1.00
    )
  ),
  
  name = "Probability"
) +
  
  labs(
    
    title =
      "Markov Transition Matrix",
    
    subtitle =
      "Financial Inclusion Quartiles, Indian States (2016–17 to 2024–25)",
    
    x =
      "Next-Year Quartile",
    
    y =
      "Current-Year Quartile"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 15,
      hjust = 0.5
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 16
    ),
    
    axis.text = element_text(
      size = 14,
      colour = "black"
    ),
    
    legend.title = element_text(
      face = "bold",
      size = 14
    ),
    
    legend.text = element_text(
      size = 12
    ),
    
    panel.grid = element_blank(),
    
    axis.ticks = element_blank()
  )


# ============================================================
# 58. QUARTILE MOBILITY — 2016-17 TO 2024-25
# ============================================================

markov_alluvial <- panel_z %>%
  
  group_by(
    Year
  ) %>%
  
  mutate(
    
    Quartile =
      paste0(
        "Q",
        ntile(
          State_Index,
          4
        )
      )
  ) %>%
  
  ungroup()


# ------------------------------------------------------------
# Extract beginning and ending quartiles
# ------------------------------------------------------------

alluvial_2yr <- markov_alluvial %>%
  
  filter(
    Year %in% c(
      "2016-17",
      "2024-25"
    )
  ) %>%
  
  select(
    State,
    Year,
    Quartile
  ) %>%
  
  pivot_wider(
    names_from = Year,
    values_from = Quartile
  )


# ============================================================
# 59. STATE-LEVEL QUARTILE CHANGES
# ============================================================

quartile_changes <- alluvial_2yr %>%
  
  mutate(
    
    Change = case_when(
      
      `2016-17` == `2024-25` ~
        "No Change",
      
      TRUE ~
        paste(
          `2016-17`,
          "→",
          `2024-25`
        )
    )
  ) %>%
  
  arrange(
    `2016-17`,
    `2024-25`
  )


quartile_changes


# ============================================================
# 60. ALLUVIAL PLOT — QUARTILE MOBILITY
# ============================================================

# Probability labels correspond to the transition
# probabilities shown in the Markov transition matrix.

prob_labels <- data.frame(
  
  x = c(
    1.50, 1.50, 1.50, 1.50,
    1.50, 1.50,
    1.50, 1.50,
    1.50, 1.50
  ),
  
  y = c(
    24.5,
    17.5,
    10.5,
    3.5,
    21.0,
    20.1,
    14.2,
    13.0,
    7.1,
    6.0
  ),
  
  label = c(
    "93%",
    "91%",
    "88%",
    "89%",
    "7%",
    "7%",
    "2%",
    "2%",
    "11%",
    "11%"
  ),
  
  type = c(
    rep(
      "stay",
      4
    ),
    rep(
      "move",
      6
    )
  )
)


# ------------------------------------------------------------
# Final alluvial plot
# ------------------------------------------------------------
library(ggplot2)
library(ggalluvial)
library(scales)
ggplot(
  
  alluvial_2yr,
  
  aes(
    axis1 = `2016-17`,
    axis2 = `2024-25`
  )
) +
  
  geom_alluvium(
    
    aes(
      fill = `2016-17`
    ),
    
    width = 0.28,
    
    alpha = 0.72,
    
    knot.pos = 0.5
  ) +
  
  geom_stratum(
    
    width = 0.46,
    
    fill = scales::alpha(
      "white",
      0.85
    ),
    
    colour = "grey40",
    
    linewidth = 0.7
  ) +
  
  geom_text(
    
    stat = "stratum",
    
    aes(
      label = paste0(
        after_stat(stratum),
        "\n(7)"
      )
    ),
    
    fontface = "bold",
    
    size = 5.2,
    
    lineheight = 0.9
  ) +
  
  # ----------------------------------------------------------
# Stay probabilities
# ----------------------------------------------------------

geom_label(
  
  data =
    subset(
      prob_labels,
      type == "stay"
    ),
  
  aes(
    x = x,
    y = y,
    label = label
  ),
  
  inherit.aes = FALSE,
  
  size = 3.8,
  
  fontface = "bold",
  
  fill = scales::alpha(
    "white",
    0.85
  ),
  
  label.size = 0.25,
  
  colour = "black"
) +
  
  # ----------------------------------------------------------
# Mobility probabilities
# ----------------------------------------------------------

geom_label(
  
  data =
    subset(
      prob_labels,
      type == "move"
    ),
  
  aes(
    x = x,
    y = y,
    label = label
  ),
  
  inherit.aes = FALSE,
  
  size = 3.1,
  
  fontface = "bold",
  
  fill = "white",
  
  label.size = 0.2,
  
  colour = "black"
) +
  
  # ----------------------------------------------------------
# Quartile colours
# ----------------------------------------------------------

scale_fill_manual(
  
  values = c(
    
    "Q1" = "#D55E5E",
    "Q2" = "#7CAE00",
    "Q3" = "#2C9FB3",
    "Q4" = "#8E63CE"
    
  ),
  
  name = "2016–17\nQuartile"
) +
  
  # ----------------------------------------------------------
# Axes
# ----------------------------------------------------------

scale_x_discrete(
  
  labels = c(
    "2016–17",
    "2024–25"
  ),
  
  expand =
    expansion(
      mult = c(
        0.22,
        0.22
      )
    )
) +
  
  scale_y_continuous(
    
    breaks = c(
      0,
      7,
      14,
      21,
      28
    ),
    
    limits = c(
      0,
      28
    ),
    
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    
    title =
      "Movement Across Financial Inclusion Quartiles",
    
    subtitle =
      "Indian States, 2016–17 to 2024–25",
    
    x = NULL,
    
    y =
      "Number of States"
  ) +
  
  theme_minimal(
    base_size = 15
  ) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 20,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 15,
      hjust = 0.5
    ),
    
    axis.title.y = element_text(
      face = "bold",
      size = 16
    ),
    
    axis.text = element_text(
      size = 14,
      colour = "black"
    ),
    
    legend.title = element_text(
      face = "bold",
      size = 14
    ),
    
    legend.text = element_text(
      size = 13
    ),
    
    legend.position = "right",
    
    panel.grid = element_blank(),
    
    axis.ticks = element_blank(),
    
    axis.line = element_blank()
  )


# ============================================================
# 61. EXPORT MARKOV RESULTS
# ============================================================

write.csv(
  
  sigma_results,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/sigma_convergence_results.csv",
  
  row.names = FALSE
)


write.csv(
  
  transition_table,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/markov_transition_matrix.csv",
  
  row.names = TRUE
)


write.csv(
  
  quartile_changes,
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/outputs/state_quartile_changes.csv",
  
  row.names = FALSE
)
