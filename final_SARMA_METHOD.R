#==================================================
#  1. LOAD PACKAGES
#==================================================
library(readxl)
library(psych)
library(ggplot2)
library(reshape2)
library(corrplot)
library(dplyr)
library(tidyr)
#==================================================
# 2. IMPORT PANEL DATA
#=================================================
excel_sheets("C:/Users/KIIT/Downloads/FINAL_DATASHEET.xlsx")   
panel_data <- read_excel(
  "C:/Users/KIIT/Downloads/FINAL_DATASHEET.xlsx",
  sheet="STATE_DATA", range = "A1:L253",
)

head(panel_data)
#==================================================
#  3. SELECT VARIABLES
#==================================================

panel <- panel_data %>%
  
  select(
    
    State,
    
    Year,
    
    DEP_PC,
    
    CR_OUT_PC,
    
    INS_DEN,
    
    SCB_OFF,
    
    DEP_ACC,
    
    SCB_OFF_R,
    
    EMP,
    
    INS_OFF,
    
    INT_SUB,
    
    CR_ACC
    
  )

#==================================================
#  4. CHECK MISSING VALUES
#==================================================
colSums(is.na(panel))


#==================================================
#5. DESCRIPTIVE STATISTICS
#==================================================

describe(
  
  panel[,-c(1,2)]
  
)


desc_stats<-
  
  describe(
    
    panel[,-c(1,2)]
    
  )

desc_stats

#==================================================
#  6. HISTOGRAMS
#==================================================
hist_data<-
  
  melt(
    
    panel,
    
    id.vars=c(
      
      "State",
      
      "Year"
      
    )
    
  )

ggplot(
  
  hist_data,
  
  aes(value)
  
)+
  
  geom_histogram(
    
    bins=20,
    
    fill="steelblue",
    
    color="black"
    
  )+
  
  facet_wrap(
    
    ~variable,
    
    scales="free"
    
  )+
  
  theme_minimal()

#==================================================
#  7. BOXPLOTS
#==================================================
boxplot(
  
  panel[,-c(1,2)],
  
  las=2,
  
  col="lightblue",
  
  main="Boxplots of Financial Inclusion Variables"
  
)
#==================================================
#  8. CORRELATION MATRIX
#==================================================

cor_matrix<-
  
  cor(
    
    panel[,-c(1,2)]
    
  )

cor_matrix

#Heatmap

corrplot(
  
  cor_matrix,
  
  method="color",
  
  type="upper",
  
  addCoef.col="black",
  
  number.cex=0.7,
  
  tl.cex=0.8
  
)

#==================================================
#  9. COMPUTE NORMALISATION LIMITS
#==================================================

summary_limits<-
  
  data.frame(
    
    Variable=
      
      names(
        
        panel[,-c(1,2)]
        
      )
    
  )

summary_limits$Minimum<-
  
  sapply(
    
    panel[,-c(1,2)],
    
    min,
    
    na.rm=TRUE
    
  )

summary_limits$Maximum<-
  
  sapply(
    
    panel[,-c(1,2)],
    
    max,
    
    na.rm=TRUE
    
  )

summary_limits$P94<-
  
  sapply(
    
    panel[,-c(1,2)],
    
    function(x)
      
      quantile(
        
        x,
        
        0.94,
        
        na.rm=TRUE
        
      )
    
  )

summary_limits

write.csv(
  
  summary_limits,
  
  "Sarma_Normalisation_Limits.csv",
  
  row.names=FALSE
  
)


#==================================================
#  10. SARMA NORMALISATION FUNCTION
#==================================================

#Now we normalize every variable between 0 and 1 using P94 as the upper bound.

normalize_sarma<-
  
  function(x){
    
    lower<-
      
      min(
        
        x,
        
        na.rm=TRUE
        
      )
    
    upper<-
      
      quantile(
        
        x,
        
        0.94,
        
        na.rm=TRUE
        
      )
    
    z<-
      
      (x-lower)/(upper-lower)
    
    z[z<0]<-0
    
    z[z>1]<-1
    
    return(z)
    
  }

#Apply

panel_norm<-
  
  panel

panel_norm[,-c(1,2)]<-
  
  lapply(
    
    panel[,-c(1,2)],
    
    normalize_sarma
    
  )

#Check

summary(
  
  panel_norm
)

#Everything should lie between 0 and 1


#==================================================
#  11. CONSTRUCT DIMENSION INDICES
#==================================================


penetration_vars <- c("DEP_ACC", "CR_ACC")

availability_vars <- c("SCB_OFF", "SCB_OFF_R", "EMP", "INS_OFF", "INT_SUB")

usage_vars <- c("DEP_PC", "CR_OUT_PC", "INS_DEN")

panel_norm$Penetration <- rowMeans(panel_norm[, penetration_vars])

panel_norm$Availability <- rowMeans(panel_norm[, availability_vars])

panel_norm$Usage <- rowMeans(panel_norm[, usage_vars])







#Inspect

describe(
  
  panel_norm[,c(
    
    "Penetration",
    
    "Availability",
    
    "Usage"
    
  )]
  
)

write.csv(
  
  panel_norm,
  
  "Sarma_Normalised_Dataset.csv",
  
  row.names=FALSE
  
)




# ==================================================
# 13.DISTANCE FROM WORST POINT
# ==================================================

panel_norm$X1 <-
  
  sqrt(
    
    panel_norm$Penetration^2 +
      
      panel_norm$Availability^2 +
      
      panel_norm$Usage^2
    
  ) / sqrt(3)

summary(panel_norm$X1)

# ==================================================
# 14.DISTANCE FROM IDEAL POINT
# ==================================================

panel_norm$X2 <-
  
  1 -
  
  sqrt(
    
    (1-panel_norm$Penetration)^2 +
      
      (1-panel_norm$Availability)^2 +
      
      (1-panel_norm$Usage)^2
    
  ) / sqrt(3)

summary(panel_norm$X2)


# ==================================================
# 15.SARMA FINANCIAL INCLUSION INDEX
# ==================================================

panel_norm$FII_Sarma <-
  
  (panel_norm$X1 +
     
     panel_norm$X2)/2

summary(panel_norm$FII_Sarma)

#16. CHECK INDEX RANGE
range(panel_norm$FII_Sarma)



#==================================================
#  17. YEAR-WISE STATE RANKINGS
#==================================================
library(dplyr)

panel_norm <-
  
  panel_norm %>%
  
  group_by(Year) %>%
  
  mutate(
    
    Rank_Sarma =
      
      dense_rank(
        
        desc(FII_Sarma)
        
      )
    
  ) %>%
  
  ungroup()

#Inspect

panel_norm %>%
  
  select(
    
    State,
    
    Year,
    
    FII_Sarma,
    
    Rank_Sarma
    
  )


top10 <-
  
  panel_norm %>%
  
  group_by(Year) %>%
  
  slice_max(
    
    FII_Sarma,
    
    n=10
    
  )

top10

bottom10 <-
  
  panel_norm %>%
  
  group_by(Year) %>%
  
  slice_min(
    
    FII_Sarma,
    
    n=10
    
  )

bottom10

#==================================================
#  20. HEATMAP OF STATE RANKINGS
#==================================================
library(ggplot2)

ggplot(
  
  panel_norm,
  
  aes(
    
    Year,
    
    reorder(
      
      State,
      
      -Rank_Sarma
      
    ),
    
    fill=FII_Sarma
    
  )
  
)+
  
  geom_tile(
    
    color="white"
    
  )+
  
  scale_fill_gradient(
    
    low="white",
    
    high="darkblue"
    
  )+
  
  labs(
    
    title="State-wise Financial Inclusion Index (Sarma Method)",
    
    x="Financial Year",
    
    y="State",
    
    fill="IFI"
    
  )+
  
  theme_minimal()+
  
  theme(
    
    axis.text.x=
      
      element_text(
        
        angle=45,
        
        hjust=1
        
      )
    
  )
#==================================================
#  21. TREND OF EACH STATE
#==================================================
ggplot(
  
  panel_norm,
  
  aes(
    
    Year,
    
    FII_Sarma,
    
    group=State,
    
    colour=State
    
  )
  
)+
  
  geom_line(
    
    linewidth=0.8
    
  )+
  
  theme_minimal()+
  
  theme(
    
    axis.text.x=
      
      element_text(
        
        angle=45,
        
        hjust=1
        
      ),
    
    legend.position="none"
    
  )+
  
  labs(
    
    title="Financial Inclusion Trends Across States",
    
    y="IFI"
    
  )

#==================================================
# INDIA CHOROPLETH MAP (2024-25)
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
# Latest Sarma Index
#--------------------------------------------------

latest_rank <-
  panel_norm %>%
  filter(Year == "2024-25")

#--------------------------------------------------
# Standardize state names
#--------------------------------------------------

india_map$state <- toupper(india_map$state)
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
# Keep only 28 States + Jammu & Kashmir
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
    aes(fill = FII_Sarma),
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
    na.value = "grey80",
    name = "Financial\nInclusion\nIndex"
  ) +
  
  guides(
    fill = guide_colourbar(
      barheight = unit(4, "cm"),
      barwidth = unit(0.6, "cm")
    )
  ) +
  
  labs(
    title = "Financial Inclusion across Indian States (2024-25)",
    subtitle = "Sarma (2012) Distance-Based Financial Inclusion Index",
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
    aes(fill = FII_Sarma),
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
    na.value = "grey80",
    breaks = c(
      min(india_2025$FII_Sarma, na.rm = TRUE),
      max(india_2025$FII_Sarma, na.rm = TRUE)
    ),
    labels = c("Low", "High"),
    name = "Financial Inclusion\nIndex"
  ) +
  
  guides(
    fill = guide_colourbar(
      barheight = unit(4, "cm"),
      barwidth  = unit(0.6, "cm"),
      ticks = FALSE,
      frame.colour = "black"
    )
  ) +
  
  labs(
    title = "Financial Inclusion across Indian States (2024-25)",
    subtitle = "Sarma (2012) Distance-Based Financial Inclusion Index",
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
  "India_SARMAFII_Choropleth_2024_25.png",
  width = 10,
  height = 7,
  dpi = 600,
  bg = "white"
)




#==================================================
#  23. EXPORT FINAL INDEX
#==================================================
write.csv(
  
  panel_norm,
  
  "Sarma_Financial_Inclusion_Index.csv",
  
  row.names=FALSE
  
)

#==================================================
#  24. EXPORT YEAR-WISE RANKINGS
#==================================================
ranking_table <-
  
  panel_norm %>%
  
  select(
    
    State,
    
    Year,
    
    FII_Sarma,
    
    Rank_Sarma
    
  )

write.csv(
  
  ranking_table,
  
  "Sarma_State_Rankings.csv",
  
  row.names=FALSE
  
)

#25. SUMMARY TABLE
#==================================================
summary_table <-
  
  panel_norm %>%
  
  group_by(Year) %>%
  
  summarise(
    
    Mean_IFI=
      
      mean(FII_Sarma),
    
    Median_IFI=
      
      median(FII_Sarma),
    
    Minimum_IFI=
      
      min(FII_Sarma),
    
    Maximum_IFI=
      
      max(FII_Sarma),
    
    SD_IFI=
      
      sd(FII_Sarma)
    
  )

summary_table





# ==================================================
# REGION CLASSIFICATION
# ==================================================

panel_norm$Region <- case_when(
  
  panel_norm$State %in% c(
    "Bihar",
    "Jharkhand",
    "Odisha",
    "West Bengal",
    "Sikkim"
  ) ~ "Eastern",
  
  panel_norm$State %in% c(
    "Chhattisgarh",
    "Madhya Pradesh",
    "Uttar Pradesh",
    "Uttarakhand"
  ) ~ "Central",
  
  panel_norm$State %in% c(
    "Goa",
    "Gujarat",
    "Maharashtra"
  ) ~ "Western",
  
  panel_norm$State %in% c(
    "Andhra Pradesh",
    "Karnataka",
    "Kerala",
    "Tamil Nadu",
    "Telangana"
  ) ~ "Southern",
  
  panel_norm$State %in% c(
    "Haryana",
    "Himachal Pradesh",
    "Punjab",
    "Rajasthan"
  ) ~ "Northern",
  
  panel_norm$State %in% c(
    "Arunachal Pradesh",
    "Assam",
    "Manipur",
    "Meghalaya",
    "Mizoram",
    "Nagaland",
    "Tripura"
  ) ~ "North-Eastern"
)

table(panel_norm$Region)

region_summary <- panel_norm %>%
  group_by(Year, Region) %>%
  summarise(
    Mean_IFI = mean(FII_Sarma),
    SD_IFI = sd(FII_Sarma),
    .groups = "drop"
  )

region_summary


ggplot(
  region_summary,
  aes(
    x = Year,
    y = Mean_IFI,
    colour = Region,
    group = Region
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  theme_minimal() +
  labs(
    title = "Regional Trends in Financial Inclusion",
    x = "Financial Year",
    y = "Mean Financial Inclusion Index",
    colour = "Region"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(
  region_summary,
  aes(
    x = Year,
    y = Region,
    fill = Mean_IFI
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "white",
    high = "darkblue"
  ) +
  theme_minimal() +
  labs(
    title = "Regional Financial Inclusion Index",
    x = "Financial Year",
    y = "Region",
    fill = "Mean IFI"
  )


# ==================================================
# CORRELATION OF DIMENSIONS WITH SARMA INDEX
# ==================================================

dimension_cor <- cor(
  panel_norm[, c(
    "Penetration",
    "Availability",
    "Usage",
    "FII_Sarma"
  )]
)

round(dimension_cor, 3)
corrplot(
  dimension_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.9,
  number.cex = 0.8,
  col = colorRampPalette(
    c("red", "white", "blue")
  )(200)
)

##==================================================
#  26. INTERNAL VALIDATION OF THE SARMA INDEX
##==================================================

# ==================================================
# CORRELATION OF DIMENSIONS WITH SARMA INDEX
# ==================================================

dimension_cor <- cor(
  panel_norm[, c(
    "Penetration",
    "Availability",
    "Usage",
    "FII_Sarma"
  )]
)

round(dimension_cor, 3)

corrplot(
  dimension_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.9,
  number.cex = 0.8,
  col = colorRampPalette(
    c("red", "white", "blue")
  )(200)
)
#26.2 Pairwise Correlation Among Dimensions-This checks whether the dimensions are measuring related but distinct aspects of financial inclusion.

# ==================================================
# PAIRWISE CORRELATION AMONG DIMENSIONS
# ==================================================

pairwise_cor <- cor(
  panel_norm[, c(
    "Penetration",
    "Availability",
    "Usage"
  )]
)

round(pairwise_cor, 3)


corrplot(
  pairwise_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  number.cex = 0.8
)

##26.3 Scatter Plots


#Penetration
ggplot(
  panel_norm,
  aes(
    Penetration,
    FII_Sarma
  )
) +
  
  geom_point(
    colour = "steelblue",
    alpha = 0.7
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "red"
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Penetration vs Financial Inclusion Index",
    x = "Penetration Index",
    y = "Sarma Financial Inclusion Index"
  )
#Availability
ggplot(
  panel_norm,
  aes(
    Availability,
    FII_Sarma
  )
) +
  
  geom_point(
    colour = "steelblue",
    alpha = 0.7
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "red"
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Availability vs Financial Inclusion Index",
    x = "Availability Index",
    y = "Sarma Financial Inclusion Index"
  )
#Usage
ggplot(
  panel_norm,
  aes(
    Usage,
    FII_Sarma
  )
) +
  
  geom_point(
    colour = "steelblue",
    alpha = 0.7
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "red"
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Usage vs Financial Inclusion Index",
    x = "Usage Index",
    y = "Sarma Financial Inclusion Index"
  )

#6.4 Summary Statistics of Dimension Indices
describe(
  panel_norm[, c(
    "Penetration",
    "Availability",
    "Usage",
    "FII_Sarma"
  )]
)

#26.5 Dimension Contribution Plot

library(tidyr)

dimension_means <- panel_norm %>%
  summarise(
    Penetration = mean(Penetration),
    Availability = mean(Availability),
    Usage = mean(Usage)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Dimension",
    values_to = "Mean"
  )

ggplot(
  dimension_means,
  aes(
    Dimension,
    Mean
  )
) +
  
  geom_col(fill = "steelblue") +
  
  geom_text(
    aes(label = round(Mean, 3)),
    vjust = -0.4
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Average Dimension Scores",
    y = "Mean Normalised Score",
    x = ""
  )


#==================================================
#  27. YEAR-WISE AVERAGE DIMENSION SCORES
#==================================================
library(dplyr)
library(tidyr)
library(ggplot2)

dimension_year <- panel_norm %>%
  
  group_by(Year) %>%
  
  summarise(
    
    Penetration = mean(Penetration),
    
    Availability = mean(Availability),
    
    Usage = mean(Usage),
    
    .groups = "drop"
    
  )

#dimension_year-Convert to Long Format
dimension_year_long <-
  
  dimension_year %>%
  
  pivot_longer(
    
    cols = c(
      Penetration,
      Availability,
      Usage
    ),
    
    names_to = "Dimension",
    
    values_to = "Mean_Index"
    
  )

dimension_year_long

#Plot Year-wise Dimension Trends
ggplot(
  
  dimension_year_long,
  
  aes(
    
    x = Year,
    
    y = Mean_Index,
    
    colour = Dimension,
    
    group = Dimension
    
  )
  
) +
  
  geom_line(
    
    linewidth = 1.3
    
  ) +
  
  geom_point(
    
    size = 2.5
    
  ) +
  
  scale_colour_manual(
    
    values = c(
      
      "Penetration" = "#1B9E77",
      
      "Availability" = "#D95F02",
      
      "Usage" = "#7570B3"
      
    )
    
  ) +
  
  labs(
    
    title = "Average Financial Inclusion Dimension Scores Across States",
    
    subtitle = "Sarma Method (2016–17 to 2024–25)",
    
    x = "Financial Year",
    
    y = "Mean Dimension Index",
    
    colour = "Dimension"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.text.x = element_text(
      
      angle = 45,
      
      hjust = 1
      
    ),
    
    legend.position = "bottom"
    
  )

###==================================================###
# COMPARISON OF PCA AND SARMA INDICES
##==================================================###

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

pca_index <- read_excel(
  
  "C:/Users/KIIT/Downloads/RBI PROJECT/rbi_proj_data2.xlsx",
  
  sheet = "all states_2stagePCA",
  
  range = "A1:AC10"
  
)

# Convert Wide → Long
pca_long <-
  
  pca_index %>%
  
  pivot_longer(
    
    cols = -Year,
    
    names_to = "State",
    
    values_to = "FII_PCA"
    
  )

head(pca_long)
summary(pca_long)

#Merge with Sarma
comparison <-
  
  panel_norm %>%
  
  select(
    
    State,
    
    Year,
    
    FII_Sarma,
    
    Rank_Sarma
    
  ) %>%
  
  left_join(
    
    pca_long,
    
    by=c("State","Year")
    
  )

# Check
summary(comparison)

head(comparison)

#Pearson Correlation
cor.test(
  
  comparison$FII_PCA,
  
  comparison$FII_Sarma,
  
  method="pearson"
  
)
#Spearman Correlation
cor.test(
  
  comparison$FII_PCA,
  
  comparison$FII_Sarma,
  
  method="spearman"
  
)

#==================================================
# YEAR-WISE CORRELATION
#==================================================

yearwise_cor <-
  
  comparison %>%
  
  group_by(Year) %>%
  
  summarise(
    
    Pearson =
      cor(FII_PCA,
          FII_Sarma,
          method="pearson"),
    
    Spearman =
      cor(FII_PCA,
          FII_Sarma,
          method="spearman")
    
  )

yearwise_cor

write.csv(
  yearwise_cor,
  "Yearwise_PCA_Sarma_Correlation.csv",
  row.names=FALSE
)

yearwise_long <-
  
  yearwise_cor %>%
  
  pivot_longer(
    
    cols=c(Pearson,Spearman),
    
    names_to="Method",
    
    values_to="Correlation"
    
  )

ggplot(
  
  yearwise_long,
  
  aes(
    
    Year,
    
    Correlation,
    
    group=Method,
    
    colour=Method
    
  )
  
)+
  
  geom_line(linewidth=1.2)+
  
  geom_point(size=3)+
  
  ylim(0.7,1)+
  
  theme_minimal()+
  
  labs(
    
    title="Year-wise Correlation between PCA and Sarma Indices",
    
    y="Correlation"
    
  )+
  
  theme(
    
    axis.text.x=
      element_text(
        angle=45,
        hjust=1
      )
    
  )

#==================================================
# RANK AGREEMENT BETWEEN PCA AND SARMA INDICES
# SPEARMAN'S RHO AND KENDALL'S TAU
#==================================================

library(dplyr)

#--------------------------------------------------
# 1. CREATE PCA RANKS
#--------------------------------------------------

comparison <-
  comparison %>%
  group_by(Year) %>%
  mutate(
    Rank_PCA = dense_rank(desc(FII_PCA))
  ) %>%
  ungroup()


#--------------------------------------------------
# 2. OVERALL RANK AGREEMENT
#--------------------------------------------------

overall_spearman <-
  cor(
    comparison$Rank_PCA,
    comparison$Rank_Sarma,
    method = "spearman",
    use = "complete.obs"
  )

overall_kendall <-
  cor(
    comparison$Rank_PCA,
    comparison$Rank_Sarma,
    method = "kendall",
    use = "complete.obs"
  )


# Overall results table

overall_rank_agreement <-
  data.frame(
    Period = "Overall (2016-17 to 2024-25)",
    Spearman_Rho = overall_spearman,
    Kendall_Tau = overall_kendall
  )

overall_rank_agreement


#--------------------------------------------------
# 3. YEAR-WISE RANK AGREEMENT
#--------------------------------------------------

yearwise_rank_agreement <-
  comparison %>%
  group_by(Year) %>%
  summarise(
    
    Spearman_Rho =
      cor(
        Rank_PCA,
        Rank_Sarma,
        method = "spearman",
        use = "complete.obs"
      ),
    
    Kendall_Tau =
      cor(
        Rank_PCA,
        Rank_Sarma,
        method = "kendall",
        use = "complete.obs"
      ),
    
    .groups = "drop"
  )

yearwise_rank_agreement







#Rank PCA States
comparison <-
  
  comparison %>%
  
  group_by(Year) %>%
  
  mutate(
    
    Rank_PCA=
      
      dense_rank(
        
        desc(FII_PCA)
        
      )
    
  )%>%
  
  ungroup()

#Rank Difference
comparison <-
  
  comparison %>%
  
  mutate(
    
    Rank_Difference=
      
      Rank_PCA-
      
      Rank_Sarma
    
  )
# Summary
summary(
  
  comparison$Rank_Difference
  
)
# Largest Agreements
comparison %>%
  
  arrange(
    
    abs(Rank_Difference)
    
  ) %>%
  
  head(20)
# Largest Disagreements
comparison %>%
  
  arrange(
    
    desc(abs(Rank_Difference))
    
  ) %>%
  
  head(20)
# Histogram
ggplot(
  
  comparison,
  
  aes(
    
    Rank_Difference
    
  )
  
)+
  
  geom_histogram(
    
    bins=20,
    
    fill="steelblue",
    
    colour="white"
    
  )+
  
  theme_minimal()+
  
  labs(
    
    title="Distribution of Ranking Differences",
    
    x="PCA Rank − Sarma Rank"
    
  )


##STATES WITH MOST CONSISTENT RANKS-Average rank difference by state.

state_rank_difference <-
  
  comparison %>%
  
  group_by(State)%>%
  
  summarise(
    
    Mean_Difference=
      
      mean(abs(Rank_Difference))
    
  )%>%
  
  arrange(
    
    Mean_Difference
    
  )

state_rank_difference

#Bar chart

ggplot(
  
  state_rank_difference,
  
  aes(
    
    reorder(
      
      State,
      
      Mean_Difference
      
    ),
    
    Mean_Difference
    
  )
  
)+
  
  geom_col(fill="steelblue")+
  
  coord_flip()+
  
  theme_minimal()+
  
  labs(
    
    title="Average Rank Difference Between PCA and Sarma",
    
    x="State",
    
    y="Average Absolute Rank Difference"
    
  )
#==================================================
# EXTENT OF RANK DIVERGENCE
#==================================================

rank_divergence_summary <- comparison %>%
  summarise(
    
    Total_Observations = n(),
    
    Exact_Agreement =
      sum(abs(Rank_Difference) == 0),
    
    Exact_Agreement_Percent =
      mean(abs(Rank_Difference) == 0) * 100,
    
    Within_1_Rank_Percent =
      mean(abs(Rank_Difference) <= 1) * 100,
    
    Within_3_Ranks_Percent =
      mean(abs(Rank_Difference) <= 3) * 100,
    
    Within_5_Ranks_Percent =
      mean(abs(Rank_Difference) <= 5) * 100,
    
    Mean_Absolute_Rank_Difference =
      mean(abs(Rank_Difference)),
    
    Median_Absolute_Rank_Difference =
      median(abs(Rank_Difference)),
    
    Maximum_Absolute_Rank_Difference =
      max(abs(Rank_Difference))
    
  )

rank_divergence_summary
#==================================================
# YEAR-WISE EXTENT OF RANK DIVERGENCE
#==================================================

yearwise_rank_divergence <- comparison %>%
  group_by(Year) %>%
  summarise(
    
    Exact_Agreement_Percent =
      mean(abs(Rank_Difference) == 0) * 100,
    
    Within_1_Rank_Percent =
      mean(abs(Rank_Difference) <= 1) * 100,
    
    Within_3_Ranks_Percent =
      mean(abs(Rank_Difference) <= 3) * 100,
    
    Mean_Absolute_Rank_Difference =
      mean(abs(Rank_Difference)),
    
    Median_Absolute_Rank_Difference =
      median(abs(Rank_Difference)),
    
    Maximum_Absolute_Rank_Difference =
      max(abs(Rank_Difference)),
    
    .groups = "drop"
  )

yearwise_rank_divergence


#-------------------------------

#REGION-WISE PCA VS SARMA

comparison <-
  
  comparison %>%
  
  left_join(
    
    panel_norm %>%
      
      select(
        
        State,
        Year,
        Region
        
      ),
    
    by=c("State","Year")
    
  )

#Average by region

region_compare <-
  
  comparison %>%
  
  group_by(
    
    Year,
    Region
    
  )%>%
  
  summarise(
    
    Mean_PCA=
      mean(FII_PCA),
    
    Mean_Sarma=
      mean(FII_Sarma),
    
    .groups="drop"
    
  )

region_compare

#Plot

ggplot(
  
  region_compare,
  
  aes(
    
    Year,
    
    Mean_PCA,
    
    group=Region,
    
    colour=Region
    
  )
  
)+
  
  geom_line(linewidth=1.2)+
  
  geom_point(size=2)+
  
  facet_wrap(~Region)+
  
  theme_minimal()+
  
  labs(
    
    title="Regional PCA Index"
    
  )+
  
  theme(
    
    axis.text.x=
      element_text(
        angle=45,
        hjust=1
      )
    
  )

#Same for Sarma

ggplot(
  
  region_compare,
  
  aes(
    
    Year,
    
    Mean_Sarma,
    
    group=Region,
    
    colour=Region
    
  )
  
)+
  
  geom_line(linewidth=1.2)+
  
  geom_point(size=2)+
  
  facet_wrap(~Region)+
  
  theme_minimal()+
  
  labs(
    
    title="Regional Sarma Index"
    
  )+
  
  theme(
    
    axis.text.x=
      element_text(
        angle=45,
        hjust=1
      )
    
  )



#==================================================
# 6.5.5 CROSS-VALIDATION OF SUBSTANTIVE FINDINGS
# A. ROBUSTNESS OF REGIONAL HIERARCHY
#==================================================

library(dplyr)
library(tidyr)

#--------------------------------------------------
# 1. CALCULATE REGIONAL RANKS FOR PCA AND SARMA
#--------------------------------------------------

regional_rankings <-
  
  region_compare %>%
  
  group_by(Year) %>%
  
  mutate(
    
    PCA_Rank =
      dense_rank(
        desc(Mean_PCA)
      ),
    
    Sarma_Rank =
      dense_rank(
        desc(Mean_Sarma)
      )
    
  ) %>%
  
  ungroup()


# Inspect regional rankings

regional_rankings %>%
  
  select(
    
    Year,
    Region,
    Mean_PCA,
    Mean_Sarma,
    PCA_Rank,
    Sarma_Rank
    
  ) %>%
  
  arrange(
    Year,
    PCA_Rank
  )


#==================================================
# 2. SELECT INITIAL AND FINAL YEARS
#==================================================

regional_rank_comparison <-
  
  regional_rankings %>%
  
  filter(
    
    Year %in% c(
      
      "2016-17",
      "2024-25"
      
    )
    
  ) %>%
  
  select(
    
    Region,
    Year,
    PCA_Rank,
    Sarma_Rank
    
  )


#==================================================
# 3. CREATE FINAL REGIONAL HIERARCHY TABLE
#==================================================

regional_hierarchy_table <-
  
  regional_rank_comparison %>%
  
  pivot_wider(
    
    names_from = Year,
    
    values_from = c(
      
      PCA_Rank,
      Sarma_Rank
      
    )
    
  ) %>%
  
  select(
    
    Region,
    
    `PCA_Rank_2016-17`,
    `Sarma_Rank_2016-17`,
    
    `PCA_Rank_2024-25`,
    `Sarma_Rank_2024-25`
    
  ) %>%
  
  arrange(
    `PCA_Rank_2024-25`
  )


# View final table

regional_hierarchy_table


#==================================================
# 4. OPTIONAL: CLEAN COLUMN NAMES FOR EXPORT
#==================================================

regional_hierarchy_export <-
  
  regional_hierarchy_table %>%
  
  rename(
    
    `PCA Rank 2016-17` =
      `PCA_Rank_2016-17`,
    
    `Sarma Rank 2016-17` =
      `Sarma_Rank_2016-17`,
    
    `PCA Rank 2024-25` =
      `PCA_Rank_2024-25`,
    
    `Sarma Rank 2024-25` =
      `Sarma_Rank_2024-25`
    
  )


regional_hierarchy_export


#==================================================
# 5. EXPORT TABLE
#==================================================

write.csv(
  
  regional_hierarchy_export,
  
  "PCA_Sarma_Regional_Hierarchy_Comparison.csv",
  
  row.names = FALSE
  
)


##==================================================
# 6.5.5(B) ROBUSTNESS OF SIGMA-CONVERGENCE FINDINGS
# PCA VS SARMA
##==================================================

library(dplyr)
library(tidyr)
library(ggplot2)


#==================================================
# 1. CHECK WHETHER PCA INDEX IS STRICTLY POSITIVE
#==================================================

pca_positivity_check <-
  
  comparison %>%
  
  group_by(Year) %>%
  
  summarise(
    
    Minimum_PCA =
      min(
        FII_PCA,
        na.rm = TRUE
      ),
    
    Mean_PCA =
      mean(
        FII_PCA,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )


pca_positivity_check


# Overall minimum PCA value

min(
  comparison$FII_PCA,
  na.rm = TRUE
)


#==================================================
# 2. SIGMA CONVERGENCE: SARMA INDEX
#==================================================

sigma_sarma <-
  
  comparison %>%
  
  group_by(Year) %>%
  
  summarise(
    
    Mean_FII =
      mean(
        FII_Sarma,
        na.rm = TRUE
      ),
    
    SD_FII =
      sd(
        FII_Sarma,
        na.rm = TRUE
      ),
    
    CV =
      SD_FII / Mean_FII,
    
    .groups = "drop"
    
  )


sigma_sarma


#==================================================
# 3. SIGMA CONVERGENCE: PCA INDEX
#==================================================

sigma_pca <-
  
  comparison %>%
  
  group_by(Year) %>%
  
  summarise(
    
    Mean_FII =
      mean(
        FII_PCA,
        na.rm = TRUE
      ),
    
    SD_FII =
      sd(
        FII_PCA,
        na.rm = TRUE
      ),
    
    CV =
      SD_FII / Mean_FII,
    
    .groups = "drop"
    
  )


sigma_pca


#==================================================
# 4. COMBINE PCA AND SARMA SIGMA-CONVERGENCE RESULTS
#==================================================

sigma_comparison <-
  
  bind_rows(
    
    sigma_pca %>%
      mutate(
        Method = "PCA"
      ),
    
    sigma_sarma %>%
      mutate(
        Method = "Sarma"
      )
    
  ) %>%
  
  select(
    
    Year,
    Method,
    Mean_FII,
    SD_FII,
    CV
    
  )


sigma_comparison


#==================================================
# 5. CREATE COMPARATIVE SIGMA-CONVERGENCE TABLE
#==================================================

sigma_comparison_table <-
  
  sigma_comparison %>%
  
  select(
    
    Year,
    Method,
    CV
    
  ) %>%
  
  pivot_wider(
    
    names_from = Method,
    
    values_from = CV,
    
    names_prefix = "CV_"
    
  ) %>%
  
  mutate(
    
    CV_PCA =
      round(
        CV_PCA,
        4
      ),
    
    CV_Sarma =
      round(
        CV_Sarma,
        4
      )
    
  )


sigma_comparison_table


#==================================================
# 6. COEFFICIENT OF VARIATION PLOT
# MAIN CROSS-METHOD ROBUSTNESS FIGURE
#==================================================

ggplot(
  
  sigma_comparison,
  
  aes(
    
    x = Year,
    
    y = CV,
    
    group = Method,
    
    colour = Method
    
  )
  
) +
  
  geom_line(
    
    linewidth = 1.2
    
  ) +
  
  geom_point(
    
    size = 3
    
  ) +
  
  theme_minimal() +
  
  labs(
    
    title =
      "Cross-State Dispersion in Financial Inclusion",
    
    subtitle =
      "Comparison of PCA and Sarma Indices",
    
    x =
      "Financial Year",
    
    y =
      "Coefficient of Variation",
    
    colour =
      "Index Method"
    
  ) +
  
  theme(
    
    axis.text.x =
      element_text(
        
        angle = 45,
        
        hjust = 1
        
      ),
    
    legend.position =
      "bottom"
    
  )


#==================================================
# 7. STANDARDISE THE SD SERIES
#==================================================
# This provides an additional scale-independent
# comparison of changes in dispersion over time.
#
# Each method's SD series is rebased to 100
# in the initial year (2016-17).
#==================================================

sigma_standardised <-
  
  sigma_comparison %>%
  
  group_by(Method) %>%
  
  arrange(
    
    Year,
    
    .by_group = TRUE
    
  ) %>%
  
  mutate(
    
    SD_Index =
      100 *
      SD_FII /
      first(SD_FII)
    
  ) %>%
  
  ungroup()


sigma_standardised


#==================================================
# 8. STANDARDISED DISPERSION PLOT
#==================================================

ggplot(
  
  sigma_standardised,
  
  aes(
    
    x = Year,
    
    y = SD_Index,
    
    group = Method,
    
    colour = Method
    
  )
  
) +
  
  geom_hline(
    
    yintercept = 100,
    
    linetype = "dashed",
    
    colour = "grey50"
    
  ) +
  
  geom_line(
    
    linewidth = 1.2
    
  ) +
  
  geom_point(
    
    size = 3
    
  ) +
  
  theme_minimal() +
  
  labs(
    
    title =
      "Evolution of Cross-State Dispersion in Financial Inclusion",
    
    subtitle =
      "Standard Deviation Rebased to 2016-17 = 100",
    
    x =
      "Financial Year",
    
    y =
      "Standardised Dispersion (2016-17 = 100)",
    
    colour =
      "Index Method"
    
  ) +
  
  theme(
    
    axis.text.x =
      element_text(
        
        angle = 45,
        
        hjust = 1
        
      ),
    
    legend.position =
      "bottom"
    
  )


#==================================================
# 9. INITIAL VS FINAL DISPERSION COMPARISON
#==================================================

sigma_initial_final <-
  
  sigma_comparison %>%
  
  filter(
    
    Year %in% c(
      
      "2016-17",
      "2024-25"
      
    )
    
  ) %>%
  
  select(
    
    Method,
    Year,
    CV
    
  ) %>%
  
  pivot_wider(
    
    names_from = Year,
    
    values_from = CV
    
  ) %>%
  
  mutate(
    
    Absolute_Change =
      `2024-25` -
      `2016-17`,
    
    Percent_Change =
      (
        (`2024-25` - `2016-17`) /
          `2016-17`
      ) * 100
    
  )


sigma_initial_final


#==================================================
# 10. ADD DIRECTION OF SIGMA-CONVERGENCE
#==================================================

sigma_initial_final <-
  
  sigma_initial_final %>%
  
  mutate(
    
    Finding =
      case_when(
        
        Absolute_Change < 0 ~
          "Decreasing dispersion (Sigma convergence)",
        
        Absolute_Change > 0 ~
          "Increasing dispersion (Sigma divergence)",
        
        TRUE ~
          "Stable dispersion"
        
      )
    
  )


sigma_initial_final


#==================================================
# 11. FINAL SUMMARY TABLE
#==================================================

sigma_robustness_table <-
  
  sigma_initial_final %>%
  
  mutate(
    
    `CV 2016-17` =
      round(
        `2016-17`,
        4
      ),
    
    `CV 2024-25` =
      round(
        `2024-25`,
        4
      ),
    
    `Absolute Change` =
      round(
        Absolute_Change,
        4
      ),
    
    `Percentage Change (%)` =
      round(
        Percent_Change,
        2
      )
    
  ) %>%
  
  select(
    
    Method,
    
    `CV 2016-17`,
    
    `CV 2024-25`,
    
    `Absolute Change`,
    
    `Percentage Change (%)`,
    
    Finding
    
  )


sigma_robustness_table


#==================================================
# 12. EXPORT RESULTS
#==================================================

write.csv(
  
  sigma_comparison_table,
  
  "PCA_Sarma_Sigma_Convergence_Yearwise.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  sigma_robustness_table,
  
  "PCA_Sarma_Sigma_Convergence_Robustness.csv",
  
  row.names = FALSE
  
)

