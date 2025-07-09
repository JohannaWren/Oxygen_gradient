# Date: June 26, 2025
# Author: Johanna LK Wren & Emma Scott-Wellman
# email: johanna.wren@noaa.gov emma.scott-wellman@noaa.gov
# Description: Analysis of phytoplankton data 

library(ggpmisc)
library(dplyr)
library(readxl)
library(here)
library(ggplot2)
library(lubridate)
library(tidyr)
library(data.table)
library(paletteer)

# load the data
phyto <- read.csv(paste(here(), 'fluorometry_SE2204.csv', sep='/'))  # Emma's
# phyto <- read.csv(paste(here(), 'Data/fluorometry_SE2204.csv', sep='/'))  # Johanna's
head(phyto)

# Clean up the phytoplankton data and make sure and turn filter into sizes
phyto <- phyto %>% 
  mutate(Size=as.numeric(Filter))

# Make file for nice station plotting
id.labs <- phyto$Station
names(id.labs) <- phyto$Cast
head(id.labs)

#Link Cast labels to the Station ID
cast_labels <- phyto %>%
  distinct(Cast, Station) %>%
  arrange(Cast)
label_vector <- setNames(cast_labels$Station, cast_labels$Cast)

phyto <- phyto %>%
  mutate(Filter = as.numeric(Filter))

bin_edges2 <- data.frame(
  Filter = c(0.2, 2, 20),
  plower_bound = c(0.2, 2, 20),
  pupper_bound = c(2, 20, 200 )) %>%
  mutate(pbin_width = pupper_bound - plower_bound)

# Create columns with  calculated normalized biomass and then add log biomass and  log size columns 
pnorm <- phyto %>%
  left_join(bin_edges2, by = "Filter") %>%
  mutate(p_normalized_biomass = Chlorophyll / pbin_width)



# ---------------------- Bulk Phyto Box Plot ----------------------------------
bulk_phyto <- read.csv(paste(here(), 'fluorometry_SE2204.csv', sep='/'))
bulk_phyto <- subset(bulk_phyto, Filter == "bulk")

# bulk_df <- data.frame(
#   Station = bulk_phyto$Station, 
#   Biomass = bulk_phyto$Chlorophyll)
# 
# bulk_df$Station <- factor(df$Station, levels = unique(df$Station))

# bulk_sum <- bulk_df %>%
#   group_by(Station) %>%
#   summarise(
#     mean = mean(Biomass, na.rm = TRUE),
#     median = median(Biomass, na.rm = TRUE),
#     p10 = quantile(Biomass, 0.10, na.rm = TRUE),
#     p25 = quantile(Biomass, 0.25, na.rm = TRUE),
#     p75 = quantile(Biomass, 0.75, na.rm = TRUE),
#     p90 = quantile(Biomass, 0.90, na.rm = TRUE)
#   ) %>%
#   mutate(xnum = as.numeric(Station))


BoxP_phyto <- function(data, depth_value) {
  filtered <- data %>%
    filter(Depth == depth_value)
  
  summary_df <- filtered %>%
    group_by(Station, Cast) %>%
    summarise(
      p10 = quantile(Chlorophyll, 0.10, na.rm = TRUE),
      p25 = quantile(Chlorophyll, 0.25, na.rm = TRUE),
      median = median(Chlorophyll, na.rm = TRUE),
      p75 = quantile(Chlorophyll, 0.75, na.rm = TRUE),
      p90 = quantile(Chlorophyll, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Label stations as Names in chronological order based on cast
  cast_order <- filtered %>%
    arrange(Cast) %>%
    distinct(Station, Cast) %>%
    pull(Station)
  
  filtered <- filtered %>%
    mutate(Station = factor(Station, levels = cast_order),
           xnum = as.numeric(Station))
  
  #Create Plot
  ggplot(filtered, aes(x = Station, y = Chlorophyll)) +
    # Mean point
    stat_summary(fun = mean, geom = "point", color = "black", size = 1.5) +
    theme_bw() +
    labs(y = "Chlorophyll Biomass [μg/L]") +
    ggtitle(
      label = "SE2204 Bulk Chlorophyll by station and depth",
      subtitle = paste("Plotted for samples at depth =", depth_value, "[m]")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

BoxP_phyto(bulk_phyto, 200)
BoxP_phyto(bulk_phyto, 150)
BoxP_phyto(bulk_phyto, 125)
BoxP_phyto(bulk_phyto, 100)
BoxP_phyto(bulk_phyto, 80)
BoxP_phyto(bulk_phyto, 65)
BoxP_phyto(bulk_phyto, 50)
BoxP_phyto(bulk_phyto, 35)
BoxP_phyto(bulk_phyto, 20)
BoxP_phyto(bulk_phyto, 0)


# -------------------Depth Integrate the Phyto Data --------------------------
# Trapezoidal Integration Function
p_int <- function(Depth, Chlorophyll) {
  # Remove NA values
  valid <- !is.na(Depth) & !is.na(Chlorophyll)
  Depth <- Depth[valid]
  Chlorophyll <- Chlorophyll[valid]
  if(length(Depth) < 2) return(NA)
  # Sort by increasing depth
  ord <- order(Depth)
  Depth <- Depth[ord]
  Chlorophyll <- Chlorophyll[ord]
  # Trapezoidal rule
  sum(diff(Depth) * (head(Chlorophyll, -1) + tail(Chlorophyll, -1)) / 2)
}

# Summarise by station for raw data 
phyto_int <- pnorm %>%
  filter(!is.na(Chlorophyll)) %>%
  arrange(Station, Depth) %>%
  group_by(Station) %>%
  summarise(
    IntegratedBiomass = p_int(Depth, Chlorophyll),
    .groups = "drop"
  )
head(phyto_int)

# Summarise by station for noramlized data
pnorm_int <- pnorm %>%
  filter(!is.na(Chlorophyll)) %>%
  arrange(Station, Depth) %>%
  group_by(Station) %>%
  summarise(
    IntegratedBiomass = p_int(Depth, p_normalized_biomass),
    .groups = "drop"
  )
head(pnorm_int)

# Using Raw Chlorophyll Data not normalized
phyto_int2 <- phyto %>%
  arrange(Station, Depth) %>%
  group_by(Station) %>%
  summarise(
    IntegratedBiomass = p_int(Depth, Chlorophyll)
  )

phyto_int2$Station <- as.factor(phyto_int2$Station)
lm_station <- lm(IntegratedBiomass ~ Station, data = phyto_int)
summary(lm_station)

anova(lm_station)


# --------------------------- Phyto Plots --------------------------------------
# created a data set that include the integrated chlorophyll
phyto_integrated <- phyto %>%
  group_by(Station, Cast, Filter, Size) %>%
  summarise(
    integrated_chl = p_int(Depth, Chlorophyll),
    .groups = "drop"
  )

# Create Labels for the integrated data set 
cast_labels <- phyto_integrated %>%
  arrange(Cast) %>%
  distinct(Cast, Station)

label_vector <- setNames(cast_labels$Station, cast_labels$Cast)

# Plot
ggplot(phyto_integrated, aes(x = as.factor(Cast), y = integrated_chl)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  scale_x_discrete(labels = label_vector) +
  ylab("Phytoplankton Biomass Integrated by depth (g/m²)") +
  xlab("Station") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Depth-Integrated Phytoplankton Biomass by Station")

ggplot(phyto_integrated, aes(x = as.factor(Cast), y = integrated_chl)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  theme_bw() +
  scale_x_discrete(labels = label_vector) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Station",
    y = "Chlorophyll Biomass [μg/L]",
    title = "Variability in Chlorophyll Biomass Between Stations"
  )

ggplot(phyto_integrated, aes(x = as.factor(Cast), y = integrated_chl)) +
  geom_col(fill = "lightblue", color = "black") +
  theme_bw() +
  scale_x_discrete(labels = label_vector) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Station",
    y = "Chlorophyll Biomass [μg/L]",
    title = "Chlorophyll Biomass Across Stations"
  )

# Variability plot needs to be calculated for flow 

# ggplot(phyto, aes(x = factor(Station), y = Chlorophyll)) +
#   geom_boxplot() +
#   facet_wrap(~ Depth) +
#   theme_bw() +
#   labs(
#     x = "Station",
#     y = "Chlorophyll Biomass [μg/L]",
#     title = "Chlorophyll Variability by Station and Depth"
#   )


# -------------------Linear Regression Model Analysis--------------------------
# the independent variable (X) is plotted on the horizontal axis (x-axis), and the dependent variable (Y) is plotted on the vertical axis (y-axis)
# this is the predictor variable. It's the variable that is thought to influence or cause changes in the other variable. In a graph, it's on the x-axis.  <- lm(y ~ x)
selected_depth <- 125
df_filtered <- subset(pnorm, Depth == selected_depth)

# x <- log2(df_filtered$Size)
# y <- log2(df_filtered$p_normalized_biomass)

x <- (df_filtered$Station)
y <- (df_filtered$Size)

model <- lm(y ~ x)
# Get the coefficients (beta0 and beta1)
beta0 <- coef(model)[1]  # Intercept
beta1 <- coef(model)[2]  # Slope

# Calculate Residual Sum of Squares (RSS)
rss <- sum(residuals(model)^2)

# Calculate Regression Sum of Squares (SSreg)
# SSreg = SS(Total) - SS(Residual)
sst <- sum((y - mean(y))^2) # Total Sum of Squares
ssreg <- sst - rss

# Output the results
cat("Beta0 (Intercept):", beta0, "\n")
cat("Beta1 (Slope):", beta1, "\n")
cat("Residual Sum of Squares (RSS):", rss, "\n")
cat("Regression Sum of Squares (SSreg):", ssreg, "\n")

# SSreg is NA which could mean multicolinearity, a linear relationship, or insufficient data 
# Selecting a particular depth and then plotting all depths still has NA
# Plotting depth vs biomass is still NA
# doing station for 125 depth to biomass is still NA

summary(model)




#  ------------------------------- ZOOPS --------------------------------------


#  ------------------- Variability Plots --------------------------------------
zoops <- read.csv(paste(here(), 'Biomass filter weights_USE_THIS.csv', sep='/')) # Emma's
# zoops <- read_xlsx(paste(here(), 'Data/Biomass filter weights.xlsx', sep='/'), sheet = 1)  # Johanna's
head(zoops)

ggplot(zoops, aes(x = net_cast_number, y = standard_haul_factor)) +
  geom_point()

# Plot for VWS
ggplot(zoops, aes(x = net_cast_number, y = volume_water_strained)) +
  geom_point()


# SHF and VWS plotted side by side 
zoops_long <- zoops %>%
  pivot_longer(cols = c(standard_haul_factor, volume_water_strained),
               names_to = "variable", values_to = "value")


#Plot using the Standardized Phytoplankton (total_net_wt/volume water strained) 
ggplot(zoops, aes(x = net_cast_number, y = standardized_plankton_volume )) +
  geom_point() +
  theme_bw() +
  labs(y = "Standardized Plankton Volume [ml/m³]", x = "Station") +
  ggtitle(
    "SE2204 Standardized Zooplankton Volumes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# ggsave('StnadZoopV_scatter.png', width=10, height = 5.625, dpi = 300, units = 'in')

omit6 <- zoops %>% filter(!net_cast_number == 6)
  
ggplot(omit6, aes(x = net_cast_number, y = standardized_SHF )) +
  geom_point() +
  theme_bw() +
  labs(y = "Standardized Haul Factor", x = "Station") +
  ggtitle(
    "SE2204 Standardized Haul Factor Zooplankton") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# ggsave('StnadZoopV_scatter.png', width=10, height = 5.625, dpi = 300, units = 'in')


zoops_day <- zoops %>%
  filter(net_cast_number %in% c(1, 3, 4, 6, 7, 9, 10, 12))

ggplot(zoops_day, aes(x = net_cast_number, y = standardized_plankton_volume )) +
  geom_point() +
  theme_bw() +
  labs(y = "Standardized Plankton Volume [ml/m^3]", x = "Day Stations") +
  ggtitle(
    "SE2204 Standardized Zooplankton Volumes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

zoops_night <- zoops %>%
  filter(net_cast_number %in% c(2,5,8,11,13))

ggplot(zoops_night, aes(x = net_cast_number, y = standardized_plankton_volume )) +
  geom_point() +
  theme_bw() +
  labs(y = "Standardized Plankton Volume [ml/m^3]", x = "Night Stations") +
  ggtitle(
    "SE2204 Standardized Zooplankton Volumes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Plot for SHF
ggplot(zoops, aes(x = net_cast_number, y = standard_haul_factor)) +
  geom_point()

# Plot for VWS
ggplot(zoops, aes(x = net_cast_number, y = volume_water_strained)) +
  geom_point()


# SHF and VWS plotted side by side 
zoops_long <- zoops %>%
  pivot_longer(cols = c(standard_haul_factor, volume_water_strained),
               names_to = "variable", values_to = "value")

ggplot(zoops_long, aes(x = net_cast_number, y = value)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(y = "Value") +
  theme_minimal()


ggplot(zoops_long, aes(x = net_cast_number, y = value)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(y = "Value") +
  theme_minimal()
# Clean up zooplankton data
zoops <- zoops %>% 
  filter(net_cast_number <= 13) %>% 
  select(net_cast_number, size_fraction, net_dry_weight) %>% 
  group_by(net_cast_number, size_fraction) %>% 
  summarise(net_dry_weight=sum(net_dry_weight))

zoopTrend <- lm(size_fraction ~ net_dry_weight, data=zoops)
head(zoops)



# ----------------------------ZOOP DATA ---------------------------------

# Normalize the data for abundance vs size using the Normalized Biomass 
# Spectral Slope (NBSS) per Platt and Denman (1978)

#Create a dataframe using the edges of the bins to calculate bin width 
bin_edges <- data.frame(
  size_fraction = c(200, 500, 1000, 2000, 5000),
  lower_bound = c(200, 500, 1000, 2000, 5000),
  upper_bound = c(500, 1000, 2000, 5000, 10000)
) %>%
  mutate(bin_width = upper_bound - lower_bound)

# Merge with zoops data and calculate normalized biomass
# Create columns with  calculated normalized biomass and then add log biomass and  log size columns 
normalized_biomass <- zoops %>%
  left_join(bin_edges, by = "size_fraction") %>%
  mutate(
    normalized_biomass = net_dry_weight / bin_width,
    log_normalized_biomass = log2(normalized_biomass),
    log_normalized_size = log2(size_fraction))



# --------------------------ZOOP Box Plots--------------------------------------
Box_df <- data.frame(
  Station = normalized_biomass$net_cast_number, 
  Biomass = normalized_biomass$log_normalized_biomass)

Box_df$Station <- factor(df$Station, levels = unique(df$Station))

summary_df <- Box_df %>%
  group_by(Station) %>%
  summarise(
    mean = mean(Biomass, na.rm = TRUE),
    median = median(Biomass, na.rm = TRUE),
    p10 = quantile(Biomass, 0.10, na.rm = TRUE),
    p25 = quantile(Biomass, 0.25, na.rm = TRUE),
    p75 = quantile(Biomass, 0.75, na.rm = TRUE),
    p90 = quantile(Biomass, 0.90, na.rm = TRUE)
  )


ggplot(Box_df, aes(x = Station, y = Biomass)) +
  geom_crossbar(
    data = summary_df,
    aes(x = Station, y = median, ymin = p25, ymax = p75),
    fill = "lightgrey", color = "black", alpha = 0.5, width = 0.4,
    inherit.aes = FALSE
  ) +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 1.5) +
  geom_linerange(
    data = summary_df,
    aes(x = Station, ymin = p10, ymax = p90),
    color = "black", size = 0.5, inherit.aes = FALSE
  ) +
  geom_segment(
    data = summary_df,
    aes(x = as.numeric(Station) - 0.2, xend = as.numeric(Station) + 0.2,
        y = median, yend = median),
    color = "red", size = 0.8, inherit.aes = FALSE
  ) +
  theme_bw() +
  labs(y = expression(log[10]~"Normalized Biomass [g]")) +
  ggtitle(
    "SE2204 Zooplankton Normalized Biomass Variability",
    subtitle = "Blue point = mean; red line = median; whiskers = 10–90%; box = IQR"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave('NormZoopV_Boxplot.png', width=10, height = 5.625, dpi = 300, units = 'in')


# -------------------------- ZOOP ANCOVA ----------------------------------------
# ANCOVA model
library(emmeans)
library(broom)

# Read in CTD data 
ctdAll <- read.csv('CTD_data_forAnalysis.csv')
stnInfo <- read.csv('CTD_metadata_forAnalysis.csv')
id.labs <- stnInfo$Station2
names(id.labs) <- stnInfo$Cast

# Add Station2 column
ctdAll_with_station <- left_join(ctdAll, stnInfo, by = "Cast")

# Make sure they are compatible
zoops_ANCOVA <- normalized_biomass %>% rename(Station2 = net_cast_number)
zoops_ANCOVA <- zoops_ANCOVA %>%
  mutate(Station2 = as.character(Station2))

ctdAll_with_station <- ctdAll_with_station %>%
  mutate(Station2 = as.character(Station2))

# Now summarize
ctd_summary <- ctdAll_with_station %>%
  group_by(Station2) %>%
  summarise(
    mean_temperature = mean(Temperature, na.rm = TRUE),
    mean_salinity = mean(Salinity, na.rm = TRUE),
    mean_depth = mean(Depth.y, na.rm = TRUE), 
  )

# Merge CTD data with zoops data
merged_df <- left_join(ctd_summary, zoops_ANCOVA, by = "Station2")

# Run ANCOVA with temperature
ancova_model <- lm(log_normalized_biomass ~ Station2 + mean_temperature, data = merged_df)
summary(ancova_model)

# Test interaction
ancova_model_interact <- lm(log_normalized_biomass ~ Station2 * mean_temperature, data = merged_df)
anova(ancova_model, ancova_model_interact)

ancova_model <- lm(log_normalized_biomass ~ Station2 + mean_temperature, data = merged_df)
summary(ancova_model)

par(mfrow = c(2, 2))
plot(ancova_model)

# ANOVA table
anova(ancova_model)

# Post hoc comparisons (to see if Station is significant)
emmeans(ancova_model, pairwise ~ Station2)


# --------------------- Function for ANCOVA and analysis ------------------------
run_ancova_diagnostics <- function(data, covariate) {
  # Formula without interaction
  formula_main <- as.formula(paste0("log_normalized_biomass ~ Station2 + ", covariate))
  
  # Fit main ANCOVA model
  ancova_model <- lm(formula_main, data = data)
  print(summary(ancova_model))
  
  # Formula with interaction
  formula_interact <- as.formula(paste0("log_normalized_biomass ~ Station2 * ", covariate))
  
  # Fit interaction model
  ancova_model_interact <- lm(formula_interact, data = data)
  
  # Compare models with and without interaction
  print(anova(ancova_model, ancova_model_interact))
  
  # Diagnostic plots to see if the data is normalized enough to do accurate comparisons 
  # Plot 1 check linearity and homoscedasticity
  # Plot 2 To check if residuals are approximately normally distributed
  # Plot 3 Check for homoscedasticity as equal variance.
  # Plot 4 To identify influential points that might disproportionately affect the model.
  par(mfrow = c(2, 2))
  plot(ancova_model)
  par(mfrow = c(1, 1))
  
  # ANOVA table
  print(anova(ancova_model))
  
  # Post hoc comparisons for Station2 factor
  emm <- emmeans(ancova_model, pairwise ~ Station2)
  print(emm)
  
  invisible(list(
    model = ancova_model,
    interaction_model = ancova_model_interact,
    emmeans = emm
  ))
}


run_ancova_diagnostics(merged_df, "mean_temperature")
# p-value: 0.9115
# clumped pattern suggests non-constant variance or non-linerity 
# Large deviations, especially at the tails, suggest normality issues

run_ancova_diagnostics(data = merged_df, covariate = "mean_salinity")

run_ancova_diagnostics(data = merged_df, covariate = "size_fraction")

run_ancova_diagnostics(data = merged_df, covariate = "net_dry_weight")





# ----------------------- DAY/NIGHT ZOOP ANALYSIS ------------------------------


# --------------------- NIGHT ANCOVA and ANOVA analysis ------------------------
# ANOVA 
# Night Stations Zooplankton
norm_biomass_night <- normalized_biomass %>%
  filter(net_cast_number %in% c(2, 5, 8, 11, 13))


anova_night <- aov(net_dry_weight ~ as.factor(net_cast_number) * size_fraction, data = norm_biomass_night)
summary(anova_night)
#significance in size fraction means that size fraction has a strong effect on biomass — as size increases (or decreases), biomass changes dramatically.
# This is expected: particle size often has a strong relationship with biomass.

anova_night_log <- aov(log_normalized_biomass ~ as.factor(net_cast_number) * log_normalized_size, data = norm_biomass_night)
summary(anova_night_log)

# ANCOVA 
ANCOVA_night <- merged_df %>%
  filter(Station2 %in% c(1, 3, 4, 6, 7, 9, 10, 12))


run_ancova_diagnostics(ANCOVA_night, "mean_temperature")

run_ancova_diagnostics(data = ANCOVA_night, covariate = "mean_salinity")

run_ancova_diagnostics(data = ANCOVA_night, covariate = "size_fraction")
#*** (p < 2e-16)	Strong negative relationship with biomass. As size increases, biomass decreases.
# * (p = 0.0129)	Biomass varies by station (i.e., there are some significant differences in biomass between stations).
# No interaction effect 
# R² = 0.91, Adjusted R² = 0.887; The model explains nearly 91% of the variance in log-normalized biomass.
# Residuals look well-behaved (range ≈ -1.7 to +1.4), suggesting the assumptions of ANCOVA are reasonably met.

run_ancova_diagnostics(data = ANCOVA_night, covariate = "net_dry_weight")
# Covariate (net_dry_weight): Highly significant: p = 1.93e-06 *** This means net_dry_weight is a strong predictor of log_normalized_biomass.
# So, there is no strong evidence that differences among stations explain additional variation in biomass after accounting for net_dry_weight.
# No interaction
# No significant differences between any stations. Suggests that Station is not influencing biomass after controlling for net_dry_weight.


# --------------------- DAY ANCOVA and ANOVA analysis ------------------------
# ANOVA 
# Day Stations Zooplankton Biomass Spectrum
# Filter data for only day stations 
norm_biomass_day <- normalized_biomass %>%
  filter(net_cast_number %in% c(1, 3, 4, 6, 7, 9, 10, 12))

anova_day <- aov(net_dry_weight ~ as.factor(net_cast_number) * size_fraction, data = norm_biomass_day)
summary(anova_day)


anova_day_log <- aov(log_normalized_biomass ~ as.factor(net_cast_number) * log_normalized_size, data = norm_biomass_day)
summary(anova_day_log)
# signifigance for net_cast_number means the mean log-normalized biomass differs across net casts
# So: Different sampling casts (e.g., locations or times) have different average biomass, even after accounting for size.

# ANCOVA 
ANCOVA_day <- merged_df %>%
  filter(Station2 %in% c(1, 3, 4, 6, 7, 9, 10, 12))

run_ancova_diagnostics(ANCOVA_night, "mean_temperature")
# p-value: 0.9115
# clumped pattern suggests non-constant variance or non-linerity 
# Large deviations, especially at the tails, suggest normality issues

run_ancova_diagnostics(data = ANCOVA_day, covariate = "mean_salinity")


run_ancova_diagnostics(data = ANCOVA_day, covariate = "size_fraction")
# The covariate size_fraction is extremely significant. Its effect is negative (Estimate = -0.001281) 
# larger size fractions are associated with lower log-normalized biomass.
# Station2 is now significant overall, unlike in the night data. There is enough between-station variation in biomass after accounting for size_fraction.
# No interaction effect
# the model explains ~91% of the variation in biomass
# Station 12 is significantly different than Stations 1, 3, 6, and 9. 
# Station 1 vs. Station 12 → p = 0.0464
# Station 12 vs. Station 3 → p = 0.0116
# Station 12 vs. Station 6 → p = 0.0071
# Station 12 vs. Station 9 → p = 0.0486
# This suggests that something unique is happening at Station 12 during the day — possibly local conditions, productivity, or sampling effect.

run_ancova_diagnostics(data = ANCOVA_day, covariate = "net_dry_weight")
# Station212 has a statistically significant negative effect on log_normalized_biomass, even after adjusting for net_dry_weight.
# The model explains ~55% of the variance in biomass, a moderate fit
# net_dry_weight is a strong positive predictor of biomass.
# Station 12 shows a significant negative effect (lower biomass than Station 1 even after accounting for net weight), suggesting an ecological or environmental anomaly at this site.
# No interaction; the relationship between net_dry_weight and biomass is consistent across stations.
# Station212 shows a noticeably lower mean biomass, consistent with the significant negative coefficient seen earlier.
# Station212 may have unusual environmental conditions (e.g., temperature, salinity, depth) Could reflect species-specific composition such as more small-bodied taxa  = lower biomass



# --------------------- Multiple Linear Regression Model ------------------------
env_data <- read.csv("SE2204_PhyZo_Nuts.csv")


model_env <- lm(Station ~ Pressure + Temperature + Conductivity + Flourescence + Density + Salinity + Oxygen + newLat + phyto_bulk + phosphate + silicate + nitrate...nitrite + ammonia, data = env_data)
summary(model_env)

env_vars <- env_data[, c("Pressure", "Temperature", "Conductivity", "Flourescence", "Density", "Salinity", 
                         "Oxygen", "newLat", "phyto_bulk", "phosphate", "silicate", "nitrate...nitrite", "ammonia")]


# ---------------------------------- PCA -------------------------------------

# Standardize and run PCA
env_pca <- prcomp(env_vars, scale. = TRUE)

# Adding 2 PCs
env_data$PC1 <- env_pca$x[, 1]
env_data$PC2 <- env_pca$x[, 2]

#Create PCA using PCs
model_env <- lm(Station ~ PC1 + PC2, data = env_data)
summary(model_env)
# Significant — PC2 has a negative effect on Station

loadings <- env_pca$rotation
print(loadings[, 1:2])
# Nutrients (N, P, Si), Phytoplankton	Nutrient-productivity gradient
# Lower-numbered stations (like Station 1 or 2) are associated with higher PC2, i.e.: Higher phyto_bulk and Lower nutrient concentrations
# Higher-numbered stations (like Station 10 or 18) are associated with lower PC2, i.e.: Higher nutrient levels and Lower phytoplankton biomass
# Higher-numbered stations are more nutrient-rich and less productive (in terms of phytoplankton biomass), while lower-numbered stations are more productive but nutrient-depleted.
# Zooplankton and station variation are more strongly linked to ecological productivity conditions than to physical water properties such as phytoplankton ?

biplot(env_pca, scale = 0)

ggplot(env_data, aes(x = PC1, y = PC2, label = Station)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  labs(title = "PCA of Environmental Variables")


