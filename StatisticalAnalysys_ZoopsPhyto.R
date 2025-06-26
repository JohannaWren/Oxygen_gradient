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
  #filter(Filter != 'bulk') %>% 
  mutate(Size=as.numeric(Filter))
# Turning a character (bulk) into a number produces an NA. To put the size in there instead we run the below line. Bulk filters are 0.7um pore size
phyto$Size[is.na(phyto$Size)] <-  0.7
head(phyto)
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

df <- data.frame(
  Station = normalized_biomass$net_cast_number, 
  Biomass = normalized_biomass$log_normalized_biomass)

df$Station <- factor(df$Station, levels = unique(df$Station))

summary_df <- df %>%
  group_by(Station) %>%
  summarise(
    mean = mean(Biomass, na.rm = TRUE),
    median = median(Biomass, na.rm = TRUE),
    p10 = quantile(Biomass, 0.10, na.rm = TRUE),
    p25 = quantile(Biomass, 0.25, na.rm = TRUE),
    p75 = quantile(Biomass, 0.75, na.rm = TRUE),
    p90 = quantile(Biomass, 0.90, na.rm = TRUE)
  ) %>%
  mutate(xnum = as.numeric(Station))

# Create the plot
ggplot(df, aes(x = Station, y = Biomass)) +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 1.5) +
  # Whiskers (10% to 90%)
  geom_linerange(data = summary_df, aes(x = Station, ymin = p10, ymax = p90), 
                 color = "black", size = 0.5, inherit.aes = FALSE) +
  # IQR box (25% to 75%)
  geom_rect(data = summary_df,
            aes(xmin = xnum - 0.2, xmax = xnum + 0.2,
                ymin = p25, ymax = p75),
            fill = "lightgrey", color = "black", alpha = 0.5, inherit.aes = FALSE) +
  # Median line
  geom_segment(data = summary_df,
               aes(x = xnum - 0.2, xend = xnum + 0.2,
                   y = median, yend = median),
               color = "red", size = 0.8, inherit.aes = FALSE) +
  theme_bw() +
  labs(y = expression(log[10]~"Normalized Biomass [g]")) +
  ggtitle(label = "SE2204 Zooplankton Normalized Biomass Variability", subtitle = "Blue point is the station mean; the red line is the station median; whiskers are the 10 and 90 % ranges; the box is the 75th and 25th IQR.")

# ggsave('ZoopBoxPlot.png', width=10, height = 5.625, dpi = 300, units = 'in')








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

# Linear model



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


ggplot(phyto_int2, aes(x = Station, y = IntegratedBiomass)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  ylab("Phytoplankton Biomass Integrated by depth (g/m²)") +
  ggtitle("Depth-Integrated Phytoplankton Biomass by Station")






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

#  ------------------- Variability Plots --------------------------------------
zoops <- read_xlsx(paste(here(), 'Biomass filter weights.xlsx', sep='/'), sheet = 1) # Emma's
# zoops <- read_xlsx(paste(here(), 'Data/Biomass filter weights.xlsx', sep='/'), sheet = 1)  # Johanna's
head(zoops)

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
    log_normalized_biomass = log10(normalized_biomass),
    log_normalized_size = log10(size_fraction))



# --------------------------ZOOP Box Plots--------------------------------------
df <- data.frame(
  Station = normalized_biomass$net_cast_number, 
  Biomass = normalized_biomass$log_normalized_biomass)

df$Station <- factor(df$Station, levels = unique(df$Station))

  summary_df <- df %>%
    group_by(Station) %>%
    summarise(
      mean = mean(Biomass, na.rm = TRUE),
      median = median(Biomass, na.rm = TRUE),
      p10 = quantile(Biomass, 0.10, na.rm = TRUE),
      p25 = quantile(Biomass, 0.25, na.rm = TRUE),
      p75 = quantile(Biomass, 0.75, na.rm = TRUE),
      p90 = quantile(Biomass, 0.90, na.rm = TRUE)
    ) %>%
    mutate(xnum = as.numeric(Station))
  
  # Create the plot
  ggplot(df, aes(x = Station, y = Biomass)) +
    stat_summary(fun = mean, geom = "point", color = "blue", size = 1.5) +
    # Whiskers (10% to 90%)
    geom_linerange(data = summary_df, aes(x = Station, ymin = p10, ymax = p90), 
                   color = "black", size = 0.5, inherit.aes = FALSE) +
    # IQR box (25% to 75%)
    geom_rect(data = summary_df,
              aes(xmin = xnum - 0.2, xmax = xnum + 0.2,
                  ymin = p25, ymax = p75),
              fill = "lightgrey", color = "black", alpha = 0.5, inherit.aes = FALSE) +
    # Median line
    geom_segment(data = summary_df,
                 aes(x = xnum - 0.2, xend = xnum + 0.2,
                     y = median, yend = median),
                 color = "red", size = 0.8, inherit.aes = FALSE) +
    theme_bw() +
    labs(y = expression(log[10]~"Normalized Biomass [g]")) +
    ggtitle(label = "SE2204 Zooplankton Normalized Biomass Variability", subtitle = "Blue point is the station mean; the red line is the station median; whiskers are the 10 and 90 % ranges; the box is the 75th and 25th IQR.")

# ggsave('ZoopBoxPlot.png', width=10, height = 5.625, dpi = 300, units = 'in')
  
