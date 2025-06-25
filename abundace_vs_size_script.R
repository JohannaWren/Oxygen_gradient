# Skeleton script for making abundance size plots

# load libraries
#install.packages("ggpmisc")  # Only if not installed
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
id.labs

#Link Cast labels to the Station ID
cast_labels <- phyto %>%
  distinct(Cast, Station) %>%
  arrange(Cast)
label_vector <- setNames(cast_labels$Station, cast_labels$Cast)
  
# Scatter plots for all casts 
phyto %>% 
  ggplot(aes(x=Chlorophyll, y=Depth)) + 
  geom_point(colour = 'darkgreen') +
  scale_y_reverse() + 
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('Temperature [°C]')+ ylab('Depth [m]') +
  ggtitle('Temperature Depth Profiles SE2204')
# ggsave('PhytoT_scatter.png', width=10, height = 5.625, dpi = 300)

#------------------------Normalizing Phyto Data--------------------------------
# Convert phyto$Filter to numeric
phyto <- phyto %>%
  mutate(Filter = as.numeric(Filter))

bin_edges2 <- data.frame(
  Filter = c(0.2, 2, 20),
  plower_bound = c(0.2, 2, 20),
  pupper_bound = c(2, 20, 200 )
) %>%
  mutate(pbin_width = pupper_bound - plower_bound)

# Merge with zoops data and calculate normalized biomass
# Create columns with  calculated normalized biomass and then add log biomass and  log size columns 
pnorm <- phyto %>%
  left_join(bin_edges2, by = "Filter") %>%
  mutate(p_normalized_biomass = Chlorophyll / pbin_width)

# Plot!
library(ggpmisc)
ggplot(pnorm, aes(x = log10(Filter), y = log10(p_normalized_biomass))) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ Station, ncol = 4) +
  labs(
    x = expression(log[10]~"Size Fraction [µm]"),
    y = expression(log[10]~"Nomalized Biomass [g]"),
    title = "Phytoplankton Biomass Spectrum by Station"
  ) +
  theme_bw(base_size = 12)
# ggsave('ZoopSizeAbun_linearR_Normlog10.png', width=10, height = 5.625, dpi = 300, units = 'in')

ggplot(pnorm, aes(x = log2(Filter), y = log2(p_normalized_biomass))) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ Station, ncol = 4) +
  labs(
    x = expression(log[2]~"Size Fraction [µm]"),
    y = expression(log[2]~"Nomalized Biomass [g]"),
    title = "Phytoplankton Biomass Spectrum by Station"
  ) +
  theme_bw(base_size = 12)
# ggsave('ZoopSizeAbun_linearR_Normlog2.png', width=10, height = 5.625, dpi = 300, units = 'in')


# ----------------------------------------------------------------------------
# Bar chart for one depth 
selected_depth <- 0
df_filtered <- subset(phyto, Depth == selected_depth)

ggplot(df_filtered, aes(x = Cast, y=Chlorophyll)) +
  scale_x_continuous(breaks = unique(phyto$Cast)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_bw()
# ggsave('ChlCast_bar.png', width=10, height = 5.625, dpi = 300)

# Bar chart for all depths
ggplot(phyto, aes(x = Depth, y = Chlorophyll)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), nrow = 1) +
  # scale_x_discrete(breaks = unique(phyto$Cast)) +
  labs(title = "Chlorophyll at All Depths",
       x = "Depth",
       y = "Chlorophyll (mg/m^3)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))
# ggsave('ChlCast_Depth_bar.png', width=10, height = 5.625, dpi = 300)


ggplot(phyto, aes(x = Cast, y = Chlorophyll)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~ Depth, nrow = 1) +
  # scale_x_discrete(breaks = unique(phyto$Cast)) +
  labs(title = "Chlorophyll at All Depths",
       x = "Depth",
       y = "Chlorophyll (mg/m^3)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# ggsave('ChlDepth_Cast_bar.png', width=10, height = 5.625, dpi = 300)

 # Bubble Figure
  
a0.2 = 
  phyto %>% 
     filter(Size==0.2) %>% 
   ggplot(aes(x = as.factor(Cast), y = Depth, size = Chlorophyll)) +
     geom_point(alpha = 0.6, color ="darkgreen") +
     scale_y_reverse() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      xlab('Cast') + ylab('Depth [m])') +
    labs(title = "SE2204 Chlorophyll concentration", subtitle = "Plotted for chlorophyll sampled from 0.2 μm filter") +
     theme_minimal() +
     # facet_wrap(Size~., ncol=1)
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  a0.2
  # ggsave('Chl0.2_cast_bubble.png', width=10, height = 5.625, dpi = 300)
  
 a2.0 = 
     phyto %>% 
    filter(Size==2) %>% 
    ggplot(aes(x = as.factor(Cast), y = Depth, size = Chlorophyll)) +
   geom_point(alpha = 0.6, color ="darkgreen") +
     scale_y_reverse() +
   xlab('Cast') + ylab('Depth [m])') +
   labs(title = "SE2204 Chlorophyll concentration", subtitle = "Plotted for chlorophyll sampled from 2.0 μm filter") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
 a2.0 
 # ggsave('Chl2.0_cast_bubble.png', width=10, height = 5.625, dpi = 300)
 
 a20 = 
     phyto %>% 
     filter(Size==20) %>% 
     ggplot(aes(x = as.factor(Cast), y = Depth, size = Chlorophyll)) +
     geom_point(alpha = 0.6, color ="darkgreen") +
     scale_y_reverse() +
   xlab('Cast') + ylab('Depth [m])') +
   labs(title = "SE2204 Chlorophyll concentration", subtitle = "Plotted for chlorophyll sampled from 20.0 μm filter") +
   theme_minimal() +
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
 a20
 # ggsave('Chl20_cast_bubble.png', width=10, height = 5.625, dpi = 300)
 
 cowplot::plot_grid(a0.2, a2.0, a20, nrow = 1)
 
 # ggsave('ChlBubblePlot_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
 # ggsave('ChlBubblePlot_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')
 
 
 # Heatmap
 p20 <- phyto %>% 
   filter(Size == 20) %>% 
   ggplot() +
    geom_tile(aes(x=as.factor(Cast), y=Depth, fill=Chlorophyll)) +
    scale_y_reverse() +
    scale_x_discrete(labels = label_vector) +
    scale_fill_viridis_c(option = 'turbo') +
    xlab('Station') + 
    ylab('Depth [m]') +
    # labs(title = "SE2204 Chlorophyll concentration", subtitle = "Plotted for chlorophyll sampled from 20.0 μm filter") +
    theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 p20
# ggsave('ChlHeatMap20_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')
 
 p2 <- phyto %>% 
   filter(Size == 2) %>% 
   ggplot() +
   geom_tile(aes(x=as.factor(Cast), y=Depth, fill=Chlorophyll)) +
   scale_y_reverse() +
   scale_x_discrete(labels = label_vector) +
   scale_fill_viridis_c(option = 'turbo') +
   xlab('Station') + 
   ylab('Depth [m]') +
   # labs(title = "SE2204 Chlorophyll concentration", subtitle = "Plotted for chlorophyll sampled from 2.0 μm filter") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 p2
 # ggsave('ChlHeatMap2_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')
 
 p0.2 <- phyto %>% 
   filter(Size == 0.2) %>% 
   ggplot() +
   geom_tile(aes(x=as.factor(Cast), y=Depth, fill=Chlorophyll)) +
   scale_y_reverse() +
   scale_x_discrete(labels = label_vector) +
   scale_fill_viridis_c(option = 'turbo') +
   xlab('Station') + 
   ylab('Depth [m]') +
   # labs(title = "SE2204 Chlorophyll concentration", subtitle = "Plotted for chlorophyll sampled from 0.2 μm filter") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 p0.2
 
 # ggsave('ChlHeatMap0.2_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')


 
# scatter plot
phyto %>% filter(Depth == 0) %>% 
  ggplot(aes(x=Size, y=Chlorophyll)) + 
   geom_point() +
   geom_smooth(method='lm', se=F, linewidth = 0.75, colour = 'darkgreen', alpha = 0.2) +
   stat_poly_eq(
     aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
     formula = y ~ x,
     parse = TRUE,
     size = 3) +
   facet_wrap(.~Cast, labeller=labeller(Cast=id.labs), nrow = 3) +
   scale_x_log10() +
   scale_y_log10() +
   xlab(expression('Size ('*mu*'g)')) + ylab(expression('Chlorophyll ('*mu*'g/L)')) +
   ggtitle('Size vs. Abundance for phytoplankton SE2204') +
   theme_bw() + theme(panel.grid = element_blank()) 
  
# Filter Size Breakdown at Each Station 
phyto_f <- phyto %>%
  filter(Size!=0.7) %>%
  mutate(size_class = case_when(
    Filter == 20 ~ ">20 µm",
    Filter == 2.0 ~ "2.0 - 19.99 µm",
    Filter == 0.2 ~ "0.2 - 1.99 µm",
    TRUE ~ "Unknown"
  ))

chl_summary <- phyto_f %>%
  group_by(Depth, Station, Cast) %>%
  mutate(total_chl = sum(Chlorophyll, na.rm = TRUE),
  percent_chl = (Chlorophyll / total_chl) * 100)

# Putting the filter size in chronological order to make plotting easy and clean 
chl_summary$size_class <- factor(chl_summary$size_class, levels = c("0.2 - 1.99 µm", "2.0 - 19.99 µm", ">20 µm" ))

# filter out Na's
chl_summary <- chl_summary %>%
  filter(!is.na(Cast), !is.na(percent_chl), !is.na(size_class))


ggplot(chl_summary, aes(x = Depth, y = percent_chl, fill = size_class)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("#495d86", "#d9b021", "#d26424")) +
  # scale_fill_manual(values = c("#646298", "#72a5a9", "#D75739")) +
  scale_x_reverse(expand = c(0,0)) +  
  facet_wrap(~Cast, labeller=labeller(Cast=id.labs)) + 
  coord_flip() +
  labs(title = "Size-fractionated Chlorophyll by Depth",
       x = "Depth (m)",
       y = "% of Total Chlorophyll",
       fill = "Size Class") +
  theme_bw()
# ggsave('ChlSizeFraction_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')


# --------------------------------- ZOOPLANKTON -------------------------------
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

# -----------------------------------------------------------------------------
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

# Plot!
# Plot of all normalized data together, LOG 10********
ggplot(normalized_biomass, aes(x = log_normalized_size, y = log_normalized_biomass, color = as.factor(net_cast_number))) +
  geom_point() +
  geom_line() +
  scale_color_paletteer_d("ggsci::category20_d3") +
  labs(
    x = expression(log[10]~"Size [μm]"),
    y = expression(log[10]~"Normalized Biomass [g]"),
    color = "Station",
    title = "Normalized Zooplankton Biomass Spectrum") +
  theme_bw()
# ggsave('ZoopSizeAbun_linearR_log10.png', width=10, height = 5.625, dpi = 300, units = 'in')

library(ggpmisc)
ggplot(normalized_biomass, aes(x = log_normalized_size, y = log_normalized_biomass)) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ net_cast_number, ncol = 4) +
  labs(
    x = expression(log[10]~"Size Fraction [µm]"),
    y = expression(log[10]~"Nomalized Biomass [g]"),
    title = "Zooplankton Biomass Spectrum by Station"
  ) +
  theme_bw(base_size = 12)
# ggsave('ZoopSizeAbun_linearR_Normlog10.png', width=10, height = 5.625, dpi = 300, units = 'in')



ggplot(normalized_biomass, aes(x = log2(size_fraction), y = log2(normalized_biomass))) +
  geom_point(color = "#8ab69c") +
  geom_smooth(method = "lm", se = FALSE, color = "#49755b", linewidth = 0.6) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3,
    label.x = "right",
    label.y = "top"
  ) +
  facet_wrap(~ net_cast_number, ncol = 4) +
  labs(
    x = expression(log[2]~"Size Fraction [µm]"),
    y = expression(log[2]~"Nomalized Biomass [g]"),
    title = "Zooplankton Biomass Spectrum by Station"
  ) +
  theme_bw(base_size = 12)
# ggsave('ZoopSizeAbun_linearR_Normlog2.png', width=10, height = 5.625, dpi = 300, units = 'in')



# -----------------------------------------------------------------------------
# make zooplankton plots
# Bubble Figure
ggplot(zoops, aes( x =net_cast_number, y =size_fraction, size =net_dry_weight)) +
  geom_point(shape=21, fill='#00A572', color='black', alpha=0.7) +
  scale_size_continuous(name = "Weight (g)") +
  labs(title = "Zooplankton Biomass by Station and Size Fraction",
       x = "Station",
       y = "Size Fraction [µm]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave('Zoop_bubbleDepth.png', width=10, height = 5.625, dpi = 300, units = 'in')


# LM for all Casts
ggplot(zoops, aes(x=size_fraction, y=net_dry_weight, group=as.factor(net_cast_number), color=as.factor(net_cast_number))) +
  scale_color_viridis_d() +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  labs(title = "Zooplankton ",
       x = "Size Fraction [µm]",
       y = "Net dry weight [g]", 
       color = "Station") +
  theme_bw()
ggsave('Zoop_LM.png', width=10, height = 5.625, dpi = 300, units = 'in')

# Panels for all casts and their linear regression lines/equation
zoops %>% 
  ggplot(aes(x=size_fraction, y=net_dry_weight)) + 
  geom_point() +
  geom_smooth(method='lm', se=F, linewidth = 0.75, colour = '#00A572', alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3) +
  facet_wrap(.~net_cast_number) +
  # scale_x_log10() +
  # scale_y_log10() +
  xlab('Size') + ylab('Abundance (dry weight [g])') +
  ggtitle('Size vs. Abundance for Zooplankton SE2204') +
  theme_minimal() 

# ggsave('SizeAbunPanel_RegLine.pdf', width=11, height = 8, dpi = 300, units = 'in')
# ggsave('ZoopSizeAbun_linearR.png', width=10, height = 5.625, dpi = 300, units = 'in')

# Log Scale x and y
zoops %>% 
  ggplot(aes(x=size_fraction, y=net_dry_weight)) + 
  geom_point() +
  geom_smooth(method='lm', se=F, linewidth = 0.75, colour = '#00A572', alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3) +
  facet_wrap(.~net_cast_number) +
  scale_x_log10() +
  scale_y_log10() +
  xlab('Size') + ylab('Abundance (dry weight [g])') +
  ggtitle('Size vs. Abundance for Zooplankton SE2204') +
  theme_minimal() 
# ggsave('ZoopSizeAbun_linearR_log10.png', width=10, height = 5.625, dpi = 300, units = 'in')

zoopsSub <- zoops[which(zoops$net_cast_number== 1),]
zoopsLm <- lm(net_dry_weight ~ size_fraction, data=zoopsSub)
zoopsLm$coefficients
zoopsLm$coefficients[2]

# Size Fractional Plot 
zoops_f <- zoops %>%
  mutate(Fraction = case_when(
    size_fraction == 200 ~ ">200 µm",
    size_fraction == 500 ~ "200 - 499 µm",
    size_fraction == 1000 ~ "500 - 999 µm",
    size_fraction == 2000 ~ "1000 - 1999 µm",
    size_fraction == 5000 ~ "2000 - 4999 µm",
    TRUE ~ "Unknown"
  ))  %>%
  mutate(Fraction = factor(Fraction, levels = c(
    ">200 µm",
    "200 - 499 µm",
    "500 - 999 µm",
    "1000 - 1999 µm",
    "2000 - 4999 µm",
    "Unknown"
  )))

zoops_summary <- zoops_f %>%
  group_by(net_cast_number) %>%
  mutate(total_zoops = sum(net_dry_weight, na.rm = TRUE),
         percent_zoops = (net_dry_weight / total_zoops) * 100)

zoops_summary <- zoops_summary %>%
  mutate(net_cast_number = as.numeric(net_cast_number))

# cast_order <- zoops_summary %>%
#   group_by(net_cast_number) %>%
#   summarise(total_biomass = sum(net_dry_weight, na.rm = TRUE)) %>%
#   arrange(desc(total_biomass))
# 
# zoops_summary <- zoops_summary %>%
#   mutate(net_cast_number = factor(net_cast_number,
#                                   levels = cast_order$net_cast_number))


# Size-fractionated Zooplankton stacked bar chart for proportion percentages
  ggplot(zoops_summary, aes(x = net_cast_number, y = percent_zoops, fill = Fraction)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    # scale_fill_viridis_d(option = 'mako') +
    scale_fill_paletteer_d("NatParksPalettes::Banff") +
    coord_flip() +
    labs(title = "Size-fractionated Zooplankton by Station",
         x = "Station",
         y = "% of Total Zooplankton",
         fill = "Size Class") +
    theme_bw()
# ggsave('ZoopStackedProp.png', width=10, height = 5.625, dpi = 300, units = 'in')


# Heat Maps of Zoops
  ggplot(zoops_summary, aes(x = Fraction, y = net_cast_number, fill = percent_zoops)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "turbo") +
    scale_y_reverse() +
    labs(
      title = "Heatmap of Zooplankton Biomass by Size and Station",
      x = "Size Class",
      y = "Station",
      fill = "% Biomass"
    ) +
    theme_minimal()
# ggsave('ZoopHeatMap.png', width=10, height = 5.625, dpi = 300, units = 'in')

# Stacked Bar line chart  
  ggplot(zoops_summary, aes(x = net_cast_number, y = percent_zoops, fill = Fraction)) +
    geom_area(stat = "identity", position = "stack", size=.5, colour="white") +
    # scale_fill_viridis_d(option = 'mako') +
    scale_fill_paletteer_d("NatParksPalettes::Banff") +
    scale_x_reverse() +
    labs(
      title = "Biomass Distribution Across Stations",
      x = "Station Number",
      y = "% of Total Zooplankton",
      fill = "Size Class"
    ) +
    theme_minimal()
  # ggsave('ZoopStacked.png', width=10, height = 5.625, dpi = 300, units = 'in')
  
  # Interesting plot 
  ggplot(zoops_summary, aes(x = net_cast_number, y = percent_zoops, size = percent_zoops, fill = Fraction)) +
    scale_fill_paletteer_d("NatParksPalettes::Banff") +
    geom_point(shape = 21, color = "black", alpha = 0.7) +
    scale_size(range = c(2, 10)) +
    labs(
      title = "Zooplankton Biomass Gradient by Cast and Size Class",
      x = "Station Number",
      y = "% of Total Biomass",
      fill = "Size Class",
      size = "% Biomass"
    ) +
    theme_minimal()
  # ggsave('ZoopBubble.png', width=10, height = 5.625, dpi = 300, units = 'in')
  
  
# ----------------------------------------------------------------------------
# zoopsSub <- zoops[which(zoops$net_cast_number== 2),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# 
# zoopsSub <- zoops[which(zoops$net_cast_number== 3),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# 
# zoopsSub <- zoops[which(zoops$net_cast_number== 4),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# 
# zoopsSub <- zoops[which(zoops$net_cast_number== 5),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# 
# zoopsSub <- zoops[which(zoops$net_cast_number== 5),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# 
# zoopsSub <- zoops[which(zoops$net_cast_number== 6),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 7),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 8),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 9),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 10),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 11),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 12),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]
# zoopsSub <- zoops[which(zoops$net_cast_number== 13),]
# zoopsLm <- lm(size_fraction ~ net_dry_weight, data=zoopsSub)
# zoopsLm$coefficients
# zoopsLm$coefficients[2]

# heavier dry weights to the S but smaller size fraction
# Station 5 (cast 5) as net dry weight increases so does the size 
# Station 12 as net weight decreases size fraction increases 


