# Skelleton script for making abundance size plots

# load libraries
install.packages("ggpmisc")  # Only if not installed
library(ggpmisc)
library(dplyr)
library(readxl)
library(here)
library(ggplot2)
library(lubridate)
library(tidyr)
library(data.table)

# load the data
phyto <- read.csv(paste(here(), 'fluorometry_SE2204.csv', sep='/'))
head(phyto)

# Clean up the phytoplankton data and make sure and turn filter into sizes
phyto <- phyto %>% 
  filter(Filter != 'bulk') %>% 
  mutate(Size=as.numeric(Filter))
head(phyto)
id.labs <- phyto$Cast
id.labs

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

ggsave('PhytoT_scatter.png', width=10, height = 5.625, dpi = 300)

# Bar chart for one depth 
selected_depth <- 0
df_filtered <- subset(phyto, Depth == selected_depth)

ggplot(df_filtered, aes(x = Cast, y=Chlorophyll)) +
  scale_x_continuous(breaks = unique(phyto$Cast)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_bar(stat = "identity", fill = "darkgreen")

ggsave('ChlCast_bar.png', width=10, height = 5.625, dpi = 300)

# Bar chart for all depths
ggplot(phyto, aes(x = Depth, y = Chlorophyll)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~ Cast, nrow = 1) +
  # scale_x_discrete(breaks = unique(phyto$Cast)) +
  labs(title = "Chlorophyll at All Depths",
       x = "Depth",
       y = "Chlorophyll (mg/m^3)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))

ggsave('ChlCast_Depth_bar.png', width=10, height = 5.625, dpi = 300)


ggplot(phyto, aes(x = Cast, y = Chlorophyll)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~ Depth, nrow = 1) +
  # scale_x_discrete(breaks = unique(phyto$Cast)) +
  labs(title = "Chlorophyll at All Depths",
       x = "Depth",
       y = "Chlorophyll (mg/m^3)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

ggsave('ChlDepth_Cast_bar.png', width=10, height = 5.625, dpi = 300)

 # Bubble Figure
  a.02 = 
  phyto %>% 
     filter(Size==0.2) %>% 
   ggplot(aes(x = as.factor(Cast), y = Depth, size = Chlorophyll)) +
     geom_point(alpha = 0.6, color ="darkgreen") +
     scale_y_reverse() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      xlab('Cast') + ylab('Depth [m])') +
    ggtitle('Chlorophyll Concentration (bubble size) SE2204') +
     theme_minimal() +
     # facet_wrap(Size~., ncol=1)
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  a.02
  ggsave('Chl.02_cast_bubble.png', width=10, height = 5.625, dpi = 300)
  
 a.20 = 
     phyto %>% 
    filter(Size==0.2) %>% 
    ggplot(aes(x = as.factor(Cast), y = Depth, size = Chlorophyll)) +
   geom_point(alpha = 0.6, color ="darkgreen") +
     scale_y_reverse() +
   xlab('Cast') + ylab('Depth [m])') +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
 ggsave('Chl.20_cast_bubble.png', width=10, height = 5.625, dpi = 300)
 
 a20 = 
     phyto %>% 
     filter(Size==0.2) %>% 
     ggplot(aes(x = as.factor(Cast), y = Depth, size = Chlorophyll)) +
     geom_point(alpha = 0.6, color ="darkgreen") +
     scale_y_reverse() +
   xlab('Cast') + ylab('Depth [m])') +
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
 ggsave('Chl20_cast_bubble.png', width=10, height = 5.625, dpi = 300)
 
 cowplot::plot_grid(a.02, a.20, a20, nrow = 1)
 
 ggsave('ChlBubblePlot_AllStns.pdf', width=11, height = 8, dpi = 300, units = 'in')
 ggsave('ChlBubblePlot_AllStns.png', width=10, height = 5.625, dpi = 300, units = 'in')
 
 
 
 
 
 
# --------------------------------- ZOOPLANKTON -------------------------------
zoops <- read_xlsx(paste(here(), 'Biomass filter weights.xlsx', sep='/'), sheet = 1)
head(zoops)

# Clean up zooplankton data
zoops <- zoops %>% 
  filter(net_cast_number <= 13) %>% 
  select(net_cast_number, size_fraction, net_dry_weight) %>% 
  group_by(net_cast_number, size_fraction) %>% 
  summarise(net_dry_weight=sum(net_dry_weight))

zoopTrend <- lm(size_fraction ~ net_dry_weight, data=zoops)

# make zooplankton plots
# Bubble Figure
ggplot(zoops, aes( x =net_cast_number, y =size_fraction, size =net_dry_weight)) +
  geom_point(shape=21, fill='#00A572', color='black', alpha=0.7) +
  scale_size_continuous(name = "Weight (g)") +
  labs(title = "Zooplankton Biomass by Cast and Size Fraction",
       x = "Net Cast Number",
       y = "Size Fraction (µm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('Zoop_bubble.png', width=10, height = 5.625, dpi = 300, units = 'in')


# LM for all Casts
ggplot(zoops, aes(x=size_fraction, y=net_dry_weight, group=as.factor(net_cast_number), color=as.factor(net_cast_number))) +
  scale_color_viridis_d() +
  geom_point() + 
  geom_smooth(method='lm', se=F)
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

ggsave('SizeAbunPanel_RegLine.pdf', width=11, height = 8, dpi = 300, units = 'in')
ggsave('ZoopSizeAbun_linearR.png', width=10, height = 5.625, dpi = 300, units = 'in')

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
ggsave('ZoopSizeAbun_linearR_log10.png', width=10, height = 5.625, dpi = 300, units = 'in')

zoopsSub <- zoops[which(zoops$net_cast_number== 1),]
zoopsLm <- lm(net_dry_weight ~ size_fraction, data=zoopsSub)
zoopsLm$coefficients
zoopsLm$coefficients[2]

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


