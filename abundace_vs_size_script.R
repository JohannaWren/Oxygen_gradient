# Skelleton script for making abundance size plots

# load libraries
library(dplyr)
library(readxl)
library(here)

# load the data
phyto <- read.csv(paste(here(), 'fluorometry_SE2204.csv', sep='/'))
head(phyto)
zoops <- read_xlsx(paste(here(), 'Biomass filter weights.xlsx', sep='/'), sheet = 1)
head(zoops)

# filter(depth=)

# Clean up the phytoplankton data and make sure and turn filter into sizes
phyto <- phyto %>% 
  filter(Filter != 'bulk') %>% 
  mutate(Size=as.numeric(Filter))

# Clean up zooplankton data
zoops <- zoops %>% 
  filter(net_cast_number <= 13) %>% 
  select(net_cast_number, size_fraction, net_dry_weight)

zoopTrend <- lm(size_fraction ~ net_dry_weight, data=zoops)

# make a zooplankton plot
# library("viridis")
# my_colors = 'viridis'
ggplot(zoops, aes(x=size_fraction, y=net_dry_weight, group=as.factor(net_cast_number), color=as.factor(net_cast_number))) +
  scale_color_viridis_d() +
  geom_point() + 
  geom_smooth(method='lm', se=F)
  
# heavier dry weights to the S but smaller size fraction
# Station 5 (cast 5) as net dry weight increases so does the size 
# Station 12 as net weight decreases size fraction increases 


