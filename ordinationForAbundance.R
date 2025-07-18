# Take the phyloseq object and use the built-in wrapper function 
# to ordinate using a principle coordinate analysis & the bray-curtis dissimilarity
# Bray-Curtis discerns abundance differences between samples 
library('tidyverse')
ps <- readRDS('Reads_PhyloseqObject.RDS')
# otu_table <- reads@otu_table

# readRDS('ZOTU_PhyloseqObject.RDS')

all_pcoa <- ordinate(
  physeq = ps, 
  method = "PCoA",
  distance = "bray"
)

# Named colors for each station
station_colors <- c("A" = "cornflowerblue",
                    "B" = "yellow3", 
                    "C" = "purple",
                    "D" = "limegreen", 
                    "E" = "indianred2")


# ReadCounts
# Built-in general ordination plot creates ggplot2 object


readOrdin = plot_ordination(
  physeq = ps, # Phyloseq Object
  ordination = all_pcoa) + # Ordinated Phyloseq object.
  geom_point(aes(color = station), size = 3) + 
  scale_shape_manual(values = c(21, 22)) +
  scale_color_manual(values = station_colors) + # Use defined colors to discern station
  theme_classic() +
  theme( # Legend formatting tweaks
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black"))+
  theme(axis.text.y.left = element_text(size = 20), # Axis font tweaks
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  labs(title = "Station Similarity Based on Reads Abundance (Bray-Curtis Dissimilarity)",
       subtitle = "Ellipses represent a 95% confidence interval") +
  stat_ellipse(aes(group = station, color = station), type = "t", level = 0.95) # 95% confidence ellipses.

# View plot in R
readOrdin

# Export for presentation/publication
ggsave("phyloseq/reads_ordination.png", readOrdin,
       width = 14, height = 10, dpi = 300., bg = "white")