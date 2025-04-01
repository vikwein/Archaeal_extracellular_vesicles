#########################################
## Luminex heatmap - cytokines - log10 ##
#########################################

#set working directory 
setwd("C:/Users/o_weinberv/Nextcloud/Viktoria/PHD/01_VW PAPERS/VESICLE PAPER/Revision/Luminex")

#load packages
library(reshape2)
library(ggplot2)

#Import data
data <- read.csv("Prep_luminex_heatmap_log10_negative_values.csv", sep = ";")
View(data)

#adjust data frame
df_melted <- melt(data, id.vars = "cytokine", variable.name = "Condition", value.name = "Value")

# Check the melted data frame structure
print(df_melted)
View(df_melted)

#heatmap with negative values
heatmap_plot <- ggplot(df_melted, aes(x = Condition, y = cytokine)) +
  geom_tile(aes(fill = Value), color = "grey54") +
  scale_fill_gradient2(
    low = "lightyellow2",   # Color for negative values
    mid = "gray92",        # Color for zero values
    high = "steelblue4",  # Color for positive values
    midpoint = 0,         # Ensure zero is represented distinctly
    na.value = "white",  # Color for NA values (if any)
    name = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Display the plot
print(heatmap_plot)

svglite::svglite(filename = "Luminex_log10_THP1_HT29_heatmap_negative_values_2.svg", width = 10, height = 8)
print(heatmap_plot)
dev.off()
