##########################
## adhesin heatmap_Mean ##
##########################

#set working directory 
setwd("C:/Users/o_weinberv/Nextcloud/Viktoria/PHD/01_VW PAPERS/VESICLE PAPER/PROTEOMICS/final_proteomics_files")

#load packages
library(reshape2)
library(ggplot2)

#Import data
data <- read.csv("adhesin_annotation_mean.csv", sep = ";")
View(data)


#adjust data frame
df_melted <- melt(data, id.vars = "Category", variable.name = "Condition", value.name = "Value")

# Check the melted data frame structure
print(df_melted)
View(df_melted)

heatmap_plot <- ggplot(df_melted, aes(x = Condition, y = Category)) +
  geom_tile(aes(fill = Value), color = "darkgray") +
  scale_fill_gradient2(low = "wheat4", high = "steelblue4", mid = "white", name = "Value", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Display the plot
print(heatmap_plot)

svglite::svglite(filename = "adhesin_heatmap_mean.svg", width = 10, height = 8)
print(heatmap_plot)
dev.off()
