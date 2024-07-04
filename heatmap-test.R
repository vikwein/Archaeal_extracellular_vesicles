#adhesin heatmap##

#set working directory 
setwd("/Users/Ich/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/Figures_Tables/heatmap")

#load packages
library(reshape2)
library(ggplot2)

#Import data
data <- read.csv("adhesin_annotation.csv", sep = ";")
View(data)

 
#adjust data frame
df_melted <- melt(df, id.vars = "Category", variable.name = "Condition", value.name = "Value")

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

svglite::svglite(filename = "adhesin_heatmap.svg", width = 10, height = 8)
print(heatmap_plot)
dev.off()

###Heatmap summary###

library(reshape2)
library(ggplot2)

setwd("/Users/Ich/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/Figures_Tables/heatmap")
#Import data
data <- read.csv("prep_heatmap_1.csv", sep = ";")
View(data)

#Melt your data
data_melted <- melt(data, id.vars = "T_Protein", variable.name = "Condition", value.name = "Value")
View(data_melted)

#Create Plot
heatmap_plot <- ggplot(data_melted, aes(x = Condition, y = T_Protein)) +
  geom_tile(aes(fill = Value), color = "darkgray") +
  scale_fill_gradient2(low = "wheat4", high = "steelblue4", mid = "white", name = "Value", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Display the plot
print(heatmap_plot)

svglite::svglite(filename = "summary_heatmap.svg", width = 10, height = 40) #vorher 10 + 8, 10,30=gut
print(heatmap_plot)
dev.off()

### Heatmap all annotations ###

setwd("/Users/Ich/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/PROTEOMICS/final_proteomics_files")
#Import data
data <- read.csv("intensities_v2_annotations.csv", sep = ";")
View(data)

#Melt your data
data_melted <- melt(data, id.vars = "Category", variable.name = "Condition", value.name = "Value")
View(data_melted)

#Create Plot
heatmap_plot3 <- ggplot(data_melted, aes(x = Condition, y = Category)) +
  geom_tile(aes(fill = Value), color = "darkgray") +
  scale_fill_gradient2(low = "wheat4", high = "steelblue4", mid = "white", name = "Value", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Display the plot
print(heatmap_plot3)

svglite::svglite(filename = "all_annotations_heatmap_2.svg", width = 7, height = 25) #vorher 10 + 8, 10,30=gut
print(heatmap_plot3)
dev.off()
 