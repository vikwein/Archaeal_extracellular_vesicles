##############################################
## Proteomics bar plot with individual dots ##
##############################################
# adapted CME script

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(svglite)

#Set working directory
setwd("C:/Users/o_weinberv/Nextcloud/AG Moissl-Eichinger_people/Viktoria/05_Final_VESICLE_PAPER/Figures/Fig 3c")

# Import the data (make sure the data is in a tab-separated format or CSV)

# Figure 3 
data <- read.table("proteomics_data_Fig.3c.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Reshape the data to long format for ggplot
data_long <- melt(data, id.vars = "Category", variable.name = "Experiment", value.name = "Value")

# Remove NA values
data_long <- na.omit(data_long)

# Calculate mean value for each category and experiment
data_mean <- data_long %>%
  group_by(Category, Experiment) %>%
  summarize(Mean = mean(Value))

# Create the bar plot with individual data points (dots in the middle)
p <- ggplot() +
  # Bar plot for mean values
  geom_point(data = data_long, aes(x = Category, y = Value, fill = Experiment), 
             position = position_dodge(width = 0.7), size = 0.6, shape = 21, stroke = 0.4) +
  scale_fill_manual(values = c("#018571ff", "#e6bb18ff", "#01453bff","#92760ff0")) +
  
  geom_bar(data = data_mean, aes(x = Category, y = Mean, fill = Experiment), 
           stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
  scale_fill_manual(values = c("#018571ff", "#e6bb18ff", "#01453bff","#92760ff0")) +
  theme_minimal() +
  labs(x = "Category", y = "Mean Intensity", title = "Grouped Barplot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank())

p

# Save the plot as an SVG file
svglite::svglite(filename = "Proteomics_figure_3c_1.svg", width = 13, height = 8)
print(p)
dev.off()



## Proteomics figure 

#Set working directory
setwd("/Users/Ich/Nextcloud/AG Moissl-Eichinger_people/Viktoria/05_Final_VESICLE_PAPER/Figures/Fig 3c")

# Import the data (make sure the data is in a tab-separated format or CSV)

# Figure S5
data <- read.table("proteomics_data_Fig_S5.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Reshape the data to long format for ggplot
data_long <- melt(data, id.vars = "Annotation", variable.name = "Experiment", value.name = "Value")

# Remove NA values
data_long <- na.omit(data_long)

# Calculate mean value for each category and experiment
data_mean <- data_long %>%
  group_by(Annotation, Experiment) %>%
  summarize(Mean = mean(Value))

# Create the bar plot with individual data points (dots in the middle)
p <- ggplot() +
  geom_bar(data = data_mean, aes(x = Annotation, y = Mean, fill = Experiment), 
           stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#018571ff", "#e6bb18ff", "#01453bff","#92760ff0")) +
  geom_point(data = data_long, aes(x = Annotation, y = Value, fill = Experiment), 
             position = position_dodge(width = 0.7), size = 0.6, shape = 21, stroke = 0.4, color = "black") +
  theme_minimal() +
  labs(x = "Category", y = "Mean Intensity", title = "Grouped Barplot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank())

p

# Save the plot as an SVG file
svglite::svglite(filename = "Proteomics_figure_S5_1.svg", width = 10, height = 8)
print(p)
dev.off()



