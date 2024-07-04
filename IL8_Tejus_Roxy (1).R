###  Normalization of vesicle properites ###
#install.packages("FSA")
# load libs
library(ggplot2)
library(ggbeeswarm)
library(ggpol)
library(gghalves)
library(svglite)
library(reshape2)
library(dplyr)
library(FSA)
library(ggsignif)

#set working directory + load data frame 
setwd("C:/Users/o_weinberv/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/Figures_Tables/HT-29")

#choose the right csv file: Normalization_"DNA"/"size"/"RNA"/"Protein"/"Lipid"
dataIL8<- read.table('EVs_MUG_particles.tsv', header = T, sep = '\t')
View(dataIL8)

# RESHAPE data from wide format to long format
dataIL8_long <- melt(dataIL8, id.vars = c('ELISA_Round'), variable.name = 'SampleStrain', value.name = 'EV_counts')

# add factors to strain names and reorder
Strains_names <- c('X01_NTC', 'X02_ETEC','X03_B.fragilis','X04_M. smithii ALI','X05_M. intestini', 'X06_M. smithii GRAZ-2','X07_M. stadtmanae')

dataIL8_long$SampleStrain <- factor(dataIL8_long$SampleStrain) #, levels = Strains_names) 


# check strain order in factors
levels(dataIL8_long$SampleStrain)
str(dataIL8_long$SampleStrain)

# Intestini -> '#e6bb18ff' & smithi -> '#018571ff', GRAZ-2 '#80cdc1ff', stm '#a67a1aff'
# colors for the strains
Colors_Strains <- c('#D3D3D3', '#A9A9A9', '#404040', '#e6bb18ff','#018571ff', '#80cdc1ff', '#a67a1aff')

# Calculate statistics
kruskal_test <- kruskal.test(EV_counts ~ SampleStrain, data = dataIL8_long)
dunn_test <- dunnTest(EV_counts ~ SampleStrain, data = dataIL8_long, method = "bh")

# Extract significant comparisons and format them for significance annotation
dunn_results <- dunn_test$res
dunn_results <- dunn_results %>% filter(grepl("NTC", Comparison)) %>%
  mutate(signif = case_when(
    P.adj < 0.001 ~ "***",
    P.adj < 0.01 ~ "**",
    P.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))

#Print the adjusted p value  results, in case you want the results 
print(dunn_results)


# Create comparison list and annotation list for geom_signif for visualization 
comparisons <- strsplit(dunn_results$Comparison, " - ")
annotations <- dunn_results$signif

# Calculate median and IQR for further visualization 
summary_stats <- dataIL8_long %>%
  group_by(SampleStrain) %>%
  summarize(
    median = median(EV_counts, na.rm = TRUE),
    ymin = quantile(EV_counts, 0.25, na.rm = TRUE),
    ymax = quantile(EV_counts, 0.75, na.rm = TRUE)
  )


# Plot with median_ interquartile range
p <- ggplot(data = summary_stats, aes(x = SampleStrain, y = median, fill = SampleStrain)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  labs(title = "HT29 stimulation", x = "Strains", y = "IL-8 [pg/mL] per 10^8 particles") +
  theme_minimal() +
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "white"))+      # Adjust the fill color and transparency as needed
        axis.text.x=element_text(face="italic") #make the x axis italic 
# Add significance annotations separately for each comparison
base_y_pos <- max(summary_stats$ymax) + 120  # base y position (change to increase the distance between lines of significances)
increment_y_pos <- 70  # increment y position to increase distance 

# Add significance annotations separately for each comparison
for (i in seq_along(comparisons)) {
  comp <- comparisons[[i]]
  annotation <- annotations[i]
  y_pos <- max(summary_stats$ymax) + 70 + 120 * i  # adjust the y position for each comparison
  p <- p + geom_signif(
    comparisons = list(comp),
    annotations = annotation,
    tip_length = 0.01, #Change to increase or decrease the length of the tips of lines 
    textsize = 4, #Change to decrease or increase the star sizes 
    y_position = y_pos,
    map_signif_level = TRUE
  )
}

print(p)

svglite::svglite(filename = "HT29_IL8.svg", width = 10, height = 8)
print(p)
dev.off()
  

  DNA <- ggplot(dataNORM, aes(x= Strain, y= normalized_DNA, fill=Strain))+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_boxplot(data = dataNORM, aes(x = Strain, y = normalized_DNA, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "DNA", x = "Strains", y = "Size (nm/10^10 particles)") +
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "white"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic

