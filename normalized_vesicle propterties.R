#################################################
##  Normalized DNA, RNA and Protein content   ###
#################################################

#set working directory + load data frame 

setwd("C:/Users/o_weinberv/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/Figures_Tables/Vesicle properties/normalization")

#choose the right csv file: Normalization_"DNA"/"RNA"/"Protein"/"Lipid"
dataNORM<- read.table('normalization_protein.csv', header = T, sep = ';')
View(dataNORM)

# load libs
library(ggplot2)
library(ggbeeswarm)
library(ggpol)
library(gghalves)
library(svglite)

# add factors to strain names and reorder
Strains_names <- c('1_M. smithii ALI', '2_Ca. M. intestini', '3_M. smithii GRAZ-2', '4_M. stadtmanae')

dataNORM$Strain <- factor(dataNORM$Strain) 


# check strain order in factors
str(dataNORM$Strain)
factor(dataNORM$Strain)

# Intestini -> '#e6bb18ff' & smithi -> '#018571ff', GRAZ-2 '#80cdc1ff', stm '#a67a1aff'
# colors for the strains
Colors_Strains <- c('#018571ff','#e6bb18ff', '#80cdc1ff', '#a67a1aff')


#normalized DNA 
#calculate the mean of vesicle properties per batch and the concentration if I have more replicates
DNA <- ggplot(dataNORM, aes(x= Strain, y= normalized_DNA, fill=Strain))+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_boxplot(data = dataNORM, aes(x = Strain, y = normalized_DNA, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "DNA", x = "Strains", y = "Size (nm/10^10 particles)")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"))+      # Adjust the fill color and transparency as needed
  axis.text.x=element_text(face="italic") #make the x axis italic

DNA + ylim(0, 5)

svglite::svglite(filename = "norm_DNA_AEV.svg", width = 10, height = 8)
print(DNA + ylim(0, 5))
dev.off()

#removed outlier M. intestini E08 
DNA1 <-ggplot(dataNORM, aes(x= Strain, y= normalized_DNA, fill=Strain), drop = FALSE)+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain),position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r", width = 1.1) +
  geom_boxjitter(data = dataNORM, aes(x = Strain, y = normalized_DNA, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "DNA [ug/10^10 particles]", x = "Strains", y = "DNA [ug/10^10 particles]")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic

DNA1 #+ ylim(0, 5)

svglite::svglite(filename = "norm_DNA_AEV_1.svg", width = 10, height = 8)
print(DNA1)
dev.off()

#size
setwd("C:/Users/o_weinberv/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/Figures_Tables/Vesicle properties")

dataNORM<- read.table('AEV_prep_table_for_plot_all4.csv', header = T, sep = ';')
print(dataNORM)
View(dataNORM)

Size <- ggplot(dataNORM, aes(x= Strain, y= Size_nm, fill=Strain))+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain),position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r", width = 1.1) +
  geom_boxjitter(data = dataNORM, aes(x = Strain, y = Size_nm, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "Protein [nm/10^10 particles]", x = "Strains", y = "Protein [nm/10^10 particles]")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic
  
Size + ylim(0, 150)

svglite::svglite(filename = "norm_size_AEV.svg", width = 10, height = 8)
print(Size + ylim(0, 150))
dev.off()

#normalized_RNA

RNA1 <- ggplot(dataNORM, aes(x= Strain, y= normalized_RNA, fill=Strain), drop = FALSE)+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain),position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r", width = 1) +
  geom_boxjitter(data = dataNORM, aes(x = Strain, y = normalized_RNA, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "RNA [ug/10^10 particles]", x = "Strains", y = "RNA [ug/10^10 particles]")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic
 

RNA1 #+ ylim(0, 0.75)

svglite::svglite(filename = "norm_RNA_AEV_1.svg", width = 10, height = 8)
print(RNA1)
dev.off()


 #normalized_protein

Protein <- ggplot(dataNORM, aes(x= Strain, y= normalized_protein, fill=Strain))+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain),position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r", width = 1) +
  geom_boxjitter(data = dataNORM, aes(x = Strain, y = normalized_protein, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "Protein [nm/10^10 particles]", x = "Strains", y = "Protein [nm/10^10 particles]")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic

Protein + ylim(-20, 75)

svglite::svglite(filename = "norm_Protein_AEV1.svg", width = 10, height = 8)
print(Protein + ylim(-20, 75))
dev.off()

#removed outlier for Graz-2 #11
Protein1 <- ggplot(dataNORM, aes(x= Strain, y= normalized_protein, fill=Strain), drop = FALSE)+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain),position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r", width = 1) +
  geom_boxjitter(data = dataNORM, aes(x = Strain, y = normalized_protein, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) +
  labs (title = "Protein [ug/10^10 particles]", x = "Strains", y = "Protein [ug/10^10 particles]")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic


Protein1 #+ ylim(-10, 200)

svglite::svglite(filename = "norm_protein_AEV_1.svg", width = 10, height = 8)
print(Protein1 + ylim(-10, 50))
dev.off()

##############################################################################################
### Script to plot vesicle properties (Size, Concentration (C)Tejus ###
##############################################################################################

#set working directory + load data frame 

setwd("C:/Users/o_weinberv/Nextcloud/AG Moissl-Eichinger_people/Viktoria/02_VESICLE_PAPER/Figures_Tables/Vesicle properties")

dataDF<- read.table('AEV_prep_table_for_plot_all4.csv', header = T, sep = ';')
View(dataDF)

# load libs
library(ggplot2)
library(ggbeeswarm)
library(ggpol)
library(gghalves)
library(svglite)

# add factors to strain names and reorder
Strains_names <- c('1_M. smithii ALI', '2_Ca. M. intestini', '3_M. smithii GRAZ-2', '4_M. stadtmanae')

dataDF$Strain <- factor(dataDF$Strain) 
dataDF$Log10Conc <- log10(dataDF$Concentration_ml)

# check strain order in factors
str(dataDF$Strain)
factor(dataDF$Strain)

# Intestini -> '#e6bb18ff' & smithi -> '#018571ff', GRAZ-2 '#80cdc1ff', stm '#a67a1aff'
# colors for the strains
Colors_Strains <- c('#018571ff','#e6bb18ff', '#80cdc1ff', '#a67a1aff')

#################################################
####### Concentration and size            #######
#################################################

## concentration ##
dataDF_filtered <- subset(dataDF, Log10Conc != "NA")
View(dataDF_filtered)

threeinone <- ggplot(dataDF_filtered, aes(x= Strain, y=Log10Conc , fill=Strain))+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain), position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r") +
  geom_boxjitter(data = dataDF_filtered, aes(x = Strain, y = Log10Conc, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA) + labs (title = "Concentration [particles/ml]")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic

threeinone

svglite::svglite(filename = "conc_AEV.svg", width = 10, height = 8)
print(threeinone)
dev.off()

## size ##
size <- ggplot(dataDF_filtered, aes(x= Strain, y=Size_nm, fill=Strain))+
  scale_fill_manual(values = Colors_Strains, name = "Strain") +
  geom_half_violin(aes(fill = Strain),position = position_nudge(x = 0.2, y = 0), adjust = 1, trim = FALSE, alpha = 0.5, side = "r") +
  geom_boxjitter(data = dataDF, aes(x = Strain, y = Size_nm, fill = Strain), alpha = 1, width = 0.3, outlier.shape = NA)+labs (title = "Size [nm]")+ 
  scale_fill_manual(values = Colors_Strains, name = "Strain")+
  theme(panel.background = element_blank(), #To remove the grey background 
        panel.grid.major = element_line(color = "grey95"), # Adjust the fill color and transparency as needed 
        axis.text.x=element_text(face="italic"))    # make the x axis italic

size

svglite::svglite(filename = "size_AEV.svg", width = 10, height = 8)
print(size)
dev.off()

