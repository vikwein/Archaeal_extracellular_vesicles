# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)  # For arranging multiple plots

# Define a function for plotting
plot_metabolite <- function(data, title) {
  summary_data <- data %>%
    group_by(samples) %>%
    summarize(mean_norm = mean(norm_area),
              sd_norm = sd(norm_area),
              .groups = 'drop')
  
  data$samples <- factor(data$samples, levels = c("M. SMITHII ALI", "M. INTESTINI", "MEDIUM"))
  summary_data$samples <- factor(summary_data$samples, levels = c("M. SMITHII ALI", "M. INTESTINI", "MEDIUM"))
  
  ggplot(data, aes(x = samples, y = norm_area, color = samples, fill = samples)) +
    geom_jitter(width = 0.15, size = 4, alpha = 0.8, shape = 16) +
    geom_point(data = summary_data, aes(y = mean_norm), size = 6, shape = 18, color = "black") +
    geom_errorbar(data = summary_data, aes(y = mean_norm, ymin = mean_norm - sd_norm, ymax = mean_norm + sd_norm), 
                  width = 0.15, lwd = 1.2, color = "black") +
    labs(title = title, x = " ", y = "Normalized Peak Area") +
    scale_color_manual(values = c("M. SMITHII ALI" = "#016450", "M. INTESTINI" = "#d4a017", "MEDIUM" = "#7F7F7F")) +
    scale_fill_manual(values = c("M. SMITHII ALI" = "#016450", "M. INTESTINI" = "#d4a017", "MEDIUM" = "#7F7F7F")) +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 10, angle = 30, hjust = 1, face = "bold", color = "#4D4D4D"),
          axis.text.y = element_text(size = 10, face = "bold", color = "#4D4D4D"),
          axis.title = element_text(size =10, face = "bold"),
          panel.grid.major.y = element_line(color = "gray60", linetype = "dashed"),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
}

# Oxalic Acid
oxalic_data <- data.frame(
  samples = c(rep("M. SMITHII ALI", 3), rep("M. INTESTINI", 3), rep("MEDIUM", 2)),
  norm_area = c(20752179, 30541086, 29721857, 22495119, 27799927, 21223158, 21790339, 19168585)
)
plot_oxalic <- plot_metabolite(oxalic_data, "Oxalic Acid")

# Aspartic Acid
aspartic_data <- data.frame(
  samples = c(rep("M. SMITHII ALI", 3), rep("M. INTESTINI", 3), rep("MEDIUM", 2)),
  norm_area = c(6169003, 1585309, 10604133, 47140111, 9336077, 23217815, 737236, 346709)
)
plot_aspartic <- plot_metabolite(aspartic_data, "L-(+)-Aspartic Acid")

# Salicylic Acid
salicylic_data <- data.frame(
  samples = c(rep("M. SMITHII ALI", 3), rep("M. INTESTINI", 3), rep("MEDIUM", 2)),
  norm_area = c(9224109, 6223309, 10015630, 11595791, 5142877, 9790206, 5418493, 5313741)
)
plot_salicylic <- plot_metabolite(salicylic_data, "Salicylic Acid")

# Glutamic Acid
glutamic_data <- data.frame(
  samples = c(rep("M. SMITHII ALI", 3), rep("M. INTESTINI", 3), rep("MEDIUM", 2)),
  norm_area = c(22594620, 14579469, 31625608, 81819523, 24710770, 51098603, 9825361, 9390199)
)
plot_glutamic <- plot_metabolite(glutamic_data, "L-(+)-Glutamic Acid")

# Arginine
arginine_data <- data.frame(
  samples = c("M. SMITHII ALI", "M. SMITHII ALI", "M. SMITHII ALI", "M. INTESTINI", "M. INTESTINI", "M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(5632274, 375776, 8633584, 3316116, 672296, 2009596, 604200, 187604)
)
plot_arginine <- plot_metabolite(arginine_data, "L-(+)-Arginine")

# Choline glycerophosphate
cholineglycerophosphate_data <- data.frame(
  samples = c("M. SMITHII ALI", "M. SMITHII ALI", "M. SMITHII ALI", "M. INTESTINI", "M. INTESTINI", "M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(3069622,2804314,3004487,7999183,2674479,3729186,2467644,648959)
)
plot_cholineglycerophosphate <- plot_metabolite(cholineglycerophosphate_data, "Choline glycerophosphate")

# Arrange all plots
grid.arrange(plot_oxalic, plot_aspartic, plot_salicylic, plot_glutamic, plot_cholineglycerophosphate, plot_arginine, ncol = 3)
