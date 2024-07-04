# Load necessary libraries (if not already installed)
# install.packages("ggplot2")
library(ggplot2)

###########################################
#ALI-INT-Medium
# Create a data frame with your data
# Load necessary libraries
library(ggplot2)

# Create the dataframe
data <- data.frame(
  samples = c("a_M. SMITHII ALI", "a_M. SMITHII ALI", "a_M. SMITHII ALI", "b_M. INTESTINI", "b_M. INTESTINI", "b_M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(20752179, 30541086, 29721857, 22495119, 27799927, 21223158, 21790339, 19168585)
)

# Create the boxplot
ggplot(data, aes(x = samples, y = norm_area, fill = samples)) +
  geom_boxplot() +
  labs(title = "Oxalic acid",
       x = "Category",
       y = "Normalized Peak Area") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("a_M. SMITHII ALI" = "#018571ff", "b_M. INTESTINI" = "#e6bb18ff", "MEDIUM" = "lightgrey")) +
  theme_minimal()


###########################################
###########################################
#ALI-INT-Medium
# Create a data frame with your data
# Load necessary libraries
library(ggplot2)

# Create the dataframe
data <- data.frame(
  samples = c("a_M. SMITHII ALI", "a_M. SMITHII ALI", "a_M. SMITHII ALI", "b_M. INTESTINI", "b_M. INTESTINI", "b_M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(5632274, 375776, 8633584, 3316116, 672296, 2009596, 604200, 187604)
)

# Create the boxplot
ggplot(data, aes(x = samples, y = norm_area, fill = samples)) +
  geom_boxplot() +
  labs(title = "L-(+)-Arginine",
       x = "Category",
       y = "Normalized Peak Area") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("a_M. SMITHII ALI" = "#018571ff", "b_M. INTESTINI" = "#e6bb18ff", "MEDIUM" = "lightgrey")) +
  theme_minimal()


###########################################
###########################################
###########################################
#ALI-INT-Medium
# Create a data frame with your data
# Load necessary libraries
library(ggplot2)

# Create the dataframe
data <- data.frame(
  samples = c("a_M. SMITHII ALI", "a_M. SMITHII ALI", "a_M. SMITHII ALI", "b_M. INTESTINI", "b_M. INTESTINI", "b_M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(6169003, 1585309, 10604133, 47140111, 9336077, 23217815, 737236, 346709)
)

# Create the boxplot
ggplot(data, aes(x = samples, y = norm_area, fill = samples)) +
  geom_boxplot() +
  labs(title = "L-(+)-Aspartic acid",
       x = "Category",
       y = "Normalized Peak Area") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("a_M. SMITHII ALI" = "#018571ff", "b_M. INTESTINI" = "#e6bb18ff", "MEDIUM" = "lightgrey")) +
  theme_minimal()


###########################################
###########################################
#ALI-INT-Medium
# Create a data frame with your data
# Load necessary libraries
library(ggplot2)

# Create the dataframe
data <- data.frame(
  samples = c("a_M. SMITHII ALI", "a_M. SMITHII ALI", "a_M. SMITHII ALI", "b_M. INTESTINI", "b_M. INTESTINI", "b_M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(9224109,6223309,10015630,11595791,5142877,9790206,5418493,5313741)
)

# Create the boxplot
ggplot(data, aes(x = samples, y = norm_area, fill = samples)) +
  geom_boxplot() +
  labs(title = "Salicylic acid",
       x = "Category",
       y = "Normalized Peak Area") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("a_M. SMITHII ALI" = "#018571ff", "b_M. INTESTINI" = "#e6bb18ff", "MEDIUM" = "lightgrey")) +
  theme_minimal()


###########################################
###########################################
#ALI-INT-Medium
# Create a data frame with your data
# Load necessary libraries
library(ggplot2)

# Create the dataframe
data <- data.frame(
  samples = c("a_M. SMITHII ALI", "a_M. SMITHII ALI", "a_M. SMITHII ALI", "b_M. INTESTINI", "b_M. INTESTINI", "b_M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(22594620,14579469,31625608,81819523,24710770,51098603,9825361,9390199)
)

# Create the boxplot
ggplot(data, aes(x = samples, y = norm_area, fill = samples)) +
  geom_boxplot() +
  labs(title = "L-(+)-Glutamic acid",
       x = "Category",
       y = "Normalized Peak Area") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("a_M. SMITHII ALI" = "#018571ff", "b_M. INTESTINI" = "#e6bb18ff", "MEDIUM" = "lightgrey")) +
  theme_minimal()


###########################################
###########################################
###########################################
#ALI-INT-Medium
# Create a data frame with your data
# Load necessary libraries
library(ggplot2)

# Create the dataframe
data <- data.frame(
  samples = c("a_M. SMITHII ALI", "a_M. SMITHII ALI", "a_M. SMITHII ALI", "b_M. INTESTINI", "b_M. INTESTINI", "b_M. INTESTINI", "MEDIUM", "MEDIUM"),
  norm_area = c(3069622,2804314,3004487,7999183,2674479,3729186,2467644,648959)
)

# Create the boxplot
ggplot(data, aes(x = samples, y = norm_area, fill = samples)) +
  geom_boxplot() +
  labs(title = "Choline glycerophosphate",
       x = "Category",
       y = "Normalized Peak Area") +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("a_M. SMITHII ALI" = "#018571ff", "b_M. INTESTINI" = "#e6bb18ff", "MEDIUM" = "lightgrey")) +
  theme_minimal()

###########################################


