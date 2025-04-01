###### Cell_viability plots with dots ######


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(svglite)


#### HT-29 Cytokine response ####

# Set working directory and load data
setwd("C:/Users/o_weinberv/Nextcloud/Viktoria/PHD/01_VW PAPERS/VESICLE PAPER/Revision/HT-29")

data <- read.csv("data_himadri_ht29.csv", sep = ";")

#Calculate the mean of the control group (01_Control)
control_mean <- mean(data$Viability[data$Strain == "NTC"])

#Normalize the viability values to the control group mean
data <- data %>%
  mutate(Viability_Percent = (Viability / control_mean) * 100)

#Summarize the data by Strain
summarized_data <- data %>%
  group_by(Strain) %>%
  summarize(
    Mean = mean(Viability_Percent),
    SD = sd(Viability_Percent),
    SE = SD / sqrt(n())
  )

#Round the normalized viability values
data <- data %>%
  mutate(Viability_Percent = round(Viability_Percent, 2))

# Define the exact desired order for the Strain column
desired_order <- c(
  "NTC", 
  "ETEC 0.1x 10^8", "B. fragilis 0.1x 10^8", "M. smithii ALI 0.1x 10^8", 
  "M. intestini 0.1x 10^8", "M. smithii GRAZ-2 0.1x 10^8", "M. stadtmanae 0.1x 10^8", 
  "ETEC 1x 10^8", "B. fragilis 1x 10^8", "M. smithii ALI 1x 10^8", 
  "M. intestini 1x 10^8", "M. smithii GRAZ-2 1x 10^8", "M. stadtmanae 1x 10^8", 
  "ETEC 10 x 10^8", "B. fragilis 10 x 10^8", "M. smithii ALI 10 x 10^8", 
  "M. intestini 10 x 10^8", "M. smithii GRAZ-2 10 x 10^8", "M. stadtmanae 10 x 10^8"
)

# Apply the desired order to the Strain column in raw_data
data$Strain <- factor(data$Strain, levels = desired_order)

# Assign dose levels to raw_data
data$Dose <- factor(
  case_when(
    grepl("0.1x 10\\^8", data$Strain) ~ "0.1x 10^8",
    grepl("1x 10\\^8", data$Strain) ~ "1x 10^8",
    grepl("10 x 10\\^8", data$Strain) ~ "10x 10^8",
    TRUE ~ "Control"
  ),
  levels = c("Control", "0.1x 10^8", "1x 10^8", "10x 10^8")
)


# Assign dose levels to summarized_data (using the same logic)
summarized_data$Dose <- factor(
  case_when(
    grepl("0.1x 10\\^8", summarized_data$Strain) ~ "0.1x 10^8",
    grepl("1x 10\\^8", summarized_data$Strain) ~ "1x 10^8",
    grepl("10 x 10\\^8", summarized_data$Strain) ~ "10x 10^8",
    TRUE ~ "Control"
  ),
  levels = c("Control", "0.1x 10^8", "1x 10^8", "10x 10^8")
)

# Create the plot with bars, error bars, and individual data points
p <- ggplot() +
  # Bar plot for the summarized means
  geom_bar(data = summarized_data, aes(x = Strain, y = Mean), 
           stat = "identity", fill = "lightblue", width = 0.7) +
  # Error bars for the standard deviation
  geom_errorbar(data = summarized_data, aes(x = Strain, ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2) +
  # Jittered points for individual data observations
  geom_jitter(data = data, aes(x = Strain, y = Viability_Percent), 
              width = 0.15, color = "black", alpha = 0.6) +
  labs(x = "Strain", y = "Cell Viability (%)", 
       title = "Cell Viability of Different Strains by Dose") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray69"),
    panel.grid.minor = element_line(color = "gray69")
  ) +
  facet_grid(~ Dose, scales = "free_x", space = "free_x")

p

# Save the plot as an SVG file
svglite::svglite(filename = "HT29_22Cytokines_final_with_dots.svg", width = 10, height = 8)
print(p)
dev.off()

#### THP-1 Cytokine response ####

data <- read.csv("beate_thp1_prep_1.csv", sep = ";")

#Calculate the mean of the control group (01_Control)
control_mean <- mean(data$Viability[data$Strain == "NTC"])

#Normalize the viability values to the control group mean
data <- data %>%
  mutate(Viability_Percent = (Viability / control_mean) * 100)

#Summarize the data by Strain
summarized_data <- data %>%
  group_by(Strain) %>%
  summarize(
    Mean = mean(Viability_Percent),
    SD = sd(Viability_Percent),
    SE = SD / sqrt(n())
  )

#Round the normalized viability values
data <- data %>%
  mutate(Viability_Percent = round(Viability_Percent, 2))

# Add the Dose column (manually mapped here as an example)
data <- data %>%
  mutate(Dose = case_when(
    Strain == "NTC" ~ "Control",
    Strain == "ETEC_1x10^8" ~ "1x10^8",
    Strain == "BF_1x10^8" ~ "1x10^8",
    Strain == "ALI_1x10^8" ~ "1x10^8",
    Strain == "INT_1x10^8" ~ "1x10^8",
    Strain == "G2_1x10^8" ~ "1x10^8",
    Strain == "STM_1x10^8" ~ "10x10^8",
    Strain == "ETEC_10x10^8" ~ "10x10^8",
    Strain == "BF_10x10^8" ~ "10x10^8",
    Strain == "ALI_10x10^8" ~ "10x10^8",
    Strain == "INT_10x10^8" ~ "10x10^8",
    Strain == "G2_10x10^8" ~ "10x10^8",
    Strain == "STM_10x10^8" ~ "10x10^8"
  ))

# Add the full Strain names with doses
data <- data %>%
  mutate(Full_Strain = case_when(
    Strain == "NTC" ~ "NTC",
    Strain == "ETEC_B" & Dose == "1x10^8" ~ "ETEC_1x10^8",
    Strain == "BF_B" & Dose == "1x10^8" ~ "BF_1x10^8",
    Strain == "ALI_B" & Dose == "10x10^8" ~ "ALI_10x10^8"
  ))

# Define the desired strain order
desired_order <- c(
  "NTC",
  "ETEC_1x10^8", "BF_1x10^8", "ALI_1x10^8", "INT_1x10^8", "G2_1x10^8", "STM_1x10^8",
  "ETEC_10x10^8", "BF_10x10^8", "ALI_10x10^8", "INT_10x10^8", "G2_10x10^8", "STM_10x10^8"
)


# Apply the desired order to the Full_Strain column
summarized_data$Strain <- factor(summarized_data$Strain, levels = desired_order)

# Add a column for Dose for facetting (extract from strain names)
summarized_data$Dose <- case_when(
  grepl("1x10\\^8", summarized_data$Strain) ~ "1x10^8",
  grepl("10x10\\^8", summarized_data$Strain) ~ "10x10^8",
  TRUE ~ "Control"
)

# Create the plot with bars, error bars, and individual data points
p <- ggplot() +
  # Bar plot for the summarized means
  geom_bar(data = summarized_data, aes(x = Strain, y = Mean), 
           stat = "identity", fill = "lightblue", width = 0.7) +
  # Error bars for the standard deviation
  geom_errorbar(data = summarized_data, aes(x = Strain, ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2) +
  # Jittered points for individual data observations
  geom_jitter(data = data, aes(x = Strain, y = Viability_Percent), 
              width = 0.15, color = "black", alpha = 0.6) +
  labs(x = "Strain", y = "Cell Viability (%)", 
       title = "Cell Viability of Different Strains by Dose") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray69"),
    panel.grid.minor = element_line(color = "gray69")
  ) +
  facet_grid(~ Dose, scales = "free_x", space = "free_x")

p

# Save the plot as an SVG file
svglite::svglite(filename = "THP1_22Cytokines_final_with_dots_1.svg", width = 10, height = 8)
print(p)
dev.off()


#### THP-1 Confocal microscopy ####

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data (replace the file path with the actual file location)
setwd("C:/Users/o_weinberv/Nextcloud/Viktoria/PHD/01_VW PAPERS/VESICLE PAPER/Revision/HT-29")

data <- read.csv("emiliy_thp1_prep.csv", sep = ";")

#Calculate the mean of the control group (01_Control)
control_mean <- mean(data$Viability[data$Strain == "01_Control"])

#Normalize the viability values to the control group mean
data <- data %>%
  mutate(Viability_Percent = (Viability / control_mean) * 100)

#Summarize the data by Strain
summarized_data <- data %>%
  group_by(Strain) %>%
  summarize(
    Mean = mean(Viability_Percent),
    SD = sd(Viability_Percent),
    SE = SD / sqrt(n())
  )

#Round the normalized viability values
data <- data %>%
  mutate(Viability_Percent = round(Viability_Percent, 2))

#Create the plot with bars, error bars, and individual data points
p <- ggplot() +
  # Bar plot for the summarized means
  geom_bar(data = summarized_data, aes(x = Strain, y = Mean), 
           stat = "identity", fill = "lightblue", width = 0.7) +
  # Error bars for the standard deviation
  geom_errorbar(data = summarized_data, aes(x = Strain, ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2) +
  # Jittered points for individual data observations (using normalized values)
  geom_jitter(data = data, aes(x = Strain, y = Viability_Percent), 
              width = 0.15, color = "black", alpha = 0.6) +
  labs(x = "Strain", y = "Cell Viability (%)", 
       title = "Cell Viability of Different Strains by Dose") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray69"),
    panel.grid.minor = element_line(color = "gray69")
  )

p

# Save the plot as an SVG file
svglite::svglite(filename = "THP1_confocal_final_with_dots.svg", width = 10, height = 8)
print(p)
dev.off()

