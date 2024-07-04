# Install and load necessary packages
install.packages("ggplot2")
library(ggplot2)

# Read the data from tab-delimited file
data <- read.delim("intensities_v2.txt")

# Check the structure of your data
str(data)

# Calculate mean values for each category and each group
library(dplyr)
data_mean <- data %>%
  mutate_at(vars(starts_with("EV_Ali"), starts_with("EV_int"), starts_with("WCL_Ali"), starts_with("WCL_Int")), function(x) as.numeric(gsub(",", "", x))) %>%
  group_by(Category) %>%
  summarise(
    EV_Ali_mean = mean(c(EV_Ali_19, EV_Ali_25, EV_Ali_26)),
    EV_int_mean = mean(c(EV_int24_2, EV_int30_2, EV_int32_1)),
    WCL_Ali_mean = mean(c(WCL_Ali_19, WCL_Ali_25, WCL_Ali_26)),
    WCL_Int_mean = mean(c(WCL_Int_24, WCL_Int_30, WCL_Int_31))
  )

# Reshape the data for plotting
data_mean_long <- tidyr::gather(data_mean, key = "Group", value = "Mean", -Category)

# Create the grouped barplot with specific colors and increased space between sets
ggplot(data_mean_long, aes(x = Category, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +  # Adjust dodge width
  scale_fill_manual(values = c("EV_Ali_mean" = "#018571ff", "EV_int_mean" = "#e6bb18ff", "WCL_Ali_mean" = "#01453bff", "WCL_Int_mean" = "#92760ff0")) +  # Set specific colors for each group
  labs(x = "Category", y = "Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####select specific functions
# Read the data from tab-delimited file
data <- read.delim("intensities_v2.txt")

# Calculate standard deviation for each category and each group
library(dplyr)

data_sd <- data %>%
  mutate_at(vars(starts_with("EV_Ali"), starts_with("EV_int"), starts_with("WCL_Ali"), starts_with("WCL_Int")), function(x) as.numeric(gsub(",", "", x))) %>%
  group_by(Annotation) %>%
  summarise(
    EV_Ali_sd = sd(c(EV_Ali_19, EV_Ali_25, EV_Ali_26)),
    EV_int_sd = sd(c(EV_int24_2, EV_int30_2, EV_int32_1)),
    WCL_Ali_sd = sd(c(WCL_Ali_19, WCL_Ali_25, WCL_Ali_26)),
    WCL_Int_sd = sd(c(WCL_Int_24, WCL_Int_30, WCL_Int_31))
  )

# Calculate mean values for each category and each group
data_mean <- data %>%
  mutate_at(vars(starts_with("EV_Ali"), starts_with("EV_int"), starts_with("WCL_Ali"), starts_with("WCL_Int")), function(x) as.numeric(gsub(",", "", x))) %>%
  group_by(Annotation) %>%
  summarise(
    EV_Ali_mean = mean(c(EV_Ali_19, EV_Ali_25, EV_Ali_26)),
    EV_int_mean = mean(c(EV_int24_2, EV_int30_2, EV_int32_1)),
    WCL_Ali_mean = mean(c(WCL_Ali_19, WCL_Ali_25, WCL_Ali_26)),
    WCL_Int_mean = mean(c(WCL_Int_24, WCL_Int_30, WCL_Int_31))
  )

# Merge mean and standard deviation data
data_summary <- left_join(data_mean, data_sd, by = "Annotation")

# Reshape the data for plotting
data_mean_long <- tidyr::gather(data_summary, key = "Group", value = "Mean", -Annotation, -ends_with("_sd"))

# Filter the data for the specific annotation (e.g., "Adenylat kinase")
specific_annotation <- data_mean_long[data_mean_long$Annotation == "Adenylat kinase", ]

# Create a new column for error bars
specific_annotation <- specific_annotation %>%
  mutate(SD = case_when(
    Group == "EV_Ali_mean" ~ EV_Ali_sd,
    Group == "EV_int_mean" ~ EV_int_sd,
    Group == "WCL_Ali_mean" ~ WCL_Ali_sd,
    Group == "WCL_Int_mean" ~ WCL_Int_sd
  ))

# Create the bar plot with error bars for the specific annotation
ggplot(specific_annotation, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
  scale_fill_manual(values = c("EV_Ali_mean" = "blue", "EV_int_mean" = "green", "WCL_Ali_mean" = "red", "WCL_Int_mean" = "purple")) +  # Set specific colors for each group
  labs(x = "Group", y = "Mean Value", title = "Mean Values for Adenylat kinase") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
