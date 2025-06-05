#Question 1
setwd("C:/Users/USER/OneDrive/Desktop/SP/Assignment")
dataset <- read.csv("filtered_final_dataset.csv")


#Question 2
head(dataset)

summary(dataset)

                         
library(DataExplorer)
create_report(dataset)


#Question 3
#a
#Check for duplicate rows
duplicate_rows <- dataset[duplicated(dataset), ]
num_duplicates <- nrow(duplicate_rows)

#Report the number of duplicate rows  
cat("Total number of duplicate rows:", num_duplicates, "\n")

#View the duplicate rows                  ******* 22 duplictaes 
print(duplicate_rows)

#Identify and remove actual duplicates for the specific case of the 21-year-old male
dataset_clean <- dataset[!(duplicated(dataset) & 
                             dataset$Age == 21 & 
                             dataset$Height == 1.62 & 
                             dataset$Weight == 70 & 
                             dataset$Gender == "Male" & 
                             dataset$Family_history == "no" & 
                             dataset$Obesity_level == "Overweight_Level_I"), ]

#Verify that specific duplicates have been removed
cat("Number of rows before removing duplicates:", nrow(dataset), "\n")
cat("Number of rows after removing duplicates:", nrow(dataset_clean), "\n")

#b
#Check for missing values in the dataset
missing_values <- colSums(is.na(dataset_clean))

#Display the number of missing values for each column
cat("Missing values in each column:\n")
print(missing_values)                    #******* no missing values

#c
# Convert categorical variables to factors
dataset_clean$Gender <- as.factor(dataset_clean$Gender)
dataset_clean$Family_history <- as.factor(dataset_clean$Family_history)
dataset_clean$Obesity_level <- as.factor(dataset_clean$Obesity_level)

# Apply label encoding
dataset_clean$Gender <- as.numeric(dataset_clean$Gender)
dataset_clean$Family_history <- as.numeric(dataset_clean$Family_history)
dataset_clean$Obesity_level <- as.numeric(dataset_clean$Obesity_level)

# View the first few rows of the label-encoded dataset
head(dataset_clean)
dim(dataset_clean)


#d
#Generate boxplot for detecting outliers for Age, Height, and Weight together
boxplot(dataset_clean[, c("Age", "Height", "Weight")], 
        main="Boxplot for Outlier Detection (Age, Height, Weight)", 
        col=c("lightblue", "lightgreen", "lightcoral"), 
        names=c("Age", "Height", "Weight"))

# Identify outliers for a specific attribute, e.g., Age
outliers_age <- boxplot.stats(dataset_clean$Age)$out
num_outliers_age <- length(outliers_age)
print(paste("Number of outliers in Age:", num_outliers_age))

# Identify outliers for Weight
outliers_weight <- boxplot.stats(dataset_clean$Weight)$out
num_outliers_weight <- length(outliers_weight)
print(paste("Number of outliers in Weight:", num_outliers_weight))

# Define the function to identify valid (non-outliers) using the IQR method
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x >= lower_bound & x <= upper_bound)
}

# Apply the function to the Age and Weight columns
valid_age <- remove_outliers(dataset_clean$Age)
valid_weight <- remove_outliers(dataset_clean$Weight)

# Combine valid indices to ensure consistency (both columns should have valid values)
valid_data <- valid_age & valid_weight

# Filter the dataset to remove outliers from Age and Weight
dataset_no_outliers <- dataset_clean[valid_data, ]

# Check the dimensions of the cleaned data frame (no outliers in Age and Weight)
print(dim(dataset_no_outliers))

# Generate a boxplot again after removing outliers
boxplot(dataset_no_outliers[, c("Age", "Height", "Weight")], 
        main="Boxplot After Removing Outliers (Age, Height, Weight)", 
        col=c("lightblue", "lightgreen", "lightcoral"), 
        names=c("Age", "Height", "Weight"))

#density plot
library(ggplot2)
library(patchwork)
# Density for Age
p5 <- ggplot(dataset_clean, aes(x=Age)) +
  geom_density(fill="lightblue") +
  ggtitle("Age Density (Before Outlier Removal)") + theme_minimal()

p6 <- ggplot(dataset_no_outliers, aes(x=Age)) +
  geom_density(fill="lightblue") +
  ggtitle("Age Density (After Outlier Removal)") + theme_minimal()

# Density for Weight
p7 <- ggplot(dataset_clean, aes(x=Weight)) +
  geom_density(fill="lightcoral") +
  ggtitle("Weight Density (Before Outlier Removal)") + theme_minimal()

p8 <- ggplot(dataset_no_outliers, aes(x=Weight)) +
  geom_density(fill="lightcoral") +
  ggtitle("Weight Density (After Outlier Removal)") + theme_minimal()

# Side-by-side display
(p5 + p6) / (p7 + p8)


# QQ plot for Age
p15 <- ggplot(dataset_clean, aes(sample=Age)) +
  stat_qq() + stat_qq_line() +
  ggtitle("QQ Plot of Age (Before Outlier Removal)") + theme_minimal()

p16 <- ggplot(dataset_no_outliers, aes(sample=Age)) +
  stat_qq() + stat_qq_line() +
  ggtitle("QQ Plot of Age (After Outlier Removal)") + theme_minimal()

# QQ plot for Weight
p17 <- ggplot(dataset_clean, aes(sample=Weight)) +
  stat_qq() + stat_qq_line() +
  ggtitle("QQ Plot of Weight (Before Outlier Removal)") + theme_minimal()

p18 <- ggplot(dataset_no_outliers, aes(sample=Weight)) +
  stat_qq() + stat_qq_line() +
  ggtitle("QQ Plot of Weight (After Outlier Removal)") + theme_minimal()

# Side-by-side QQ plots
(p15 + p16) / (p17 + p18)


# Q4
#Descriptive statistics by Obesity Level for numerical attributes
library(dplyr)
age_stats <- dataset_no_outliers %>%
  group_by(Obesity_level) %>%
  summarise(
    Age_Mean = mean(Age, na.rm = TRUE),
    Age_Median = median(Age, na.rm = TRUE),
    Age_Mode = as.numeric(names(sort(table(Age), decreasing=TRUE))[1]),
    Age_SD = sd(Age, na.rm = TRUE),
    Age_Variance = var(Age, na.rm = TRUE)
  )

print(age_stats)  # Descriptive statistics for Height by Obesity Level
height_stats <- dataset_no_outliers %>%
  group_by(Obesity_level) %>%
  summarise(
    Height_Mean = mean(Height, na.rm = TRUE),
    Height_Median = median(Height, na.rm = TRUE),
    Height_Mode = as.numeric(names(sort(table(Height), decreasing=TRUE))[1]),
    Height_SD = sd(Height, na.rm = TRUE),
    Height_Variance = var(Height, na.rm = TRUE)
  )

print(height_stats)      # Descriptive statistics for Weight by Obesity Level
weight_stats <- dataset_no_outliers %>%
  group_by(Obesity_level) %>%
  summarise(
    Weight_Mean = mean(Weight, na.rm = TRUE),
    Weight_Median = median(Weight, na.rm = TRUE),
    Weight_Mode = as.numeric(names(sort(table(Weight), decreasing=TRUE))[1]),
    Weight_SD = sd(Weight, na.rm = TRUE),
    Weight_Variance = var(Weight, na.rm = TRUE)
  )

print(weight_stats)


#Q5
#age
library(ggplot2)
# Ensure Obesity_level is a factor with proper labels
dataset_no_outliers$Obesity_level <- factor(dataset_no_outliers$Obesity_level, 
                                levels = c("Normal_Weight", "Overweight_Level_I", "Obesity_Type_I"),
                                labels = c("Normal Weight", "Overweight Level I", "Obesity Type I"))

# Boxplot for Age by Obesity Level
ggplot(dataset, aes(x=Obesity_level, y=Age, fill=Obesity_level)) +
  geom_boxplot() 
  labs(title="Boxplot of Age by Obesity Level", x="Obesity Level", y="Age") +
  theme_minimal()

#weight
ggplot(dataset, aes(x=Obesity_level, y=Weight, fill=Obesity_level)) +
  geom_boxplot() +
  labs(title="Boxplot of Weight by Obesity Level", x="Obesity Level", y="Weight") +
  theme_minimal()

#height
ggplot(dataset, aes(x=Height, fill=Obesity_level)) +
  geom_histogram(binwidth=0.05, position="dodge") +
  facet_wrap(~Obesity_level, scales="free_y") +
  labs(title="Histogram of Height by Obesity Level", x="Height", y="Count") +
  theme_minimal()

#gender
library(ggplot2)
ggplot(dataset, aes(x = Gender, fill = Gender)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Obesity_level, scales = "free_y") +
  labs(title = "Bar Plot of Gender by Obesity Level", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("Female" = "lightblue", "Male" = "lightcoral")) +
  theme_minimal()

#family history
library(ggplot2)
# Grouped bar plot for Family History
ggplot(dataset, aes(x = Obesity_level, fill = Family_history)) +
  geom_bar(position = "dodge") +
  labs(title = "Grouped Bar Plot of Family History by Obesity Level", x = "Obesity Level", y = "Count") +
  scale_fill_manual(values = c("yes" = "lightblue", "no" = "lightcoral")) +
  theme_minimal()


#Correlation
library(corrplot)
library(colorspace)
numeric_vars <- dataset %>% select_if(is.numeric)  # Select only numeric variables
cor_matrix <- cor(numeric_vars, use = "complete.obs")  # Compute correlation matrix
# Plot correlation matrix
corrplot(cor_matrix, method="color", 
         col=colorspace::diverge_hcl(200), 
         addCoef.col = "black", 
         tl.col = "black", 
         title = "Correlation Matrix of Numerical Variables")

