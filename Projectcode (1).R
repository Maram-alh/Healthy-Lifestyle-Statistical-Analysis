

# -------------------------------------------------------
# Statistical Analysis of Healthy Lifestyle Dataset
# Author: Maram Alharbi
# Tools: R, ggplot2, dplyr
# -------------------------------------------------------


# 1. load dataset
df <- read.csv("healthy_lifestyle.csv", stringsAsFactors = FALSE)

# 1. Show NA counts per column
na_counts <- colSums(is.na(df))
na_counts

# 2. Show empty / whitespace-only string counts per column
empty_string_counts <- sapply(df, function(col) {
  if (is.factor(col)) col <- as.character(col)
  if (is.character(col)) {
    sum(trimws(col) == "" & !is.na(col))
  } else {
    0
  }
})
empty_string_counts

# 3. Convert empty / whitespace-only strings to NA (for character columns)
char_cols <- sapply(df, is.character)

df[char_cols] <- lapply(df[char_cols], function(col) {
  col[trimws(col) == ""] <- NA
  col
})

# (Optional) Check again after cleaning
colSums(is.na(df))

# 4. Remove any rows that contain NA in any column
df_clean <- na.omit(df)



#----Descriptive stat for age
summary(df$Age)  # shows Min, 1st Qu., Median, Mean, 3rd Qu., Max
sd(df$Age) # Standard deviation
getmode(df$Age) # Mode

#----Descriptive stat for occupation
# Frequency table
table(df$Occupation)

# Mode (most frequent category)
getmode(df$Occupation)

#----Descriptive stat for sleep duration
summary(df$Sleep.Duration)  # Min, 1st Qu., Median, Mean, 3rd Qu., Max
sd(df$Sleep.Duration)        # Standard deviation
getmode(df$Sleep.Duration)   # Mode

#-----Visualizations for numeric values

# Select only numeric columns
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]


par(
  mar      = c(5, 5, 4, 2) + 0.1,  # margins
  cex.main = 2.2,  # title size
  cex.lab  = 2.0,  # axis labels
  cex.axis = 1.6   # tick labels
)
# Histogram for each numeric column
for(col in names(df_numeric)) {
  hist(df_numeric[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "skyblue",
       border = "white")
}
par(
  mar      = c(5, 5, 4, 2) + 0.1,
  cex.main = 2.2,
  cex.lab  = 2.0,
  cex.axis = 1.6
)

colors()
# Boxplot for each numeric column to detect outliers
for(col in names(df_numeric)) {
  boxplot(df_numeric[[col]],
          main = paste("Boxplot of", col),
          ylab = col,
          col = "lightgreen")
}

library(e1071)
for(col in names(df_numeric)) {
  cat(col, "skewness:", skewness(df_numeric[[col]]), "\n")
}


#---- Effect of Age, Stress Level, and Physical Activity on Blood Pressure ----

# Split BP into systolic and diastolic
bp_split <- strsplit(as.character(df$Blood.Pressure), "/")

# Extract systolic and diastolic as numeric variables
df$Systolic  <- sapply(bp_split, function(x) as.numeric(x[1]))
df$Diastolic <- sapply(bp_split, function(x) as.numeric(x[2]))

# Multiple linear regression with Systolic BP as response
model <- lm(Systolic ~ Age + Stress.Level + Physical.Activity.Level, data = df)

# Show regression output
summary(model)


#----- Correlation for heart rate

# Numeric correlations
cor(df$Daily.Steps, df$Heart.Rate, use = "complete.obs")
cor(df$Age, df$Heart.Rate, use = "complete.obs")

aggregate(Heart.Rate ~ Gender, data = df, mean)
aggregate(Heart.Rate ~ Occupation, data = df, mean)


library(ggplot2)

ggplot(df, aes(x = Gender, y = Heart.Rate, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Female" = "lightgreen",
                               "Male"   = "skyblue")) +
  labs(
    title = "Heart Rate by Gender",
    x = "Gender",
    y = "Heart Rate"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                                # overall bigger text
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), # centered big title
    axis.title = element_text(size = 18),                           # big axis labels
    axis.text  = element_text(size = 14)                            # big tick labels
  )


library(dplyr)
library(ggplot2)

# 1. Remove NA or empty occupations
df_occ <- df %>%
  mutate(Occupation = trimws(Occupation)) %>%          # remove spaces
  filter(!is.na(Occupation), Occupation != "")         # drop NA & empty

# 2. Aggregate: mean Heart Rate by Occupation
occ_hr <- df_occ %>%
  group_by(Occupation) %>%
  summarise(mean_hr = mean(Heart.Rate, na.rm = TRUE)) %>%
  arrange(mean_hr)   # sort by mean

ggplot(occ_hr, aes(x = reorder(Occupation, mean_hr), y = mean_hr)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Mean Heart Rate by Occupation",
    x = "Occupation",
    y = "Mean Heart Rate"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                 # overall text size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # center + bigger title
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 14)
  )




# C. Daily Steps vs Heart Rate
ggplot(df, aes(x = Daily.Steps, y = Heart.Rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "blue", linewidth = 1.2) +
  labs(
    title = "Correlation between Daily Steps and Heart Rate",
    x = "Daily Steps",
    y = "Heart Rate"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                                 # bigger overall text
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # centered big title
    axis.title = element_text(size = 18),                            # bigger axis labels
    axis.text  = element_text(size = 14)                             # bigger tick mark labels
  )


# d. Age vs Heart Rate
ggplot(df, aes(x = Age, y = Heart.Rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "blue", linewidth = 1.2) +
  labs(
    title = "Correlation between Age and Heart Rate",
    x = "Age",
    y = "Heart Rate"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),                                  # bigger overall text
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),# centered big title
    axis.title = element_text(size = 18),                            # bigger axis labels
    axis.text  = element_text(size = 14)                             # bigger tick labels
  )


#--------- Probability of normal BMI

# 1. Probability that BMI = "Normal" given Occupation = "Doctor"
p_bmi_normal_given_doctor <- sum(df$BMI == "Normal" & df$Occupation == "Doctor", na.rm = TRUE) /
  sum(df$Occupation == "Doctor", na.rm = TRUE)

# 2. Probability that BMI = "Normal" given Gender = "Male"
p_bmi_normal_given_male <- sum(df$BMI == "Normal" & df$Gender == "Male", na.rm = TRUE) /
  sum(df$Gender == "Male", na.rm = TRUE)

# Display results
p_bmi_normal_given_doctor
p_bmi_normal_given_male

#-----------outlier for sleep duration

# 1. Summary statistics
summary(df$Sleep.Duration)

# 2. Identify outliers using the IQR method
Q1 <- quantile(df$Sleep.Duration, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Sleep.Duration, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Detect outlier values
outliers <- df$Sleep.Duration[df$Sleep.Duration < lower_bound | df$Sleep.Duration > upper_bound]
outliers

# 3. Effect of outliers: compare mean vs median
mean_sleep <- mean(df$Sleep.Duration, na.rm = TRUE)
median_sleep <- median(df$Sleep.Duration, na.rm = TRUE)

mean_sleep
median_sleep


#--------- Correlation with sleep disorder

library(ggplot2)
library(reshape2)
library(dplyr)

# Convert SleepDisorder to numeric
df$SleepDisorder.Num <- as.numeric(as.factor(df$Sleep.Disorder))

# Convert BloodPressure to numeric (extract systolic and diastolic)
df$BloodPressure.Systolic <- as.numeric(sub("/.*", "", df$Blood.Pressure))
df$BloodPressure.Diastolic <- as.numeric(sub(".*/", "", df$Blood.Pressure))

# SleepDuration is already numeric
df$Sleep.Duration <- df$Sleep.Duration  # optional if you want dot style

# Select variables for correlation
df.Corr <- df %>% select(SleepDisorder.Num, BloodPressure.Systolic, BloodPressure.Diastolic, Sleep.Duration)

# Calculate correlation
cor.Matrix <- cor(df.Corr, use = "complete.obs")
cor.Matrix

# Extract correlation of SleepDisorder with other variables
cor.With.SleepDisorder <- cor.Matrix["SleepDisorder.Num", ]
cor.With.SleepDisorder

# Melt correlation matrix for plotting
melted.Cor <- melt(cor.Matrix)

# Plot heatmap
ggplot(data = melted.Cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(title = "Correlation Heatmap", x = "", y = "")


library(ggplot2)
library(reshape2)
library(dplyr)

# Convert Sleep Disorder to numeric (0/1/2 style based on factor levels)
df$SleepDisorder.Num <- as.numeric(as.factor(df$Sleep.Disorder))

# Extract systolic and diastolic from Blood Pressure (e.g., "120/80")
df$BloodPressure.Systolic  <- as.numeric(sub("/.*", "", df$Blood.Pressure))
df$BloodPressure.Diastolic <- as.numeric(sub(".*/", "", df$Blood.Pressure))

# Sleep.Duration is already numeric

# Select variables for correlation
df.Corr <- df %>%
  select(SleepDisorder.Num,
         BloodPressure.Systolic,
         BloodPressure.Diastolic,
         Sleep.Duration)

# Optional: rename columns for nicer labels in the heatmap
colnames(df.Corr) <- c("SleepDisorder",
                       "SystolicBP",
                       "DiastolicBP",
                       "SleepDuration")

# Calculate correlation matrix
cor.Matrix <- cor(df.Corr, use = "complete.obs")
cor.Matrix

# Extract correlation of SleepDisorder with other variables (if needed)
cor.With.SleepDisorder <- cor.Matrix["SleepDisorder", ]
cor.With.SleepDisorder

# Melt correlation matrix for plotting
melted.Cor <- melt(cor.Matrix)

# Plot heatmap with bigger text and centered title
ggplot(data = melted.Cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  labs(
    title = "Correlation Heatmap: Sleep Disorder, Blood Pressure, and Sleep Duration",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),  # overall text size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), # centered, bigger title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )


# Save the cleaned dataframe to a new CSV
#write.csv(df, "df_cleaned.csv", row.names = FALSE)

# Optional: view first few rows in R
#head(df)
