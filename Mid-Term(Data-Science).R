library(readxl)
xlsx_file <- "F:/Data-Science/Project/Titanic - Modified.xlsx"
csv_file <- "F:/Data-Science/Project/Titanic - Modified.csv"

data <- read_xlsx(xlsx_file)
write.csv(data, file = csv_file, row.names = FALSE)
cat("Conversion complete...", csv_file)

data<-read.csv("F:/Data-Science/Project/Titanic - Modified.csv")
data

install.packages("dplyr")
library(dplyr)
summary(data)

names(data)
summary(data)

gender_counts <- table(data$gender)
barplot(gender_counts, col = "orange", border = "black",
        xlab = "Gender", ylab = "Count", main = "Bar Plot of Gender")

age_counts <- table(data$age)
barplot(age_counts, col = "orange", border = "black",
        xlab = "Age", ylab = "Count", main = "Bar Plot of Age")

sibs_counts <- table(data$sibs)
barplot(sibs_counts, col = "orange", border = "black",
        xlab = "Sibs", ylab = "Count", main = "Bar Plot of Sibs")

parch_counts <- table(data$parch)
barplot(parch_counts, col = "orange", border = "black",
        xlab = "Parch", ylab = "Count", main = "Bar Plot of parch")

fare_counts <- table(data$fare)
barplot(fare_counts, col = "orange", border = "black",
        xlab = "Fare", ylab = "Count", main = "Bar Plot of fare")

embarked_counts <- table(data$embarked)
barplot(embarked_counts, col = "orange", border = "black",
        xlab = "Embarked", ylab = "Count", main = "Bar Plot of Embarked")

class_counts <- table(data$class)
barplot(class_counts, col = "orange", border = "black",
        xlab = "Class", ylab = "Count", main = "Bar Plot of Class")

who_counts <- table(data$who)
barplot(who_counts, col = "orange", border = "black",
        xlab = "WHo", ylab = "Count", main = "Bar Plot of WHo")

alone_counts <- table(data$alone)
barplot(alone_counts, col = "orange", border = "black",
        xlab = "Person", ylab = "Count", main = "Bar Plot of Person")

survived_counts <- table(data$survived)
barplot(survived_counts, col = "orange", border = "black",
        xlab = "Survived", ylab = "Count", main = "Bar Plot of Survived")


data_completeness <- colMeans(!is.na(data))
print(data_completeness)

colSums(is.na(data))

data$age <- ifelse(is.na(data$age),mean(data$age, na.rm = TRUE),data$age)
colSums(is.na(data))
embarked <- names(table(data$embarked))[which.max(table(data$embarked))]
data$embarked <- ifelse(is.na(data$embarked), embarked, data$embarked)
colSums(is.na(data))
gender <- names(table(data$gender))[which.max(table(data$gender))]
data$gender <- ifelse(is.na(data$gender), gender, data$gender)
colSums(is.na(data))

class <- names(table(data$class))[which.max(table(data$class))]
data$class <- ifelse(is.na(data$class), class, data$class)
colSums(is.na(data))
data_completeness <- colMeans(!is.na(data))
print(data_completeness)

for (col in names(data)) {
  data_type <- typeof(data[[col]])
  print(paste("Column:", col, "Data Type:", data_type))
}

data$age_Category <- cut(data$age, breaks = c(0, 18, 30, 50, Inf), labels = c("Child", "Young Adult", "Adult", "Senior"))
data

data %>% summarise_if(is.numeric,sd)

data %>% summarise_if(is.null,var)

data %>% summarise_if(is.numeric,median)

data %>% summarise_if(is.numeric,mean)

names(data)[1]<- "PGender"
names(data)[2]<- "PAge"
names(data)[3]<- "PSibsp"
names(data)[4]<- "PParch"
names(data)[5]<- "PFare"
names(data)[6]<- "PEmbarked"
names(data)[7]<-  "PClass"
names(data)[8]<- "PPerson"
names(data)[9]<- "PAlone"
names(data)[10]<- "PSurvived"
names(data)

data$PGender <- factor(data$PGender,
levels = c(0,1),
labels = c("Male", "Female"))

data$PSurvived<-factor(data$PSurvived,
                       levels=c(0,1),
                       labels = c("Male","Female"))
data

normalized_data <- data[, c("PAge", "PFare")]
normalized_data$PAge <- (data$PAge - min(data$PAge, na.rm = TRUE)) / (max(data$PAge, na.rm = TRUE) - min(data$PAge, na.rm = TRUE))
print(normalized_data$PAge)

normalized_data$PFare <- (data$PFare - min(data$PFare, na.rm = TRUE)) / (max(data$PFare, na.rm = TRUE) - min(data$PFare, na.rm = TRUE))
print(normalized_data$PFare)

detect_outliers_zscore <- function(x, threshold = 3) {
  z_scores <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  outliers <- x[abs(z_scores) > threshold]
  return(outliers)
}
outliers_age <- detect_outliers_zscore(data$PAge)
outliers_fare <- detect_outliers_zscore(data$PFare)
data <- subset(data, !(PAge %in% outliers_age) & !(PFare %in% outliers_fare))
summary(data)

for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    cat("Column:", col, "\n")
    cat("Min:", min(data[[col]], na.rm = TRUE), "\n")
    cat("Median:", median(data[[col]], na.rm = TRUE), "\n")
    cat("Mean:", mean(data[[col]], na.rm = TRUE), "\n")
    cat("3rd Quartile:", quantile(data[[col]], 0.75, na.rm = TRUE), "\n")
    cat("Max:", max(data[[col]], na.rm = TRUE), "\n")
    cat("Standard Deviation:", sd(data[[col]], na.rm = TRUE), "\n")
    cat("\n")
    # Draw histogram for numeric columns
    hist(data[[col]], main = paste("Histogram of", col), xlab = col, col = "skyblue", border = "black")
    cat("\n")
  }
}

for (col in names(data)) {
  if (is.factor(data[[col]])) {
    cat("Column:", col, "\n")
    cat("Frequency Table:\n")
    cat(table(data[[col]]), "\n")
    cat("\n")
    
    barplot(table(data[[col]]), main = paste("Bar Plot of", col), xlab = col, ylab = "Frequency", col = "orange")
    cat("\n")
  }
}

install.packages("readr")
library(readr)
set.seed(42)

sample_size <- 0.7 

sampled_data <- data[sample(nrow(data), size = sample_size * nrow(data), replace = FALSE), ]

cat("Sampled Data Dimensions:", dim(data)," \n ")

cols_with_missing <- colnames(data)[apply(is.na(data), 2, any)]
for (col in cols_with_missing) {
  if (is.numeric(data[[col]])) {
    data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm = TRUE), data[[col]])
  } else {
    data[[col]] <- ifelse(is.na(data[[col]]), "Unknown", data[[col]])
  }
}

data <- unique(data)
print(cols_with_missing)

outlier_threshold <- 3  

z_scores <- (data$PAge - mean(data$PAge)) / sd(data$PAge)

outlier_indices <- which(abs(z_scores) > outlier_threshold)

noisy_values <- data$PAge[outlier_indices]

cat("Noisy Values (Outliers):\n")
print(noisy_values)

median_age <- median(data$PAge, na.rm = TRUE)
data$PAge[outlier_indices] <- median_age

z_scores <- (data$PFare - mean(data$PFare)) / sd(data$PFare)

outlier_indices <- which(abs(z_scores) > outlier_threshold)

noisy_values <- data$PFare[outlier_indices]

cat("Noisy Values (Outliers):\n")
print(noisy_values)

median_fare <- median(data$PFare, na.rm = TRUE)
data$PFare[outlier_indices] <- median_fare
data

invalid_age_indices <- which(data$PAge < 0)
invalid_fare_indices <- which(data$PFare < 0)

invalid_age_values <- data$PAge[invalid_age_indices]
invalid_fare_values <- data$PFare[invalid_fare_indices]

cat("Invalid Age Values:\n")
print(invalid_age_values)

cat("Invalid Fare Values:\n")
print(invalid_fare_values)

write_csv(data, "Titanic_Cleaned.csv")

















