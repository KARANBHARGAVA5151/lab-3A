library(dplyr)
library(tidyverse)  
library(factoextra)  
library(caret)
library(cluster)
library(ggplot2)

# Importing the data
student = read.csv("C:\\Users\\KARAN BHARGAVA\\Downloads\\oulad-students.csv")

# Analyzing the data
str(student)
summary(student)

student$result = as.factor(student$result)
student$gender = as.numeric(student$gender)
student$id = as.numeric(student$id)


# Generating learning data
set.seed(123)
no.students <- 1000
st_id = 1:no.students
f1 = rnorm(no.students, mean = 50, sd = 10)
f2 = rnorm(no.students, mean = 70, sd = 15)
student = data.frame(St_ID = st_id, F1 = f1, F2 = f2)

# Dimensionality Reduction
reduced = prcomp(student[,c("F1", "F2")], center = TRUE, scale. = TRUE)

# Plot 
fviz_eig(reduced) + ggtitle("Scree Plot")
fviz_pca_var(reduced) + ggtitle("Variable Loading Plot")
fviz_pca_biplot(reduced, repel = TRUE, title = "Biplot")

# K-means clustering
set.seed(456)
result = kmeans(reduced$x[,1:2], centers = 3, nstart = 25)

# Plot
fviz_cluster(result, student[,c("F1", "F2")]) + ggtitle("Clustering Results")

# Adding cluster labels
student$cluster(kmeans$cluster)

# Summary
summary(student$Cluster)



