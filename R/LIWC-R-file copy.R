# LIWC 

install.packages("readxl")
install.packages("psych")
install.packages("corrplot")

library(ggplot2)
library(e1071)
library(dplyr)
library(readxl)
library(psych)
library(corrplot)

setwd("~/Documents/SET_Project")

# Import CSV

liwc <- read_xlsx("liwc_analysis.xlsx")
liwc

# Subset the data 

liwc_subset <- liwc[,c(1,18:95)]
liwc_subset # clean

# Correlation matrix of Summary Variables

cor(liwc_subset[c(3:6)])

corrplot(cor(liwc_subset[c(3:6)]), method = "circle")
corrplot(cor(liwc_subset[c(3:6)]), method = "number")
         
# First metric - environment

liwc_1 <- liwc[,c(26, 29, 62:66, 56)]
liwc_1
corrplot(cor(liwc_1), method = "number") # CorrMatrix with nums
corrplot(cor(liwc_1), method = "circle") # CorrMatrix with circles

# Second metric - breadth of learning

liwc_2 <- liwc[,c(19, 30, 32, 27:28, 39, 82:88)]
liwc_2

corrplot(cor(liwc_2), method = "number") # CorrMatrix with nums
corrplot(cor(liwc_2), method = "circle") # CorrMatrix with circles

# Third metric - student disposition 

liwc_3 <- liwc[,c(33, 40:48, 50:57, 68:71, 93, 95)]
liwc_3

corrplot(cor(liwc_3), method = "number") # CorrMatrix with nums
corrplot(cor(liwc_3), method = "circle") # CorrMatrix with circles

# Fourth metric - Course achievement

liwc_4 <- liwc[,c(27:28, 30:31, 32:38, 72:81,93, 95)]
liwc_4

corrplot(cor(liwc_4), method = "number") # CorrMatrix with nums
corrplot(cor(liwc_4), method = "circle") # CorrMatrix with circles


# Cross-metric analysis

frq_tb <- (with(liwc, table(liwc[,19],liwc[, 33]))) #contingency table defining attribute "frq_tb"
frq_tb # Visualize
colnames(frq_tb) <- c("Daily smoking","Moderate smoking","Not at all","Skipped","Unstated")
rownames(frq_tb) <- c("Daily drinking","Moderate drinking","Occasional drinking", "Not at all", "Unstated")
frq_tb # Visualize

# 