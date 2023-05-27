# LIWC 

#install.packages("readxl")
#install.packages("psych")
#install.packages("corrplot")
#install.packages("tidyverse")
#install.packages("factoextra")

library(factoextra)
library(tidyverse)
library(ggplot2)
library(e1071)
library(dplyr)
library(readxl)
library(psych)
library(corrplot)

getwd()
setwd("~/Documents/SET_Project")

# Import CSV

liwc <- read_xlsx("LIWC-22 Results - Course Evaluation Summary Sheet 2022.6.28.xlsx")

# Subset the data

liwc_subset <- liwc[c(4:34),]
liwc_subset # clean

colnames(liwc_subset) <- c("course_code", "year", "lecture_code", "students_answered", 
                           "students_invited", "q1", "q2", "q3", "q4", "q5", "ICM", "q6",
                           "q9", "q10", "q11")

# Scatterplot between positive tone and negative tone

colnames(liwc_subset)[41] <- "Positive_Tone"
colnames(liwc_subset)[42] <- "Negative_Tone"

with(liwc_subset, plot(Positive_Tone ~ Negative_Tone, cex = 0.5))

# Correlation matrix of Summary Variables

cor(liwc_subset[c(20:22)])

corrplot(cor(liwc_subset[c(19:22)]), method = "circle")
corrplot(cor(liwc_subset[c(3:6)]), method = "number")
         
# Correlating ratings and PCA dimensions

liwc_ratings <- read_xlsx("LIWC-22 Results - Course Evaluation Summary Sheet 2022.6.28.xlsx", sheet = "ratings")
liwc_ratings

setwd("~/Documents/SET_Project")
sadcat <- read_xlsx("sadcat.xlsx", sheet="clean_sheet")

# Correlating ratings + Sadcat for lo
corrplot(cor(liwc_ratings[0:10],sadcat[2:13]), method = "shade")
corrplot(cor(liwc_ratings[0:10],sadcat[2:13]), method = "number")

# Correlating ratings + Sadcat for hi
corrplot(cor(liwc_ratings[0:10],sadcat[14:23]), method = "shade")
corrplot(cor(liwc_ratings[0:10],sadcat[14:25]), method = "number")






corrplot(cor(liwc_ratings[0:10],liwc_ratings[13:25]), method = "number")

corrplot(cor(liwc_ratings[0:10],liwc_ratings[15:17]), method = "number")

# Professor R's data

phani <- read_xlsx("SETs.xlsx", sheet="in")
corrplot(cor(phani[7:15],phani[43:55]), method = "shade")

test <- read_xlsx("test.xlsx")
corrplot(cor(test[0:9],test[130:132]), method = "number")

# Correlating Sadcat with ratings

corrplot(cor(sadcat[2:13], liwc_ratings[13:25]), method = 'shade')
