library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readxl)

setwd("~/Documents/SET_Project")

data <- read_xlsx("LIWC-22 parameters sheet.xlsx", sheet="K-means") # Read data 
                                                                    # set

#Subset and scale data

data.active <- (data[c(4:111)]) %>% scale() 
typeof(data.active)

distance <- get_dist(data.active)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(data.active, centers = 2, nstart = 25)
str(k2)
typeof(k2)
k2

fviz_cluster(k2, data=data.active) # View cluster

# Determine the optimal number of clusters

set.seed(123)
 
  fviz_nbclust(data.active, kmeans, method = "wss")

  fviz_nbclust(data.active, kmeans, method = "silhouette")

  gap_stat <- clusGap(data.active, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
  # Print the result
  print(gap_stat, method = "firstmax")

  fviz_gap_stat(gap_stat)

  # function to compute total within-cluster sum of square 
  wss <- function(k) {
    kmeans(data.active, k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:15
  
  # extract wss for 2-15 clusters
  wss_values <- map_dbl(k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")





# Test data

df <- USArrests

k3 <- kmeans(df, centers = 2, nstart = 25)

df %>%
  as_tibble() %>%
  mutate(cluster = k3$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(df, kmeans, method = "wss")
