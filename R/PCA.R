# install.packages(c("FactoMineR", "factoextra"))
library(ggplot2)
library("FactoMineR")
library("factoextra")
library(readxl)

setwd("~/Documents/SET_Project")

data <- read_xlsx("LIWC-22-all-parameters.xlsx")

data.active <- data[,(3:26)] #Subset data

# Perform PCA

res.pca <- PCA(data.active, scale.unit = TRUE, graph = FALSE) # Conduct PCA on all 31 courses
print(res.pca)

# Create Scree Plot

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Get eigenvalues

eig.val <- get_eigenvalue(res.pca)
eig.val

# Extract the PCA output into a variable

var <- get_pca_var(res.pca)
var

# Coordinates:
head(var$coord, 119)
# Quality of representation:
head(var$cos2, 119)
# Contributions to the principal components:
head(var$contrib, 119)
# Correlations
res.pca$var$cor

# Correlation Circle
head(var$coord, 40)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


# Phani's data

phani <- read_xlsx("Phani's data set.xlsx")
phani.pca <- PCA(phani, scale.unit = TRUE, graph = FALSE) # Conduct PCA on all 31 courses)

phani.eigen <- get_eigenvalue(phani.pca)
phani.eigen

phani.var <- get_pca_var(phani.pca)
phani.var

fviz_eig(phani.pca, addlabels = TRUE, title = "Variances - PCA", x = "Principal Components", y = "% of variances", ylim = c(0, 50))

fviz_pca_var(phani.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)







# Coordinates:
head(phani.var$coord, 119)
# Quality of representation:
head(phani.var$cos2, 119)
# Contributions to the principal components:
head(phani.var$contrib, 119)
# Correlations
phani.var$cor
