# Load the packages
library(corpcor)
library(GPArotation)
library(psych)

# Load data
raqData<-read.delim("raq.dat", header = TRUE)
raqData
head(raqData)
tail(raqData)

# Create a correlation matrix
raqMatrix<-cor(raqData)
round(raqMatrix, 2)

# Bartlett's Test of Sphericity
cortest.bartlett(raqData) # Using Raw Data
cortest.bartlett(raqMatrix, n = 2571) # Using Correlation Matrix

# Determinant of the R-matrix 
det(raqMatrix)

# KMO Test (Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy)
KMO(raqData)

# Factor extraction Using PCA Method
'principal(dataframe/R-matrix, nfactors = number of factors, rotate =
"method of rotation", scores = TRUE/FALSE)'
pc1 <- principal(raqData, nfactors = 23, rotate = "none") # Using Raw Data
pc1 <- principal(raqMatrix, nfactors = 23, rotate = "none") # Using Correlation Matrix
# These commands create a model called pc1, which extracts 23 factors

# Check the communalities & amount of unique variance for each variable
pc1$communality
pc1$uniquenesses

# Eigen Values
eigenvalues <- pc1$values
eigenvalues

# Scree Plot
plot(pc1$values, type = "b")

# Rerun the analysis by specifying the Number of extracted factors
pc2 <- principal(raqMatrix, nfactors = 4, rotate = "none")
pc2
print.psych(pc2, cut = 0.3, sort = TRUE)

# Factor Rotation
'----------------'
# 1) Orthogonal rotation (varimax) 
pc3 <-  principal(raqData, nfactors = 4, rotate = "varimax")
print.psych(pc3, cut = 0.3, sort = TRUE)

# 2) Oblique rotation (oblimin) 
pc4 <-  principal(raqData, nfactors = 4, rotate = "oblimin")
print.psych(pc4, cut = 0.3, sort = TRUE)


# Factor Score
'-------------'
pc5 <- principal(raqData, nfactors = 4, rotate = "oblimin", scores = TRUE)
pc5$scores

# Combining the raw data and factor score in a data frame
raqData <- cbind(raqData, pc5$scores)
raqData

# Reliability analysis
computerFear<-raqData[,c(6, 7, 10, 13, 14, 15, 18)]
statisticsFear <- raqData[, c(1, 3, 4, 5, 12, 16, 20, 21)]
mathFear <- raqData[, c(8, 11, 17)]
peerEvaluation <- raqData[, c(2, 9, 19, 22, 23)]

# Cronbach’s α
cf <- alpha(computerFear)
cf
cf$total

alpha(statisticsFear, keys = c(1, -1, 1, 1, 1, 1, 1, 1)) # Because Q3 (the negatively scored - because inverse question) here it is second item
alpha(mathFear)
alpha(peerEvaluation)









