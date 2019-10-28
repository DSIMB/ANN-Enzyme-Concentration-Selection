# Load libraries ----------------------------------------------------------
library(neuralnet)
library(caret)

# Set seed for reproducible results ---------------------------------------
set.seed(1)

# Read input --------------------------------------------------------------
data <- read.csv("Input_De_Vienne.csv", header = TRUE, sep = ",")

# Normalisation of data ---------------------------------------------------
normalize <- function(x) {
  x <- as.numeric(x)
  return( (x - min(x))/ (max(x) - min(x)))
}

normalisedData <- as.data.frame(apply(data, 2,  normalize))
n <- nrow(normalisedData)

# Build model using all data ----------------------------------------------
train_data <- normalisedData
form <- as.formula("Jobs~PGI+PFK+FBA+TPI")
model <- neuralnet(form, train_data, hidden = 13, act.fct = "logistic")

# Generate new enzyme concentration ---------------------------------------
maxData <- apply(data[,1:4], 2, max)
minData <- apply(data[,1:4], 2, min)
maxJobs <- max(data$Jobs)  
minJobs <- min(data$Jobs)

stepsize <- 1
seqPGI <- seq(minData["PGI"], maxData["PGI"], stepsize)
seqPFK <- seq(minData["PFK"], maxData["PFK"], stepsize)
seqTPI <- seq(minData["TPI"], maxData["TPI"], stepsize)
seqPGIPFK <- cbind(rep(seqPGI, each = length(seqPFK)), rep(seqPFK, times = length(seqPGI)))

hugeMatrix <- cbind(rep(seqPGIPFK[,1], each = length(seqTPI)), rep(seqPGIPFK[,2], each = length(seqTPI)), rep(seqTPI, times = nrow(seqPGIPFK))) 
hugeMatrix <- cbind(hugeMatrix, 101.9 - apply(hugeMatrix,1,sum))
colnames(hugeMatrix) <-  c("PGI","PFK","TPI", "FBA")

bool <- apply(hugeMatrix[,1:2],1,sum) > 101.9
hugeMatrix[bool, 4] <- 0
hugeMatrix[bool, 3] <- 0
bool <- apply(hugeMatrix[,1:3],1,sum) > 101.9
hugeMatrix[bool,3] <- 101.9 - hugeMatrix[bool,1] - hugeMatrix[bool,2]
hugeMatrix[bool, 4] <- 0
bool <- duplicated(hugeMatrix)
hugeMatrix <- hugeMatrix[!bool,]

myOrder <- order(hugeMatrix[,1], hugeMatrix[,2], hugeMatrix[,3])
hugeMatrix <- hugeMatrix[myOrder,]

hugeMatrix <- hugeMatrix[,c(1,2,4,3)]

# Normalising new data ----------------------------------------------------
normNewData <- t((t(hugeMatrix) - minData) / (maxData - minData))

# Predict flux for new enzyme concentrations  -----------------------------
pred <- (compute(model, normNewData)$net.result) * (maxJobs - minJobs) + minJobs
allpred <- cbind(hugeMatrix,pred)

# summary(pred)
who <- which(pred > 12 )
result <- cbind(hugeMatrix[who, ],pred[who])
colnames(result) <- c("PGI","PFK", "FBA","TPI","JANN")

# write.csv(result, file= "Higher_Flux_Enzyme_Concentration.csv", row.names = F)

# 3D-plot for the predicted flux ------------------------------------------
library("plot3D")
library("plot3Drgl")

# plot graph for predicted flux above 5?M/s-----------------------------------
bool <- pred > 5
  
x <- allpred[bool,1]
y <- allpred[bool,2]
z <-  allpred[bool,4]
cv <- allpred[bool,5]

scatter3D(x, y, z, colvar = cv , col = NULL, add = FALSE, pch = 19, bty ="f", ticktype = "detailed", theta = -55, phi = -25,
          xlab = "PGI in mg/l", ylab = "PFK in mg/l", zlab = "TPI in mg/l")
plotrgl()
