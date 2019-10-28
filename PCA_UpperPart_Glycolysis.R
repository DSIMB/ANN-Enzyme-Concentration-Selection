# Load libraries -------------------------------------
library("Factoshiny")
library("plot3D")
library("plot3Drgl")

# Read input ---------------------------------------------------------
data <- read.table("Input_De_Vienne.csv", header = T, sep = ',')

conc <- data[, c(1:4,6)]
pcaRes <- Factoshiny::PCAshiny(conc)

#plot data after PCA-------------------------------------------
dataPCA <- pcaRes$ind$coord
scatter3D(dataPCA[,1], dataPCA[,2], dataPCA[,3], colvar = data$Jobs, col = NULL, add = FALSE, pch = 19, cex = 2, ticktype = "detailed", xlab = "Dim. 1", ylab = "Dim. 2", zlab = "Dim. 3")

