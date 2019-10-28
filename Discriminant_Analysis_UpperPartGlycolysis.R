# Load libraries ----------------------------------------------------------
library(klaR)

# Read input --------------------------------------------------------------
data <- read.table("Input_De_Vienne.csv", header = T, sep = ',')

# Breaking data into Groups -----------------------------------------------
data$G <- as.factor(as.numeric(cut(data$Jobs, breaks = 5)))

# Performning Discriminant analysis (method could be changed) -------------
x11(width = 15, height = 10)
partimat(G ~ PGI + PFK + FBA + TPI, data = data, method= "rpart", main = paste("method : ", "method:rpart"))   
savePlot(paste0("rpart",'.jpg'))
graphics.off()
