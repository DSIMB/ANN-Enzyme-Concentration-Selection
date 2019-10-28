library(rpart)
library(rpart.plot)
library(rattle)

# Read input --------------------------------------------------------------
data <- read.table("Input_De_Vienne.csv", header = T, sep = ',')

# Breaking data into Groups -----------------------------------------------
data$Cut <- as.factor(as.numeric(cut(data$Jobs, breaks = 5)))

# Performning classification of data --------------------------------------
fit <- rpart(Cut ~ PFK + FBA, data = data)
rpart.plot(fit, type = 1)
fancyRpartPlot(fit, main = "Decision tree of Data")
