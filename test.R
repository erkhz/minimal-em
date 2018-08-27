source("emStep.R")
data("iris")
# random starting point
clusters <- sample(c(1,2,3), nrow(iris), replace = TRUE)
clusters2 <- emStep(iris[,-5], clusters)$clusters
par(mfrow = c(1,2))
plot(iris[,1], iris[,3], col = clusters)
title("Before clustering")
plot(iris[,1], iris[,3], col = clusters2)
title("After one EM step")
