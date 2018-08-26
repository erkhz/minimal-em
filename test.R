data("iris")
ls()
# random starting point
clusters <- sample(c(1,2,3), nrow(iris), replace = TRUE)
plot(iris[,1], iris[,2], col = clusters)
