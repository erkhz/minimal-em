# given data and current labels, return new labels after single em step.
emStep <- function(data, clusters) {
  labels <- unique(clusters)
  priorProbs <- rep(0, length(labels))
  means <- rep(0, length(labels), ncol(data))
  Sigmas <- rep(0, length(labels), ncol(data), ncol(data))

  # compute prob, mu and sigma
  for (i in seq(length(labels))) {
    label <- labels[i]
    clusterData <- data[clusters == label, ]
    clusterSize <- nrow(clusterData)
    priorProbs[i] <- clusterSize / nrow(data)
    means[i, ] <- apply(clusterData, 2, sum) / clusterSize
  }
  return(rep(0, nrow(data)))
}
