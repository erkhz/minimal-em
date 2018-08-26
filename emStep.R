# given data and current labels, return new labels after single em step.
emStep <- function(data, clusters) {
  labels <- unique(clusters)
  priorProbs <- rep(0, length(labels))
  # compute data
  for (i in seq(length(labels))) {
    label <- labels[i]
    clusterData <- data[clusters == label, ]
    priorProbs[i] <- nrow(clusterData) / nrow(data)
  }
  return(rep(0, nrow(data)))
}
