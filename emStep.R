# given data and current labels, return new labels after single em step.
emStep <- function(data, clusters) {
  labels <- unique(clusters)
  priorProbs <- rep(0, length(labels))
  means <- array(0, c(length(labels), ncol(data)))
  Sigmas <- array(0, c(length(labels), ncol(data), ncol(data)))

  # compute prob, mu and sigma
  for (i in seq(length(labels))) {
    label <- labels[i]
    clusterData <- data[clusters == label, ]
    clusterSize <- nrow(clusterData)
    priorProbs[i] <- clusterSize / nrow(data)
    means[i, ] <- apply(clusterData, 2, sum) / clusterSize
    for (j in seq(clusterSize)) {
      res <- as.numeric(clusterData[j, ] - means[i, ])
      Sigmas[i, , ] <- Sigmas[i, , ] + res %*% t(res)
    }
    Sigmas[i, , ] <- Sigmas[i, , ] / clusterSize
  }

  # compute posterior probabilities and new clusters
  for (i in seq(nrow(data))) {
  }
  return(list(mu = means, Sigma = Sigmas))
}
