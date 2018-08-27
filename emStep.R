#===================================================#
# Minimal EM clustering function. Does one EM step. #
#===================================================#

# multivariate normal pdf in terms of mean and precision := covariance^-1
mvNormPdf <- function(x, mu, precision) {
  r <- as.numeric(x - mu)
  dens <- exp(-0.5 * t(r) %*% precision %*% r)
  normalization <- sqrt(det(precision)) / (2 * pi)^(length(x) / 2)
  return(dens / normalization)
}

# compute the max posterior probability cluster from the summary stats
mapCluster <- function(x, labels, priorProbs, means, precisions) {
  nLabels <- length(labels)
  posteriorProbs <- rep(0, nLabels)
  for (i in seq(nLabels)) {
    posteriorProbs[i] <- priorProbs[i] * mvNormPdf(x, means[i, ], precisions[i, , ])
  }
  # since we only want the maximum, normalization is not needed
  return(labels[which.max(posteriorProbs)])
}

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
  precs <- Sigmas # precision matrices
  for (i in dim(precs)[1]) {
    precs[i, , ] <- solve(precs[i, , ])
  }
  output <- rep(NaN, length(clusters))
  for (i in seq(nrow(data))) {
    output[i] <- mapCluster(data[i, ], labels, priorProbs, means, precs)
  }
  return(list(mu = means, Sigma = Sigmas, clusters = output))
}
