kmean_cluster <- function(data, model){
  find_nearest_centroid <- function(point ,centroids){
    distances <- rowSums((t(t(centroids) - point))^2)
    return(which.min(distances))
  }
  return(apply(data, 1, find_nearest_centroid, centroids = model$centers))
}