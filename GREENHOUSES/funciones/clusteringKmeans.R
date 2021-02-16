clustering.kmeans<-function(df, n_clusters, features){
  nstart = 30
  feat1<-features[1]
  feat2<-features[2]
  clusters<-kmeans(df[, features], n_clusters, nstart = nstart)
  df$clusters <- factor(clusters$cluster)
  centroids <- data.frame(cluster = factor(seq(1:n_clusters)),
                          feat1 = clusters$centers[, feat1],
                          feat2 = clusters$centers[, feat2])
  return(list(df, centroids))
}