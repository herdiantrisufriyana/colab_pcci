arrange_cluster_by_pc1 <- function(data){
  data |>
    left_join(new_cluster, by = join_by(cluster)) |>
    mutate(cluster = new_cluster) |>
    select(-new_cluster)
}