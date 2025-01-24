find_intersections <- function(density1, density2){
  density1$x[!(density1$y == 0 & density2$y == 0)][
    abs(density1$y - density2$y)[!(density1$y == 0 & density2$y == 0)] |>
      which.min()
  ]
}