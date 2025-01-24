interpolate_density <- function(new_data, density){
  interp_dens <- approx(density$x, density$y, xout = new_data)
  interp_dens$y
}