# Integrating using the trapezium area rule
trapezium_intg_2 <- function(heights,x1,x2){
  area <- (0.5 * (heights[2] - heights[1]) * (x1 + x2))
  return(area)
}