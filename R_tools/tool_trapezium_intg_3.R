# Integrating using the trapezium area rule
trapezium_intg_3 <- function(heights,x1,x2,x3){
  area <- (0.5 * (heights[2] - heights[1]) * (x1 + x2)) +
    (0.5 * (heights[3] - heights[2]) * (x2 + x3))
  
  return(area)
}