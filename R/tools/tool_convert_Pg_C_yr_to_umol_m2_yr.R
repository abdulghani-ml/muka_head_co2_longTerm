Pg_C_yr_to_umol_m2_yr <- function(Pg_C_yr, A = 1) {
  # Convert Pg C yr-1 to umol m-2 s-1
  # Pg_C_yr is in Pg C per year
  # A is in km2
  umol_m2_s <- Pg_C_yr * 10^12  # Convert Peta C per year to kilo C per year
  umol_m2_s <- umol_m2_s / (365 * 24 * 60 * 60) # Convert yr to s
  umol_m2_s <- umol_m2_s * 12 # Convert kg C to kgmol C
  umol_m2_s <- umol_m2_s * 1000 # Convert kgmol C to gmol C
  umol_m2_s <- umol_m2_s * 10^6 # Convert gmol C to umol C
  umol_m2_s <- umol_m2_s / (A * 10^6) # Calculate flux for a certain area, A, and convert from km2 to m2.
  
}